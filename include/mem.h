/*
* MIT License
* Copyright (c) 2023 mxlol233 (mxlol233@outlook.com)

* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:

* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.

* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*/

#pragma once

#include <memory>
#include "meta.h"

namespace lps::basic::mem {

namespace details {

template <meta::Str TagName>
class TraceTag {
 public:
  [[nodiscard]] virtual constexpr const char* tag() const { return TagName.value; }
};

template <meta::Str TagName, size_t N, typename T>
class Buffer : virtual public TraceTag<TagName> {
  using type = Buffer<TagName, N, T>;

 public:
  static std::unique_ptr<type> create() { return std::make_unique(type()); }
  virtual ~Buffer() = default;
  char* ptr() { return data_; }

 private:
  alignas(T) char data_[N * sizeof(T)];
};

template <meta::Str TagName, typename BlockSizeType, typename T>
class BlockInBuffer : virtual public TraceTag<TagName> {

  template <meta::Str TagName0, typename BlockSizeType0, typename T0>
  friend class Block;

 public:
  virtual ~BlockInBuffer() = default;

 protected:
  explicit BlockInBuffer(size_t buffer_size) : buffer_size_(buffer_size) {}

  bool ok() { return pos_ >= 0 && pos_ < buffer_size_; }

  void add(BlockSizeType block_size) {
    pos_ = std::min(pos_ + block_size, buffer_size_);
  }

  void remove(BlockSizeType block_size) {
    pos_ = std::max(pos_ - block_size, 0);
  }

  size_t pos_{0};  // last alloced block position on `buffer_`;
  size_t buffer_size_;
};

template <meta::Str TagName, typename Size, typename T>
class Block : virtual public TraceTag<TagName> {
  using manager_type = BlockInBuffer<TagName, Size, T>;

 private:
  Size n_;
  void* ptr_;
  manager_type& manager_;

 public:
  Block(const Block&) = delete;
  Block& operator=(const Block&) = delete;

 private:
  explicit Block(manager_type& manager, void* ptr, Size n)
      : manager_(manager), ptr_(ptr), n_(n) {
    manager_.add(n_);
  }
  virtual ~Block() { manager_.remove(n_); }
};

template <meta::Str TagName, typename Size>
class Heap : virtual public TraceTag<TagName> {
 public:
  Heap() = delete;
  virtual ~Heap() = default;

 protected:
  [[nodiscard]] size_t size() const { return size_; }
  [[nodiscard]] size_t capacity() const { return capacity_; }

  [[nodiscard]] bool empty() const { return size_; }

  Heap(void* first, Size capacity) : first_(first), capacity_(capacity) {}

  static Size new_capacity(Size min, Size old_capacity);

  void* malloc_grow(void* first, Size min, Size type_size, Size& new_capacity);

  void grow_pod(void* first, Size MinSize, Size type_size);

  void* replace(void* first, Size type_size, Size new_capacity, Size vsize = 0);

  void* first_;
  Size size_ = 0;
  Size capacity_;
};

}  // namespace details

template <meta::Str TagName, size_t NBuffer, size_t BufferSize,
          typename BlockSizeType, typename T>
class MemoryBuffer : virtual public details::TraceTag<TagName>,
                     public details::BlockInBuffer<TagName, BlockSizeType, T>,
                     public details::Heap<TagName, BlockSizeType> {
  using manager_type = details::BlockInBuffer<TagName, BlockSizeType, T>;
  using buffer_type = details::Buffer<TagName, BufferSize, T>;
  using buffer_ptr_type = std::unique_ptr<buffer_type>;
  using block_type = details::Block<TagName, BlockSizeType, T>;
  using heap_type = details::Heap<TagName, BlockSizeType>;
  using block_ptr_type =
      std::unique_ptr<details::Block<TagName, BlockSizeType, T>>;

 public:
  MemoryBuffer()
      : manager_type(BufferSize), heap_type(heap_, 2 * BufferSize + 1) {}
  MemoryBuffer(const MemoryBuffer&) = delete;
  MemoryBuffer& operator=(const MemoryBuffer&) = delete;
  virtual ~MemoryBuffer() = default;

 private:
  // One basic question arise: How to travel the list of `buffers_` as a whole single buffer?



  void expand();
  void block(BlockSizeType block_size);

  buffer_ptr_type buffers_[NBuffer];  // the specific `buffer`;
  size_t buffer_pos_{0};
  size_t next_buffer_pos_{0};
  void* heap_;
};

template <meta::Str TagName, typename BlockSizeType, typename T>
struct  MemoryBufferInterface{

public:
 template <size_t NBuffer, size_t BufferSize>
 std::unique_ptr<MemoryBuffer<TagName, NBuffer, BufferSize, BlockSizeType, T>>
 create();

private:
 void* ptr_;
 BlockSizeType size_;

};



void* malloc(size_t sz);
void* realloc(void* ptr, size_t sz);

}  // namespace lps::basic::mem
