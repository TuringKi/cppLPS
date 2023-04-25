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

#include <cstdint>
#include <memory>
#include "meta.h"

namespace lps::basic::mem {

namespace details {

template <meta::Str TagName>
class TraceTag {
 public:
  [[nodiscard]] virtual constexpr const char* tag() const {
    return TagName.value;
  }
};

template <meta::Str TagName, size_t N, typename T>
class StaticBuffer : virtual public TraceTag<TagName> {
  using type = StaticBuffer<TagName, N, T>;
  using pointer = T*;

 public:
  StaticBuffer() = default;
  virtual ~StaticBuffer() = default;
  static std::unique_ptr<type> create() { return std::make_unique<type>(); }
  pointer ptr() { return data_; }

 private:
  T data_[N];
};

template <meta::Str TagName, typename BlockSizeType, typename T>
class BlockInBuffer : virtual public TraceTag<TagName> {

  template <meta::Str TagName0, typename BlockSizeType0, typename T0>
  friend class Block;

 public:
  virtual ~BlockInBuffer() = default;

 protected:
  explicit BlockInBuffer(size_t buffer_size) : buffer_size_(buffer_size) {}

  [[nodiscard]] bool ok() const { return pos_ >= 0 && pos_ < buffer_size_; }
  bool ok(BlockSizeType block_size) const {
    return (pos_ + block_size) >= 0 && (pos_ + block_size) < buffer_size_;
  }

  void set_max(size_t buffer_size) { buffer_size_ = buffer_size; }

  bool add(BlockSizeType block_size) {
    if (!ok(block_size)) {
      return false;
    }
    pos_ = std::min(pos_ + block_size, buffer_size_);
    return true;
  }

  bool remove(BlockSizeType block_size) {
    if (!ok(block_size)) {
      return false;
    }
    pos_ = std::max(pos_ - block_size, 0);
    return true;
  }

  size_t pos_{0};  // last alloced block position on `buffer_`;
  size_t buffer_size_;
};

template <meta::Str TagName, typename Size, typename T>
class Block : virtual public TraceTag<TagName> {
  using manager_type = BlockInBuffer<TagName, Size, T>;
  using pointer = T*;

 private:
  Size n_;
  pointer ptr_;
  manager_type& manager_;

 public:
  Block(const Block&) = delete;
  Block& operator=(const Block&) = delete;

 private:
  static std::unique_ptr<Block<TagName, Size, T>> create(manager_type& manager,
                                                         pointer ptr, Size n) {
    return std::make_unique<Block<TagName, Size, T>>(manager, ptr, n);
  }
  explicit Block(manager_type& manager, void* ptr, Size n)
      : manager_(manager), ptr_(ptr), n_(n) {
    manager_.add(n_);
  }
  virtual ~Block() { manager_.remove(n_); }
};

template <meta::Str TagName, typename Size, typename T>
class DynamicBuffer : virtual public TraceTag<TagName> {
  using pointer = T*;

 public:
  DynamicBuffer() = delete;
  virtual ~DynamicBuffer() = default;
  DynamicBuffer(pointer first, Size capacity)
      : first_(first), capacity_(capacity) {}

 protected:
  [[nodiscard]] size_t size() const { return size_; }
  [[nodiscard]] size_t capacity() const { return capacity_; }

  [[nodiscard]] bool empty() const { return size_; }

  static Size new_capacity(Size min, Size old_capacity);

  pointer malloc_grow(pointer first, Size min, Size& new_capacity);

  void grow_pod(pointer first, Size min);

  pointer replace(pointer first, Size old_capacity, Size new_capacity);

  pointer first_{nullptr};
  Size size_{0};
  Size capacity_{0};
  std::allocator<T> allocator_;
};

}  // namespace details

template <meta::Str TagName, size_t BufferSize, typename BlockSizeType,
          typename T>
class MemoryBuffer : virtual public details::TraceTag<TagName>,
                     public details::BlockInBuffer<TagName, BlockSizeType, T> {
  using manager_type = details::BlockInBuffer<TagName, BlockSizeType, T>;
  using static_buffer_type = details::StaticBuffer<TagName, BufferSize, T>;
  using static_buffer_ptr_type = std::unique_ptr<static_buffer_type>;
  using block_type = details::Block<TagName, BlockSizeType, T>;
  using dynamic_buffer_type = details::DynamicBuffer<TagName, BlockSizeType, T>;
  using dynamic_buffer_ptr_type = std::unique_ptr<dynamic_buffer_type>;
  using block_ptr_type =
      std::unique_ptr<details::Block<TagName, BlockSizeType, T>>;

  using pointer = T*;

 public:
  MemoryBuffer()
      : manager_type(BufferSize), static_(static_buffer_type::create()) {
    top_ = static_->ptr();
  }
  MemoryBuffer(const MemoryBuffer&) = delete;
  MemoryBuffer& operator=(const MemoryBuffer&) = delete;
  virtual ~MemoryBuffer() = default;

 private:
  enum Location : uint8_t { kStack = 0, kHeap };

  // One basic question arise: How to travel the list of `buffers_` as a whole single buffer?

  void expand();
  block_ptr_type block(BlockSizeType block_size);

  dynamic_buffer_ptr_type dynamic_{nullptr};
  pointer top_{nullptr};
  static_buffer_ptr_type static_{nullptr};
  Location location_{Location::kStack};
};

template <meta::Str TagName, typename BlockSizeType, typename T>
struct MemoryBufferInterface {

 public:
  template <size_t BufferSize>
  std::unique_ptr<MemoryBuffer<TagName, BufferSize, BlockSizeType, T>> create();

 private:
  void* ptr_;
  BlockSizeType size_;
};

void* malloc(size_t sz);
void* realloc(void* ptr, size_t sz);

}  // namespace lps::basic::mem
