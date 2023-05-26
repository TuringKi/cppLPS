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

#include <algorithm>
#include <cstdint>
#include <limits>
#include <memory>
#include <type_traits>
#include "basic/exception.h"
#include "basic/meta.h"

namespace lps::basic::mem {

template <meta::Str TagName>
class TraceTag {
 public:
  [[nodiscard]] virtual constexpr const char* tag() const {
    return TagName.value;
  }

  static constexpr meta::Str kTag = TagName;
};

namespace details {

template <meta::Str TagName, typename Size, typename T>
class Data : virtual public TraceTag<TagName> {

  using pointer = T*;
  using const_pointer = const T*;
  using type = Data<TagName, Size, T>;
  using ptr_type = std::unique_ptr<type>;

  friend ptr_type std::make_unique<type>(pointer&, Size&);

 public:
  pointer ptr() { return data_; }
  Size size() const { return size_; }
  static ptr_type create(Size size) {
    std::allocator<T> allocator;
    auto ptr = allocator.allocate(size);
    if (ptr == nullptr) {
      LPS_ERROR(TagName, "allocate memory failed");
    }
    return std::make_unique<type>(ptr, size);
  }

  ~Data() {
    if (ptr() != nullptr) {
      std::allocator<T> allocator;
      allocator.deallocate(ptr(), size());
    }
  }

 private:
  explicit Data(pointer data, Size size) : data_(data), size_(size) {}

  pointer data_;
  Size size_;
};

template <meta::Str TagName, size_t N, typename T, class Enable = void>
class StaticBuffer : virtual public TraceTag<TagName> {
  using pointer = T*;
  using type = StaticBuffer<TagName, N, T>;
  using ptr_type = std::unique_ptr<type>;

  friend ptr_type std::make_unique<type>();

 public:
  virtual ~StaticBuffer() = default;
  static ptr_type create() { return std::make_unique<type>(); }
  pointer ptr() { return data_; }

 private:
  StaticBuffer() = default;

  T data_[N];
};

template <meta::Str TagName, size_t N, typename T>
class StaticBuffer<TagName, N, T,
                   typename std::enable_if<!(
                       (std::is_trivially_copy_constructible<T>::value) &&
                       (std::is_trivially_move_constructible<T>::value) &&
                       std::is_trivially_destructible<T>::value)>::type>
    : virtual public TraceTag<TagName> {
  using pointer = T*;
  using type = StaticBuffer<TagName, N, T>;
  using ptr_type = std::unique_ptr<type>;

  friend ptr_type std::make_unique<type>();

 public:
  virtual ~StaticBuffer() {
    lps_assert(TagName, ptr() != nullptr);
    std::allocator<T> allocator;
    allocator.deallocate(ptr(), N);
  }
  static ptr_type create() { return std::make_unique<type>(); }
  pointer ptr() { return data_; }

 private:
  T* data_;
  StaticBuffer() {
    std::allocator<T> allocator;
    data_ = allocator.allocate(N);
    lps_assert(TagName, ptr() != nullptr);
  }
};

template <meta::Str TagName, typename BlockSizeType, typename T>
class BlockInBuffer : virtual public TraceTag<TagName> {

  template <meta::Str TagName0, typename BlockSizeType0, typename T0>
  friend class Block;

 public:
  virtual ~BlockInBuffer() = default;
  [[nodiscard]] size_t pos() const { return pos_; }

 protected:
  explicit BlockInBuffer(size_t buffer_size) : buffer_size_(buffer_size) {}

  [[nodiscard]] bool ok() const { return pos_ >= 0 && pos_ < buffer_size_; }
  bool ok(BlockSizeType block_size) const {
    return (pos_ + block_size) >= 0 && (pos_ + block_size) < buffer_size_;
  }

  void set_max(size_t buffer_size) { buffer_size_ = buffer_size; }
  void clear() { pos_ = 0; }

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

template <typename T, typename std::enable_if<
                          ((std::is_trivially_copy_constructible<T>::value) &&
                           (std::is_trivially_move_constructible<T>::value) &&
                           std::is_trivially_destructible<T>::value),
                          bool>::type = true>
void copy(T* src, size_t size, T* dst) {
  std::copy_n(src, size, dst);
}

template <typename T, typename std::enable_if<
                          !((std::is_trivially_copy_constructible<T>::value) &&
                            (std::is_trivially_move_constructible<T>::value) &&
                            std::is_trivially_destructible<T>::value),
                          bool>::type = true>
void copy(T* src, size_t size, T* dst) {
  for (size_t i = 0; i < size; i++) {
    *(dst + i) = std::move(*(src + i));
  }
}

template <meta::Str TagName, typename Size, typename T>
class DynamicBuffer : virtual public TraceTag<TagName> {
  using pointer = T*;
  using const_pointer = const T*;
  static constexpr meta::Str kDataTagname = TagName + "_data";
  using data_type = Data<kDataTagname, Size, T>;
  using type = DynamicBuffer<TagName, Size, T>;
  using ptr_type = std::unique_ptr<type>;
  using data_ptr_type = std::unique_ptr<data_type>;

  friend ptr_type std::make_unique<type>(Size&);

 public:
  DynamicBuffer() = delete;
  virtual ~DynamicBuffer() = default;

  static ptr_type create(Size capacity) {
    return std::make_unique<type>(capacity);
  }

  pointer ptr() { return data_->ptr(); }

  [[nodiscard]] size_t size() const { return size_; }
  [[nodiscard]] size_t capacity() const { return data_ ? data_->size() : 0; }

  [[nodiscard]] bool empty() const { return size_; }

  static Size new_capacity(Size min, Size old_capacity) {
    constexpr auto kTheMaxSize = std::numeric_limits<Size>::max();
    if (min > kTheMaxSize) {
      LPS_ERROR(TagName, "overflow");
    }
    if (old_capacity == kTheMaxSize) {
      LPS_ERROR(TagName, "already at maximum size");
    }
    Size new_capacity = 2 * old_capacity + 1;
    return std::clamp(new_capacity, min, kTheMaxSize);
  }

  void grow(Size min) {
    auto new_capacity =
        DynamicBuffer<TagName, Size, T>::new_capacity(min, capacity());

    data_ptr_type new_data = data_type::create(new_capacity);
    details::copy(this->data_->ptr(), this->data_->size(), new_data->ptr());
    //std::copy_n(other->ptr(), other->size(), ptr());
    this->data_ = std::move(new_data);
  }

 private:
  explicit DynamicBuffer(Size capacity) : data_(data_type::create(capacity)) {}

  data_ptr_type data_{nullptr};
  Size size_;
};

}  // namespace details

template <meta::Str TagName, typename Size, typename T>
class Block : virtual public TraceTag<TagName> {
  using manager_type = details::BlockInBuffer<TagName, Size, T>;
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

template <typename T, size_t BufferSize, typename BlockSizeType,
          meta::Str TagName>
class MemoryBuffer : public details::BlockInBuffer<TagName, BlockSizeType, T> {
  using type = MemoryBuffer<T, BufferSize, BlockSizeType, TagName>;
  using ptr_type = std::unique_ptr<type>;
  using manager_type = details::BlockInBuffer<TagName, BlockSizeType, T>;
  using static_buffer_type = details::StaticBuffer<TagName, BufferSize, T>;
  using static_buffer_ptr_type = std::unique_ptr<static_buffer_type>;
  using dynamic_buffer_type = details::DynamicBuffer<TagName, BlockSizeType, T>;
  using dynamic_buffer_ptr_type = std::unique_ptr<dynamic_buffer_type>;
  using block_type = Block<TagName, BlockSizeType, T>;
  using block_ptr_type = std::unique_ptr<block_type>;

  using pointer = T*;
  using const_pointer = const T*;

  friend ptr_type std::make_unique<type>();
  friend ptr_type std::make_unique<type>(size_t&);

 public:
  MemoryBuffer(const MemoryBuffer&) = delete;
  MemoryBuffer& operator=(const MemoryBuffer&) = delete;
  virtual ~MemoryBuffer() = default;

  static ptr_type create(size_t capacity = 0) {
    if (BufferSize > 0 && capacity == 0) {
      return std::make_unique<type>();
    }
    lps_assert(TagName, capacity > 0);
    return std::make_unique<type>(capacity);
  }

  void grow() {
    if (dynamic_ == nullptr) {
      LPS_NOTE(TagName, "grow to heap");
      dynamic_ = dynamic_buffer_type::create(BufferSize * 2 + 1);
      this->set_max(BufferSize * 2 + 1);
      this->clear();
      details::copy(top_, BufferSize, dynamic_->ptr());
      top_ = dynamic_->ptr();
      static_ = nullptr;  // the static buffer is useless now, we remove it.
      location_ = Location::kHeap;
    } else {
      auto sz = dynamic_->capacity() * 2 + 1;
      LPS_NOTE(TagName, "the heap grow[size = ", sz, "]");
      dynamic_->grow(sz);
      top_ = dynamic_->ptr();
    }
  }

  block_ptr_type block(BlockSizeType block_size) {
    block_ptr_type block;
    if (location_ == Location::kStack) {

      if (!this->ok(block_size)) {
        // static buffer is full, now we need to use the heap
        grow();
      } else {
        return block_ptr_type::create(*this, this->top_ + this->pos_,
                                      block_size);
      }
    }
    if (dynamic_) {
      LPS_NOTE(TagName, "using heap to create block");
      if (!this->ok(block_size)) {
        grow();
      }
      return block_ptr_type::create(*this, this->top_ + this->pos_, block_size);
    }

    unreachable(TagName);
  }

  pointer top() { return top_; }

  [[nodiscard]] size_t capacity() const {
    return dynamic_ ? dynamic_->capacity() : BufferSize;
  }

 private:
  MemoryBuffer()
      : manager_type(BufferSize), static_(static_buffer_type::create()) {
    top_ = static_->ptr();
  }

  explicit MemoryBuffer(size_t capacity)
      : manager_type(BufferSize),
        dynamic_(dynamic_buffer_type::create(capacity)) {
    top_ = dynamic_->ptr();
  }

  enum Location : uint8_t { kStack = 0, kHeap };

  dynamic_buffer_ptr_type dynamic_{nullptr};
  pointer top_{nullptr};
  static_buffer_ptr_type static_{nullptr};
  Location location_{Location::kStack};
};

void* malloc(size_t sz);
void* realloc(void* ptr, size_t sz);

}  // namespace lps::basic::mem
