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

class TraceTag {
 public:
  using tag_type = const char*;
  explicit TraceTag() : tag_("") {}
  explicit TraceTag(tag_type tag) : tag_(tag) {}
  [[nodiscard]] tag_type tag() const { return tag_; }

 protected:
  tag_type tag_{nullptr};
};

namespace details {

template <
    typename T, typename Size,
    typename std::enable_if<((std::is_trivially_copy_constructible<T>::value) &&
                             (std::is_trivially_move_constructible<T>::value) &&
                             std::is_trivially_destructible<T>::value),
                            bool>::type = true>
void val_init(T* ptr, Size size) {}

template <typename T, typename Size,
          typename std::enable_if<
              !((std::is_trivially_copy_constructible<T>::value) &&
                (std::is_trivially_move_constructible<T>::value) &&
                std::is_trivially_destructible<T>::value),
              bool>::type = true>
void val_init(T* ptr, Size size) {
  for (size_t i = 0; i < size; i++) {
    new (ptr + i) T();
  }
}

template <
    typename T, typename Size,
    typename std::enable_if<((std::is_trivially_copy_constructible<T>::value) &&
                             (std::is_trivially_move_constructible<T>::value) &&
                             std::is_trivially_destructible<T>::value),
                            bool>::type = true>
void val_remove(T* ptr, Size size) {}

template <typename T, typename Size,
          typename std::enable_if<
              !((std::is_trivially_copy_constructible<T>::value) &&
                (std::is_trivially_move_constructible<T>::value) &&
                std::is_trivially_destructible<T>::value),
              bool>::type = true>
void val_remove(T* ptr, Size size) {
  for (size_t i = 0; i < size; i++) {
    ptr[i].~T();
  }
}

template <typename Size, typename T>
class Data : virtual public TraceTag {

  using pointer = T*;
  using const_pointer = const T*;
  using type = Data<Size, T>;
  using ptr_type = std::unique_ptr<type>;

 public:
  pointer ptr() { return data_; }
  Size size() const { return size_; }

  static ptr_type create(TraceTag::tag_type tag, Size size) {
    std::allocator<T> allocator;
    auto ptr = allocator.allocate(size);
    if (ptr == nullptr) {
      LPS_ERROR("mem::Data::create", "allocate memory failed");
    }
    val_init(ptr, size);
    return std::make_unique<type>(tag, ptr, size);
  }

  ~Data() {
    if (ptr() != nullptr) {
      std::allocator<T> allocator;
      val_remove(ptr(), size());
      allocator.deallocate(ptr(), size());
    }
  }

  explicit Data(TraceTag::tag_type tag, pointer data, Size size)
      : TraceTag(tag), data_(data), size_(size) {}

 private:
  pointer data_;
  Size size_;
};

template <size_t N, typename T, class Enable = void>
class StaticBuffer : virtual public TraceTag {
  using pointer = T*;
  using type = StaticBuffer<N, T>;
  using ptr_type = std::unique_ptr<type>;

 public:
  virtual ~StaticBuffer() = default;
  static ptr_type create(mem::TraceTag::tag_type tag) {
    return std::make_unique<type>(tag);
  }
  pointer ptr() { return data_; }

  explicit StaticBuffer(mem::TraceTag::tag_type tag) : TraceTag(tag) {}

 private:
  T data_[N];
};

template <size_t N, typename T>
class StaticBuffer<N, T,
                   typename std::enable_if<!(
                       (std::is_trivially_copy_constructible<T>::value) &&
                       (std::is_trivially_move_constructible<T>::value) &&
                       std::is_trivially_destructible<T>::value)>::type>
    : virtual public TraceTag {
  using pointer = T*;
  using type = StaticBuffer<N, T>;
  using ptr_type = std::unique_ptr<type>;

 public:
  virtual ~StaticBuffer() {
    lps_assert(tag_, ptr() != nullptr);
    val_remove(ptr(), N);
    std::allocator<T> allocator;
    allocator.deallocate(ptr(), N);
  }
  static ptr_type create(TraceTag::tag_type tag) {
    return std::make_unique<type>(tag);
  }
  pointer ptr() { return data_; }
  explicit StaticBuffer(TraceTag::tag_type tag) : TraceTag(tag) {
    std::allocator<T> allocator;
    data_ = allocator.allocate(N);
    lps_assert(tag_, ptr() != nullptr);
    val_init(data_, N);
  }

 private:
  T* data_;
};

template <typename BlockSizeType, typename T>
class BlockInBuffer : virtual public TraceTag {

  template <typename BlockSizeType0, typename T0>
  friend class Block;

 public:
  virtual ~BlockInBuffer() = default;
  [[nodiscard]] size_t pos() const { return pos_; }

 protected:
  explicit BlockInBuffer(TraceTag::tag_type tag, size_t buffer_size)
      : TraceTag(tag), buffer_size_(buffer_size) {}

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

template <typename Size, typename T>
class DynamicBuffer : virtual public TraceTag {
  using pointer = T*;
  using const_pointer = const T*;
  static constexpr const char* kDataTagname = "DynamicBuffer_data";
  using data_type = Data<Size, T>;
  using type = DynamicBuffer<Size, T>;
  using ptr_type = std::unique_ptr<type>;
  using data_ptr_type = std::unique_ptr<data_type>;

 public:
  DynamicBuffer() = delete;
  virtual ~DynamicBuffer() = default;

  static ptr_type create(TraceTag::tag_type tag, Size capacity) {
    return std::make_unique<type>(tag, capacity);
  }

  pointer ptr() { return data_->ptr(); }

  [[nodiscard]] size_t size() const { return size_; }
  [[nodiscard]] size_t capacity() const { return data_ ? data_->size() : 0; }

  [[nodiscard]] bool empty() const { return size_; }

  static Size new_capacity(Size min, Size old_capacity) {
    constexpr auto kTheMaxSize = std::numeric_limits<Size>::max();
    if (min > kTheMaxSize) {
      LPS_ERROR(kDataTagname, "overflow");
    }
    if (old_capacity == kTheMaxSize) {
      LPS_ERROR(kDataTagname, "already at maximum size");
    }
    Size new_capacity = 2 * old_capacity + 1;
    return std::clamp(new_capacity, min, kTheMaxSize);
  }

  void grow(Size min) {
    auto new_capacity = DynamicBuffer<Size, T>::new_capacity(min, capacity());

    data_ptr_type new_data = data_type::create(tag_, new_capacity);
    details::copy(this->data_->ptr(), this->data_->size(), new_data->ptr());

    this->data_ = std::move(new_data);
  }

  explicit DynamicBuffer(TraceTag::tag_type tag, Size capacity)
      : TraceTag(tag), data_(data_type::create(tag, capacity)) {}

 private:
  data_ptr_type data_{nullptr};
  Size size_;
};

}  // namespace details

template <typename Size, typename T>
class Block : virtual public TraceTag {
  using manager_type = details::BlockInBuffer<Size, T>;
  using pointer = T*;

 private:
  Size n_;
  pointer ptr_;
  manager_type& manager_;

 public:
  Block(const Block&) = delete;
  Block& operator=(const Block&) = delete;

 private:
  static std::unique_ptr<Block<Size, T>> create(TraceTag::tag_type tag,
                                                manager_type& manager,
                                                pointer ptr, Size n) {
    return std::make_unique<Block<Size, T>>(tag, manager, ptr, n);
  }
  explicit Block(TraceTag::tag_type tag, manager_type& manager, void* ptr,
                 Size n)
      : TraceTag(tag), manager_(manager), ptr_(ptr), n_(n) {
    manager_.add(n_);
  }
  virtual ~Block() { manager_.remove(n_); }
};

template <typename T, size_t BufferSize, typename BlockSizeType>
class MemoryBuffer : public details::BlockInBuffer<BlockSizeType, T>,
                     virtual public TraceTag {
  using type = MemoryBuffer<T, BufferSize, BlockSizeType>;
  using ptr_type = std::unique_ptr<type>;
  using manager_type = details::BlockInBuffer<BlockSizeType, T>;
  using static_buffer_type = details::StaticBuffer<BufferSize, T>;
  using static_buffer_ptr_type = std::unique_ptr<static_buffer_type>;
  using dynamic_buffer_type = details::DynamicBuffer<BlockSizeType, T>;
  using dynamic_buffer_ptr_type = std::unique_ptr<dynamic_buffer_type>;
  using block_type = Block<BlockSizeType, T>;
  using block_ptr_type = std::unique_ptr<block_type>;

  using pointer = T*;
  using const_pointer = const T*;

 public:
  MemoryBuffer(const MemoryBuffer&) = delete;
  MemoryBuffer& operator=(const MemoryBuffer&) = delete;
  virtual ~MemoryBuffer() = default;

  static ptr_type create(TraceTag::tag_type tag, size_t capacity = 0) {
    if (BufferSize > 0 && capacity == 0) {
      return std::make_unique<type>(tag);
    }
    lps_assert(tag, capacity > 0);
    return std::make_unique<type>(tag, capacity);
  }

  void grow() {
    if (dynamic_ == nullptr) {
      LPS_NOTE(tag_, "grow to heap");
      dynamic_ = dynamic_buffer_type::create(tag_, BufferSize * 2 + 1);
      this->set_max(BufferSize * 2 + 1);
      this->clear();
      details::copy(top_, BufferSize, dynamic_->ptr());
      top_ = dynamic_->ptr();
      static_ = nullptr;  // the static buffer is useless now, we remove it.
      location_ = Location::kHeap;
    } else {
      auto sz = dynamic_->capacity() * 2 + 1;
      LPS_NOTE(tag_, "the heap grow[size = ", sz, "]");
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
        return block_ptr_type::create(tag_, *this, this->top_ + this->pos_,
                                      block_size);
      }
    }
    if (dynamic_) {
      LPS_NOTE(tag_, "using heap to create block");
      if (!this->ok(block_size)) {
        grow();
      }
      return block_ptr_type::create(tag_, *this, this->top_ + this->pos_,
                                    block_size);
    }

    unreachable(tag_);
  }

  pointer top() { return top_; }

  [[nodiscard]] size_t capacity() const {
    return dynamic_ ? dynamic_->capacity() : BufferSize;
  }

  explicit MemoryBuffer(TraceTag::tag_type tag)
      : TraceTag(tag),
        manager_type(tag, BufferSize),
        static_(static_buffer_type::create(tag)) {
    top_ = static_->ptr();
  }

  explicit MemoryBuffer(TraceTag::tag_type tag, size_t capacity)
      : TraceTag(tag),
        manager_type(tag, BufferSize),
        dynamic_(dynamic_buffer_type::create(tag, capacity)) {
    top_ = dynamic_->ptr();
  }

 private:
  enum Location : uint8_t { kStack = 0, kHeap };

  dynamic_buffer_ptr_type dynamic_{nullptr};
  pointer top_{nullptr};
  static_buffer_ptr_type static_{nullptr};
  Location location_{Location::kStack};
};

}  // namespace lps::basic::mem
