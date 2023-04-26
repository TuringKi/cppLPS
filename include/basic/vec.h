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
#include <iterator>
#include <memory>
#include <type_traits>
#include "basic/exception.h"
#include "basic/mem.h"

template <typename T>
using SizeType =
    std::conditional_t<sizeof(T) < 4 && sizeof(void*) >= 8, uint64_t, uint32_t>;

namespace lps::basic::vec::details {

template <meta::Str TagName, typename T>
class Base : virtual public mem::TraceTag<TagName> {
  using pointer = T*;
  using const_pointer = T*;
  using Size = SizeType<T>;

 public:
  Base() = default;

  [[nodiscard]] size_t size() const { return size_; }
  void size(size_t n) { size_ = n; }
  [[nodiscard]] size_t capacity() const { return capacity_; }
  [[nodiscard]] bool empty() const { return !size_; }

  pointer first_;
  Size size_{0};
  Size capacity_;
};

template <meta::Str TagName, typename T>
class Common : public Base<TagName, T> {
  using base_type = Base<TagName, T>;

 public:
  using iterator = T*;
  using const_iterator = const T*;
  using reference = T&;
  using rvalue_reference = T&&;
  using const_reference = const T&;
  using pointer = T*;
  using const_pointer = const T*;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using size_type = size_t;

  Common() : base_type() {}

  iterator begin() { return this->first_; }
  iterator end() { return this->first_ + this->size(); }
  const_iterator begin() const { return this->first_; }
  const_iterator end() const { return this->first_ + this->size(); }

  reverse_iterator rbegin() { return reverse_iterator(end()); }
  reverse_iterator rend() { return reverse_iterator(begin()); }
  const_reverse_iterator rbegin() const {
    return const_reverse_iterator(end());
  }
  const_reverse_iterator rend() const {
    return const_reverse_iterator(begin());
  }

  pointer data() { return pointer(begin()); }
  const_pointer data() const { return const_pointer(begin()); }

  reference operator[](size_type idx) {
    lps_assert(TagName, idx < this->size());
    return begin()[idx];
  }
  const_reference operator[](size_type idx) const {
    lps_assert(TagName, idx < this->size());
    return begin()[idx];
  }

  reference front() {
    lps_assert(TagName, !this->empty());
    return begin()[0];
  }
  const_reference front() const {
    lps_assert(TagName, !this->empty());
    return begin()[0];
  }

  reference back() {
    lps_assert(TagName, !this->empty());
    return end()[-1];
  }
  const_reference back() const {
    lps_assert(TagName, !this->empty());
    return end()[-1];
  }

  void clear() { this->size_ = 0; }
};

template <meta::Str TagName, typename T>
class TemplateCommon : public Common<TagName, T> {
  using common = Common<TagName, T>;

 public:
  inline void append(typename common::const_reference a) {
    need_grow();
    *(this->end()) = a;
    size_inc();
  }

  inline void append(typename common::rvalue_reference a) {
    need_grow();
    *(this->end()) = std::forward<T>(a);
    size_inc();
  }

 protected:
  TemplateCommon() : common() {}

  inline void need_grow() {
    if (this->size() >= this->capacity()) {
      grow();
    }
  }
  inline void size_inc() { this->size(this->size() + 1); }
  virtual void grow() = 0;
};

template <meta::Str TagName, size_t N, typename T>
class Impl : public TemplateCommon<TagName, T> {
  using base_type = TemplateCommon<TagName, T>;
  using buffer_type =
      mem::MemoryBuffer<TagName + "_memory_buffer", N, SizeType<T>, T>;
  using buffer_ptr_type = std::unique_ptr<buffer_type>;
  using block_type = mem::Block<TagName + "_block", SizeType<T>, T>;
  using block_ptr_type = std::unique_ptr<block_type>;

 protected:
  void grow() override {
    buffer_->grow();
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
  }

  Impl() : base_type() {
    buffer_ = Impl<TagName, N, T>::buffer_type::create();
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
    this->size_ = 0;
  }

  buffer_ptr_type buffer_{nullptr};
};

}  // namespace lps::basic::vec::details

namespace lps::basic {
template <meta::Str TagName, size_t N, typename T>
class Vector : public vec::details::Impl<TagName, N, T> {
  using base_type = vec::details::Impl<TagName, N, T>;
  using common = vec::details::Common<TagName, T>;

 public:
  Vector() : base_type() {}

 private:
  void grow() override { base_type::grow(); }
};

namespace str::details {
template <meta::Str TagName, size_t N, typename T>
std::ostream& operator<<(std::ostream& s, const Vector<TagName, N, T>& arr) {

  if (arr.empty()) {
    return s;
  }
  const size_t max = 32;
  size_t max_idx = std::min(max, arr.size());
  s << "[";
  for (size_t i = 0; i < max_idx; i++) {
    if (i < max_idx - 1)
      s << arr[i] << ", ";
    else {
      if (arr.size() > max_idx) {
        s << arr[i] << ", ...";
      } else {
        s << arr[i];
      }
    }
  }
  s << "]";
  return s;
}
}  // namespace str::details

}  // namespace lps::basic
