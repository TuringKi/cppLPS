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

#include <array>
#include <cstdint>
#include <iterator>
#include <limits>
#include <type_traits>
#include "meta.h"

namespace lps::basic::mem {

template <meta::Str TagName, size_t NBuffer, size_t BufferSize,
          typename BlockSizeType, typename T>
class MemoryBuffer;

}

namespace lps::basic::vec::details {

template <typename Size>
class Base {
 public:
  Base() = delete;

 protected:
  [[nodiscard]] size_t size() const { return size_; }
  [[nodiscard]] size_t capacity() const { return capacity_; }
  [[nodiscard]] bool empty() const { return size_; }

  void* first_;
  Size size_ = 0;
  Size capacity_;
};

template <typename T>
using SizeType =
    std::conditional_t<sizeof(T) < 4 && sizeof(void*) >= 8, uint64_t, uint32_t>;

template <typename T>
class Common : public Base<SizeType<T>> {
  using base_type = Base<SizeType<T>>;

 public:
  using iterator = T*;
  using const_iterator = const T*;
  using reference = T&;
  using const_reference = const T&;
  using pointer = T*;
  using const_pointer = const T*;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using size_type = size_t;

  Common() = delete;

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
    assert(idx < this->size());
    return begin()[idx];
  }
  const_reference operator[](size_type idx) const {
    assert(idx < this->size());
    return begin()[idx];
  }

  reference front() {
    assert(!this->empty());
    return begin()[0];
  }
  const_reference front() const {
    assert(!this->empty());
    return begin()[0];
  }

  reference back() {
    assert(!this->empty());
    return end()[-1];
  }
  const_reference back() const {
    assert(!this->empty());
    return end()[-1];
  }

  void clear() { this->size_ = 0; }
};

template <size_t NBuffer, size_t N, typename T>
class Impl : public Common<T> {
  using buffer_type =
      mem::MemoryBuffer<meta::Str("vector"), NBuffer, N, SizeType<T>, T>;

 protected:
};

constexpr size_t kDefaultNBuffer = 1;

}  // namespace lps::basic::vec::details

namespace lps::basic {
template <typename T, size_t N>
class Vector : public vec::details::Impl<vec::details::kDefaultNBuffer, N, T> {
};

}  // namespace lps::basic
