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

#include <bits/ranges_algo.h>
#include <algorithm>
#include <array>
#include <bitset>
#include "basic/exception.h"

namespace lps::basic {
template <size_t N, typename T = void>
class Bitset {
 public:
  Bitset() = default;
  template <size_t N1>
  explicit Bitset(const std::array<bool, N1>& other) {
    static_assert(N1 >= N);
    for (size_t i = 0; i < value_.size(); i++) {
      value_[i] = other[i];
    }
  }

  template <size_t N1>
  explicit Bitset(const std::bitset<N1>& other) : value_(other.to_ullong()) {}

  void reset() { value_.reset(); }
  void set() { value_.set(); }
  void set(size_t pos) { value_[pos] = true; }
  bool at(size_t pos) { return value_[pos]; }
  std::bitset<N> value() { return value_; }
  bool all() { return value_.all(); }

 private:
  std::bitset<N> value_;
};

template <size_t N>
class Bitset<N, typename std::enable_if<(N > (8 * sizeof(unsigned long long))),
                                        bool>::type> {
 public:
  Bitset() = default;
  template <size_t N1>
  explicit Bitset(const std::array<bool, N1>& other) {
    static_assert(N1 >= N);
    for (size_t i = 0; i < value_.size(); i++) {
      value_[i] = other[i];
    }
  }
  void reset() { value_.fill(false); }
  void set() { value_.fill(true); }
  void set(size_t pos) { value_[pos] = true; }
  bool at(size_t pos) { return value_[pos]; }
  std::array<bool, N> value() { return value_; }
  bool all() {
    return std::ranges::all_of(value_, [](bool a) { return a; });
  }

 private:
  std::array<bool, N> value_;
};
}  // namespace lps::basic
