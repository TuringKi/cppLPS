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
#include <bitset>
#include "basic/exception.h"
#include "basic/vec.h"

namespace lps::basic {
template <meta::Str TagName, size_t N, typename T = void>
class Bitset {
 public:
  template <std::size_t R, std::size_t L>
  static std::bitset<N> range(std::bitset<N> b) {
    static_assert(R <= L && L <= N, "Not valid.");
    b >>= R;
    b <<= (N - L + R);
    b >>= (N - L);
    return b;
  }

  static std::bitset<N> range(std::bitset<N> b, std::size_t R, std::size_t L) {
    lps_assert(TagName, R <= L && L <= N);
    b >>= R;
    b <<= (N - L + R);
    b >>= (N - L);
    return b;
  }

  static constexpr size_t kN = N;
  Bitset() : len_(N) { value_.reset(); }
  template <size_t N1>
  explicit Bitset(const std::array<bool, N1>& other, size_t start_idx = 0)
      : start_idx_(start_idx) {
    size_t m = std::min(N, N1);
    for (size_t i = start_idx; i < m; i++) {
      value_[i] = other[i];
    }
    len_ = m;
  }

  template <size_t N1>
  explicit Bitset(const std::bitset<N1>& other, size_t start_idx = 0)
      : start_idx_(start_idx) {
    size_t m = std::min(N, N1);
    if (start_idx > 0) {
      lps_assert(TagName, N1 < N);
      value_ = value_.to_ullong() || (other.to_ullong() << start_idx);
    } else {
      value_ = other.to_ullong();
    }
    len_ = m;
  }

  void reset() { value_ = value(start_idx_).reset().to_ullong(); }
  void set() { value_ = value(start_idx_).set().to_ullong(); }
  void set(size_t pos) {
    lps_assert(TagName, (pos + start_idx_) < len_);
    value_[pos + start_idx_] = true;
  }
  bool at(size_t pos) {
    lps_assert(TagName, (pos + start_idx_) < len_);
    return value_[pos + start_idx_];
  }
  std::bitset<N> value(size_t start_idx = 0) {
    return std::bitset<N>(
        range(value_, start_idx, start_idx + len_).to_ullong());
  }

  template <size_t N1>
  std::bitset<N1> value(size_t start_idx = 0) {
    return std::bitset<N1>(
        range(value_, start_idx, start_idx + len_).to_ullong());
  }

  bool all() {
    return range(value_, start_idx_, start_idx_ + len_).count() == len_;
  }
  [[nodiscard]] size_t start_idx() const { return start_idx_; }

 private:
  std::bitset<N> value_;
  size_t len_{0};
  size_t start_idx_{0};
};

template <meta::Str TagName, size_t N>
class Bitset<TagName, N,
             typename std::enable_if<(N > (8 * sizeof(unsigned long long))),
                                     bool>::type> {
 public:
  static constexpr size_t kN = N;
  Bitset() : len_(N) { LPS_ERROR(TagName, "Unsupport yet."); }
  template <size_t N1>
  explicit Bitset(const std::array<bool, N1>& other, size_t start_idx = 0) {
    constexpr size_t kM = std::min(N, N1);
    for (size_t i = start_idx; i < kM; i++) {
      value_[i] = other[i];
    }
    len_ = kM;
  }
  template <size_t N1>
  explicit Bitset(const std::bitset<N1>& other, size_t start_idx = 0) {
    static_assert(N1 < N, "not valid size");
    for (size_t i = start_idx; i < N1; i++) {
      value_[i] = other[i];
    }
    len_ = N1;
  }
  void reset() { value_.fill(false); }
  void set() { value_.fill(true); }
  void set(size_t pos) {
    lps_assert(TagName, pos < len_);
    value_[pos] = true;
  }
  bool at(size_t pos) {
    lps_assert(TagName, pos < len_);
    return value_[pos];
  }
  std::array<bool, N> value(size_t start_idx = 0) {
    lps_assert(TagName, start_idx == 0);
    return value_;
  }
  bool all() {
    lps_assert(TagName, len_ <= N);
    return std::ranges::all_of(value_.begin(), value_.begin() + len_,
                               [](bool a) { return a; });
  }

 private:
  basic::Vector<N, bool, TagName> value_;
  size_t len_{0};
};
}  // namespace lps::basic
