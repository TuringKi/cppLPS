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
#include <cstddef>
#include "basic/exception.h"
#include "basic/mem.h"

namespace lps::basic {
template <size_t N>
class Bitset {
 public:
  using ele_type = bool;
  using bitset = std::array<ele_type, N>;
  constexpr static mem::TraceTag::tag_type kTag = "Bitset";
  template <std::size_t R, std::size_t L>
  static std::array<ele_type, L - R> range(const bitset& b) {
    static_assert(R <= L && L <= N, "Not valid.");
    std::array<ele_type, L - R> new_b;
    size_t idx = 0;
    for (int i = R; i < L; i++) {
      new_b[idx++] = b[i];
    }
    return new_b;
  }

  static bitset range(const bitset& b, std::size_t R, std::size_t L) {
    lps_assert(kTag, R <= L && L <= N);
    bitset new_b;
    size_t idx = 0;
    for (int i = R; i < L; i++) {
      new_b[idx++] = b[i];
    }
    return new_b;
  }

  static constexpr size_t kN = N;

  explicit Bitset() { reset(); }

  template <size_t N1>
  explicit Bitset(const std::array<ele_type, N1>& other, size_t start_idx = 0)
      : start_idx_(start_idx) {
    set(other, start_idx);
  }

  template <size_t N1>
  void set(const std::array<ele_type, N1>& other, size_t start_idx = 0) {
    start_idx_ = start_idx;
    size_t m = std::min(N, N1);
    if (start_idx > 0) {
      lps_assert(kTag, N1 < N);
      for (int i = 0; i < N1; i++) {
        value_[start_idx + i] = other[i];
      }
    } else {

      for (int i = 0; i < m; i++) {
        value_[i] = other[i];
      }
    }
    len_ = m;
    lps_assert(kTag, len_ + start_idx_ <= N);
  }

  void reset() {
    for (int i = start_idx_; i < start_idx_ + len_; i++) {
      value_[i] = false;
    }
  }
  void set() {
    for (int i = start_idx_; i < start_idx_ + len_; i++) {
      value_[i] = true;
    }
  }
  void set(size_t pos) {
    lps_assert(kTag, (pos + start_idx_) < len_);
    value_[pos + start_idx_] = true;
  }
  bool at(size_t pos) {
    lps_assert(kTag, (pos + start_idx_) < len_);
    return value_[pos + start_idx_];
  }
  bitset value(size_t start_idx = 0) {
    if (start_idx == 0) {
      return range(value_, 0, len_);
    }
    auto a = range(value_, start_idx, len_);
    return a;
  }
  [[nodiscard]] size_t cnt() const {
    size_t cnt = 0;
    for (int i = start_idx_; i < start_idx_ + len_; i++) {
      if (value_[i]) {
        cnt++;
      }
    }
    return cnt;
  }
  [[nodiscard]] size_t size() const { return len_; }
  [[nodiscard]] bool all() const { return cnt() == len_; }
  [[nodiscard]] size_t start_idx() const { return start_idx_; }

 private:
  bitset value_;
  size_t len_{N};
  size_t start_idx_{0};
};

}  // namespace lps::basic
