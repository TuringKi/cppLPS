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
#include <array>
#include "basic/exception.h"

namespace lps::basic::map {

template <typename Key, typename Value, std::size_t Size, meta::Str TagName>
struct Map {
  std::array<std::pair<Key, Value>, Size> data_;

  [[nodiscard]] constexpr Value at(const Key& key) const {
    const auto itr =
        std::find_if(begin(data_), end(data_),
                     [&key](const auto& v) { return v.first == key; });
    if (itr != end(data_)) {
      return itr->second;
    }
    LPS_ERROR(TagName, "Not Found: ", key);
    return Value();
  }
};
}  // namespace lps::basic::map
namespace lps::basic {
template <typename T0, typename T1>
struct Pair {
  Pair() = default;
  Pair(T0&& t0, T1&& t1) {
    t0_ = std::move(t0);
    t1_ = std::move(t1);
  }
  Pair(const T0& t0, const T1& t1) = delete;

  Pair& operator=(Pair&& other) noexcept {
    this->t0_ = std::move(other.t0_);
    this->t1_ = std::move(other.t1_);
    return *this;
  }
  Pair(Pair&& other) noexcept {
    this->t0_ = std::move(other.t0_);
    this->t1_ = std::move(other.t1_);
  }

  T0 t0_;
  T1 t1_;
};

}  // namespace lps::basic
