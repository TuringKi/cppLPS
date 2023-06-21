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
#include <iostream>

namespace meta {

template <size_t N>
struct Str {
  constexpr explicit Str(const char (&str)[N]) { std::copy_n(str, N, value); }
  constexpr Str() = default;
  constexpr static bool empty() { return N == 0; }

  char value[N];
};
template <size_t N>
constexpr Str<N> S(const char (&str)[N]) {
  return Str<N>(str);
}
template <size_t N0, size_t N1>
constexpr Str<N0 + N1> operator+(const Str<N0>& s0, const char (&str)[N1]) {
  Str<N0 + N1> s1;
  std::copy_n(s0.value, N0, s1.value);
  std::copy_n(str, N1, s1.value + N0);
  return s1;
}

template <size_t N0, size_t N1>
constexpr Str<N0 + N1> operator+(const Str<N0>& s0, const Str<N1>& s1) {
  Str<N0 + N1> s2;
  std::copy_n(s0.value, N0, s2.value);
  std::copy_n(s1.value, N1, s2.value + N0);
  return s2;
}

}  // namespace meta
