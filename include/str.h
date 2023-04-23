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
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

namespace lps::basic::str {

namespace details {

template <auto N>
constexpr auto array(char const (&cstr)[N]) {
  std::array<char, N> arr;
  for (std::size_t i = 0; i < N; ++i)
    arr[i] = cstr[i];
  return arr;
}

template <auto N>
std::ostream& operator<<(std::ostream& s, const std::array<char, N>& arr) {
  s << std::string(arr.begin(), arr.end());
  return s;
}

struct Empty {
  explicit operator const std::string&() const {
    static std::string empty;
    return empty;
  }
  explicit operator const char*() const { return ""; }
};

inline void str(std::ostream& s) {}

template <typename T>
inline void str(std::ostream& s, const T& t) {
  s << t;
}

template <>
inline void str<Empty>(std::ostream& s, const Empty& /*a*/) {}

template <typename T, typename... Args>
inline void str(std::ostream& s, const T& t, const Args&... args) {
  str(s, t);
  str(s, args...);
}

template <typename T>
struct Canonicalize {
  using type = const T&;
};

template <size_t N>
struct Canonicalize<char[N]> {
  using type = const char*;
};

template <typename... Args>
struct Warper final {
  inline static std::string call(const Args&... args) {
    std::ostringstream ss;
    details::str(ss, args...);
    return ss.str();
  }
};

template <>
struct Warper<std::string> final {
  static const std::string& call(const std::string& str) { return str; }
};

template <>
struct Warper<const char*> final {
  static const char* call(const char* str) { return str; }
};

template <>
struct Warper<> final {
  static Empty call() { return Empty(); }
};

}  // namespace details

template <typename... Args>
inline decltype(auto) to(const Args&... args) {
  return details::Warper<typename details::Canonicalize<Args>::type...>::call(
      args...);
}

}  // namespace lps::basic::str
