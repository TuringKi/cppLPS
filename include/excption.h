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

#include <exception>
#include <stdexcept>
#include "str.h"

namespace lps::basic::exception {

class Error : public std::exception {};

template <int Line, typename... Args>
inline void fail(const std::string& file_name, Args... args) {
  auto msg = lps::basic::str::to("[File:", file_name,
                                 ","
                                 "Line:",
                                 Line, "]: ");
  std::throw_with_nested(std::runtime_error(lps::basic::str::to(msg, args...)));
}
}  // namespace lps::basic::exception

#define FAIL(COND, ...)                                           \
  if (!(COND)) {                                                  \
    lps::basic::exception::fail<__LINE__>(__FILE__, __VA_ARGS__); \
  }
