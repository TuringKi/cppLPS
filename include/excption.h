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
#include <utility>
#include "str.h"
#include "tui.h"

namespace lps::basic::exception {

class Error : public std::exception {
 public:
  explicit Error(std::string msg) : msg_(std::move(msg)) {}
  [[nodiscard]] const char* what() const noexcept override {
    return msg_.c_str();
  }

 private:
  std::string msg_;
};

template <auto Filename, uint32_t Line, auto Funcname, typename... Args>
inline void fail(Args... args) {
  auto msg = tui::color::Shell::colorize(
      lps::basic::str::from("[File:", Filename,
                            ","
                            "Line:",
                            Line, ", Func: ", Funcname, "]: "),
      lps::basic::tui::color::Shell::fgreen());

  throw Error(lps::basic::str::from(msg, args...));
}
}  // namespace lps::basic::exception

#define LPS_CHECK(COND, ...)                                                \
  if (!(COND)) {                                                            \
    lps::basic::exception::fail<lps::basic::str::details::array(__FILE__),  \
                                __LINE__,                                   \
                                lps::basic::str::details::array(__func__)>( \
        lps::basic::tui::color::Shell::colorize(                            \
            " [`" #COND "`] ", lps::basic::tui::color::Shell::fred()),      \
        "not true. ", ##__VA_ARGS__);                                       \
  }

#define LPS_ERROR(COND, ...)                                                \
  {                                                                         \
    lps::basic::exception::fail<lps::basic::str::details::array(__FILE__),  \
                                __LINE__,                                   \
                                lps::basic::str::details::array(__func__)>( \
        lps::basic::tui::color::Shell::colorize(                            \
            " [ERROR] ", lps::basic::tui::color::Shell::fred()),            \
        ##__VA_ARGS__);                                                     \
  }
