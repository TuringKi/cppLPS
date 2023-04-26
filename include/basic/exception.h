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
#include <ostream>
#include <stdexcept>
#include <utility>
#include "log.h"
#include "meta.h"
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
namespace details {

template <meta::Str TagName, meta::Str FileName, uint32_t Line,
          meta::Str FuncName, typename... Args>
std::string msg(Args... args) {
  std::string msg;
  if (decltype(TagName)::empty()) {
    msg = tui::color::Shell::colorize(
        lps::basic::str::from("[File:", FileName,
                              ","
                              "Line:",
                              Line, ", Func: ", FuncName, "]: "),
        lps::basic::tui::color::Shell::fgreen());

  } else {
    auto m0 = tui::color::Shell::colorize(
        lps::basic::str::from(TagName), lps::basic::tui::color::Shell::fblue());

    auto m1 = tui::color::Shell::colorize(
        lps::basic::str::from(", File:", FileName,
                              ","
                              "Line:",
                              Line, ", Func: ", FuncName, "]: "),
        lps::basic::tui::color::Shell::fgreen());
    msg = tui::color::Shell::colorize(lps::basic::str::from("[Tag:", m0, m1),
                                      lps::basic::tui::color::Shell::fgreen());
  }

  msg = lps::basic::str::from(msg, args...);
  return msg;
}
}  // namespace details

template <meta::Str TagName, meta::Str FileName, uint32_t Line,
          meta::Str FuncName, typename... Args>
inline void warning(Args... args) {

  std::cout << details::msg<TagName, FileName, Line, FuncName, Args...>(args...)
            << std::endl;
}

template <meta::Str TagName, meta::Str FileName, uint32_t Line,
          meta::Str FuncName, typename... Args>
inline void note(Args... args) {

  std::cout << details::msg<TagName, FileName, Line, FuncName, Args...>(args...)
            << std::endl;
}

template <meta::Str TagName, meta::Str FileName, uint32_t Line,
          meta::Str FuncName, typename... Args>
inline void fail(Args... args) {
  throw Error(
      details::msg<TagName, FileName, Line, FuncName, Args...>(args...));
}

template <meta::Str TagName, meta::Str FileName, uint32_t Line,
          meta::Str FuncName>
inline void assert(bool condition, std::string&& msg) {
  if (!condition)
    throw Error(details::msg<TagName, FileName, Line, FuncName>(msg));
}

}  // namespace lps::basic::exception

#define LPS_CHECK_ERROR(COND, ...)                                            \
  if (!(COND)) {                                                              \
    lps::basic::exception::fail<meta::Str(""), meta::Str(__FILE__), __LINE__, \
                                meta::Str(__func__)>(                         \
        lps::basic::tui::color::Shell::colorize(                              \
            " [`" #COND "`] ", lps::basic::tui::color::Shell::fred()),        \
        "not true. ", ##__VA_ARGS__);                                         \
  }

#define LPS_CHECK_WARNING(COND, ...)                                      \
  if (!(COND)) {                                                          \
    lps::basic::exception::warning<meta::Str(""), meta::Str(__FILE__),    \
                                   __LINE__, meta::Str(__func__)>(        \
        lps::basic::tui::color::Shell::colorize(                          \
            " [`" #COND "`] ", lps::basic::tui::color::Shell::fyellow()), \
        "not true. ", ##__VA_ARGS__);                                     \
  }

#define LPS_ERROR(TagName, ...)                                         \
  {                                                                     \
    lps::basic::exception::fail<TagName, meta::Str(__FILE__), __LINE__, \
                                meta::Str(__func__)>(                   \
        lps::basic::tui::color::Shell::colorize(                        \
            " [ERROR]: ", lps::basic::tui::color::Shell::fred()),       \
        ##__VA_ARGS__);                                                 \
  }

#define LPS_WARNING(TagName, ...)                                          \
  {                                                                        \
    lps::basic::exception::warning<TagName, meta::Str(__FILE__), __LINE__, \
                                   meta::Str(__func__)>(                   \
        lps::basic::tui::color::Shell::colorize(                           \
            " [ERROR]: ", lps::basic::tui::color::Shell::fyellow()),       \
        ##__VA_ARGS__);                                                    \
  }

#define LPS_NOTE(TagName, ...)                                          \
  {                                                                     \
    lps::basic::exception::note<TagName, meta::Str(__FILE__), __LINE__, \
                                meta::Str(__func__)>(                   \
                                                                        \
        " [NOTE]: ", ##__VA_ARGS__);                                    \
  }

#define unreachable(TagName) LPS_ERROR(TagName)

#define lps_assert(TagName, COND)                                       \
  lps::basic::exception::assert<TagName, meta::Str(__FILE__), __LINE__, \
                                meta::Str(__func__)>(                   \
      (COND), lps::basic::tui::color::Shell::colorize(                  \
                  " [ASSERT FAILED]: [`" #COND "`] ",                   \
                  lps::basic::tui::color::Shell::fred()));
