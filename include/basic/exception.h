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

#include <cstring>
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
  explicit Error(std::string&& msg) : msg_(std::move(msg)) {
    msg_.erase(std::remove(msg_.begin(), msg_.end(), 0), msg_.end());
  }
  [[nodiscard]] const char* what() const noexcept override {
    return msg_.c_str();
  }

 private:
  std::string msg_;
};
namespace details {

template <typename... Args>
std::string msg(const char* tag_name, const char* file_name, uint32_t line,
                const char* func_name, const Args&... args) {
  std::string msg;
  if (std::strlen(tag_name) == 0) {
    msg = tui::color::Shell::colorize(
        lps::basic::str::from("[File:", file_name,
                              ","
                              "Line:",
                              line, ", Func: ", func_name, "]: "),
        lps::basic::tui::color::Shell::fgreen());

  } else {
    auto m0 =
        tui::color::Shell::colorize(lps::basic::str::from(tag_name),
                                    lps::basic::tui::color::Shell::fblue());

    auto m1 = tui::color::Shell::colorize(
        lps::basic::str::from(", File:", file_name,
                              ","
                              "Line:",
                              line, ", Func: ", func_name, "]: "),
        lps::basic::tui::color::Shell::fgreen());
    msg = tui::color::Shell::colorize(lps::basic::str::from("[Tag:", m0, m1),
                                      lps::basic::tui::color::Shell::fgreen());
  }

  msg = lps::basic::str::from(msg, args...);
  return msg;
}
}  // namespace details

template <typename... Args>
inline void warning(const char* tag_name, const char* file_name, uint32_t line,
                    const char* func_name, const Args&... args) {

  std::cout << details::msg<Args...>(tag_name, file_name, line, func_name,
                                     args...)
            << std::endl;
}

template <typename... Args>
inline void note(const char* tag_name, const char* file_name, uint32_t line,
                 const char* func_name, const Args&... args) {

  std::cout << details::msg<Args...>(tag_name, file_name, line, func_name,
                                     args...)
            << std::endl;
}

template <typename... Args>
inline void fail(const char* tag_name, const char* file_name, uint32_t line,
                 const char* func_name, const Args&... args) {
  throw Error(
      details::msg<Args...>(tag_name, file_name, line, func_name, args...));
}

inline void assert(bool condition, const char* tag_name, const char* file_name,
                   uint32_t line, const char* func_name, std::string&& msg) {
  if (!condition)
    throw Error(details::msg(tag_name, file_name, line, func_name, msg));
}

}  // namespace lps::basic::exception

#define LPS_CHECK_ERROR(TagName, COND, ...)                            \
  if (!(COND)) {                                                       \
    lps::basic::exception::fail(                                       \
        TagName, __FILE__, __LINE__, __func__,                         \
        lps::basic::tui::color::Shell::colorize(                       \
            " [`" #COND "`] ", lps::basic::tui::color::Shell::fred()), \
        "not true. ", ##__VA_ARGS__);                                  \
  }

#define LPS_CHECK_WARNING(TagName, COND, ...)                         \
  if (!(COND)) {                                                      \
    lps::basic::exception::warning(                                   \
        lps::basic::tui::color::Shell::colorize(                      \
            TagName, __FILE__, __LINE__, __func__, " [`" #COND "`] ", \
            lps::basic::tui::color::Shell::fyellow()),                \
        "not true. ", ##__VA_ARGS__);                                 \
  }

#define LPS_ERROR(TagName, ...)                                   \
  {                                                               \
    lps::basic::exception::fail(                                  \
        TagName, __FILE__, __LINE__, __func__,                    \
        lps::basic::tui::color::Shell::colorize(                  \
            " [ERROR]: ", lps::basic::tui::color::Shell::fred()), \
        ##__VA_ARGS__);                                           \
  }

#define LPS_WARNING(TagName, ...)                                    \
  {                                                                  \
    lps::basic::exception::warning(                                  \
        TagName, __FILE__, __LINE__, __func__,                       \
        lps::basic::tui::color::Shell::colorize(                     \
            " [ERROR]: ", lps::basic::tui::color::Shell::fyellow()), \
        ##__VA_ARGS__);                                              \
  }

#define LPS_NOTE(TagName, ...)                                         \
  {                                                                    \
    lps::basic::exception::note(TagName, __FILE__, __LINE__, __func__, \
                                " [NOTE]: ", ##__VA_ARGS__);           \
  }

#define unreachable(TagName) LPS_ERROR(TagName, "unreachable")

#define lps_assert(TagName, COND)                                              \
  lps::basic::exception::assert((COND), TagName, __FILE__, __LINE__, __func__, \
                                lps::basic::tui::color::Shell::colorize(       \
                                    " [ASSERT FAILED]: [`" #COND "`] ",        \
                                    lps::basic::tui::color::Shell::fred()));
