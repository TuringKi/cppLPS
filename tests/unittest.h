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

#include <map>
#include <unordered_map>
#include <vector>
#include "basic/exception.h"

#define EXPECT(cond) EXPECT_TRUE(cond);

#define EXPECT_TRUE(cond)                                                   \
  if (!(cond))                                                              \
    lps::basic::exception::do_assert(false, "unittest", __FILE__, __LINE__, \
                                     __func__, #cond);

#define EXPECT_FALSE(cond)                                                  \
  if (cond)                                                                 \
    lps::basic::exception::do_assert(false, "unittest", __FILE__, __LINE__, \
                                     __func__, #cond);

#define EXPECT_NULL(value) EXPECT_TRUE((value) == NULL);

#define EXPECT_NOTNULL(value) EXPECT_TRUE((value) != NULL);

#define EXPECT_STREQ(a, b)                                                    \
  if (std::string(a).compare(std::string(b)) != 0) {                          \
    printf("%s{    info} %s", lps::unittest::yellow(), lps::unittest::def()); \
    std::cout << "values: " << (a) << " != " << (b) << std::endl;             \
    lps::basic::exception::do_assert(false, "unittest", __FILE__, __LINE__,   \
                                     __func__, #a " == " #b);                 \
  }

#define EXPECT_STRNEQ(a, b)                                                   \
  if (std::string(a).compare(std::string(b)) != = 0) {                        \
    printf("%s{    info} %s", lps::unittest::yellow(), lps::unittest::def()); \
    std::cout << "values: " << #a << " == " << #b << std::endl;               \
    lps::basic::exception::do_assert(false, "unittest", __FILE__, __LINE__,   \
                                     __func__, #a " != " #b);                 \
  }

#define EXPECT_EQ(a, b)                                                       \
  if ((a) != (b)) {                                                           \
    printf("%s{    info} %s", lps::unittest::yellow(), lps::unittest::def()); \
    std::cout << "values: " << #a << " != " << #b << std::endl;               \
    EXPECT_TRUE(false);                                                       \
  }

#define EXPECT_NEQ(a, b)                                                      \
  if ((a) == (b)) {                                                           \
    printf("%s{    info} %s", lps::unittest::yellow(), lps::unittest::def()); \
    std::cout << "values: " << #a << " == " << #b << std::endl;               \
    EXPECT_TRUE(false);                                                       \
  }

#define TEST(group_name, name)                                              \
  void name();                                                              \
  namespace {                                                               \
  bool __##name = lps::unittest::Manager::append(#group_name, #name, name); \
  }                                                                         \
  void name()

namespace lps::unittest {

inline const char* red() {
  return "\033[1;31m";
}

inline const char* green() {
  return "\033[0;32m";
}

inline const char* yellow() {
  return "\033[0;33m";
}

inline const char* def() {
  return "\033[0m";
}
namespace print {
inline void running(const char* group_name, const char* name,
                    FILE* file = stdout) {
  if (name != nullptr) {
    fprintf(file, "-->%s{running}%s [%s]-[%s]\n", green(), def(), group_name,
            name);
    return;
  }
  fprintf(file, "%s{   running}%s [%s]\n", green(), def(), group_name);
}

inline void ok(const char* group_name, const char* name, FILE* file = stdout) {
  if (name != nullptr) {
    fprintf(file, "-->%s{     ok}%s [%s]-[%s]\n", green(), def(), group_name,
            name);
    return;
  }
  fprintf(file, "%s{        ok}%s [%s]\n", green(), def(), group_name);
}

inline void failed(const char* group_name, const char* name,
                   FILE* file = stdout) {
  if (name != nullptr) {
    fprintf(file, "-->%s{ failed} [%s]-[%s]%s\n", red(), group_name, name,
            def());
    return;
  }
  fprintf(file, "%s{    failed} [%s]%s\n", red(), group_name, def());
}
}  // namespace print

class Manager {
 public:
  struct Test {
    const char* name_;
    std::function<void()> fn_;
  };

  static std::unordered_map<std::string, std::vector<Test>>& tests() {
    static std::unordered_map<std::string, std::vector<Test>> tests;
    return tests;
  }

  inline static bool append(const char* group_name, const char* name,
                            const std::function<void()>& fn) {
    if (tests().contains(group_name)) {
      tests()[group_name].push_back({name, fn});
    } else {
      tests()[group_name] = {{{name, fn}}};
    }
    return true;
  }

  inline static size_t run(FILE* file = stdout) {
    size_t num_failed = 0;

    for (const auto& test_group : tests()) {
      print::running(test_group.first.c_str(), nullptr, file);
      bool all_ok = true;
      for (const auto& test : test_group.second) {
        try {
          print::running(test_group.first.c_str(), test.name_, file);
          test.fn_();
          print::ok(test_group.first.c_str(), test.name_, file);

        } catch (lps::basic::exception::Error& e) {
          print::failed(test_group.first.c_str(), test.name_, file);
          fprintf(file, "           %sAssertion failed: %s%s\n", red(),
                  e.what(), def());
          ++num_failed;
          all_ok = false;
        }
      }
      if (all_ok) {
        print::ok(test_group.first.c_str(), nullptr, file);
      } else {
        print::failed(test_group.first.c_str(), nullptr, file);
      }
    }

    int return_code = (num_failed > 0) ? 1 : 0;
    return return_code;
  }
};

class Runtime {
 public:
  static const std::vector<std::string>& args(int argc = -1,
                                              char** argv = nullptr) {
    static std::vector<std::string> args;
    if (argc >= 0) {
      for (int i = 0; i < argc; ++i) {
        args.emplace_back(argv[i]);
      }
    }
    return args;
  }
};
}  // namespace lps::unittest

#define TEST_MAIN()                                                  \
  int main(int argc, char* argv[]) {                                 \
    lps::unittest::Runtime::args(argc, argv);                        \
                                                                     \
    size_t num_failed = lps::unittest::Manager::run(stdout);         \
    if (num_failed == 0) {                                           \
      fprintf(stdout, "%s{ summary} all tests succeeded!%s\n",       \
              lps::unittest::green(), lps::unittest::def());         \
      return 0;                                                      \
    }                                                                \
    double percentage =                                              \
        100.0 * num_failed / lps::unittest::Manager::tests().size(); \
    fprintf(stderr, "%s{ summary} %lu tests failed (%.2f%%)%s\n",    \
            lps::unittest::red(), num_failed, percentage,            \
            lps::unittest::def());                                   \
    return -1;                                                       \
  }
