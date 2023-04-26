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

#include <cstdint>
#include <memory>
#include "basic/exception.h"
#include "basic/str.h"
#include "basic/vec.h"
#include "diag.h"
#include "token.h"

namespace lps::lexer {

namespace details {

enum MethodType : uint8_t { kNone = 0, kBasic };

template <meta::Str TagName>
class Base : virtual public lps::basic::mem::TraceTag<TagName> {

 public:
  using type = Base<TagName>;
  using const_ptr_type = const type*;
  inline virtual void lex(const char* ptr, lps::token::Token<TagName>& tok) = 0;
  [[nodiscard]] MethodType method() const { return type_; }

 protected:
  inline uint32_t escaped_newline_scanning(const char* ptr) {
    using namespace basic::str::ascii;
    uint32_t sz = 0;

    // until we find `'\r'` or `'\n'`:
    while (is::Ws(ptr[sz])) {
      sz++;
      if (ptr[sz - 1] != '\n' && ptr[sz - 1] != '\r') {
        continue;
      }
      if ((ptr[sz] == '\r' || ptr[sz] == '\n') && ptr[sz - 1] != ptr[sz]) {
        sz++;
      }
      return sz;
    }

    return 0;
  }

  inline char char_scanning(const char* ptr, lps::token::Token<TagName>* tok,
                            uint32_t& size) {
    using namespace basic::str::ascii;

    if (*ptr == '\\') {
      ptr++;
      size++;
    Slash:
      if (!is::Ws(
              *ptr)) {  // if next char is not `white space`, then it's just `'\\'`.
        return '\\';
      }
      if (uint32_t newline_size = escaped_newline_scanning(ptr)) {
        if (tok) {
          tok->set_flag(token::Flag::kNeedClean);
        }
        if (*ptr != '\n' && *ptr != '\r' && tok) {
          diag::doing(ptr, diag::DiagKind::kBackslashNewlineSpace);
        }
        size += newline_size;
        ptr += newline_size;
        return char_scanning(ptr, tok, size);
      }
      return '\\';
    }

    //TODO(@mxlol233): handle `trigraph char`
    if (*ptr == '?' && ptr[1] == '?') {}

    size++;
    return *ptr;
  }

  inline char char_scanning_2(const char*& ptr,
                              lps::token::Token<TagName>& tok) {
    using namespace basic::str::ascii;

    if (is::NormalChar(ptr[0]))
      return *ptr++;

    uint32_t sz = 0;
    char c = char_scanning(ptr, &tok, sz);
    ptr += sz;
    return c;
  }

  MethodType type_{MethodType::kNone};
};

template <meta::Str TagName>
class Basic : public Base<TagName> {

 public:
  inline void lex(const char* ptr, lps::token::Token<TagName>& tok) override {
    using namespace basic::str::ascii;

    const char* first = ptr;
    if (is::HorzWs(*ptr)) {
      do {
        ptr++;
      } while (is::HorzWs(*ptr));
    }

    char c = this->char_scanning_2(ptr, tok);
    lps::token::tok::TokenKind token_lind;
  }
};

}  // namespace details

class Lexer {
 public:
  template <meta::Str TagName>
  void lex(lps::token::Token<TagName>& tok,
           details::MethodType method = details::MethodType::kBasic) {
    switch (method) {
      case details::kBasic: {
        details::Basic<TagName> m;
        m.lex(ptr_, tok);
        break;
      }
      case details::kNone:
      default:
        unreachable(TagName);
        break;
    }
  }

  Lexer() = delete;
  explicit Lexer(const char* ptr) : ptr_(ptr) {}

 private:
  [[nodiscard]] const char* cur() const { return ptr_ + pos_; }
  void inc(size_t n) { pos_ += n; }

  const char* ptr_{nullptr};
  size_t pos_{0};
};

}  // namespace lps::lexer
