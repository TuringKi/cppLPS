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
#include <cstring>
#include <limits>
#include <memory>
#include "lex/base.h"
#include "lex/basic.h"
#include "lex/pp.h"
namespace lps::lexer {

class Lexer {
 public:
  template <meta::Str TagName>
  void lex(lps::token::Token<TagName>& tok,
           details::MethodType method = details::MethodType::kBasic) {
    tok.clear();
    if (finish()) {
      tok.kind(token::tok::TokenKind::eof);
      return;
    }
    switch (method) {
      case details::kBasic: {
        details::Basic<TagName> m(file_id_, cur(), end_);
        m.lex(tok);
        inc(m.pos());
        break;
      }
      case details::kPreprocessing: {
        details::Preprocessing<TagName> m(file_id_, cur(), end_);
        m.lex(tok);
        inc(m.pos());
        break;
      }
      case details::kNone:
      default:
        unreachable(TagName);
        break;
    }
    inc(tok.offset());
  }

  Lexer() = delete;
  explicit Lexer(uint32_t start_file_id, const char* ptr, const char* end)
      : file_id_(start_file_id), ptr_(ptr), end_(end) {}
  [[nodiscard]] bool finish(size_t len) const { return !(pos_ < len); }
  [[nodiscard]] bool finish() const { return !(pos_ < (end_ - ptr_)); }
  [[nodiscard]] const char* cur() const { return ptr_ + pos_; }

 private:
  void inc(size_t n) { pos_ += n; }

  const char* ptr_{nullptr};
  const char* end_{nullptr};
  uint32_t file_id_{0};
  size_t pos_{0};
};

}  // namespace lps::lexer
