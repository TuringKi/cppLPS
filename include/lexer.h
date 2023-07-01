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
#include "src.h"
namespace lps::lexer {

class Lexer {
 public:
  constexpr static basic::mem::TraceTag::tag_type kTag = "lps::lexer::Lexer";
  void lex(lps::token::Token& tok,
           details::MethodType method = details::MethodType::kBasic) {
    tok.clear();
    if (src::Manager::instance().has<1>(file_id_)) {
      // if current file is token_file, we just return the recorded next token.
      token::TokenListsVisitor visitor =
          src::Manager::instance().visitor_of_token_file(file_id_);
      visitor += ++pos_;
      tok = *visitor;
      return;
    }
    const char* end = start_ + src::Manager::instance().size(file_id_);
    switch (method) {
      case details::kBasic: {
        details::Basic m(file_id_, cur(), end);
        m.lex(tok);
        inc(m.pos());
        break;
      }
      case details::kPreprocessing: {
        details::pp::Preprocessing m(file_id_, cur(), end);
        m.lex(tok);
        inc(m.pos());
        break;
      }
      case details::kNone:
      default:
        unreachable(kTag);
        break;
    }
    inc(tok.offset());
  }

  Lexer() = delete;
  explicit Lexer(uint32_t start_file_id, const char* ptr)
      : file_id_(start_file_id), start_(ptr) {}
  explicit Lexer(uint32_t start_file_id, size_t offset)
      : file_id_(start_file_id) {
    if (src::Manager::instance().has<0>(start_file_id)) {  // char_files
      start_ = token::TokenLists::Info::start(start_file_id);
      pos_ = offset;
    } else if (src::Manager::instance().has<1>(start_file_id)) {  // token_files
      pos_ = offset;
    }
  }

  [[nodiscard]] const char* cur() const { return start_ + pos_; }

 private:
  void inc(size_t n) { pos_ += n; }

  const char* start_{nullptr};
  uint32_t file_id_{0};
  size_t pos_{0};
};

}  // namespace lps::lexer
