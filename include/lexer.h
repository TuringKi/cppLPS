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
#include "basic/exception.h"
#include "basic/file.h"
#include "lex/base.h"
#include "lex/basic.h"
#include "lex/pp.h"
#include "src.h"
#include "token.h"
namespace lps::lexer {

class Lexer {
 public:
  constexpr static basic::mem::TraceTag::tag_type kTag = "lps::lexer::Lexer";
  void lex(lps::token::Token& tok,
           details::MethodType method = details::MethodType::kBasic) {
  start:
    tok.clear();
    if (src::Manager::instance().has<1>(file_id_)) {
      // if current file is token_file, we just return the recorded next token.
      token::TokenListsVisitor visitor =
          src::Manager::instance().visitor_of_token_file(file_id_);
      visitor += pos_;
      tok = *visitor;
      return;
    }

    // try pp first
    {
      details::pp::Preprocessing m(visitor_);
      m.lex(tok);
      if (tok.kind() != token::details::TokenKind::unknown &&
          tok.kind() != token::details::TokenKind::eod) {
        return;
      }
    }
  eod:
    if (tok.kind() == token::details::TokenKind::eod) {
      // jump over `eod`
      auto next_info = tok.next_visitor();
      if (src::Manager::instance().has<0>(file_id_)) {
        auto visitor =
            src::Manager::instance().visitor_of_char_file(next_info.second);
        visitor += next_info.first;
        details::Basic::jump_include_stack(visitor);
        file_id_ = visitor.file_id();
        visitor_ = visitor;
        lps_assert(
            kTag, file_id_ != 0 && visitor.start() && visitor.end() && visitor);
      } else if (src::Manager::instance().has<1>(next_info.second)) {
        file_id_ = next_info.second;
        pos_ = next_info.first;
        lps_assert(kTag, file_id_ != 0);
      } else {
        unreachable(kTag);
      }
      goto start;
    }

    switch (method) {
      case details::kBasic: {
        details::Basic m(visitor_);
        m.lex(tok);
        if (tok.kind() == token::details::TokenKind::eod) {
          goto eod;
        }
        break;
      }
      case details::kPreprocessing: {
        details::pp::Preprocessing m(visitor_);
        m.lex(tok);
        if (tok.kind() == token::details::TokenKind::eod) {
          goto eod;
        }
        break;
      }
      case details::kNone:
      default:
        unreachable(kTag);
        break;
    }
    lps_assert(kTag, tok.kind() != token::details::TokenKind::unknown);
  }

  Lexer() = delete;
  explicit Lexer(uint32_t start_file_id, size_t offset) {
    if (src::Manager::instance().has<0>(start_file_id)) {  // char_files
      auto visitor =
          src::Manager::instance().visitor_of_char_file(start_file_id);
      visitor += offset;
      details::Basic::jump_include_stack(visitor);
      file_id_ = visitor.file_id();
      visitor_ = visitor;
      lps_assert(kTag,
                 file_id_ != 0 && visitor_.start() && visitor_.pos() >= 0);
      lps_assert(kTag, visitor_.cur() <= visitor.end());
    } else if (src::Manager::instance().has<1>(start_file_id)) {  // token_files
      file_id_ = start_file_id;
      pos_ = offset;
      lps_assert(kTag, file_id_ != 0 && pos_ >= 0);
    }
  }

 private:
  basic::FileVisitor visitor_{nullptr, nullptr,
                              [](const basic::vfile::Visitor<char>*) {
                              }};
  uint32_t file_id_{0};
  size_t pos_{0};
};

}  // namespace lps::lexer
