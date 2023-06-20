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

#include "basic/exception.h"
#include "diag.h"
#include "lex/base.h"
#include "token.h"
#include "tu.h"

namespace lps::lexer::details {

template <meta::Str TagName>
class Preprocessing : public Base<TagName> {
  using base = Base<TagName>;

 public:
  explicit Preprocessing(uint32_t start_file_id, const char* ptr,
                         const char* end)
      : base(start_file_id, ptr, end, MethodType::kPreprocessing) {}
  inline void lex(lps::token::Token<TagName>& tok) override {
    // do a tiny parsing process.
    const char* ptr = this->cur();
    if (*ptr == '#') {  // control-line
      ptr++;
      lex_control_line(ptr, tok);
    }
    if (!this->lex_identifier(ptr, tok)) {
      unreachable(TagName);
    }
  }

 private:
  // pp-number:
  // 	digit
  // 	`.`, digit
  // 	pp_number, digit
  // 	pp_number, identifier_nondigit
  // 	pp_number, `'`, digit
  // 	pp_number, `'`, nondigit
  // 	pp_number, `e`, sign
  // 	pp_number, `E`, sign
  // 	pp_number, `p`, sign
  // 	pp_number, `P`, sign
  // 	pp_number, `.`

  inline bool lex_pp_number(char c, const char*& ptr,
                            lps::token::Token<TagName>& tok) {
    unreachable(TagName);
    auto lex_pp_number_impl = [this]() {

    };

    auto is_digit = [](const char*& ptr) {
      if (*ptr >= '0' && *ptr <= '9') {
        ptr++;
        return true;
      }
      return false;
    };
  }

  // preprocessing-token:
  // 	header_name
  // 	`import`
  // 	`module`
  // 	`export`
  // 	`identifier`
  // 	pp_number
  // 	character_literal
  // 	user_defined_character_literal
  // 	string_literal
  // 	user_defined_string_literal
  // 	preprocessing_op_or_punc
  // each non-whitespace character that cannot be one of the above
  inline bool lex_preprocessing_token(char c, const char*& ptr,
                                      lps::token::Token<TagName>& tok) {
#define try_lex(func)                                                 \
  [&](char c_, const char*& ptr_, lps::token::Token<TagName>& tok_) { \
    const char* tmp_ptr = ptr_;                                       \
    lps::token::Token<TagName> tmp_tok;                               \
    if (func(c_, tmp_ptr, tmp_tok)) {                                 \
      tok_ = tmp_tok;                                                 \
      ptr_ = tmp_ptr;                                                 \
      return true;                                                    \
    }                                                                 \
    return false;                                                     \
  }(c, ptr, tok)

    auto try_keyword = [this](token::tok::TokenKind kind) {
      return [this, kind](char /*c*/, const char*& ptr,
                          lps::token::Token<TagName>& tok) {
        if (this->lex_identifier(ptr, tok)) {
          return tok.kind() == kind;
        }
        return false;
      };
    };

    if (try_lex(this->lex_header_name)) {
      return true;
    }

    auto try_import = try_keyword(token::tok::TokenKind::kw_import);
    if (try_lex(try_import)) {
      return true;
    }

    auto try_module = try_keyword(token::tok::TokenKind::kw_module);
    if (try_lex(try_module)) {
      return true;
    }

    auto try_export = try_keyword(token::tok::TokenKind::kw_export);
    if (try_lex(try_export)) {
      return true;
    }

    auto try_ident = try_keyword(token::tok::TokenKind::identifier);
    if (try_lex(try_ident)) {
      return true;
    }

    unreachable(TagName);

    return false;
  }

  // control-line:
  // 	`#`, `include`, pp_tokens, new_line
  // 	pp_import
  // 	`#`, `define`, `identifier`, replacement_list, new_line
  // 	`#`, `define`, `identifier`, lparen, identifier_list[opt], `)`, replacement_list, new_line
  // 	`#`, `define`, `identifier`, lparen, `...`, `)`, replacement_list, new_line
  // 	`#`, `define`, `identifier`, lparen, identifier_list, `...`, `)`, replacement_list, new_line
  // 	`#`, `undef`, `identifier`, new_line
  // 	`#`, `line`, pp_tokens, new_line
  // 	`#`, `error`, pp_tokens[opt], new_line
  // 	`#`, `pragma`, pp_tokens[opt], new_line
  // 	`#`, new_line
  inline bool lex_control_line(const char*& ptr,
                               lps::token::Token<TagName>& tok) {
    using namespace basic::str::ascii;

    const char* tmp_ptr = ptr;
    if (this->lex_identifier(tmp_ptr, tok)) {
      switch (tok.kind()) {
        case token::tok::TokenKind::kw_define:
        case token::tok::TokenKind::kw_undef: {
          lps::token::Token<TagName> define_tok;
          if (!this->lex_identifier(tmp_ptr, define_tok)) {
            this->diag(tmp_ptr,
                       diag::DiagKind::expected_ident_after_define_undef);
            return false;
          }
          if (define_tok.kind() != token::tok::TokenKind::identifier) {
            this->diag(tmp_ptr,
                       diag::DiagKind::expected_ident_after_define_undef);
            return false;
          }

          if (tok.kind() == token::tok::TokenKind::kw_define) {
            const char* tmp_ptr2 = tmp_ptr;
            tmp_ptr2++;
            lps::token::Token<TagName> next_tok;

            lex_preprocessing_token(*tmp_ptr, tmp_ptr2, next_tok);

            tu::TU::instance().define(define_tok);
          } else {
            tu::TU::instance().undef(define_tok);
          }

          break;
        }
        case token::tok::TokenKind::kw_line:
        case token::tok::TokenKind::kw_error:
        case token::tok::TokenKind::kw_pragma:

        default:
          unreachable(TagName);
      }
      ptr = tmp_ptr;
    }
    return is::VertWs(*ptr);
  }
  void command() {}
};

}  // namespace lps::lexer::details
