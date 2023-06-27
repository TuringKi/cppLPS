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
#include "basic/str.h"
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
    typename base::ptr_type ptr = this->cur();
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

  inline bool lex_pp_number(char c, typename base::ptr_type& ptr,
                            lps::token::Token<TagName>& tok) {
    return [this](char c, typename base::ptr_type& ptr,
                  lps::token::Token<TagName>& tok) {
      auto digit = [](typename base::ptr_type& ptr) {
        return base::template lex_char<basic::str::ascii::is::Digit>(ptr);
      };
      auto nodigit = [](typename base::ptr_type& ptr) {
        return base::template lex_char<basic::str::ascii::is::NonDigit>(ptr);
      };
      auto sign = [](typename base::ptr_type& ptr) {
        return base::template lex_char<[](char c) {
          return c == '+' || c == '-';
        }>(ptr);
      };

      auto lex = [this, &digit, &nodigit, &sign](
                     char c, typename base::ptr_type& ptr,
                     lps::token::Token<TagName>& tok, auto func) -> bool {
        bool flg = false;
        if (basic::str::ascii::is::Digit(c)) {  // 	digit
          flg = true;
        }
        if (!flg) {
          typename base::ptr_type tmp_ptr = ptr;
          if (*tmp_ptr == '.') {  // 	`.`, digit
            if (digit(tmp_ptr)) {
              ptr = tmp_ptr;
              flg = true;
            }
          }
        }

        if (flg) {
          typename base::ptr_type tmp_ptr = ptr;
          bool sub_flg = true;
          char cc = *tmp_ptr;
          tmp_ptr++;
          if (func(cc, tmp_ptr, tok, func)) {  // 	pp_number, ...
            ptr = tmp_ptr;
            sub_flg = false;
          }

          if (!sub_flg) {  // pp_number, identifier_nondigit
            // todo(@mxlol233): take `identifier_nondigit` into account.
          }

#define CASE(COND)    \
  if (!sub_flg) {     \
    tmp_ptr = ptr;    \
    if (COND) {       \
      sub_flg = true; \
      ptr = tmp_ptr;  \
    }                 \
  }
          CASE(digit(tmp_ptr));                        // pp_number, digit
          CASE(*tmp_ptr == '\'' && digit(++tmp_ptr));  // pp_number, `'`, digit
          CASE(*tmp_ptr == '\'' &&
               nodigit(++tmp_ptr));  // pp_number, `'`, nondigit
          CASE((*tmp_ptr == 'e' || *tmp_ptr == 'E' || *tmp_ptr == 'p' ||
                *tmp_ptr == 'P') &&
               sign(++tmp_ptr));  // pp_number, x, sign
          CASE(*tmp_ptr == '.');  // pp_number, `.`
#undef CASE
          this->token_formulate(tok, ptr,
                                lps::token::tok::TokenKind::pp_number);
          return true;
        }
        return false;
      };
      return lex(c, ptr, tok, lex);
    }(c, ptr, tok);
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
  inline bool lex_preprocessing_token(char c, typename base::ptr_type& ptr,
                                      lps::token::Token<TagName>& tok) {
#define TRY(FUNC)                             \
  [&](char c_, typename base::ptr_type& ptr_, \
      lps::token::Token<TagName>& tok_) {     \
    typename base::ptr_type tmp_ptr = ptr_;   \
    lps::token::Token<TagName> tmp_tok;       \
    if (FUNC(c_, tmp_ptr, tmp_tok)) {         \
      tok_ = tmp_tok;                         \
      ptr_ = tmp_ptr;                         \
      return true;                            \
    }                                         \
    return false;                             \
  }(c, ptr, tok)
#define CASE(func) \
  if (TRY(func)) { \
    return true;   \
  }

    auto try_keyword = [this](token::tok::TokenKind kind) {
      return [this, kind](char /*c*/, typename base::ptr_type& ptr,
                          lps::token::Token<TagName>& tok) {
        if (this->lex_identifier(ptr, tok)) {
          return tok.kind() == kind;
        }
        return false;
      };
    };

    auto try_import = try_keyword(token::tok::TokenKind::kw_import);
    auto try_module = try_keyword(token::tok::TokenKind::kw_module);
    auto try_export = try_keyword(token::tok::TokenKind::kw_export);
    auto try_ident = try_keyword(token::tok::TokenKind::identifier);

#define TRY_UD(FUNC, KIND)                                 \
  [this, &try_ident](char c, typename base::ptr_type& ptr, \
                     lps::token::Token<TagName>& tok) {    \
    if (FUNC(c, ptr, tok)) {                               \
      char cc = *ptr;                                      \
      ptr++;                                               \
      if (try_ident(cc, ptr, tok)) {                       \
        this->token_formulate(tok, ptr, KIND);             \
        return true;                                       \
      }                                                    \
    }                                                      \
    return false;                                          \
  }

    auto try_char_literal_ud =
        TRY_UD(this->lex_character_literal,
               token::tok::TokenKind::user_defined_char_literal);
    auto try_string_literal_ud =
        TRY_UD(this->lex_string_literal,
               token::tok::TokenKind::user_defined_string_literal);
#undef TRY_UD

    CASE(this->lex_header_name);        // 	header_name
    CASE(try_import);                   // 	`import`
    CASE(try_module);                   // 	`module`
    CASE(try_export);                   // 	`export`
    CASE(try_ident);                    // 	`identifier`
    CASE(lex_pp_number);                // 	pp_number
    CASE(this->lex_character_literal);  // 	character_literal
    CASE(try_char_literal_ud);          // 	user_defined_character_literal
    CASE(this->lex_string_literal);     // 	string_literal
    CASE(try_string_literal_ud);        // 	user_defined_string_literal
    CASE(this->lex_preprocessing_op_or_punc);  // preprocessing_op_or_punc
#undef TRY
#undef CASE

    return false;
  }

  // pp-tokens:
  // 	preprocessing_token
  // 	pp_tokens, preprocessing_token
  inline bool lex_pp_tokens(char c, typename base::ptr_type& ptr,
                            lps::token::Token<TagName>& tok) {
    return [this](char c, typename base::ptr_type& ptr,
                  lps::token::Token<TagName>& tok) {
      auto lex = [this](char c, typename base::ptr_type& ptr,
                        lps::token::Token<TagName>& tok,
                        const auto& func) -> bool {
        if (lex_preprocessing_token(c, ptr, tok)) {

          char tmp_c = *ptr;
          typename base::ptr_type tmp_ptr = ptr + 1;
          lps::token::Token<TagName> tmp_tok;
          if (func(tmp_c, tmp_ptr, tmp_tok, func)) {
            ptr = tmp_ptr;
            tok = tmp_tok;
          }
          tok.kind(token::tok::TokenKind::pp_tokens);
          return true;
        }
        return false;
      };
      return lex(c, ptr, tok, lex);
    }(c, ptr, tok);
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
  inline bool lex_control_line(typename base::ptr_type& ptr,
                               lps::token::Token<TagName>& tok) {
    using namespace basic::str::ascii;

    typename base::ptr_type tmp_ptr = ptr;
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
            typename base::ptr_type tmp_ptr2 = tmp_ptr;
            tmp_ptr2++;
            lps::token::Token<TagName> next_tok;
            // replacement-list: pp_tokens[opt]
            if (lex_pp_tokens(*tmp_ptr, tmp_ptr2, next_tok)) {
              if (basic::str::ascii::is::VertWs(*tmp_ptr2)) {
                // [`#`, `define`, `identifier`, replacement_list, new_line] matched
                tmp_ptr = tmp_ptr2;
              }
            } else {  // try others.

              if (*tmp_ptr == '(' && !basic::str::ascii::is::Ws(*tmp_ptr2)) {
                // [lparen] matched, now consider:
                // 1. identifier_list[opt], `)`, replacement_list, new_line
                // 2. `...`, `)`, replacement_list, new_line
                // 3. identifier_list, `...`, `)`, replacement_list, new_line
                char tmp_c = *tmp_ptr2;
                tmp_ptr2++;
                std::vector<typename base::lex_func_type2> funcs = {
                    [this](char c, typename base::ptr_type& ptr,
                           lps::token::Token<TagName>& tok) -> bool {
                      return false;
                    },
                    [this](char c, typename base::ptr_type& ptr,
                           lps::token::Token<TagName>& tok) -> bool {
                      return false;
                    },

                };
                if (this->lex_something_parallel(tmp_c, tmp_ptr2, next_tok,
                                                 funcs)) {
                  unreachable(TagName);
                }
              }
            }

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
};  // namespace lps::lexer::details

}  // namespace lps::lexer::details
