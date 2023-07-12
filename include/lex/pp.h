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
#include "basic/vfile.h"
#include "diag.h"
#include "lex/base.h"
#include "src.h"
#include "token.h"
#include "tu.h"

namespace lps::lexer::details::pp {

class Preprocessing : public Base {

 public:
  using base = Base;
  constexpr static basic::mem::TraceTag::tag_type kTag =
      "lps::lexer::details::pp::Preprocessing";
  explicit Preprocessing(const typename base::ptr_type& ptr)
      : base(ptr, MethodType::kPreprocessing) {}
  inline void lex_impl(lps::token::Token& tok) override {

    typename base::ptr_type ptr = this->cur();

    if (basic::str::ascii::is::Ws(*ptr)) {
      do {
        this->inc(1);
        ++ptr;
      } while (basic::str::ascii::is::Ws(*ptr));
    }

    auto first_ptr = ptr;
    if (*ptr == '#') {  // control-line
      ++ptr;
      auto tmp_ptr = ptr;
      auto tmp_tok = tok;
      if (lex_control_line(first_ptr, tmp_ptr, tmp_tok)) {
        ptr = tmp_ptr;
        tok = tmp_tok;
        return;
      }
    } else {
      if (basic::str::ascii::is::NonDigit(*first_ptr)) {
        auto tmp_ptr = ptr;
        auto tmp_tok = tok;
        ++tmp_ptr;
        if (this->lex_identifier(first_ptr, tmp_ptr, tmp_tok)) {
          if (tmp_tok.kind() == token::details::TokenKind::kw___has_include) {
            tok = tmp_tok;
            unreachable(kTag);
          }
        }
      }
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
  inline bool lex_pp_number(const typename base::ptr_type& first_ptr,
                            typename base::ptr_type& ptr,
                            lps::token::Token& tok) {
    if (!basic::str::ascii::is::Digit(*first_ptr)) {
      if (*first_ptr == '.') {
        if (!basic::str::ascii::is::Digit(*(ptr))) {
          return false;
        }
        ++ptr;
      } else {
        return false;
      }
    }
    while (!basic::str::ascii::is::Ws(*ptr)) {
      if (*ptr == '.') {
        ++ptr;
        continue;
      }
      if (basic::str::ascii::is::Digit(*ptr)) {  // pp_number, digit
        ++ptr;
        continue;
      }
      if (*ptr == '\'') {
        if (basic::str::ascii::is::Digit(
                *(ptr + 1))) {  // pp_number, `'`, digit
          ++ptr;
          ++ptr;
          continue;
        }
        diag(first_ptr, ptr,
             diag::DiagKind::unexpected_characters_in_pp_number);
        return false;
      }
      if (*ptr == 'e' || *ptr == 'E' || *ptr == 'p' ||
          *ptr == 'P') {  // pp_number, `e`, sign etc.
        if (*(ptr + 1) == '+' || *(ptr + 1) == '-') {
          ++ptr;
          ++ptr;
          continue;
        }
        diag(first_ptr, ptr,
             diag::DiagKind::unexpected_characters_in_pp_number);
        return false;
      }
      break;
    }
    token_formulate(tok, first_ptr, ptr,
                    lps::token::details::TokenKind::pp_number);
    return true;
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
  inline bool lex_preprocessing_token(const typename base::ptr_type& first_ptr_,
                                      typename base::ptr_type& ptr,
                                      lps::token::Token& tok) {
    typename base::ptr_type first_ptr = first_ptr_;
    if (basic::str::ascii::is::HorzWs(*first_ptr)) {
      first_ptr.horzws_skipping();
      ptr = first_ptr + 1;
    }

#define TRY(FUNC)                                                              \
  [&](const typename base::ptr_type& first_ptr, typename base::ptr_type& ptr_, \
      lps::token::Token& tok_) {                                               \
    typename base::ptr_type tmp_ptr = ptr_;                                    \
    lps::token::Token tmp_tok;                                                 \
    try {                                                                      \
      if (FUNC(first_ptr, tmp_ptr, tmp_tok)) {                                 \
        tok_ = tmp_tok;                                                        \
        ptr_ = tmp_ptr;                                                        \
        return true;                                                           \
      }                                                                        \
    } catch (basic::vfile::Eof & except_eof) {                                 \
      return false;                                                            \
    }                                                                          \
    return false;                                                              \
  }(first_ptr, ptr, tok)
#define CASE(func) \
  if (TRY(func)) { \
    return true;   \
  }

    auto try_keyword = [this](token::details::TokenKind kind) {
      return
          [this, kind](const typename base::ptr_type& first_ptr,
                       typename base::ptr_type& ptr, lps::token::Token& tok) {
            if (basic::str::ascii::is::NonDigit(*first_ptr)) {
              if (this->lex_identifier(first_ptr, ptr, tok)) {
                return tok.kind() == kind;
              }
            }
            return false;
          };
    };

    auto try_import = try_keyword(token::details::TokenKind::kw_import);
    auto try_module = try_keyword(token::details::TokenKind::kw_module);
    auto try_export = try_keyword(token::details::TokenKind::kw_export);
    auto try_ident = try_keyword(token::details::TokenKind::identifier);

#define TRY_UD(FUNC, KIND)                                                   \
  [this, &try_ident](const typename base::ptr_type& first_ptr,               \
                     typename base::ptr_type& ptr, lps::token::Token& tok) { \
    if (FUNC(first_ptr, ptr, tok)) {                                         \
      auto cc = ptr;                                                         \
      ++ptr;                                                                 \
      if (try_ident(cc, ptr, tok)) {                                         \
        token_formulate(tok, first_ptr, ptr, KIND);                          \
        return true;                                                         \
      }                                                                      \
    }                                                                        \
    return false;                                                            \
  }

    auto try_char_literal_ud =
        TRY_UD(this->lex_character_literal,
               token::details::TokenKind::user_defined_char_literal);
    auto try_string_literal_ud =
        TRY_UD(this->lex_string_literal,
               token::details::TokenKind::user_defined_string_literal);
#undef TRY_UD

    // CASE(this->lex_header_name);  // 	header_name
    CASE([this](const typename base::ptr_type& first_ptr,
                typename base::ptr_type& ptr, lps::token::Token& tok) {
      if (this->lex_identifier(first_ptr, ptr, tok)) {
        return tok.kind() == token::details::TokenKind::kw_import ||
               tok.kind() == token::details::TokenKind::kw_module ||
               tok.kind() == token::details::TokenKind::kw_export;
      }
      return false;
    });

    CASE(lex_pp_number);                // 	pp_number
    CASE(this->lex_character_literal);  // 	character_literal
    CASE(try_char_literal_ud);          // 	user_defined_character_literal
    CASE(this->lex_string_literal);     // 	string_literal
    CASE(try_string_literal_ud);        // 	user_defined_string_literal
    CASE(this->lex_preprocessing_op_or_punc);  // preprocessing_op_or_punc
    CASE(try_ident);                           // 	`identifier`
    CASE([this](const typename base::ptr_type& first_ptr,
                typename base::ptr_type& ptr, lps::token::Token& tok) {
      return this->lex_identifier(first_ptr, ptr, tok);
    });
#undef TRY
#undef CASE

    return false;
  }

  // pp-tokens:
  // 	preprocessing_token
  // 	pp_tokens, preprocessing_token
  inline typename lps::token::Token::tokens_type lex_pp_tokens(
      const typename base::ptr_type& first_ptr, typename base::ptr_type& ptr,
      lps::token::Token& tok) {
    typename lps::token::Token::tokens_type tokens;
    if (this->template lex_something_recursive<
            token::details::TokenKind::pp_tokens>(
            first_ptr, ptr, tok,
            [this, &tokens](const typename base::ptr_type& first_ptr,
                            typename base::ptr_type& ptr,
                            lps::token::Token& tok) -> bool {
              if (this->lex_preprocessing_token(first_ptr, ptr, tok)) {
                tokens.append(tok);
                return true;
              }
              return false;
            },
            [this](const typename base::ptr_type& first_ptr_, ptr_type& ptr,
                   lps::token::Token&) -> bool {
              auto tmp_ptr = ptr;
              typename base::ptr_type first_ptr = first_ptr_;
              if (basic::str::ascii::is::HorzWs(*first_ptr)) {
                first_ptr.horzws_skipping();
                tmp_ptr = first_ptr;
                ++tmp_ptr;
              }

              if (basic::str::ascii::is::VertWs(*first_ptr)) {
                return true;
              }
              if (*first_ptr == '\\' &&
                  basic::str::ascii::is::VertWs(*tmp_ptr)) {
                ++tmp_ptr;
                horzws_skipping(tmp_ptr);
                ++tmp_ptr;
                ptr = tmp_ptr;
              }
              return false;
            })) {
      return tokens;
    }
    return typename lps::token::Token::tokens_type();
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
  inline bool lex_control_line(const typename base::ptr_type& first_ptr,
                               typename base::ptr_type& ptr,
                               lps::token::Token& tok) {
    using namespace basic::str::ascii;
    typename base::ptr_type tmp_ptr = ptr;
    bool lex_ident_ok = this->lex_identifier(first_ptr, tmp_ptr, tok);
    if (lex_ident_ok) {
      horzws_skipping(tmp_ptr);
    }
    if (lex_ident_ok) {
      switch (tok.kind()) {
        case token::details::TokenKind::kw_include: {
          lps::token::Token included_tok;
          auto tmp_c = tmp_ptr;
          tmp_ptr++;
          if (this->lex_header_name(tmp_c, tmp_ptr, included_tok)) {
            auto next_info = tu::TU::instance().include(included_tok);
            if (next_info.second == 0) {
              ptr = tmp_ptr;
              tok = included_tok;
              tok.next_visitor_offset(tok.next_visitor().first);
            } else {
              tok.next_visitor(next_info.first, next_info.second);
            }
          } else {
            diag(tmp_c, tmp_ptr,
                 diag::DiagKind::expected_header_name_after_include);
          }

          break;
        }
        case token::details::TokenKind::kw_define:
        case token::details::TokenKind::kw_undef: {
          lps::token::Token define_tok;
          auto tmp_c = tmp_ptr;
          tmp_ptr++;
          if (!this->lex_identifier(tmp_c, tmp_ptr, define_tok)) {
            diag(first_ptr, tmp_ptr,
                 diag::DiagKind::expected_ident_after_define_undef);
            return false;
          }
          if (define_tok.kind() != token::details::TokenKind::identifier) {
            diag(first_ptr, tmp_ptr,
                 diag::DiagKind::expected_ident_after_define_undef);
            return false;
          }

          if (tok.kind() == token::details::TokenKind::kw_define) {
            horzws_skipping(tmp_ptr);
            typename base::ptr_type tmp_ptr2 = tmp_ptr;
            ++tmp_ptr2;
            lps::token::Token next_tok;
            token::Token::tokens_type replacement_tokens;
            token::Token::tokens_type parameter_tokens;

            if (*tmp_ptr == '(' && !basic::str::ascii::is::Ws(*tmp_ptr2)) {
              // [lparen] matched, now consider:
              // 1. identifier_list[opt],
              // 2. `...`
              // 3. identifier_list, `...`

            start_check_params:
              horzws_skipping(tmp_ptr2);
              auto tmp_first_ptr = tmp_ptr2;
              ++tmp_ptr2;
              token::Token ident_of_ellipsis_token;
              if (this->lex_identifier(tmp_first_ptr, tmp_ptr2,
                                       ident_of_ellipsis_token)) {
                parameter_tokens.append(ident_of_ellipsis_token);
                horzws_skipping(tmp_ptr2);
                auto tmp_ptr3 = tmp_ptr2;
                --tmp_ptr3;
                advance(tmp_ptr3);
                if (*tmp_ptr3 == ',') {
                  ++tmp_ptr2;
                  goto start_check_params;
                }
              } else {
                auto tmp_ptr3 = tmp_ptr2;
                if (this->lex_operator_or_punctuator(tmp_first_ptr, tmp_ptr3,
                                                     ident_of_ellipsis_token)) {
                  if (ident_of_ellipsis_token.kind() ==
                      token::details::TokenKind::ellipsis) {
                    if (parameter_tokens.empty()) {
                      diag(
                          tmp_ptr, tmp_ptr2,
                          diag::DiagKind::
                              expected_at_least_one_identifier_before_ellipsis);
                      // early stop
                      tok.kind(token::details::TokenKind::eof);
                      return true;
                    }
                    tmp_ptr2 = tmp_ptr3;
                  }
                }
              }

              horzws_skipping(tmp_ptr2);
              if (*tmp_ptr2 != ')') {
                diag(tmp_ptr2, tmp_ptr2,
                     diag::DiagKind::
                         expected_r_paren_in_the_define_with_parameters);
                tok.kind(token::details::TokenKind::eof);
                return true;
              }

              horzws_skipping(tmp_ptr2);
              advance(tmp_ptr2);
              horzws_skipping(tmp_ptr2);
              if (*tmp_ptr2 == '\\') {
                if (!basic::str::ascii::is::VertWs(*(tmp_ptr2 + 1))) {
                  diag(tmp_ptr2, tmp_ptr2 + 1,
                       diag::DiagKind::
                           expected_vertws_after_slash_in_preprocessing);
                  tok.kind(token::details::TokenKind::eof);
                  return true;
                }
                ++tmp_ptr2;
                ++tmp_ptr2;
                horzws_skipping(tmp_ptr2);
              }
              tmp_first_ptr = tmp_ptr2;
              ++tmp_ptr2;
              replacement_tokens =
                  lex_pp_tokens(tmp_first_ptr, tmp_ptr2, next_tok);

            } else {
              // replacement-list: pp_tokens[opt]
              replacement_tokens = lex_pp_tokens(tmp_ptr, tmp_ptr2, next_tok);
            }
            if (!replacement_tokens.empty()) {
              horzws_skipping(tmp_ptr2);

              if (basic::str::ascii::is::VertWs(*tmp_ptr2)) {
                // [`#`, `define`, `identifier`[(P0, P1, ...)]_opt, replacement_list, new_line] matched
                ptr = ++tmp_ptr2;

                tu::TU::instance().define(define_tok,
                                          std::move(parameter_tokens),
                                          std::move(replacement_tokens));
              } else {
                diag(first_ptr, tmp_ptr,
                     diag::DiagKind::
                         expected_new_line_at_the_end_of_define_undef);
                return false;
              }
            } else {  // try others.
              if (!is::VertWs(*tmp_ptr)) {
                tmp_ptr.horzws_skipping();
              }
              if (!is::VertWs(*tmp_ptr)) {
                diag(first_ptr, tmp_ptr,
                     diag::DiagKind::
                         expected_new_line_at_the_end_of_define_undef);
                return false;
              }
              ++tmp_ptr;
              ptr = tmp_ptr;
              tu::TU::instance().define(define_tok);
            }
            // jump over the define_tok, replacement_tokens
            tok.next_visitor(ptr.pos(), ptr.file_id());
            tok.kind(token::details::TokenKind::eod);
            return true;
          } else {
            tu::TU::instance().undef(define_tok);
          }

          break;
        }
        case token::details::TokenKind::kw_line:
        case token::details::TokenKind::kw_error:
        case token::details::TokenKind::kw_pragma:

        default:
          unreachable(kTag);
      }
      ptr = tmp_ptr;
    }
    if (is::VertWs(*ptr)) {
      tok.kind(token::details::TokenKind::eod);
      return true;
    }
    return false;
  }
  void command() {}
};  // namespace lps::lexer::details

}  // namespace lps::lexer::details::pp
