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

#include "lex/base.h"
#include "token.h"

namespace lps::lexer::details {

class Basic : public Base {
  using base = Base;

 public:
  explicit Basic(uint32_t start_file_id, const char* ptr, const char* end)
      : base(start_file_id, ptr, end, MethodType::kBasic) {}

  // All the lex processing can be modeled as a state machine.
  // The states are following:
  // *start*: the start of this lex function.
  // *next*: temp state.
  // *skip_comments*: skip comments, these characters are useless for parser.
  // *skip_horz_ws*: skip line-changing characters, like `'\n'`.
  inline void lex_impl(lps::token::Token& tok) override {
    using namespace basic::str::ascii;
    typename base::ptr_type ptr = this->cur();
  start:  // state: start
    if (is::HorzWs(*ptr)) {
      do {
        this->inc(1);
        ++ptr;
      } while (is::HorzWs(*ptr));
      tok.set_flag(token::Flag::kLeadingSpace);
    }
    uint32_t sz_tmp;
    uint32_t sz_tmp2;
    auto c_sz = base::advance(ptr);  // read `char` by skipping `'\'`
    char c = std::get<0>(c_sz);
    lps::token::details::TokenKind token_kind =
        lps::token::details::TokenKind::unknown;

    if (!is::VertWs(c)) {
      // todo(@mxlol233): not a new line ?
    }

    switch (c) {

      case 0: {  // null
        token_kind = lps::token::details::TokenKind::eof;
        break;
      }
      case 26: {  // `^Z`
        break;
      }
      case '\r': {
        if (*ptr == '\n') {
          base::advance(ptr);
        }
        [[fallthrough]];
      }
      case '\n': {
        {
          // todo(@mxlol233): handle preprocessor
        }

        tok.clear_flag(token::Flag::kLeadingSpace);
        this->inc(1);
        goto next;
      }
      case ' ':
      case '\t':
      case '\f':
      case '\v':
      skip_horz_ws:
        tok.set_flag(token::Flag::kLeadingSpace);
        // if (this->ws_skipping()) {
        //   return;
        // }
      skip_comments:
        // todo(@mxlol233): skip comments.
        goto next;

      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9': {
        typename base::ptr_type tmp_ptr = ptr;
        if (this->lex_floating_point_literal(c, tmp_ptr, tok)) {
          return;
        }
        tmp_ptr = ptr;
        if (this->lex_integer_literal(c, tmp_ptr, tok)) {
          return;
        }
        break;
      }

      case 'R':
        if (this->lex_raw_string(ptr, tok)) {
          return;
        };
        [[fallthrough]];
      case 'u':
      case 'U':
      case 'L': {
        typename base::ptr_type tmp_ptr = ptr;
        if (this->lex_character_literal(c, tmp_ptr, tok)) {
          return;
        }
        tmp_ptr = ptr;
        if (this->lex_string_literal(c, tmp_ptr, tok)) {
          return;
        }
      }
        [[fallthrough]];
      case 'A':
      case 'B':
      case 'C':
      case 'D':
      case 'E':
      case 'F':
      case 'G':
      case 'H':
      case 'I':
      case 'J':
      case 'K':
      case 'M':
      case 'N':
      case 'O':
      case 'P':
      case 'Q':
      case 'S':
      case 'T':
      case 'V':
      case 'W':
      case 'X':
      case 'Y':
      case 'Z':
      case 'a':
      case 'b':
      case 'c':
      case 'd':
      case 'e':
      case 'f':
      case 'g':
      case 'h':
      case 'i':
      case 'j':
      case 'k':
      case 'l':
      case 'm':
      case 'n':
      case 'o':
      case 'p':
      case 'q':
      case 'r':
      case 's':
      case 't':
      case 'v':
      case 'w':
      case 'x':
      case 'y':
      case 'z':
      case '_':
        if (this->lex_identifier(ptr, tok)) {
          // a valid identifier, we must check if it was defined by `TU`.
          if (tok.kind() != token::details::TokenKind::identifier) {
            return;
          }
          if (tu::TU::instance().defined(tok)) {
            auto new_tok = tu::TU::instance().expand(tok);
            if (new_tok.kind() != token::details::TokenKind::unknown) {
              return;
            }
            //if the defined macro is empty, we skip this token and lex next one.
            ptr += tok.offset();
            goto next;
          }
          return;
        }
      case '$':
        // todo(@mxlol233): `$` is identifier?
        token_kind = token::details::TokenKind::unknown;
        break;

      case '\'':
        if (this->lex_character_literal(c, ptr, tok)) {
          return;
        }
        break;
      case '"':
        if (this->lex_string_literal(c, ptr, tok)) {
          return;
        }
        break;
      case '?':
        token_kind = token::details::TokenKind::question;
        break;
      case '[':
        token_kind = token::details::TokenKind::l_square;
        break;
      case ']':
        token_kind = token::details::TokenKind::r_square;
        break;
      case '(':
        token_kind = token::details::TokenKind::l_paren;
        break;
      case ')':
        token_kind = token::details::TokenKind::r_paren;
        break;
      case '{':
        token_kind = token::details::TokenKind::l_brace;
        break;
      case '}':
        token_kind = token::details::TokenKind::r_brace;
        break;
      case '.':
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);

        {
          typename base::ptr_type tmp_ptr = ptr;
          if (this->lex_floating_point_literal('.', tmp_ptr, tok)) {
            ptr = tmp_ptr;
            return;
          }
        }

        if (c == '*') {
          token_kind = token::details::TokenKind::periodstar;
          ptr += sz_tmp;
        } else if (c == '.') {
          c_sz = lps::lexer::details::Basic::char_size(ptr + sz_tmp);
          if (c_sz == '.') {
            sz_tmp2 = std::get<1>(c_sz);
            token_kind = token::details::TokenKind::ellipsis;
            ptr = base::consume_char(ptr, sz_tmp);   // consume second `.`
            ptr = base::consume_char(ptr, sz_tmp2);  // consume third `.`
          }
        } else {
          token_kind = token::details::TokenKind::period;
        }
        break;
      case '&':
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '&') {
          token_kind = token::details::TokenKind::ampamp;
          ptr = base::consume_char(ptr, sz_tmp);
        } else if (c == '=') {
          token_kind = token::details::TokenKind::ampequal;
          ptr = base::consume_char(ptr, sz_tmp);
        } else {
          token_kind = token::details::TokenKind::amp;
        }
        break;
      case '*':
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '=') {
          token_kind = token::details::TokenKind::starequal;
          ptr = base::consume_char(ptr, sz_tmp);
        } else {
          token_kind = token::details::TokenKind::star;
        }
        break;
      case '+':
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '+') {
          token_kind = token::details::TokenKind::plusplus;
          ptr = base::consume_char(ptr, sz_tmp);
        } else if (c == '=') {
          token_kind = token::details::TokenKind::plusequal;
          ptr = base::consume_char(ptr, sz_tmp);
        } else {
          token_kind = token::details::TokenKind::plus;
        }
        break;
      case '-':
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '-') {  // --
          token_kind = token::details::TokenKind::minusminus;
          ptr = base::consume_char(ptr, sz_tmp);
        } else if (c == '>') {
          c_sz = lps::lexer::details::Basic::char_size(ptr + sz_tmp);
          c = std::get<0>(c_sz);
          if (c == '*') {  // ->*
            sz_tmp2 = std::get<1>(c_sz);
            token_kind = token::details::TokenKind::arrowstar;
            ptr = base::consume_char(ptr, sz_tmp);
            ptr = base::consume_char(ptr, sz_tmp2);
          } else {
            token_kind = token::details::TokenKind::arrow;
            ptr = base::consume_char(ptr, sz_tmp);
          }
        } else if (c == '=') {  // -=
          token_kind = token::details::TokenKind::minusequal;
          ptr = base::consume_char(ptr, sz_tmp);
        } else {
          token_kind = token::details::TokenKind::minus;
        }
        break;
      case '~':
        token_kind = token::details::TokenKind::tilde;
        break;
      case '!':
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '=') {
          token_kind = token::details::TokenKind::exclaimequal;
          ptr = base::consume_char(ptr, sz_tmp);
        } else {
          token_kind = token::details::TokenKind::exclaim;
        }
        break;
      case '/':
        // todo(@mxlol233): handle comments
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '/') {
          auto tmp_ptr = base::consume_char(ptr, sz_tmp);
          if (line_comment_skipping()) {
            return;
          }
          goto skip_comments;
        }
        if (c == '*') {
          auto tmp_ptr = base::consume_char(ptr, sz_tmp);
          if (block_comment_skipping()) {
            return;
          }
          goto next;
        }
        if (c == '=') {
          token_kind = token::details::TokenKind::slashequal;
          ptr = base::consume_char(ptr, sz_tmp);
        } else {
          token_kind = token::details::TokenKind::slash;
        }
        break;
      case '%':
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '=') {
          token_kind = token::details::TokenKind::percentequal;
          ptr = base::consume_char(ptr, sz_tmp);
        } else {
          token_kind = token::details::TokenKind::percent;
        }
        break;
      case '<':
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        // todo(@mxlol233): handle #include<...>
        if (c == '<') {
          auto c_sz_2 = lps::lexer::details::Basic::char_size(ptr + sz_tmp);
          auto c2 = std::get<0>(c_sz_2);
          sz_tmp2 = std::get<1>(c_sz_2);
          if (c2 == '=') {  // <<=
            token_kind = token::details::TokenKind::lesslessequal;
            ptr = base::consume_char(ptr, sz_tmp);
            ptr = base::consume_char(ptr, sz_tmp2);

          } else if (c2 == '<') {  // <<<
            token_kind = token::details::TokenKind::lesslessless;
            ptr = base::consume_char(ptr, sz_tmp);
            ptr = base::consume_char(ptr, sz_tmp2);

          } else {
            token_kind = token::details::TokenKind::lessless;
            ptr = base::consume_char(ptr, sz_tmp);
          }
        } else if (c == '=') {  // <=
          auto c_sz_2 = lps::lexer::details::Basic::char_size(ptr + sz_tmp);
          auto c2 = std::get<0>(c_sz_2);
          sz_tmp2 = std::get<1>(c_sz_2);
          if (c2 == '>') {  // <=>
            token_kind = token::details::TokenKind::spaceship;
            ptr = base::consume_char(ptr, sz_tmp);
            ptr = base::consume_char(ptr, sz_tmp2);
            break;
          }
          token_kind = token::details::TokenKind::lessequal;
          ptr = base::consume_char(ptr, sz_tmp);

        } else {
          token_kind = token::details::TokenKind::less;
        }
        break;
      case '>':
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '=') {  // >=
          token_kind = token::details::TokenKind::greaterequal;
          ptr = base::consume_char(ptr, sz_tmp);
        } else if (c == '>') {
          auto c_sz_2 = lps::lexer::details::Basic::char_size(ptr + sz_tmp);
          auto c2 = std::get<0>(c_sz_2);
          sz_tmp2 = std::get<1>(c_sz_2);
          if (c2 == '=') {  // >>=
            token_kind = token::details::TokenKind::greatergreaterequal;
            ptr = base::consume_char(ptr, sz_tmp);
            ptr = base::consume_char(ptr, sz_tmp2);
          } else if (c2 == '>') {
            token_kind = token::details::TokenKind::greatergreatergreater;
            ptr = base::consume_char(ptr, sz_tmp);
            ptr = base::consume_char(ptr, sz_tmp2);
          } else {
            token_kind = token::details::TokenKind::greatergreater;
            ptr = base::consume_char(ptr, sz_tmp);
          }

        } else {
          token_kind = token::details::TokenKind::greater;
        }
        break;
      case '^':
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '=') {
          token_kind = token::details::TokenKind::caretequal;
          ptr = base::consume_char(ptr, sz_tmp);
        } else if (c == '^') {
          token_kind = token::details::TokenKind::caretcaret;
          ptr = base::consume_char(ptr, sz_tmp);
        } else {
          token_kind = token::details::TokenKind::caret;
        }
        break;
      case '|':
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '=') {
          token_kind = token::details::TokenKind::pipeequal;
          ptr = base::consume_char(ptr, sz_tmp);
        } else if (c == '|') {
          token_kind = token::details::TokenKind::pipepipe;
          ptr = base::consume_char(ptr, sz_tmp);
        } else {
          token_kind = token::details::TokenKind::pipe;
        }
        break;
      case ':':
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == ':') {
          token_kind = token::details::TokenKind::coloncolon;
          ptr = base::consume_char(ptr, sz_tmp);
        } else {
          token_kind = token::details::TokenKind::colon;
        }
        break;
      case ';':
        token_kind = token::details::TokenKind::semi;
        break;
      case '=':
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '=') {
          token_kind = token::details::TokenKind::equalequal;
          ptr = base::consume_char(ptr, sz_tmp);
        } else {
          token_kind = token::details::TokenKind::equal;
        }
        break;
      case ',':
        token_kind = token::details::TokenKind::comma;
        break;
      case '#':
        c_sz = lps::lexer::details::Basic::char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '#') {
          token_kind = token::details::TokenKind::hashhash;
          ptr = base::consume_char(ptr, sz_tmp);
        } else {
          // todo(@mxlol233): handle pp directive
          token_kind = token::details::TokenKind::hash;
        }
        break;
      case '@':
        // todo(@mxlol233): handle objectC ?
        token_kind = token::details::TokenKind::unknown;
        break;
      case '\\':
        // todo(@mxlol233): handle  UCNs
        token_kind = token::details::TokenKind::unknown;
        break;

      default: {
        if (is::ASCII(c)) {
          token_kind = token::details::TokenKind::unknown;
          break;
        }

        return;
      }
    }

    this->token_formulate(tok, ptr, token_kind);

    return;

  next:  // state: next token
    goto start;
  }

 private:
  inline bool line_comment_skipping() { return false; }
  inline bool block_comment_skipping() { return false; }
};

}  // namespace lps::lexer::details
