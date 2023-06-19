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
#include <limits>
#include <memory>
#include "basic/exception.h"
#include "basic/str.h"
#include "basic/vec.h"
#include "diag.h"
#include "token.h"

namespace lps::lexer {

namespace details {

enum MethodType : uint8_t { kNone = 0, kBasic };

using CharSize = std::tuple<char, uint32_t>;

inline bool operator==(const std::tuple<char, uint32_t>& cz0, const char& c1) {
  char c0 = std::get<0>(cz0);
  return c0 == c1;
}

template <meta::Str TagName>
class Base : virtual public lps::basic::mem::TraceTag<TagName> {

 public:
  using type = Base<TagName>;
  using const_ptr_type = const type*;
  inline virtual void lex(lps::token::Token<TagName>& tok) = 0;
  [[nodiscard]] MethodType method() const { return type_; }
  explicit Base(uint32_t start_file_id, const char* ptr, const char* end,
                MethodType m)
      : file_id_(start_file_id), ptr_(ptr), end_(end), type_(m) {}
  [[nodiscard]] inline size_t pos() const { return pos_; }

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
          // diag::doing(tok, diag::DiagKind::back_slash_new_space);
        }
        size += newline_size;
        ptr += newline_size;
        return char_scanning(ptr, tok, size);
      }
      return '\\';
    }

    // todo(@mxlol233): handle `trigraph char`
    if (*ptr == '?' && ptr[1] == '?') {}

    size++;
    return *ptr;
  }

  inline const char* consume_char(const char* ptr, uint32_t sz,
                                  lps::token::Token<TagName>& tok) {
    if (sz == 1) {
      return ptr + sz;
    }
    sz = 0;
    char c = char_scanning(ptr, &tok, sz);
    return ptr + sz;
  }

  inline CharSize char_size(const char* ptr) {
    if (basic::str::ascii::is::NormalChar(ptr[0])) {
      return {*ptr++, 1};
    }
    uint32_t sz = 0;
    char c = char_scanning(ptr, nullptr, sz);
    return {c, sz};
  }

  inline CharSize advance(const char*& ptr, lps::token::Token<TagName>& tok) {
    uint32_t sz = 0;
    if (basic::str::ascii::is::NormalChar(ptr[0])) {
      return {*ptr++, 1};
    }

    char c = char_scanning(ptr, &tok, sz);
    ptr += sz;
    return {c, sz};
  }

  inline CharSize ws_skipping(const char*& ptr,
                              lps::token::Token<TagName>& /*tok*/) {
    uint32_t sz = 0;
    while (basic::str::ascii::is::Ws(*ptr)) {
      ptr++;
      sz++;
    }
    return {*ptr, sz};
  }

  [[nodiscard]] inline const char* cur() const { return ptr_ + pos_; }
  inline void inc(size_t n) { pos_ += n; }

  inline void diag(const char* ptr, diag::DiagKind kind) {
    diag::DiagInputs<TagName> diag_input;
    diag_input.kind_ = kind;
    lps::token::Token<TagName> tok_error;
    this->token_formulate(tok_error, ptr, token::tok::TokenKind::unknown);
    tok_error.data(this->cur());
    diag_input.main_token_ = tok_error;
    diag::doing<TagName>(diag_input.main_token_, diag_input.kind_,
                         diag_input.context_tokens_);
  }

  inline void token_formulate(lps::token::Token<TagName>& tok, const char* end,
                              lps::token::tok::TokenKind kind) {
    lps_assert(TagName, this->cur() != nullptr);
    auto offset = end - this->cur();
    lps_assert(TagName,
               offset >= 0 && offset < std::numeric_limits<uint32_t>::max());

    tok.offset(offset);
    tok.file_id(file_id_);
    tok.kind(kind);
    tok.data(this->cur());
  }

  MethodType type_{MethodType::kNone};
  const char* ptr_{nullptr};
  uint32_t file_id_{0};
  size_t pos_{0};
  const char* end_{nullptr};
};

template <meta::Str TagName>
class Basic : public Base<TagName> {
  using base = Base<TagName>;

 public:
  explicit Basic(uint32_t start_file_id, const char* ptr, const char* end,
                 MethodType m)
      : base(start_file_id, ptr, end, m) {}

  // All the lex processing can be modeled as a state machine.
  // The states are following:
  // *start*: the start of this lex function.
  // *next*: temp state.
  // *skip_comments*: skip comments, these characters are useless for parser.
  // *skip_horz_ws*: skip line-changing characters, like `'\n'`.
  inline void lex(lps::token::Token<TagName>& tok) override {
    using namespace basic::str::ascii;
    const char* ptr = this->cur();
  start:  // state: start
    if (is::HorzWs(*ptr)) {
      do {
        this->inc(1);
        ptr++;
      } while (is::HorzWs(*ptr));
      tok.set_flag(token::Flag::kLeadingSpace);
    }
    uint32_t sz_tmp;
    uint32_t sz_tmp2;
    auto c_sz = this->advance(ptr, tok);  // read `char` by skipping `'\'`
    char c = std::get<0>(c_sz);
    lps::token::tok::TokenKind token_kind = lps::token::tok::TokenKind::unknown;

    if (!is::VertWs(c)) {
      // todo(@mxlol233): not a new line ?
    }

    switch (c) {

      case 0: {  // null
        token_kind = lps::token::tok::TokenKind::eof;
        break;
      }
      case 26: {  // `^Z`
        break;
      }
      case '\r': {
        if (*ptr == '\n') {
          this->advance(ptr, tok);
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
        const char* tmp_ptr = ptr;
        if (lex_floating_point_literal(c, tmp_ptr, tok)) {
          return;
        }
        tmp_ptr = ptr;
        if (lex_integer_literal(c, tmp_ptr, tok)) {
          return;
        }
        break;
      }

      case 'R':
        if (lex_raw_string()) {
          return;
        };
        [[fallthrough]];
      case 'u':
      case 'U':
      case 'L':
        if (lex_character_literal(c, ptr, tok)) {
          return;
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
        if (lex_identifier(ptr, tok)) {
          return;
        }
      case '$':
        // todo(@mxlol233): `$` is identifier?
        token_kind = token::tok::TokenKind::unknown;
        break;

      case '\'':
        if (lex_character_literal(c, ptr, tok)) {
          return;
        }
        break;
      case '"':
        if (lex_string_literal()) {
          return;
        }
        break;
      case '?':
        token_kind = token::tok::TokenKind::question;
        break;
      case '[':
        token_kind = token::tok::TokenKind::l_square;
        break;
      case ']':
        token_kind = token::tok::TokenKind::r_square;
        break;
      case '(':
        token_kind = token::tok::TokenKind::l_paren;
        break;
      case ')':
        token_kind = token::tok::TokenKind::r_paren;
        break;
      case '{':
        token_kind = token::tok::TokenKind::l_brace;
        break;
      case '}':
        token_kind = token::tok::TokenKind::r_brace;
        break;
      case '.':
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);

        {
          const char* tmp_ptr = ptr;
          if (lex_floating_point_literal('.', tmp_ptr, tok)) {
            ptr = tmp_ptr;
            return;
          }
        }

        if (c == '*') {
          token_kind = token::tok::TokenKind::periodstar;
          ptr += sz_tmp;
        } else if (c == '.') {
          c_sz = this->char_size(ptr + sz_tmp);
          if (c_sz == '.') {
            sz_tmp2 = std::get<1>(c_sz);
            token_kind = token::tok::TokenKind::ellipsis;
            ptr = this->consume_char(ptr, sz_tmp, tok);   // consume second `.`
            ptr = this->consume_char(ptr, sz_tmp2, tok);  // consume third `.`
          }
        } else {
          token_kind = token::tok::TokenKind::period;
        }
        break;
      case '&':
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '&') {
          token_kind = token::tok::TokenKind::ampamp;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else if (c == '=') {
          token_kind = token::tok::TokenKind::ampequal;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else {
          token_kind = token::tok::TokenKind::amp;
        }
        break;
      case '*':
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '=') {
          token_kind = token::tok::TokenKind::starequal;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else {
          token_kind = token::tok::TokenKind::star;
        }
        break;
      case '+':
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '+') {
          token_kind = token::tok::TokenKind::plusplus;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else if (c == '=') {
          token_kind = token::tok::TokenKind::plusequal;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else {
          token_kind = token::tok::TokenKind::plus;
        }
        break;
      case '-':
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '-') {  // --
          token_kind = token::tok::TokenKind::minusminus;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else if (c == '>') {
          c_sz = this->char_size(ptr + sz_tmp);
          c = std::get<0>(c_sz);
          if (c == '*') {  // ->*
            sz_tmp2 = std::get<1>(c_sz);
            token_kind = token::tok::TokenKind::arrowstar;
            ptr = this->consume_char(ptr, sz_tmp, tok);
            ptr = this->consume_char(ptr, sz_tmp2, tok);
          } else {
            token_kind = token::tok::TokenKind::arrow;
            ptr = this->consume_char(ptr, sz_tmp, tok);
          }
        } else if (c == '=') {  // -=
          token_kind = token::tok::TokenKind::minusequal;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else {
          token_kind = token::tok::TokenKind::minus;
        }
        break;
      case '~':
        token_kind = token::tok::TokenKind::tilde;
        break;
      case '!':
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '=') {
          token_kind = token::tok::TokenKind::exclaimequal;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else {
          token_kind = token::tok::TokenKind::exclaim;
        }
        break;
      case '/':
        // todo(@mxlol233): handle comments
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '/') {
          auto tmp_ptr = this->consume_char(ptr, sz_tmp, tok);
          if (line_comment_skipping()) {
            return;
          }
          goto skip_comments;
        }
        if (c == '*') {
          auto tmp_ptr = this->consume_char(ptr, sz_tmp, tok);
          if (block_comment_skipping()) {
            return;
          }
          goto next;
        }
        if (c == '=') {
          token_kind = token::tok::TokenKind::slashequal;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else {
          token_kind = token::tok::TokenKind::slash;
        }
        break;
      case '%':
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '=') {
          token_kind = token::tok::TokenKind::percentequal;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else {
          token_kind = token::tok::TokenKind::percent;
        }
        break;
      case '<':
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        // todo(@mxlol233): handle #include<...>
        if (c == '<') {
          auto c_sz_2 = this->char_size(ptr + sz_tmp);
          auto c2 = std::get<0>(c_sz_2);
          sz_tmp2 = std::get<1>(c_sz_2);
          if (c2 == '=') {  // <<=
            token_kind = token::tok::TokenKind::lesslessequal;
            ptr = this->consume_char(ptr, sz_tmp, tok);
            ptr = this->consume_char(ptr, sz_tmp2, tok);

          } else if (c2 == '<') {  // <<<
            token_kind = token::tok::TokenKind::lesslessless;
            ptr = this->consume_char(ptr, sz_tmp, tok);
            ptr = this->consume_char(ptr, sz_tmp2, tok);

          } else {
            token_kind = token::tok::TokenKind::lessless;
            ptr = this->consume_char(ptr, sz_tmp, tok);
          }
        } else if (c == '=') {  // <=
          auto c_sz_2 = this->char_size(ptr + sz_tmp);
          auto c2 = std::get<0>(c_sz_2);
          sz_tmp2 = std::get<1>(c_sz_2);
          if (c2 == '>') {  // <=>
            token_kind = token::tok::TokenKind::spaceship;
            ptr = this->consume_char(ptr, sz_tmp, tok);
            ptr = this->consume_char(ptr, sz_tmp2, tok);
            break;
          }
          token_kind = token::tok::TokenKind::lessequal;
          ptr = this->consume_char(ptr, sz_tmp, tok);

        } else {
          token_kind = token::tok::TokenKind::less;
        }
        break;
      case '>':
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '=') {  // >=
          token_kind = token::tok::TokenKind::greaterequal;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else if (c == '>') {
          auto c_sz_2 = this->char_size(ptr + sz_tmp);
          auto c2 = std::get<0>(c_sz_2);
          sz_tmp2 = std::get<1>(c_sz_2);
          if (c2 == '=') {  // >>=
            token_kind = token::tok::TokenKind::greatergreaterequal;
            ptr = this->consume_char(ptr, sz_tmp, tok);
            ptr = this->consume_char(ptr, sz_tmp2, tok);
          } else if (c2 == '>') {
            token_kind = token::tok::TokenKind::greatergreatergreater;
            ptr = this->consume_char(ptr, sz_tmp, tok);
            ptr = this->consume_char(ptr, sz_tmp2, tok);
          } else {
            token_kind = token::tok::TokenKind::greatergreater;
            ptr = this->consume_char(ptr, sz_tmp, tok);
          }

        } else {
          token_kind = token::tok::TokenKind::greater;
        }
        break;
      case '^':
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '=') {
          token_kind = token::tok::TokenKind::caretequal;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else if (c == '^') {
          token_kind = token::tok::TokenKind::caretcaret;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else {
          token_kind = token::tok::TokenKind::caret;
        }
        break;
      case '|':
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '=') {
          token_kind = token::tok::TokenKind::pipeequal;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else if (c == '|') {
          token_kind = token::tok::TokenKind::pipepipe;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else {
          token_kind = token::tok::TokenKind::pipe;
        }
        break;
      case ':':
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == ':') {
          token_kind = token::tok::TokenKind::coloncolon;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else {
          token_kind = token::tok::TokenKind::colon;
        }
        break;
      case ';':
        token_kind = token::tok::TokenKind::semi;
        break;
      case '=':
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '=') {
          token_kind = token::tok::TokenKind::equalequal;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else {
          token_kind = token::tok::TokenKind::equal;
        }
        break;
      case ',':
        token_kind = token::tok::TokenKind::comma;
        break;
      case '#':
        c_sz = this->char_size(ptr);
        c = std::get<0>(c_sz);
        sz_tmp = std::get<1>(c_sz);
        if (c == '#') {
          token_kind = token::tok::TokenKind::hashhash;
          ptr = this->consume_char(ptr, sz_tmp, tok);
        } else {
          // todo(@mxlol233): handle pp directive
          token_kind = token::tok::TokenKind::hash;
        }
        break;
      case '@':
        // todo(@mxlol233): handle objectC ?
        token_kind = token::tok::TokenKind::unknown;
        break;
      case '\\':
        // todo(@mxlol233): handle  UCNs
        token_kind = token::tok::TokenKind::unknown;
        break;

      default: {
        if (is::ASCII(c)) {
          token_kind = token::tok::TokenKind::unknown;
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
  inline bool lex_character_literal(char c_, const char*& ptr,
                                    lps::token::Token<TagName>& tok) {
    bool flg = false;
    const char* start = ptr - 1;
    if (c_ == 'u') {
      if (*ptr == '8') {
        ptr++;
      }
      flg = true;
    } else if (c_ == 'U' || c_ == 'L') {
      flg = true;
    }

    if (flg) {
      if (*ptr == '\'') {
        ptr++;
      } else {
        return false;
      }
    } else {
      if (c_ == '\'') {
        flg = true;
      }
    }
    if (flg) {
      uint32_t cnt_char = 0;
      while (*ptr != '\'') {
        if (*ptr == '\\') {
          this->advance(ptr, tok);
        }
        if (*ptr == '\n' || *ptr == '\r' ||
            (*ptr == 0 && ptr == this->end_ - 1)) {
          this->diag(ptr, diag::unfinished_char_literal);
          return false;
        }
        this->advance(ptr, tok);
        cnt_char++;
      }
      if (cnt_char == 0) {
        this->diag(ptr, diag::empty_char_literal);
        return false;
      }
      this->token_formulate(tok, ptr + 1, token::tok::TokenKind::char_literal);
      tok.data(this->cur());
    }

    return flg;
  }
  inline bool lex_integer_suffix(const char*& ptr,
                                 lps::token::Token<TagName>& /*tok*/) {

    auto c_sz = this->char_size(ptr);
    auto c = std::get<0>(c_sz);
    auto sz_tmp = std::get<1>(c_sz);
    if (c == 'l') {  //l
      ptr++;
      if (*ptr == 'l') {  //ll
        ptr++;
        if (*ptr == 'u') {  //llu
          ptr++;
        } else if (*ptr == 'U') {  // llU
          ptr++;
        }
      } else if (*ptr == 'U') {  // lU
        ptr++;
      } else if (*ptr == 'u') {  // lu
        ptr++;
      }
    } else if (c == 'L') {  // L
      ptr++;
      if (*ptr == 'L') {  // LL
        ptr++;
        if (*ptr == 'u') {  // LLu
          ptr++;
        } else if (*ptr == 'U') {  // LLU
          ptr++;
        }
      } else if (*ptr == 'U') {  // LU
        ptr++;
      } else if (*ptr == 'u') {  // Lu
        ptr++;
      }
    } else if (c == 'u') {
      ptr++;
      if (*ptr == 'l') {  //ul
        ptr++;
        if (*ptr == 'l') {  //ull
          ptr++;
        }
      } else if (*ptr == 'L') {  // uL
        ptr++;
        if (*ptr == 'L') {  // uLL
          ptr++;
        }
      }
    } else if (c == 'U') {
      ptr++;
      if (*ptr == 'l') {  //Ul
        ptr++;
        if (*ptr == 'l') {  //Ull
          ptr++;
        }
      } else if (*ptr == 'L') {  // UL
        ptr++;
        if (*ptr == 'L') {  // ULL
          ptr++;
        }
      }
    } else {
      return false;
    }
    return true;
  }

  inline bool lex_octal_digit(const char*& ptr,
                              lps::token::Token<TagName>& tok) {
    if (*ptr >= '0' && *ptr <= '7') {
      ptr++;
      return true;
    }
    return false;
  }

  template <auto FuncDigit, lps::token::tok::TokenKind TokenKind>
  inline bool lex_number_literal(const char*& ptr,
                                 lps::token::Token<TagName>& tok) {

    if (!FuncDigit(ptr, tok)) {
      return false;
    }
    const char* matched_ptr = ptr;

    if (*ptr == '\'') {
      ptr++;
    }

    if (lex_number_literal<FuncDigit, TokenKind>(ptr, tok)) {
      matched_ptr = ptr;
    }
    this->token_formulate(tok, matched_ptr, TokenKind);
    tok.data(this->cur());
    ptr = matched_ptr;

    return true;
  }

  inline bool lex_octal_literal(const char*& ptr,
                                lps::token::Token<TagName>& tok) {
    return lex_number_literal<[](const char*& ptr,
                                 lps::token::Token<TagName>& tok) {
      if (*ptr >= '0' && *ptr <= '7') {
        ptr++;
        return true;
      }
      return false;
    },
                              token::tok::TokenKind::octal_literal>(ptr, tok);
  }

  inline bool lex_decimal_literal(const char*& ptr,
                                  lps::token::Token<TagName>& tok) {
    return lex_number_literal<[](const char*& ptr,
                                 lps::token::Token<TagName>& tok) {
      if (*ptr >= '0' && *ptr <= '9') {
        ptr++;
        return true;
      }
      return false;
    },
                              token::tok::TokenKind::decimal_literal>(ptr, tok);
  }

  inline bool lex_hexadecimal_literal(const char*& ptr,
                                      lps::token::Token<TagName>& tok) {
    return lex_number_literal<[](const char*& ptr,
                                 lps::token::Token<TagName>& tok) {
      if (*ptr >= '0' && *ptr <= '9' || *ptr >= 'a' && *ptr <= 'f' ||
          *ptr >= 'A' && *ptr <= 'F') {
        ptr++;
        return true;
      }
      return false;
    },
                              token::tok::TokenKind::hexadecimal_literal>(ptr,
                                                                          tok);
  }

  inline bool lex_binary_literal(const char*& ptr,
                                 lps::token::Token<TagName>& tok) {
    return lex_number_literal<[](const char*& ptr,
                                 lps::token::Token<TagName>& tok) {
      if (*ptr == '0' || *ptr == '1') {
        ptr++;
        return true;
      }
      return false;
    },
                              token::tok::TokenKind::binary_literal>(ptr, tok);
  }

  template <auto FuncDigit, lps::token::tok::TokenKind TokenKind0,
            lps::token::tok::TokenKind TokenKind1>
  inline bool lex_fractional_constant_any(char c, const char*& ptr,
                                          lps::token::Token<TagName>& tok) {
    const char* p_dot = ptr;
    bool has_first_digit_seq = false;
    if (c != '.') {
      if (lex_number_literal<FuncDigit, TokenKind0>(ptr, tok)) {
        has_first_digit_seq = true;
        p_dot = ptr;
      }
      this->ws_skipping(p_dot, tok);
      if (*p_dot != '.') {
        return false;
      }
      p_dot++;
    }
    this->ws_skipping(p_dot, tok);
    ptr = p_dot;
    const char* end = ptr;
    if (lex_number_literal<FuncDigit, TokenKind0>(ptr, tok)) {
      end = ptr;
    } else {
      if (!has_first_digit_seq) {
        return false;
      }
    }

    ptr = end;
    this->token_formulate(tok, ptr, TokenKind1);
    tok.data(this->cur());
    return true;
  }

  // hexadecimal-fractional-constant:
  // 	hexadecimal_digit_sequence[opt], `.`, hexadecimal_digit_sequence
  // 	hexadecimal_digit_sequence, `.`
  inline bool lex_hexadecimal_fractional_constant(
      char c, const char*& ptr, lps::token::Token<TagName>& tok) {
    return lex_fractional_constant_any<
        [](const char*& ptr, lps::token::Token<TagName>& /*tok*/) {
          if (*ptr >= '0' && *ptr <= '9' || *ptr >= 'a' && *ptr <= 'f' ||
              *ptr >= 'A' && *ptr <= 'F') {
            ptr++;
            return true;
          }
          return false;
        },
        token::tok::TokenKind::hexadecimal_literal,
        token::tok::TokenKind::fractional_constant>(c, ptr, tok);
  }

  // fractional-constant:
  // 	digit_sequence[opt], `.`, digit_sequence
  // 	digit_sequence, `.`
  inline bool lex_fractional_constant(char c, const char*& ptr,
                                      lps::token::Token<TagName>& tok) {
    return lex_fractional_constant_any<
        [](const char*& ptr, lps::token::Token<TagName>& /*tok*/) {
          if (*ptr >= '0' && *ptr <= '9') {
            ptr++;
            return true;
          }
          return false;
        },
        token::tok::TokenKind::decimal_literal,
        token::tok::TokenKind::fractional_constant>(c, ptr, tok);
  }

  inline bool lex_exponent_part_any(const char*& ptr,
                                    lps::token::Token<TagName>& tok,
                                    char part_char0, char part_char1) {
    if (*ptr == part_char0 || *ptr == part_char1) {
      ptr++;
      if (*ptr == '+' || *ptr == '-') {
        ptr++;
      }
      if (lex_decimal_literal(ptr, tok)) {
        return true;
      }
    }
    return false;
  }

  // exponent-part:
  // 	`e`, sign[opt], digit_sequence
  // 	`E`, sign[opt], digit_sequence
  inline bool lex_exponent_part(const char*& ptr,
                                lps::token::Token<TagName>& tok) {
    return lex_exponent_part_any(ptr, tok, 'e', 'E');
  }

  inline bool lex_floating_point_suffix(const char*& ptr,
                                        lps::token::Token<TagName>& tok) {
    if (*ptr == 'L' || *ptr == 'l' || *ptr == 'F' || *ptr == 'f') {
      ptr++;
      return true;
    }
    return false;
  }

  // decimal-floating-point-literal:
  // 	fractional_constant, exponent_part[opt], floating_point_suffix[opt]
  // 	digit_sequence, exponent_part[opt], floating_point_suffix[opt]
  inline bool lex_decimal_floating_point_literal(
      char c, const char*& ptr, lps::token::Token<TagName>& tok) {
    const char* tmp_ptr0 = ptr;
    const char* tmp_ptr1 = ptr;
    bool is_type0 = false;
    bool has_exponent_part = false;
    bool has_floating_point_suffix = false;
    if (lex_fractional_constant(c, tmp_ptr0, tok)) {
      ptr = tmp_ptr0;
      is_type0 = true;
    } else if (lex_decimal_literal(tmp_ptr1, tok)) {
      ptr = tmp_ptr1;
    } else {
      return false;
    }
    tmp_ptr0 = ptr;
    if (lex_exponent_part(tmp_ptr0, tok)) {
      has_exponent_part = true;
      ptr = tmp_ptr0;
    }
    tmp_ptr1 = ptr;
    if (lex_floating_point_suffix(tmp_ptr1, tok)) {
      has_floating_point_suffix = true;
      ptr = tmp_ptr1;
    }
    if (!has_floating_point_suffix && !has_exponent_part) {
      return is_type0;
    }
    this->token_formulate(
        tok, ptr, token::tok::TokenKind::decimal_floating_point_literal);
    tok.data(this->cur());

    int dummy = -1;

    return true;
  }

  // binary-exponent-part:
  // 	`p`, sign[opt], digit_sequence
  // 	`P`, sign[opt], digit_sequence
  inline bool lex_binary_exponent_part(const char*& ptr,
                                       lps::token::Token<TagName>& tok) {
    return lex_exponent_part_any(ptr, tok, 'p', 'P');
  }

  // hexadecimal-floating-point-literal:
  // 	hexadecimal_prefix, hexadecimal_fractional_constant, binary_exponent_part, floating_point_suffix[opt]
  // 	hexadecimal_prefix, hexadecimal_digit_sequence, binary_exponent_part, floating_point_suffix[opt]
  inline bool lex_hexadecimal_floating_point_literal(
      char c, const char*& ptr, lps::token::Token<TagName>& tok) {
    if (c == '0') {
      if (*ptr == 'x' || *ptr == 'X') {
        ptr++;
        char z = *ptr;
        const char* tmp_ptr = ptr;
        bool hexadecimal_fractional_constant_ok = false;
        if (!lex_hexadecimal_fractional_constant(z, tmp_ptr, tok)) {
          tmp_ptr = ptr;
          if (!lex_hexadecimal_literal(tmp_ptr, tok)) {
            return false;
          }
        } else {
          hexadecimal_fractional_constant_ok = true;
        }
        ptr = tmp_ptr;
        if (!lex_binary_exponent_part(ptr, tok)) {
          return false;
        }
        tmp_ptr = ptr;
        if (lex_floating_point_suffix(tmp_ptr, tok)) {
          ptr = tmp_ptr;
        }
        this->token_formulate(
            tok, ptr,
            token::tok::TokenKind::hexadecimal_floating_point_literal);
        tok.data(this->cur());
        return true;
      }
    }
    return false;
  }

  // floating-point-literal:
  // 	decimal_floating_point_literal
  // 	hexadecimal_floating_point_literal
  inline bool lex_floating_point_literal(char c, const char*& ptr,
                                         lps::token::Token<TagName>& tok) {

    const char* tmp_ptr0 = ptr;
    const char* tmp_ptr1 = ptr;
    bool flg = false;
    if (lex_hexadecimal_floating_point_literal(c, tmp_ptr0, tok)) {
      ptr = tmp_ptr0;
      flg = true;
    } else if (lex_decimal_floating_point_literal(c, tmp_ptr1, tok)) {
      ptr = tmp_ptr1;
      flg = true;
    }

    if (flg) {
      this->token_formulate(tok, ptr,
                            token::tok::TokenKind::floating_point_literal);
      tok.data(this->cur());
      return true;
    }
    return false;
  }

  inline bool lex_numeric_constant() { return false; }

  // integer-literal:
  // 	binary_literal, integer_suffix[opt]
  // 	octal_literal, integer_suffix[opt]
  // 	decimal_literal, integer_suffix[opt]
  // 	hexadecimal_literal, integer_suffix[opt]
  inline bool lex_integer_literal(char c, const char*& ptr,
                                  lps::token::Token<TagName>& tok) {
    const char* matched_ptr = ptr;
    token::tok::TokenKind kind = token::tok::TokenKind::unknown;
    bool flg = false;
    if (c == '0') {
      if (*ptr == 'b' || *ptr == 'B') {
        ptr++;
        if (lex_binary_literal(ptr, tok)) {
          matched_ptr = ptr;
          kind = tok.kind();
          flg = true;
        }
      } else if (*ptr == 'x' || *ptr == 'X') {
        ptr++;
        if (lex_hexadecimal_literal(ptr, tok)) {
          matched_ptr = ptr;
          kind = tok.kind();
          flg = true;
        }
      } else {
        if (lex_octal_literal(ptr, tok)) {
          matched_ptr = ptr;
          kind = tok.kind();
          flg = true;
        }
      }
    } else if (c >= '1' && c <= '9') {
      if (*ptr == '\'') {
        ptr++;
      }
      if (lex_decimal_literal(ptr, tok)) {
        matched_ptr = ptr;
        kind = tok.kind();
        flg = true;
      }
    }

    if (flg) {
      if (lex_integer_suffix(ptr, tok)) {
        matched_ptr = ptr;
      }
      this->token_formulate(tok, matched_ptr, kind);
      tok.data(this->cur());
    }
    return flg;
  }

  inline bool lex_string_literal() { return false; }
  inline bool lex_raw_string() { return false; }
  inline bool lex_identifier(const char*& ptr,
                             lps::token::Token<TagName>& tok) {
    using namespace basic::str::ascii;
    while (true) {
      char c = *ptr;
      // Fast path.
      if (is::IdentContinue(c)) {
        ++ptr;
        continue;
      }
      auto c_sz = this->char_size(ptr);
      c = std::get<0>(c_sz);
      auto sz_tmp = std::get<1>(c_sz);
      if (is::IdentContinue(c)) {
        ptr = this->consume_char(ptr, sz_tmp, tok);
        continue;
      }
      if (c == '$') {
        // todo(@mxlol233): handle `$`
        continue;
      }
      if (c == '\\') {
        // todo(@mxlol233): handle `UCN`
        continue;
      }
      break;
    }

    this->token_formulate(tok, ptr, lps::token::tok::TokenKind::raw_identifier);
    tok.data(this->cur());
    auto ident_str = tok.template str<meta::S("ident_str")>();
    if (lps::token::tok::IdentInfo::instance().kw_has(ident_str)) {
      tok.kind(lps::token::tok::IdentInfo::instance().kw_at(ident_str));
    } else {
      tok.kind(token::tok::TokenKind::identifier);
    }

    return true;
  }
  inline bool lex_char() { return false; }
};

}  // namespace details

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
        details::Basic<TagName> m(file_id_, cur(), end_, details::kBasic);
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
