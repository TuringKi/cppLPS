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
  explicit Base(uint32_t start_file_id, const char* ptr, MethodType m)
      : file_id_(start_file_id), ptr_(ptr), type_(m) {}
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
          diag::doing(ptr, diag::DiagKind::kBackslashNewlineSpace);
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

  inline const char* consume_char(const char*& ptr, uint32_t sz,
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
    return {c, sz};
  }

  inline bool ws_skipping() { return false; }

  [[nodiscard]] inline const char* cur() const { return ptr_ + pos_; }
  inline void inc(size_t n) { pos_ += n; }

  inline void token_formulate(lps::token::Token<TagName>& tok, const char* end,
                              lps::token::tok::TokenKind kind) {
    lps_assert(TagName, this->cur() != nullptr);
    auto offset = end - this->cur();
    lps_assert(TagName,
               offset >= 0 && offset < std::numeric_limits<uint32_t>::max());

    tok.offset(offset);
    tok.file_id(file_id_);
    tok.kind(kind);
  }

  MethodType type_{MethodType::kNone};
  const char* ptr_;
  uint32_t file_id_{0};
  size_t pos_{0};
};

template <meta::Str TagName>
class Basic : public Base<TagName> {
  using base = Base<TagName>;

 public:
  explicit Basic(uint32_t start_file_id, const char* ptr, MethodType m)
      : base(start_file_id, ptr, m) {}

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
    lps::token::tok::TokenKind token_kind;

    if (!is::VertWs(c)) {
      // todo(@mxlol233): not a new line ?
    }

    switch (c) {

      case 0: {  // null
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
        goto next;
      }
      case ' ':
      case '\t':
      case '\f':
      case '\v':
      skip_horz_ws:
        tok.set_flag(token::Flag::kLeadingSpace);
        if (this->ws_skipping()) {
          return;
        }
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
      case '9':
        if (lex_numeric_constant()) {
          return;
        }
      case 'u':
        if (lex_u()) {
          return;
        };
      case 'U':
        if (lex_U()) {
          return;
        };
      case 'R':
        if (lex_raw_string()) {
          return;
        };
      case 'L':
        if (lex_L()) {
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
        if (lex_char()) {
          return;
        }
      case '"':
        if (lex_string_literal()) {
          return;
        }
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

        if (c >= '0' && c <= '9') {
          if (lex_numeric_constant()) {
            return;
          }
        } else if (c == '*') {
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

  inline bool lex_numeric_constant() { return false; }
  inline bool lex_string_literal() { return false; }
  inline bool lex_u() { return false; }
  inline bool lex_U() { return false; }
  inline bool lex_raw_string() { return false; }
  inline bool lex_L() { return false; }
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
    switch (method) {
      case details::kBasic: {
        details::Basic<TagName> m(file_id_, cur(), details::kBasic);
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
  explicit Lexer(uint32_t start_file_id, const char* ptr)
      : file_id_(start_file_id), ptr_(ptr) {}
  [[nodiscard]] bool finish(size_t len) const { return !(pos_ < len); }

 private:
  [[nodiscard]] const char* cur() const { return ptr_ + pos_; }
  void inc(size_t n) { pos_ += n; }

  const char* ptr_{nullptr};
  uint32_t file_id_{0};
  size_t pos_{0};
};

}  // namespace lps::lexer
