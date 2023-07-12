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

#include <functional>
#include <utility>
#include "basic/exception.h"
#include "basic/str.h"
#include "basic/vec.h"
#include "basic/vfile.h"
#include "diag.h"
#include "src.h"
#include "tu.h"

namespace lps::lexer::details {

enum MethodType : uint8_t {
  kNone = 0,
  kBasic,
  kPreprocessing,
};

using CharSize = std::tuple<char, uint32_t>;

inline bool operator==(const std::tuple<char, uint32_t>& cz0, const char& c1) {
  char c0 = std::get<0>(cz0);
  return c0 == c1;
}

class Base {

 public:
  static constexpr basic::mem::TraceTag::tag_type kTag =
      "lps::lexer::details::Base";
  using type = Base;
  using const_ptr_type = const type*;
  using ptr_type = basic::FileVisitor;
  using lex_func_type1 =
      std::function<token::details::TokenKind(const ptr_type&, ptr_type&)>;
  using lex_func_type2 =
      std::function<bool(const ptr_type&, ptr_type&, lps::token::Token&)>;
  inline void lex(lps::token::Token& tok) {
    try {
      lex_impl(tok);
    } catch (basic::vfile::Eof& except_eof) {
      // jump to another virtual file's visitor
      tok.kind(token::details::TokenKind::eof);
      if (src::Manager::instance().has<1>(except_eof.next_file_id())) {
        // token_file
        auto idx = except_eof.offset();
        auto visitor = src::Manager::instance().visitor_of_token_file(
            except_eof.next_file_id());
        tok = visitor[idx];
      } else if (src::Manager::instance().has<0>(except_eof.next_file_id())) {
        // char_file
      }
      return;
    }
  }
  inline virtual void lex_impl(lps::token::Token& tok) = 0;
  [[nodiscard]] MethodType method() const { return type_; }
  explicit Base(const ptr_type& ptr, MethodType m)
      : file_id_(ptr.file_id()), ptr_(ptr), type_(m) {}
  [[nodiscard]] inline size_t pos() const { return pos_; }

  static inline uint32_t escaped_newline_scanning(const ptr_type& ptr) {
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

  static inline char char_scanning(ptr_type ptr, uint32_t& size) {
    using namespace basic::str::ascii;
    auto first_ptr = ptr;
    if (*ptr == '\\') {
      ++ptr;
      size++;
    Slash:
      if (!is::Ws(
              *ptr)) {  // if next char is not `white space`, then it's just `'\\'`.
        return '\\';
      }
      if (uint32_t newline_size = escaped_newline_scanning(ptr)) {

        if (*ptr != '\n' && *ptr != '\r') {
          diag(first_ptr, ptr, diag::DiagKind::back_slash_new_space);
        }
        size += newline_size;
        ptr += newline_size;
        return char_scanning(ptr, size);
      }
      return '\\';
    }

    // todo(@mxlol233): handle `trigraph char`
    if (*ptr == '?' && ptr[1] == '?') {}

    size++;
    return *ptr;
  }

  static inline ptr_type consume_char(const ptr_type& ptr, uint32_t sz) {
    if (sz == 1) {
      return ptr + sz;
    }
    sz = 0;
    char c = char_scanning(ptr, sz);
    return ptr + sz;
  }

  static inline CharSize char_size(ptr_type ptr) {
    if (basic::str::ascii::is::NormalChar(ptr[0])) {
      return {*(ptr++), 1};
    }
    uint32_t sz = 0;
    char c = char_scanning(ptr, sz);
    return {c, sz};
  }

  static inline bool horzws_skipping(ptr_type& ptr) {
    if (basic::str::ascii::is::HorzWs(*ptr)) {
      ptr.horzws_skipping();
    }
    if (*ptr == '\\') {
      if (!basic::str::ascii::is::VertWs(*(ptr + 1))) {
        diag(ptr, ptr + 1,
             diag::DiagKind::expected_vertws_after_slash_in_preprocessing);
        throw basic::vfile::Eof();
        return false;
      }
      ++ptr;
      ++ptr;
      horzws_skipping(ptr);
    }
    return true;
  }
  static inline CharSize advance(ptr_type& ptr) {
    uint32_t sz = 0;
    if (basic::str::ascii::is::NormalChar(ptr[0])) {
      return {*(ptr++), 1};
    }

    char c = char_scanning(ptr, sz);
    ptr += sz;
    return {c, sz};
  }

  static inline CharSize ws_skipping(ptr_type& ptr) {
    uint32_t sz = 0;
    while (basic::str::ascii::is::Ws(*ptr)) {
      ++ptr;
      sz++;
    }
    return {*ptr, sz};
  }

 protected:
  [[nodiscard]] inline ptr_type cur() const { return ptr_ + pos_; }
  [[nodiscard]] inline const char* cur_char_ptr() const {
    return (ptr_ + pos_).cur();
  }
  inline void inc(size_t n) { pos_ += n; }

  static inline void diag(const ptr_type& first_ptr, const ptr_type& ptr,
                          diag::DiagKind kind) {
    diag::DiagInputs diag_input;
    diag_input.kind_ = kind;
    lps::token::Token tok_error;
    token_formulate(tok_error, first_ptr, ptr,
                    token::details::TokenKind::unknown);
    tok_error.data(first_ptr);
    diag_input.main_token_ = tok_error;
    diag::doing(diag_input.main_token_, diag_input.kind_,
                diag_input.context_tokens_);
  }

  static inline void token_formulate(lps::token::Token& tok,
                                     const ptr_type& first, const ptr_type& end,
                                     lps::token::details::TokenKind kind) {
    uint32_t offset = 0;
    if (end.file_id() == first.file_id()) {
      offset = end - first;
    } else {
      unreachable(kTag);
    }

    lps_assert(kTag,
               offset >= 0 && offset < std::numeric_limits<uint32_t>::max());

    tok.offset(offset);
    tok.file_id(end.file_id());
    tok.kind(kind);
    tok.data(first);
    lps_assert(kTag, src::Manager::instance().has(end.file_id()));
    auto tok_info = token::TokenLists::Info::create(tok);
    tok.next_visitor(tok_info.offset_ + offset, end.file_id());
  }

  template <auto Func>
  static inline bool lex_char(ptr_type& ptr) {
    if (Func(*ptr)) {
      ++ptr;
      return true;
    }
    return false;
  }

  inline bool lex_something(const ptr_type& first_ptr, ptr_type& ptr,
                            lps::token::Token& tok,
                            const lex_func_type1& func) {

    ptr_type tmp_ptr = ptr;
    auto kind = func(first_ptr, tmp_ptr);
    if (kind != token::details::TokenKind::unknown) {
      ptr = tmp_ptr;
      token_formulate(tok, first_ptr, ptr, kind);
      return true;
    }
    return false;
  }

  inline bool lex_something_parallel(const ptr_type& first_ptr, ptr_type& ptr,
                                     lps::token::Token& tok,
                                     const std::vector<lex_func_type2>& funcs) {
    for (const auto& f : funcs) {
      ptr_type tmp_ptr = ptr;
      lps::token::Token tmp_tok;
      if (f(first_ptr, tmp_ptr, tmp_tok)) {
        ptr = tmp_ptr;
        tok = tmp_tok;
        return true;
      }
    }
    return false;
  }

  template <token::details::TokenKind OutKind>
  inline bool lex_something_recursive(
      const ptr_type& first_ptr, ptr_type& ptr, lps::token::Token& tok,
      const lex_func_type2& first_lex_func,
      const lex_func_type2& stop_cond = [](const ptr_type&, ptr_type&,
                                           lps::token::Token&) -> bool {
        return false;
      }) {
    return [this](const ptr_type& first_ptr, ptr_type& ptr,
                  lps::token::Token& tok, const lex_func_type2& first_lex_func,
                  const lex_func_type2& stop_cond) {
      auto lex = [this, &first_lex_func, &stop_cond](
                     const ptr_type& first_ptr_, ptr_type& ptr,
                     lps::token::Token& tok, const auto& func) -> bool {
        if (stop_cond(first_ptr_, ptr, tok)) {
          return false;
        }
        ptr_type first_ptr = ptr - 1;
        if (first_lex_func(first_ptr, ptr, tok)) {
          ptr_type tmp_ptr = ptr;
          if (basic::str::ascii::is::HorzWs(*tmp_ptr)) {
            tmp_ptr.horzws_skipping();  // skip space, `\t` etc.
          }
          auto tmp_c = tmp_ptr;
          ++tmp_ptr;
          lps::token::Token tmp_tok;
          if (func(tmp_c, tmp_ptr, tmp_tok, func)) {
            ptr = tmp_ptr;
            tok = tmp_tok;
          }
          tok.kind(OutKind);
          return true;
        }
        return false;
      };
      return lex(first_ptr, ptr, tok, lex);
    }(first_ptr, ptr, tok, first_lex_func, stop_cond);
  }

  // preprocessing-operator:
  // 	`#`
  // 	`##`
  // 	`%:`
  // 	`%:%:`
  inline bool lex_preprocessing_operator(const ptr_type& first_ptr,
                                         ptr_type& ptr,
                                         lps::token::Token& tok) {
    return lex_something(
        first_ptr, ptr, tok,
        [this](const ptr_type& first_ptr,
               ptr_type& ptr) -> token::details::TokenKind {
          token::details::TokenKind kind = token::details::TokenKind::unknown;
          char next_c = *ptr;
          switch (*first_ptr) {
            case '%': {
              if (next_c == ':') {  // %:
                kind = token::details::TokenKind::percentcolon;
                ++ptr;
                if (*ptr == '%' && *(ptr + 1) == ':') {  // %:%:
                  kind = token::details::TokenKind::percentcolonpercentcolon;
                  ptr += 2;
                }
              }
              break;
            }
            case '#': {
              kind = token::details::TokenKind::hash;
              if (next_c == '#') {
                kind = token::details::TokenKind::hashhash;
                ++ptr;
              }
              break;
            }
            default:
              break;
          }
          return kind;
        });
  }

  inline bool lex_operator_or_punctuator(const ptr_type& first_ptr,
                                         ptr_type& ptr,
                                         lps::token::Token& tok) {
    return lex_something(
        first_ptr, ptr, tok,
        [this](const ptr_type& first_ptr,
               ptr_type& ptr) -> token::details::TokenKind {
          token::details::TokenKind kind = token::details::TokenKind::unknown;
          char next_c = *ptr;
          switch (*first_ptr) {
            case '{':
              kind = token::details::TokenKind::l_brace;
              break;
            case '}':
              kind = token::details::TokenKind::r_brace;
              break;
            case '[':
              kind = token::details::TokenKind::l_square;
              break;
            case ']':
              kind = token::details::TokenKind::r_square;
              break;
            case '(':
              kind = token::details::TokenKind::l_paren;
              break;
            case ')':
              kind = token::details::TokenKind::r_paren;
              break;
            case '?':
              kind = token::details::TokenKind::question;
              break;
            case ';':
              kind = token::details::TokenKind::semi;
              break;
            case ':': {
              kind = token::details::TokenKind::colon;
              if (next_c == '>') {
                kind = token::details::TokenKind::colongreater;
                ++ptr;
              } else if (next_c == ':') {
                kind = token::details::TokenKind::coloncolon;
                ++ptr;
              }
              break;
            }
            case '.': {
              kind = token::details::TokenKind::period;
              if (next_c == '.') {
                if (*(ptr + 1) == '.') {
                  kind = token::details::TokenKind::ellipsis;
                  ptr += 2;
                  break;
                }
              } else if (next_c == '*') {
                kind = token::details::TokenKind::periodstar;
                ++ptr;
              }
              break;
            }
            case '~':
              kind = token::details::TokenKind::tilde;
              break;
            case '!': {
              kind = token::details::TokenKind::exclaim;
              if (next_c == '=') {
                kind = token::details::TokenKind::exclaimequal;
                ++ptr;
              }
              break;
            }
              kind = token::details::TokenKind::exclaim;
              break;
            case '+': {
              kind = token::details::TokenKind::plus;
              if (next_c == '=') {
                kind = token::details::TokenKind::plusequal;
                ++ptr;
              } else if (next_c == '+') {
                kind = token::details::TokenKind::plusplus;
                ++ptr;
              }
              break;
            }

            case '-': {
              kind = token::details::TokenKind::minus;
              if (next_c == '>') {
                kind = token::details::TokenKind::arrow;
                ++ptr;
                if (*ptr == '*') {
                  kind = token::details::TokenKind::arrowstar;
                  ++ptr;
                }
              } else if (next_c == '-') {
                kind = token::details::TokenKind::minusminus;
                ++ptr;
              } else if (next_c == '=') {
                kind = token::details::TokenKind::minusequal;
                ++ptr;
              }
              break;
            }

            case '*': {
              kind = token::details::TokenKind::star;
              if (next_c == '=') {
                kind = token::details::TokenKind::starequal;
                ++ptr;
              }
              break;
            }
            case '/': {
              kind = token::details::TokenKind::slash;
              if (next_c == '=') {
                kind = token::details::TokenKind::slashequal;
                ++ptr;
              }
              break;
            }
            case '%': {
              kind = token::details::TokenKind::percent;
              if (next_c == '=') {
                kind = token::details::TokenKind::percentequal;
                ++ptr;
              } else if (next_c == '>') {
                kind = token::details::TokenKind::percentgreater;
                ++ptr;
              }
              break;
            }
            case '&': {
              kind = token::details::TokenKind::amp;
              if (next_c == '=') {
                kind = token::details::TokenKind::ampequal;
                ++ptr;
              } else if (next_c == '&') {
                kind = token::details::TokenKind::ampamp;
                ++ptr;
              }
              break;
            }
            case '^': {
              kind = token::details::TokenKind::caret;
              if (next_c == '=') {
                kind = token::details::TokenKind::caretequal;
                ++ptr;
              } else if (next_c == '^') {
                kind = token::details::TokenKind::caretcaret;
                ++ptr;
              }
              break;
            }
            case '|': {
              kind = token::details::TokenKind::pipe;
              if (next_c == '=') {
                kind = token::details::TokenKind::pipeequal;
                ++ptr;
              } else if (next_c == '|') {
                kind = token::details::TokenKind::pipepipe;
                ++ptr;
              }
              break;
            }
            case '=': {
              kind = token::details::TokenKind::equal;
              if (next_c == '=') {
                kind = token::details::TokenKind::equalequal;
                ++ptr;
              }
              break;
            }
            case ',': {
              kind = token::details::TokenKind::comma;
              break;
            }
            case '<': {
              kind = token::details::TokenKind::less;
              if (next_c == '%') {
                kind = token::details::TokenKind::lesspercent;
                ++ptr;
              } else if (next_c == ':') {
                kind = token::details::TokenKind::lesscolon;
                ++ptr;
              } else if (next_c == '=') {  // <=
                kind = token::details::TokenKind::lessequal;
                ++ptr;
                if (*ptr == '>') {  // <=>
                  kind = token::details::TokenKind::spaceship;
                  ++ptr;
                  break;
                }
              } else if (next_c == '<') {  // <<
                kind = token::details::TokenKind::lessless;
                ++ptr;
                if (*ptr == '=') {  // <<=
                  kind = token::details::TokenKind::lesslessequal;
                  ++ptr;
                  break;
                }
              }
              break;
            }
            case '>': {
              kind = token::details::TokenKind::greater;
              if (next_c == '>') {  // >>
                kind = token::details::TokenKind::greatergreater;
                ++ptr;
                if (*ptr == '=') {  // >>=
                  kind = token::details::TokenKind::greatergreaterequal;
                  ++ptr;
                  break;
                }
              } else if (next_c == '=') {  //>=
                kind = token::details::TokenKind::greaterequal;
                ++ptr;
              }
              break;
            }
            default:
              break;
          }
          return kind;
        });
  }

  // operator-or-punctuator: one of:
  //	{ } [ ] ( )
  //	<: :> <% %> ; : ...
  //	? :: . .* -> ->* ~
  //	! + - * / % ^ & |
  //	= += -= *= /= %= ^= &= |=
  //	== != < > <= >= <=> && ||
  //	<< >> <<= >>= ++ -- ,
  //	and or xor not bitand bitor compl
  //	and_eq or_eq xor_eq not_eq
  inline bool lex_operator_or_punctuator_or_keyword_operator(
      const ptr_type& first_ptr, ptr_type& ptr, lps::token::Token& tok) {
    if (!lex_operator_or_punctuator(first_ptr, ptr, tok)) {
      if (this->lex_identifier(first_ptr, ptr, tok)) {
        //	one of following key_words:
        //    and or xor not bitand bitor compl and_eq or_eq xor_eq not_eq
        if (tok.kind() == token::details::TokenKind::kw_and ||
            tok.kind() == token::details::TokenKind::kw_or ||
            tok.kind() == token::details::TokenKind::kw_xor ||
            tok.kind() == token::details::TokenKind::kw_not ||
            tok.kind() == token::details::TokenKind::kw_bitand ||
            tok.kind() == token::details::TokenKind::kw_bitor ||
            tok.kind() == token::details::TokenKind::kw_compl ||
            tok.kind() == token::details::TokenKind::kw_and_eq ||
            tok.kind() == token::details::TokenKind::kw_or_eq ||
            tok.kind() == token::details::TokenKind::kw_xor_eq ||
            tok.kind() == token::details::TokenKind::kw_not_eq) {
          return true;
        }
      }
    } else {
      return true;
    }
    return false;
  }

  inline bool lex_preprocessing_op_or_punc(const ptr_type& first_ptr,
                                           ptr_type& ptr,
                                           lps::token::Token& tok) {
    std::vector<lex_func_type2> funcs = {
        [this](const ptr_type& first_ptr, ptr_type& ptr, lps::token::Token& tok)
            -> bool { return lex_preprocessing_operator(first_ptr, ptr, tok); },
        [this](const ptr_type& first_ptr, ptr_type& ptr,
               lps::token::Token& tok) -> bool {
          return lex_operator_or_punctuator_or_keyword_operator(first_ptr, ptr,
                                                                tok);
        },
    };
    return lex_something_parallel(first_ptr, ptr, tok, funcs);
  }

  template <char StartChar, diag::DiagKind DiagKind>
  inline uint32_t lex_char_seq(ptr_type& ptr, lps::token::Token& /*tok*/) {
    uint32_t cnt_char = 0;
    auto first_ptr = ptr;
    while (*ptr != StartChar) {
      if (*ptr == '\\') {
        advance(ptr);
      }
      if (*ptr == '\n' || *ptr == '\r' || ptr.eof()) {
        diag(first_ptr, ptr, DiagKind);
        return cnt_char;
      }
      advance(ptr);
      cnt_char++;
    }
    return cnt_char;
  }

  inline bool lex_identifier(const ptr_type& first_ptr, ptr_type& ptr,
                             lps::token::Token& tok) {
    using namespace basic::str::ascii;
    while (true) {
      char c = *ptr;
      // Fast path.
      if (is::IdentContinue(c)) {
        ++ptr;
        continue;
      }
      auto c_sz = char_size(ptr);
      c = std::get<0>(c_sz);
      auto sz_tmp = std::get<1>(c_sz);
      if (is::IdentContinue(c)) {
        ptr = consume_char(ptr, sz_tmp);
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

    token_formulate(tok, first_ptr, ptr,
                    lps::token::details::TokenKind::raw_identifier);
    auto ident_str = tok.str();
    if (lps::token::details::IdentInfo::instance().kw_has(ident_str)) {
      tok.kind(lps::token::details::IdentInfo::instance().kw_at(ident_str));
    } else {
      tok.kind(token::details::TokenKind::identifier);
    }

    return true;
  }

  // header-name:
  // 	`<`, h_char_sequence, `>`
  // 	`"`, q_char_sequence, `"`
  inline bool lex_header_name(const ptr_type& first_ptr, ptr_type& ptr,
                              lps::token::Token& tok) {
    bool flg = false;
    if (*first_ptr == '<') {
      uint32_t cnt_char =
          this->template lex_char_seq<'<',
                                      diag::unfinished_header_name_expected_gt>(
              ptr, tok);
      if (cnt_char > 0) {
        flg = true;
      }
    } else if (*first_ptr == '"') {
      uint32_t cnt_char = this->template lex_char_seq<
          '"', diag::unfinished_header_name_expected_quotation>(ptr, tok);
      if (cnt_char > 0) {
        flg = true;
      }
    }
    if (flg) {
      ++ptr;
      token_formulate(tok, first_ptr, ptr,
                      lps::token::details::TokenKind::header_name);
      return true;
    }
    return false;
  }

  inline bool lex_encoding_prefix(const ptr_type& first_ptr, ptr_type& ptr,
                                  lps::token::Token& /*tok*/) {
    if (*first_ptr == 'u') {
      if (*ptr == '8') {
        ++ptr;
      }
      return true;
    }
    return *first_ptr == 'U' || *first_ptr == 'L';
  }

  inline bool lex_character_literal(const ptr_type& first_ptr, ptr_type& ptr,
                                    lps::token::Token& tok) {

    ptr_type start = ptr - 1;
    ptr_type tmp_ptr = ptr;
    bool flg = lex_encoding_prefix(first_ptr, tmp_ptr, tok);
    if (flg) {
      ptr = tmp_ptr;
    }

    if (flg) {
      if (*ptr == '\'') {
        ++ptr;
      } else {
        return false;
      }
    } else {
      if (*first_ptr == '\'') {
        flg = true;
      }
    }
    if (flg) {
      uint32_t cnt_char =
          this->template lex_char_seq<'\'', diag::unfinished_char_literal>(ptr,
                                                                           tok);

      if (cnt_char == 0) {
        diag(first_ptr, ptr, diag::empty_char_literal);
        return false;
      }
      ++ptr;
      token_formulate(tok, first_ptr, ptr,
                      token::details::TokenKind::char_literal);
    }

    return flg;
  }
  static inline bool lex_integer_suffix(ptr_type& ptr) {

    auto c_sz = char_size(ptr);
    auto c = std::get<0>(c_sz);
    auto sz_tmp = std::get<1>(c_sz);
    if (c == 'l') {  //l
      ++ptr;
      if (*ptr == 'l') {  //ll
        ++ptr;
        if (*ptr == 'u') {  //llu
          ++ptr;
        } else if (*ptr == 'U') {  // llU
          ++ptr;
        }
      } else if (*ptr == 'U') {  // lU
        ++ptr;
      } else if (*ptr == 'u') {  // lu
        ++ptr;
      }
    } else if (c == 'L') {  // L
      ++ptr;
      if (*ptr == 'L') {  // LL
        ++ptr;
        if (*ptr == 'u') {  // LLu
          ++ptr;
        } else if (*ptr == 'U') {  // LLU
          ++ptr;
        }
      } else if (*ptr == 'U') {  // LU
        ++ptr;
      } else if (*ptr == 'u') {  // Lu
        ++ptr;
      }
    } else if (c == 'u') {
      ++ptr;
      if (*ptr == 'l') {  //ul
        ++ptr;
        if (*ptr == 'l') {  //ull
          ++ptr;
        }
      } else if (*ptr == 'L') {  // uL
        ++ptr;
        if (*ptr == 'L') {  // uLL
          ++ptr;
        }
      }
    } else if (c == 'U') {
      ++ptr;
      if (*ptr == 'l') {  //Ul
        ++ptr;
        if (*ptr == 'l') {  //Ull
          ++ptr;
        }
      } else if (*ptr == 'L') {  // UL
        ++ptr;
        if (*ptr == 'L') {  // ULL
          ++ptr;
        }
      }
    } else {
      return false;
    }
    return true;
  }

  static inline bool lex_octal_digit(ptr_type& ptr) {
    if (*ptr >= '0' && *ptr <= '7') {
      ++ptr;
      return true;
    }
    return false;
  }

  template <auto FuncDigit, lps::token::details::TokenKind TokenKind>
  inline bool lex_number_literal(const ptr_type& first_ptr, ptr_type& ptr,
                                 lps::token::Token& tok) {

    if (!FuncDigit(ptr)) {
      return false;
    }
    ptr_type matched_ptr = ptr;

    if (*ptr == '\'') {
      ++ptr;
    }

    if (lex_number_literal<FuncDigit, TokenKind>(first_ptr, ptr, tok)) {
      matched_ptr = ptr;
    }
    token_formulate(tok, first_ptr, matched_ptr, TokenKind);
    ptr = matched_ptr;

    return true;
  }

  inline bool lex_octal_literal(const ptr_type& first_ptr, ptr_type& ptr,
                                lps::token::Token& tok) {
    return lex_number_literal<[](ptr_type& ptr) {
      if (*ptr >= '0' && *ptr <= '7') {
        ++ptr;
        return true;
      }
      return false;
    },
                              token::details::TokenKind::octal_literal>(
        first_ptr, ptr, tok);
  }

  inline bool lex_decimal_literal(const ptr_type& first_ptr, ptr_type& ptr,
                                  lps::token::Token& tok) {
    return lex_number_literal<[](ptr_type& ptr) {
      if (*ptr >= '0' && *ptr <= '9') {
        ++ptr;
        return true;
      }
      return false;
    },
                              token::details::TokenKind::decimal_literal>(
        first_ptr, ptr, tok);
  }

  inline bool lex_hexadecimal_literal(const ptr_type& first_ptr, ptr_type& ptr,
                                      lps::token::Token& tok) {
    return lex_number_literal<[](ptr_type& ptr) {
      if (*ptr >= '0' && *ptr <= '9' || *ptr >= 'a' && *ptr <= 'f' ||
          *ptr >= 'A' && *ptr <= 'F') {
        ++ptr;
        return true;
      }
      return false;
    },
                              token::details::TokenKind::hexadecimal_literal>(
        first_ptr, ptr, tok);
  }

  inline bool lex_binary_literal(const ptr_type& first_ptr, ptr_type& ptr,
                                 lps::token::Token& tok) {
    return lex_number_literal<[](ptr_type& ptr) {
      if (*ptr == '0' || *ptr == '1') {
        ++ptr;
        return true;
      }
      return false;
    },
                              token::details::TokenKind::binary_literal>(
        first_ptr, ptr, tok);
  }

  template <auto FuncDigit, lps::token::details::TokenKind TokenKind0,
            lps::token::details::TokenKind TokenKind1>
  inline bool lex_fractional_constant_any(const ptr_type& first_ptr_,
                                          ptr_type& ptr,
                                          lps::token::Token& tok) {
    ptr_type p_dot = ptr;
    p_dot.ws_skip(true);
    bool has_first_digit_seq = false;
    ptr_type first_ptr = first_ptr_;
    if (*first_ptr != '.') {
      if (lex_number_literal<FuncDigit, TokenKind0>(first_ptr, ptr, tok)) {
        has_first_digit_seq = true;
        p_dot = ptr;
      }
      p_dot++;
      if (*p_dot != '.') {
        return false;
      }
      p_dot++;
    }
    first_ptr = p_dot;
    p_dot++;
    ptr = p_dot;
    ptr_type end = ptr;
    if (lex_number_literal<FuncDigit, TokenKind0>(first_ptr, ptr, tok)) {
      end = ptr;
    } else {
      if (!has_first_digit_seq) {
        return false;
      }
    }

    ptr = end;
    token_formulate(tok, first_ptr, ptr, TokenKind1);
    return true;
  }

  // hexadecimal-fractional-constant:
  // 	hexadecimal_digit_sequence[opt], `.`, hexadecimal_digit_sequence
  // 	hexadecimal_digit_sequence, `.`
  inline bool lex_hexadecimal_fractional_constant(const ptr_type& first_ptr,
                                                  ptr_type& ptr,
                                                  lps::token::Token& tok) {
    return lex_fractional_constant_any<
        [](ptr_type& ptr) {
          if (*ptr >= '0' && *ptr <= '9' || *ptr >= 'a' && *ptr <= 'f' ||
              *ptr >= 'A' && *ptr <= 'F') {
            ++ptr;
            return true;
          }
          return false;
        },
        token::details::TokenKind::hexadecimal_literal,
        token::details::TokenKind::fractional_constant>(first_ptr, ptr, tok);
  }

  // fractional-constant:
  // 	digit_sequence[opt], `.`, digit_sequence
  // 	digit_sequence, `.`
  inline bool lex_fractional_constant(const ptr_type& first_ptr, ptr_type& ptr,
                                      lps::token::Token& tok) {
    return lex_fractional_constant_any<
        [](ptr_type& ptr) {
          if (*ptr >= '0' && *ptr <= '9') {
            ++ptr;
            return true;
          }
          return false;
        },
        token::details::TokenKind::decimal_literal,
        token::details::TokenKind::fractional_constant>(first_ptr, ptr, tok);
  }

  inline bool lex_exponent_part_any(const ptr_type& first_ptr, ptr_type& ptr,
                                    lps::token::Token& tok, char part_char0,
                                    char part_char1) {
    if (*ptr == part_char0 || *ptr == part_char1) {
      ++ptr;
      if (*ptr == '+' || *ptr == '-') {
        ++ptr;
      }
      if (lex_decimal_literal(first_ptr, ptr, tok)) {
        return true;
      }
    }
    return false;
  }

  // exponent-part:
  // 	`e`, sign[opt], digit_sequence
  // 	`E`, sign[opt], digit_sequence
  inline bool lex_exponent_part(const ptr_type& first_ptr, ptr_type& ptr,
                                lps::token::Token& tok) {
    return lex_exponent_part_any(first_ptr, ptr, tok, 'e', 'E');
  }

  static inline bool lex_floating_point_suffix(ptr_type& ptr) {
    if (*ptr == 'L' || *ptr == 'l' || *ptr == 'F' || *ptr == 'f') {
      ++ptr;
      return true;
    }
    return false;
  }

  // decimal-floating-point-literal:
  // 	fractional_constant, exponent_part[opt], floating_point_suffix[opt]
  // 	digit_sequence, exponent_part[opt], floating_point_suffix[opt]
  inline bool lex_decimal_floating_point_literal(const ptr_type& first_ptr,
                                                 ptr_type& ptr,
                                                 lps::token::Token& tok) {
    ptr_type tmp_ptr0 = ptr;
    ptr_type tmp_ptr1 = ptr;
    bool is_type0 = false;
    bool has_exponent_part = false;
    bool has_floating_point_suffix = false;
    if (lex_fractional_constant(first_ptr, tmp_ptr0, tok)) {
      ptr = tmp_ptr0;
      is_type0 = true;
    } else if (lex_decimal_literal(first_ptr, tmp_ptr1, tok)) {
      ptr = tmp_ptr1;
    } else {
      return false;
    }
    tmp_ptr0 = ptr;
    if (lex_exponent_part(tmp_ptr0, tmp_ptr0, tok)) {
      has_exponent_part = true;
      ptr = tmp_ptr0;
    }
    tmp_ptr1 = ptr;
    if (lex_floating_point_suffix(tmp_ptr1)) {
      has_floating_point_suffix = true;
      ptr = tmp_ptr1;
    }
    if (!has_floating_point_suffix && !has_exponent_part) {
      return is_type0;
    }
    token_formulate(tok, first_ptr, ptr,
                    token::details::TokenKind::decimal_floating_point_literal);
    return true;
  }

  // binary-exponent-part:
  // 	`p`, sign[opt], digit_sequence
  // 	`P`, sign[opt], digit_sequence
  inline bool lex_binary_exponent_part(const ptr_type& first_ptr, ptr_type& ptr,
                                       lps::token::Token& tok) {
    return lex_exponent_part_any(first_ptr, ptr, tok, 'p', 'P');
  }

  // hexadecimal-floating-point-literal:
  // 	hexadecimal_prefix, hexadecimal_fractional_constant, binary_exponent_part, floating_point_suffix[opt]
  // 	hexadecimal_prefix, hexadecimal_digit_sequence, binary_exponent_part, floating_point_suffix[opt]
  inline bool lex_hexadecimal_floating_point_literal(const ptr_type& first_ptr,
                                                     ptr_type& ptr,
                                                     lps::token::Token& tok) {
    if (*first_ptr == '0') {
      if (*ptr == 'x' || *ptr == 'X') {
        ++ptr;
        ptr_type z = ptr;
        ptr_type tmp_ptr = ptr;
        bool hexadecimal_fractional_constant_ok = false;
        if (!lex_hexadecimal_fractional_constant(z, tmp_ptr, tok)) {
          tmp_ptr = ptr;
          if (!lex_hexadecimal_literal(tmp_ptr, tmp_ptr, tok)) {
            return false;
          }
        } else {
          hexadecimal_fractional_constant_ok = true;
        }
        ptr = tmp_ptr;
        if (!lex_binary_exponent_part(ptr, ptr, tok)) {
          return false;
        }
        tmp_ptr = ptr;
        if (lex_floating_point_suffix(tmp_ptr)) {
          ptr = tmp_ptr;
        }
        token_formulate(
            tok, first_ptr, ptr,
            token::details::TokenKind::hexadecimal_floating_point_literal);
        return true;
      }
    }
    return false;
  }

  // floating-point-literal:
  // 	decimal_floating_point_literal
  // 	hexadecimal_floating_point_literal
  inline bool lex_floating_point_literal(const ptr_type& first_ptr,
                                         ptr_type& ptr,
                                         lps::token::Token& tok) {

    ptr_type tmp_ptr0 = ptr;
    ptr_type tmp_ptr1 = ptr;
    bool flg = false;
    if (lex_hexadecimal_floating_point_literal(first_ptr, tmp_ptr0, tok)) {
      ptr = tmp_ptr0;
      flg = true;
    } else if (lex_decimal_floating_point_literal(first_ptr, tmp_ptr1, tok)) {
      ptr = tmp_ptr1;
      flg = true;
    }

    if (flg) {
      token_formulate(tok, first_ptr, ptr,
                      token::details::TokenKind::floating_point_literal);
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
  inline bool lex_integer_literal(const ptr_type& first_ptr, ptr_type& ptr,
                                  lps::token::Token& tok) {
    ptr_type matched_ptr = ptr;
    token::details::TokenKind kind = token::details::TokenKind::unknown;
    bool flg = false;
    if (*first_ptr == '0') {
      if (*ptr == 'b' || *ptr == 'B') {
        ++ptr;
        if (lex_binary_literal(first_ptr, ptr, tok)) {
          matched_ptr = ptr;
          kind = tok.kind();
          flg = true;
        }
      } else if (*ptr == 'x' || *ptr == 'X') {
        ++ptr;
        if (lex_hexadecimal_literal(first_ptr, ptr, tok)) {
          matched_ptr = ptr;
          kind = tok.kind();
          flg = true;
        }
      } else {
        if (lex_octal_literal(first_ptr, ptr, tok)) {
          matched_ptr = ptr;
          kind = tok.kind();
          flg = true;
        }
      }
    } else if (*first_ptr >= '1' && *first_ptr <= '9') {
      if (*ptr == '\'') {
        ++ptr;
      }
      if (lex_decimal_literal(first_ptr, ptr, tok)) {
        matched_ptr = ptr;
      }
      kind = token::details::TokenKind::decimal_literal;
      flg = true;
    }

    if (flg) {
      if (lex_integer_suffix(ptr)) {
        matched_ptr = ptr;
      }
      token_formulate(tok, first_ptr, matched_ptr, kind);
    }
    return flg;
  }

  // raw-string:
  // 	`"`, d_char_sequence[opt], `(`, r_char_sequence[opt], `)`, d_char_sequence[opt], `"`
  inline bool lex_raw_string(ptr_type& ptr, lps::token::Token& tok) {
    using namespace basic::str::ascii;
    auto first_ptr = ptr;
    if (*ptr != '"') {
      return false;
    }
    ++ptr;

    //A string-literal that has an R in the prefix is a raw string literal.
    //The d-char-sequence serves as a delimiter. The terminating d-char-sequence
    //of a raw-string is the same sequence of characters as the initial d-char-sequence.
    //A d-char-sequence shall consist of at most 16 characters.
    unsigned prefix_len = 0;
    while (prefix_len != 16 && is::RawStringDelimBody(ptr[prefix_len])) {
      prefix_len++;
    }

    if (ptr[prefix_len] != '(') {  // not a delimiter
      ptr_type prefix_end = ptr + prefix_len;
      if (prefix_len == 16) {
        diag(first_ptr, prefix_end,
             diag::DiagKind::raw_string_delimiter_too_long);
      } else {
        diag(first_ptr, prefix_end,
             diag::DiagKind::raw_string_invalid_delimiter);
      }
      return false;
    }

    ptr_type prefix_start = ptr;
    ptr = ptr + prefix_len + 1;
    while (*ptr != ')') {  //find next `)`
      ++ptr;
    }
    ++ptr;
    if ((ptr + prefix_len + 1).eof()) {
      diag(first_ptr, ptr, diag::DiagKind::unfinished_raw_string);
      return false;
    }
    if (basic::FileVisitor::strncmp(prefix_start, ptr, prefix_len) == 0) {
      ptr = ptr + prefix_len;
      if (*ptr == '"') {
        ++ptr;
        token_formulate(tok, first_ptr, ptr,
                        lps::token::details::TokenKind::raw_string);
        return true;
      }
      diag(first_ptr, ptr,
           diag::DiagKind::unfinished_raw_string_expect_quotation);
    } else {
      diag(first_ptr, ptr + prefix_len,
           diag::DiagKind::unfinished_raw_string_because_of_delimiter);
    }

    return false;
  }

  // string-literal:
  // 	encoding_prefix[opt], `"`, s_char_sequence[opt], `"`
  // 	encoding_prefix[opt], `R`, raw_string
  inline bool lex_string_literal(const ptr_type& first_ptr, ptr_type& ptr,
                                 lps::token::Token& tok) {
    ptr_type tmp_ptr = ptr;
    bool flg = lex_encoding_prefix(first_ptr, tmp_ptr, tok);
    ptr_type new_first_ptr = first_ptr;
    if (flg) {
      ptr = tmp_ptr;
      new_first_ptr = ptr;
      ++ptr;
    }
    if (*new_first_ptr == '"') {
      uint32_t cnt_char =
          this->template lex_char_seq<'"', diag::unfinished_string_literal>(
              ptr, tok);
      if (*ptr == '"') {
        ++ptr;
        token_formulate(tok, first_ptr, ptr,
                        lps::token::details::TokenKind::string_literal);
        return true;
      }

    } else if (*new_first_ptr == 'R') {
      if (lex_raw_string(ptr, tok)) {
        token_formulate(tok, first_ptr, ptr,
                        lps::token::details::TokenKind::string_literal);
        return true;
      }
    }

    return false;
  }

  MethodType type_{MethodType::kNone};
  ptr_type ptr_{nullptr, nullptr, [](const basic::vfile::Visitor<char>*) {
                }};
  uint32_t file_id_{0};
  size_t pos_{0};
};

}  // namespace lps::lexer::details
