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
#include "basic/exception.h"
#include "basic/str.h"
#include "basic/vec.h"
#include "diag.h"
#include "token.h"
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

template <meta::Str TagName>
class Base : virtual public lps::basic::mem::TraceTag<TagName> {

 public:
  using type = Base<TagName>;
  using const_ptr_type = const type*;
  using ptr_type = basic::FileVisitor;
  using lex_func_type1 = std::function<token::tok::TokenKind(char, ptr_type&)>;
  using lex_func_type2 =
      std::function<bool(char, ptr_type&, lps::token::Token<TagName>&)>;
  inline void lex(lps::token::Token<TagName>& tok) {
    try {
      lex_impl(tok);
    } catch (basic::vfile::Eof& except_eof) {
      tok.kind(token::tok::TokenKind::eof);
      return;
    }
  }
  inline virtual void lex_impl(lps::token::Token<TagName>& tok) = 0;
  [[nodiscard]] MethodType method() const { return type_; }
  explicit Base(uint32_t start_file_id, const char* ptr, const char* end,
                MethodType m)
      : file_id_(start_file_id),
        ptr_(ptr, end, []() { throw basic::vfile::Eof(); }),
        type_(m) {}
  [[nodiscard]] inline size_t pos() const { return pos_; }

 protected:
  static inline uint32_t escaped_newline_scanning(ptr_type ptr) {
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
          // diag::doing(tok, diag::DiagKind::back_slash_new_space);
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

  static inline ptr_type consume_char(ptr_type ptr, uint32_t sz) {
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

  [[nodiscard]] inline ptr_type cur() const { return ptr_ + pos_; }
  inline void inc(size_t n) { pos_ += n; }

  inline void diag(ptr_type ptr, diag::DiagKind kind) {
    diag::DiagInputs<TagName> diag_input;
    diag_input.kind_ = kind;
    lps::token::Token<TagName> tok_error;
    this->token_formulate(tok_error, ptr, token::tok::TokenKind::unknown);
    tok_error.data(this->cur());
    diag_input.main_token_ = tok_error;
    diag::doing<TagName>(diag_input.main_token_, diag_input.kind_,
                         diag_input.context_tokens_);
  }

  inline void token_formulate(lps::token::Token<TagName>& tok, ptr_type end,
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

  template <auto Func>
  static inline bool lex_char(ptr_type& ptr) {
    if (Func(*ptr)) {
      ++ptr;
      return true;
    }
    return false;
  }

  inline bool lex_something(char c, ptr_type& ptr,
                            lps::token::Token<TagName>& tok,
                            const lex_func_type1& func) {

    ptr_type tmp_ptr = ptr;
    auto kind = func(c, tmp_ptr);
    if (kind != token::tok::TokenKind::unknown) {
      ptr = tmp_ptr;
      this->token_formulate(tok, ptr, kind);
      return true;
    }
    return false;
  }

  inline bool lex_something_parallel(char c, ptr_type& ptr,
                                     lps::token::Token<TagName>& tok,
                                     const std::vector<lex_func_type2>& funcs) {
    for (const auto& f : funcs) {
      ptr_type tmp_ptr = ptr;
      lps::token::Token<TagName> tmp_tok;
      if (f(c, tmp_ptr, tmp_tok)) {
        ptr = tmp_ptr;
        tok = tmp_tok;
        return true;
      }
    }
    return false;
  }

  template <token::tok::TokenKind OutKind>
  inline bool lex_something_recursive(char c, ptr_type& ptr,
                                      lps::token::Token<TagName>& tok,
                                      const lex_func_type2& first_lex_func) {
    return [this](char c, ptr_type& ptr, lps::token::Token<TagName>& tok,
                  const lex_func_type2& first_lex_func) {
      auto lex = [this, &first_lex_func](char c, ptr_type& ptr,
                                         lps::token::Token<TagName>& tok,
                                         const auto& func) -> bool {
        if (first_lex_func(c, ptr, tok)) {
          ptr_type tmp_ptr = ptr;
          tmp_ptr.horzws_skipping();  // skip space, `\t` etc.
          char tmp_c = *ptr;
          tmp_ptr++;
          lps::token::Token<TagName> tmp_tok;
          if (func(tmp_c, tmp_ptr, tmp_tok, func)) {
            ptr = tmp_ptr;
            tok = tmp_tok;
          }
          tok.kind(OutKind);
          return true;
        }
        return false;
      };
      return lex(c, ptr, tok, lex);
    }(c, ptr, tok, first_lex_func);
  }

  // preprocessing-operator:
  // 	`#`
  // 	`##`
  // 	`%:`
  // 	`%:%:`
  inline bool lex_preprocessing_operator(char c, ptr_type& ptr,
                                         lps::token::Token<TagName>& tok) {
    return lex_something(
        c, ptr, tok, [this](char c, ptr_type& ptr) -> token::tok::TokenKind {
          token::tok::TokenKind kind = token::tok::TokenKind::unknown;
          char next_c = *ptr;
          switch (c) {
            case '%': {
              if (next_c == ':') {  // %:
                kind = token::tok::TokenKind::percentcolon;
                ++ptr;
                if (*ptr == '%' && *(ptr + 1) == ':') {  // %:%:
                  kind = token::tok::TokenKind::percentcolonpercentcolon;
                  ptr += 2;
                }
              }
              break;
            }
            case '#': {
              kind = token::tok::TokenKind::hash;
              if (next_c == '#') {
                kind = token::tok::TokenKind::hashhash;
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

  inline bool lex_operator_or_punctuator(char c, ptr_type& ptr,
                                         lps::token::Token<TagName>& tok) {
    return lex_something(
        c, ptr, tok, [this](char c, ptr_type& ptr) -> token::tok::TokenKind {
          token::tok::TokenKind kind = token::tok::TokenKind::unknown;
          char next_c = *ptr;
          switch (c) {
            case '{':
              kind = token::tok::TokenKind::l_brace;
              break;
            case '}':
              kind = token::tok::TokenKind::r_brace;
              break;
            case '[':
              kind = token::tok::TokenKind::l_square;
              break;
            case ']':
              kind = token::tok::TokenKind::r_square;
              break;
            case '(':
              kind = token::tok::TokenKind::l_paren;
              break;
            case ')':
              kind = token::tok::TokenKind::r_paren;
              break;
            case '?':
              kind = token::tok::TokenKind::question;
              break;
            case ';':
              kind = token::tok::TokenKind::semi;
              break;
            case ':': {
              kind = token::tok::TokenKind::colon;
              if (next_c == '>') {
                kind = token::tok::TokenKind::colongreater;
                ++ptr;
              } else if (next_c == ':') {
                kind = token::tok::TokenKind::coloncolon;
                ++ptr;
              }
              break;
            }
            case '.': {
              kind = token::tok::TokenKind::period;
              if (next_c == '.') {
                if (*(ptr + 1) == '.') {
                  kind = token::tok::TokenKind::ellipsis;
                  ptr += 2;
                  break;
                }
              } else if (next_c == '*') {
                kind = token::tok::TokenKind::periodstar;
                ++ptr;
              }
              break;
            }
            case '~':
              kind = token::tok::TokenKind::tilde;
              break;
            case '!': {
              kind = token::tok::TokenKind::exclaim;
              if (next_c == '=') {
                kind = token::tok::TokenKind::exclaimequal;
                ++ptr;
              }
              break;
            }
              kind = token::tok::TokenKind::exclaim;
              break;
            case '+': {
              kind = token::tok::TokenKind::plus;
              if (next_c == '=') {
                kind = token::tok::TokenKind::plusequal;
                ++ptr;
              } else if (next_c == '+') {
                kind = token::tok::TokenKind::plusplus;
                ++ptr;
              }
              break;
            }

            case '-': {
              kind = token::tok::TokenKind::minus;
              if (next_c == '>') {
                kind = token::tok::TokenKind::arrow;
                ++ptr;
                if (*ptr == '*') {
                  kind = token::tok::TokenKind::arrowstar;
                  ++ptr;
                }
              } else if (next_c == '-') {
                kind = token::tok::TokenKind::minusminus;
                ++ptr;
              } else if (next_c == '=') {
                kind = token::tok::TokenKind::minusequal;
                ++ptr;
              }
              break;
            }

            case '*': {
              kind = token::tok::TokenKind::star;
              if (next_c == '=') {
                kind = token::tok::TokenKind::starequal;
                ++ptr;
              }
              break;
            }
            case '/': {
              kind = token::tok::TokenKind::slash;
              if (next_c == '=') {
                kind = token::tok::TokenKind::slashequal;
                ++ptr;
              }
              break;
            }
            case '%': {
              kind = token::tok::TokenKind::percent;
              if (next_c == '=') {
                kind = token::tok::TokenKind::percentequal;
                ++ptr;
              } else if (next_c == '>') {
                kind = token::tok::TokenKind::percentgreater;
                ++ptr;
              }
              break;
            }
            case '&': {
              kind = token::tok::TokenKind::amp;
              if (next_c == '=') {
                kind = token::tok::TokenKind::ampequal;
                ++ptr;
              } else if (next_c == '&') {
                kind = token::tok::TokenKind::ampamp;
                ++ptr;
              }
              break;
            }
            case '^': {
              kind = token::tok::TokenKind::caret;
              if (next_c == '=') {
                kind = token::tok::TokenKind::caretequal;
                ++ptr;
              } else if (next_c == '^') {
                kind = token::tok::TokenKind::caretcaret;
                ++ptr;
              }
              break;
            }
            case '|': {
              kind = token::tok::TokenKind::pipe;
              if (next_c == '=') {
                kind = token::tok::TokenKind::pipeequal;
                ++ptr;
              } else if (next_c == '|') {
                kind = token::tok::TokenKind::pipepipe;
                ++ptr;
              }
              break;
            }
            case '=': {
              kind = token::tok::TokenKind::equal;
              if (next_c == '=') {
                kind = token::tok::TokenKind::equalequal;
                ++ptr;
              }
              break;
            }
            case '<': {
              kind = token::tok::TokenKind::less;
              if (next_c == '%') {
                kind = token::tok::TokenKind::lesspercent;
                ++ptr;
              } else if (next_c == ':') {
                kind = token::tok::TokenKind::lesscolon;
                ++ptr;
              } else if (next_c == '=') {  // <=
                kind = token::tok::TokenKind::lessequal;
                ++ptr;
                if (*ptr == '>') {  // <=>
                  kind = token::tok::TokenKind::spaceship;
                  ++ptr;
                  break;
                }
              } else if (next_c == '<') {  // <<
                kind = token::tok::TokenKind::lessless;
                ++ptr;
                if (*ptr == '=') {  // <<=
                  kind = token::tok::TokenKind::lesslessequal;
                  ++ptr;
                  break;
                }
              }
              break;
            }
            case '>': {
              kind = token::tok::TokenKind::greater;
              if (next_c == '>') {  // >>
                kind = token::tok::TokenKind::greatergreater;
                ++ptr;
                if (*ptr == '=') {  // >>=
                  kind = token::tok::TokenKind::greatergreaterequal;
                  ++ptr;
                  break;
                }
              } else if (next_c == '=') {  //>=
                kind = token::tok::TokenKind::greaterequal;
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
      char c, ptr_type& ptr, lps::token::Token<TagName>& tok) {
    if (!lex_operator_or_punctuator(c, ptr, tok)) {
      if (this->lex_identifier(ptr, tok)) {
        //	one of following key_words:
        //    and or xor not bitand bitor compl and_eq or_eq xor_eq not_eq
        if (tok.kind() == token::tok::TokenKind::kw_and ||
            tok.kind() == token::tok::TokenKind::kw_or ||
            tok.kind() == token::tok::TokenKind::kw_xor ||
            tok.kind() == token::tok::TokenKind::kw_not ||
            tok.kind() == token::tok::TokenKind::kw_bitand ||
            tok.kind() == token::tok::TokenKind::kw_bitor ||
            tok.kind() == token::tok::TokenKind::kw_compl ||
            tok.kind() == token::tok::TokenKind::kw_and_eq ||
            tok.kind() == token::tok::TokenKind::kw_or_eq ||
            tok.kind() == token::tok::TokenKind::kw_xor_eq ||
            tok.kind() == token::tok::TokenKind::kw_not_eq) {
          return true;
        }
      }
    } else {
      return true;
    }
    return false;
  }

  inline bool lex_preprocessing_op_or_punc(char c, ptr_type& ptr,
                                           lps::token::Token<TagName>& tok) {
    std::vector<lex_func_type2> funcs = {
        [this](char c, ptr_type& ptr, lps::token::Token<TagName>& tok) -> bool {
          return lex_preprocessing_operator(c, ptr, tok);
        },
        [this](char c, ptr_type& ptr, lps::token::Token<TagName>& tok) -> bool {
          return lex_operator_or_punctuator_or_keyword_operator(c, ptr, tok);
        },
    };
    return lex_something_parallel(c, ptr, tok, funcs);
  }

  template <char StartChar, diag::DiagKind DiagKind>
  inline uint32_t lex_char_seq(ptr_type& ptr, lps::token::Token<TagName>& tok) {
    uint32_t cnt_char = 0;
    while (*ptr != StartChar) {
      if (*ptr == '\\') {
        advance(ptr);
      }
      if (*ptr == '\n' || *ptr == '\r' || ptr.eof()) {
        this->diag(ptr, DiagKind);
        return cnt_char;
      }
      advance(ptr);
      cnt_char++;
    }
    return cnt_char;
  }

  inline bool lex_identifier(ptr_type& ptr, lps::token::Token<TagName>& tok) {
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

    this->token_formulate(tok, ptr, lps::token::tok::TokenKind::raw_identifier);
    auto ident_str = tok.template str<meta::S("ident_str")>();
    if (lps::token::tok::IdentInfo::instance().kw_has(ident_str)) {
      tok.kind(lps::token::tok::IdentInfo::instance().kw_at(ident_str));
    } else {
      tok.kind(token::tok::TokenKind::identifier);

      // a valid identifier, we must check if it was defined by `TU`.
      if (tu::TU::instance().defined(tok)) {}
    }

    return true;
  }

  // header-name:
  // 	`<`, h_char_sequence, `>`
  // 	`"`, q_char_sequence, `"`
  inline bool lex_header_name(char c, ptr_type& ptr,
                              lps::token::Token<TagName>& tok) {
    bool flg = false;
    if (c == '<') {
      uint32_t cnt_char =
          this->template lex_char_seq<'<',
                                      diag::unfinished_header_name_expected_gt>(
              ptr, tok);
      if (cnt_char > 0) {
        flg = true;
      }
    } else if (c == '"') {
      uint32_t cnt_char = this->template lex_char_seq<
          '"', diag::unfinished_header_name_expected_quotation>(ptr, tok);
      if (cnt_char > 0) {
        flg = true;
      }
    }
    if (flg) {
      this->token_formulate(tok, ptr + 1,
                            lps::token::tok::TokenKind::header_name);
      return true;
    }
    return false;
  }

  inline bool lex_encoding_prefix(char c, ptr_type& ptr,
                                  lps::token::Token<TagName>& /*tok*/) {
    if (c == 'u') {
      if (*ptr == '8') {
        ++ptr;
      }
      return true;
    }
    return c == 'U' || c == 'L';
  }

  inline bool lex_character_literal(char c_, ptr_type& ptr,
                                    lps::token::Token<TagName>& tok) {

    ptr_type start = ptr - 1;
    ptr_type tmp_ptr = ptr;
    bool flg = lex_encoding_prefix(c_, tmp_ptr, tok);
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
      if (c_ == '\'') {
        flg = true;
      }
    }
    if (flg) {
      uint32_t cnt_char =
          this->template lex_char_seq<'\'', diag::unfinished_char_literal>(ptr,
                                                                           tok);

      if (cnt_char == 0) {
        this->diag(ptr, diag::empty_char_literal);
        return false;
      }
      this->token_formulate(tok, ptr + 1, token::tok::TokenKind::char_literal);
    }

    return flg;
  }
  inline bool lex_integer_suffix(ptr_type& ptr) {

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

  inline bool lex_octal_digit(ptr_type& ptr) {
    if (*ptr >= '0' && *ptr <= '7') {
      ++ptr;
      return true;
    }
    return false;
  }

  template <auto FuncDigit, lps::token::tok::TokenKind TokenKind>
  inline bool lex_number_literal(ptr_type& ptr,
                                 lps::token::Token<TagName>& tok) {

    if (!FuncDigit(ptr)) {
      return false;
    }
    ptr_type matched_ptr = ptr;

    if (*ptr == '\'') {
      ++ptr;
    }

    if (lex_number_literal<FuncDigit, TokenKind>(ptr, tok)) {
      matched_ptr = ptr;
    }
    this->token_formulate(tok, matched_ptr, TokenKind);
    ptr = matched_ptr;

    return true;
  }

  inline bool lex_octal_literal(ptr_type& ptr,
                                lps::token::Token<TagName>& tok) {
    return lex_number_literal<[](ptr_type& ptr) {
      if (*ptr >= '0' && *ptr <= '7') {
        ++ptr;
        return true;
      }
      return false;
    },
                              token::tok::TokenKind::octal_literal>(ptr, tok);
  }

  inline bool lex_decimal_literal(ptr_type& ptr,
                                  lps::token::Token<TagName>& tok) {
    return lex_number_literal<[](ptr_type& ptr) {
      if (*ptr >= '0' && *ptr <= '9') {
        ++ptr;
        return true;
      }
      return false;
    },
                              token::tok::TokenKind::decimal_literal>(ptr, tok);
  }

  inline bool lex_hexadecimal_literal(ptr_type& ptr,
                                      lps::token::Token<TagName>& tok) {
    return lex_number_literal<[](ptr_type& ptr) {
      if (*ptr >= '0' && *ptr <= '9' || *ptr >= 'a' && *ptr <= 'f' ||
          *ptr >= 'A' && *ptr <= 'F') {
        ++ptr;
        return true;
      }
      return false;
    },
                              token::tok::TokenKind::hexadecimal_literal>(ptr,
                                                                          tok);
  }

  inline bool lex_binary_literal(ptr_type& ptr,
                                 lps::token::Token<TagName>& tok) {
    return lex_number_literal<[](ptr_type& ptr) {
      if (*ptr == '0' || *ptr == '1') {
        ++ptr;
        return true;
      }
      return false;
    },
                              token::tok::TokenKind::binary_literal>(ptr, tok);
  }

  template <auto FuncDigit, lps::token::tok::TokenKind TokenKind0,
            lps::token::tok::TokenKind TokenKind1>
  inline bool lex_fractional_constant_any(char c, ptr_type& ptr,
                                          lps::token::Token<TagName>& tok) {
    ptr_type p_dot = ptr;
    p_dot.ws_skip(true);
    bool has_first_digit_seq = false;
    if (c != '.') {
      if (lex_number_literal<FuncDigit, TokenKind0>(ptr, tok)) {
        has_first_digit_seq = true;
        p_dot = ptr;
      }
      p_dot++;
      if (*p_dot != '.') {
        return false;
      }
      p_dot++;
    }
    p_dot++;
    ptr = p_dot;
    ptr_type end = ptr;
    if (lex_number_literal<FuncDigit, TokenKind0>(ptr, tok)) {
      end = ptr;
    } else {
      if (!has_first_digit_seq) {
        return false;
      }
    }

    ptr = end;
    this->token_formulate(tok, ptr, TokenKind1);
    return true;
  }

  // hexadecimal-fractional-constant:
  // 	hexadecimal_digit_sequence[opt], `.`, hexadecimal_digit_sequence
  // 	hexadecimal_digit_sequence, `.`
  inline bool lex_hexadecimal_fractional_constant(
      char c, ptr_type& ptr, lps::token::Token<TagName>& tok) {
    return lex_fractional_constant_any<
        [](ptr_type& ptr) {
          if (*ptr >= '0' && *ptr <= '9' || *ptr >= 'a' && *ptr <= 'f' ||
              *ptr >= 'A' && *ptr <= 'F') {
            ++ptr;
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
  inline bool lex_fractional_constant(char c, ptr_type& ptr,
                                      lps::token::Token<TagName>& tok) {
    return lex_fractional_constant_any<
        [](ptr_type& ptr) {
          if (*ptr >= '0' && *ptr <= '9') {
            ++ptr;
            return true;
          }
          return false;
        },
        token::tok::TokenKind::decimal_literal,
        token::tok::TokenKind::fractional_constant>(c, ptr, tok);
  }

  inline bool lex_exponent_part_any(ptr_type& ptr,
                                    lps::token::Token<TagName>& tok,
                                    char part_char0, char part_char1) {
    if (*ptr == part_char0 || *ptr == part_char1) {
      ++ptr;
      if (*ptr == '+' || *ptr == '-') {
        ++ptr;
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
  inline bool lex_exponent_part(ptr_type& ptr,
                                lps::token::Token<TagName>& tok) {
    return lex_exponent_part_any(ptr, tok, 'e', 'E');
  }

  inline bool lex_floating_point_suffix(ptr_type& ptr) {
    if (*ptr == 'L' || *ptr == 'l' || *ptr == 'F' || *ptr == 'f') {
      ++ptr;
      return true;
    }
    return false;
  }

  // decimal-floating-point-literal:
  // 	fractional_constant, exponent_part[opt], floating_point_suffix[opt]
  // 	digit_sequence, exponent_part[opt], floating_point_suffix[opt]
  inline bool lex_decimal_floating_point_literal(
      char c, ptr_type& ptr, lps::token::Token<TagName>& tok) {
    ptr_type tmp_ptr0 = ptr;
    ptr_type tmp_ptr1 = ptr;
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
    if (lex_floating_point_suffix(tmp_ptr1)) {
      has_floating_point_suffix = true;
      ptr = tmp_ptr1;
    }
    if (!has_floating_point_suffix && !has_exponent_part) {
      return is_type0;
    }
    this->token_formulate(
        tok, ptr, token::tok::TokenKind::decimal_floating_point_literal);
    return true;
  }

  // binary-exponent-part:
  // 	`p`, sign[opt], digit_sequence
  // 	`P`, sign[opt], digit_sequence
  inline bool lex_binary_exponent_part(ptr_type& ptr,
                                       lps::token::Token<TagName>& tok) {
    return lex_exponent_part_any(ptr, tok, 'p', 'P');
  }

  // hexadecimal-floating-point-literal:
  // 	hexadecimal_prefix, hexadecimal_fractional_constant, binary_exponent_part, floating_point_suffix[opt]
  // 	hexadecimal_prefix, hexadecimal_digit_sequence, binary_exponent_part, floating_point_suffix[opt]
  inline bool lex_hexadecimal_floating_point_literal(
      char c, ptr_type& ptr, lps::token::Token<TagName>& tok) {
    if (c == '0') {
      if (*ptr == 'x' || *ptr == 'X') {
        ++ptr;
        char z = *ptr;
        ptr_type tmp_ptr = ptr;
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
        if (lex_floating_point_suffix(tmp_ptr)) {
          ptr = tmp_ptr;
        }
        this->token_formulate(
            tok, ptr,
            token::tok::TokenKind::hexadecimal_floating_point_literal);
        return true;
      }
    }
    return false;
  }

  // floating-point-literal:
  // 	decimal_floating_point_literal
  // 	hexadecimal_floating_point_literal
  inline bool lex_floating_point_literal(char c, ptr_type& ptr,
                                         lps::token::Token<TagName>& tok) {

    ptr_type tmp_ptr0 = ptr;
    ptr_type tmp_ptr1 = ptr;
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
  inline bool lex_integer_literal(char c, ptr_type& ptr,
                                  lps::token::Token<TagName>& tok) {
    ptr_type matched_ptr = ptr;
    token::tok::TokenKind kind = token::tok::TokenKind::unknown;
    bool flg = false;
    if (c == '0') {
      if (*ptr == 'b' || *ptr == 'B') {
        ++ptr;
        if (lex_binary_literal(ptr, tok)) {
          matched_ptr = ptr;
          kind = tok.kind();
          flg = true;
        }
      } else if (*ptr == 'x' || *ptr == 'X') {
        ++ptr;
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
        ++ptr;
      }
      if (lex_decimal_literal(ptr, tok)) {
        matched_ptr = ptr;
        kind = tok.kind();
        flg = true;
      }
    }

    if (flg) {
      if (lex_integer_suffix(ptr)) {
        matched_ptr = ptr;
      }
      this->token_formulate(tok, matched_ptr, kind);
    }
    return flg;
  }

  // raw-string:
  // 	`"`, d_char_sequence[opt], `(`, r_char_sequence[opt], `)`, d_char_sequence[opt], `"`
  inline bool lex_raw_string(ptr_type& ptr, lps::token::Token<TagName>& tok) {
    using namespace basic::str::ascii;

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
        this->diag(prefix_end, diag::DiagKind::raw_string_delimiter_too_long);
      } else {
        this->diag(prefix_end, diag::DiagKind::raw_string_invalid_delimiter);
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
      this->diag(ptr, diag::DiagKind::unfinished_raw_string);
      return false;
    }
    if (basic::FileVisitor::strncmp(prefix_start, ptr, prefix_len) == 0) {
      ptr = ptr + prefix_len;
      if (*ptr == '"') {
        this->token_formulate(tok, ptr + 1,
                              lps::token::tok::TokenKind::raw_string);
        return true;
      }
      this->diag(ptr, diag::DiagKind::unfinished_raw_string_expect_quotation);
    } else {
      this->diag(ptr + prefix_len,
                 diag::DiagKind::unfinished_raw_string_because_of_delimiter);
    }

    return false;
  }

  // string-literal:
  // 	encoding_prefix[opt], `"`, s_char_sequence[opt], `"`
  // 	encoding_prefix[opt], `R`, raw_string
  inline bool lex_string_literal(char c, ptr_type& ptr,
                                 lps::token::Token<TagName>& tok) {
    ptr_type tmp_ptr = ptr;
    bool flg = lex_encoding_prefix(c, tmp_ptr, tok);
    if (flg) {
      ptr = tmp_ptr;
      c = *ptr;
      ++ptr;
    }
    if (c == '"') {
      uint32_t cnt_char =
          this->template lex_char_seq<'"', diag::unfinished_string_literal>(
              ptr, tok);
      if (*ptr == '"') {
        this->token_formulate(tok, ptr + 1,
                              lps::token::tok::TokenKind::string_literal);
        return true;
      }

    } else if (c == 'R') {
      if (lex_raw_string(ptr, tok)) {
        this->token_formulate(tok, ptr + 1,
                              lps::token::tok::TokenKind::string_literal);
        return true;
      }
    }

    return false;
  }

  inline typename lps::token::Token<TagName>::tokens_type lex_identifier_list(
      char c, ptr_type& ptr, lps::token::Token<TagName>& tok) {
    typename lps::token::Token<TagName>::tokens_type tokens;
    if (lex_something_recursive<token::tok::TokenKind::identifiers>(
            c, ptr, tok,
            [this, &tokens](char /*c*/, ptr_type& ptr,
                            lps::token::Token<TagName>& tok) -> bool {
              auto tmp_ptr = ptr;
              lps::token::Token<TagName> tmp_tok;
              if (lex_identifier(tmp_ptr, tmp_tok)) {
                if (tmp_tok.kind() == token::tok::TokenKind::identifier) {
                  ptr = tmp_ptr;
                  tok = tmp_tok;
                  tokens.append(tok);
                  return true;
                }
              }
              return false;
            })) {
      return tokens;
    }
    return typename lps::token::Token<TagName>::tokens_type();
  }

  MethodType type_{MethodType::kNone};
  ptr_type ptr_{nullptr, nullptr, []() {
                  throw basic::vfile::Eof();
                }};
  uint32_t file_id_{0};
  size_t pos_{0};
};

}  // namespace lps::lexer::details
