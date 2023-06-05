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
#include <limits>
#include "basic/bitset.h"
#include "basic/mem.h"
#include "lexer.h"
#include "src.h"
#include "token.h"

namespace lps::parser {

namespace details {

template <meta::Str TagName>
struct ParseFunctionOutputs {
  using diag_input_type =
      basic::Vector<4, diag::DiagInputs<TagName>, TagName + "_DiagInputs">;
  using token_list_infos_type =
      basic::Vector<16, const token::TokenLists::ele_type*,
                    TagName + "_TokenLists::Info">;
  explicit ParseFunctionOutputs() = default;

#define SET_MOVE(A, B)                             \
  (A)->work_ = (B).work_;                          \
  (A)->last_token_ = (B).last_token_;              \
  (A)->cur_token_ = (B).cur_token_;                \
  (A)->diag_inputs_ = std::move((B).diag_inputs_); \
  (A)->token_list_infos_ = std::move((B).token_list_infos_);

#define SET(A, B)                       \
  (A)->work_ = (B).work_;               \
  (A)->last_token_ = (B).last_token_;   \
  (A)->cur_token_ = (B).cur_token_;     \
  (A)->diag_inputs_ = (B).diag_inputs_; \
  (A)->token_list_infos_ = (B).token_list_infos_;

  template <meta::Str OtherTagName>
  explicit ParseFunctionOutputs(
      const ParseFunctionOutputs<OtherTagName>& other) {
    SET(this, other);
  }

  ParseFunctionOutputs(const ParseFunctionOutputs& other) {
    SET(this, other);
  }

  template <meta::Str TagNameOther>
  explicit ParseFunctionOutputs(ParseFunctionOutputs<TagNameOther>&& other) {
    SET_MOVE(this, other);
  }

  template <meta::Str TagNameOther>
  explicit ParseFunctionOutputs(
      const ParseFunctionOutputs<TagNameOther>&& other) {
    SET_MOVE(this, other);
  }

  template <meta::Str TagNameOther>
  ParseFunctionOutputs& operator=(
      const ParseFunctionOutputs<TagNameOther>& other) {
    SET(this, other);
    return *this;
  }

  ParseFunctionOutputs& operator=(ParseFunctionOutputs&& other) {
    SET_MOVE(this, other);
    return *this;
  }

#undef SET_MOVE
#undef SET

  [[nodiscard]] size_t len() const {
    return token_list_infos_.size();
  }

  void diag_record(diag::DiagInputs<TagName>&& a) {
    diag_inputs_.append(std::move(a));
  }
  template <meta::Str TagNameOther>
  void concat(ParseFunctionOutputs<TagNameOther>&& other, bool opt = true) {
    if (!opt) {
      for (auto& a : other.diag_inputs_) {
        typename decltype(diag_inputs_)::ele_type b(std::move(a));
        this->diag_inputs_.append(std::move(b));
      }
      this->work_ = other.work_;
    }
    if (other.work_) {
      this->last_token_ = other.last_token_;
      this->cur_token_ = other.cur_token_;
      this->work_ = other.work_;
      for (auto& a : other.token_list_infos_) {
        this->token_list_infos_.append(a);
      }
    }
  }

  bool work_{false};
  token::Token<TagName> last_token_;
  token::Token<TagName> cur_token_;
  diag_input_type diag_inputs_;
  token_list_infos_type token_list_infos_;
};

template <meta::Str TagName>
struct ParseFunctionInputs : virtual public basic::mem::TraceTag<TagName> {

  explicit ParseFunctionInputs() = default;
  explicit ParseFunctionInputs(
      bool opt, const token::Token<TagName>& last_tok = token::Token<TagName>(),
      const token::Token<TagName>& cur_tok = token::Token<TagName>())
      : opt_(opt), last_token_(last_tok), cur_token_(cur_tok) {}

  explicit ParseFunctionInputs(bool opt, token::Token<TagName>&& last_tok,
                               token::Token<TagName>&& cur_tok)
      : opt_(opt),
        last_token_(std::move(last_tok)),
        cur_token_(std::move(cur_tok)) {}

#define SET(A, B)                     \
  (A)->opt_ = (B).opt_;               \
  (A)->last_token_ = (B).last_token_; \
  (A)->cur_token_ = (B).cur_token_;

  template <meta::Str OtherTagName>
  explicit ParseFunctionInputs(const ParseFunctionInputs<OtherTagName>& other) {
    SET(this, other);
  }

  template <meta::Str OtherTagName>
  explicit ParseFunctionInputs(ParseFunctionInputs<OtherTagName>&& other) {
    SET(this, other);
  }

  template <meta::Str OtherTagName>
  ParseFunctionInputs& operator=(
      const ParseFunctionInputs<OtherTagName>& other) {
    SET(this, other);
    return *this;
  }

#undef SET

  bool opt_{false};

  token::Token<TagName> last_token_;
  token::Token<TagName> cur_token_;
};

template <meta::Str TagName, size_t NumElements = 1>
class ParseFunction : virtual public basic::mem::TraceTag<TagName> {
 public:
  using type = ParseFunction<TagName, NumElements>;
  using output_type = ParseFunctionOutputs<TagName>;
  using custom_func_type = std::function<output_type(type*)>;
  using bitset_type = basic::Bitset<NumElements>;
  static constexpr size_t kNumberOfElements = NumElements;

  template <typename... Params>
  explicit ParseFunction(Params... params) : params_(params...) {}
  template <typename... Params>
  explicit ParseFunction(custom_func_type func, Params... params)
      : custom_func_(func), params_(params...) {}
  explicit ParseFunction(const ParseFunctionInputs<TagName>& param,
                         custom_func_type func)
      : params_(param), custom_func_(func) {}
  explicit ParseFunction(const ParseFunctionInputs<TagName>& param)
      : params_(param) {}
  explicit ParseFunction(ParseFunctionInputs<TagName>&& param)
      : params_(std::move(param)) {}

  template <meta::Str TagNameOther>
  explicit ParseFunction(const ParseFunctionInputs<TagNameOther>& param)
      : params_(param) {}
  template <meta::Str TagNameOther>
  explicit ParseFunction(ParseFunctionInputs<TagNameOther>&& param)
      : params_(std::move(param)) {}

  virtual output_type operator()() {
    executed_mask_.set();
    if (custom_func_) {
      return custom_func_(this);
    }
    ParseFunctionOutputs<TagName> output;
    output.last_token_ = this->last_token();
    output.cur_token_ = this->cur_token();
    return output;
  };

  [[nodiscard]] bool opt() const { return params_.opt_; }
  [[nodiscard]] token::Token<TagName> last_token() const {
    return params_.last_token_;
  }
  [[nodiscard]] token::Token<TagName> cur_token() const {
    return params_.cur_token_;
  }

  [[nodiscard]] inline size_t len() const {
    return src::Manager::instance().size(params_.cur_token_.file_id());
  }

  [[nodiscard]] inline const char* eof() const {
    auto content = src::Manager::instance().ref<TagName + "_file_contents">(
        params_.cur_token_.file_id());
    return content.data() + len();
  }
  [[nodiscard]] inline bool valid() const {
    return params_.cur_token_.ptr() < eof() &&
           params_.cur_token_.kind() != token::tok::TokenKind::unknown;
  }

  void opt(bool opt) { params_.opt_ = opt; }
  void last_token(const token::Token<TagName>& tok) {
    params_.last_token_ = tok;
  }
  template <meta::Str TagNameOther>
  void last_token(const token::Token<TagNameOther>& tok) {
    params_.last_token_ = tok;
  }
  void cur_token(const token::Token<TagName>& tok) { params_.cur_token_ = tok; }
  template <meta::Str TagNameOther>
  void cur_token(const token::Token<TagNameOther>& tok) {
    params_.cur_token_ = tok;
  }
  [[nodiscard]] bitset_type executed_mask() const { return executed_mask_; }
  void executed_mask(const bitset_type& mask) { executed_mask_ = mask; }
  void reset() { executed_mask_.reset(); }
  bool ok_to_try() { return !executed_mask_.all(); }

  static type create_single_token_check(bool opt,
                                        token::tok::TokenKind token_kind,
                                        diag::DiagKind diag_kind) {
    typename type::custom_func_type z([token_kind, diag_kind](type* func) {
      ParseFunctionOutputs<TagName> output;
      output.last_token_ = func->last_token();
      output.cur_token_ = func->cur_token();
      if (!func->valid()) {
        return output;
      }
      token::Token<TagName + "_second_token"> next_tok;
      lexer::Lexer lexer(output.cur_token_.file_id(), output.cur_token_.ptr());
      lexer.lex(next_tok);
      lexer.lex(next_tok);

      if (next_tok.kind() != token::tok::TokenKind::unknown) {
        token::TokenLists::instance().append(next_tok);
      }

      if (output.cur_token_.kind() != token_kind) {
        if (!func->opt()) {
          output.diag_record(diag::record<TagName>(output.cur_token_, diag_kind,
                                                   output.last_token_));
        }
        output.work_ = false;
        return output;
      }

      {
        output.last_token_ = output.cur_token_;
        output.work_ = true;
        output.cur_token_ = next_tok;
        if (next_tok.kind() != token::tok::TokenKind::unknown) {
          output.token_list_infos_.append(&(token::TokenLists::instance().at(
              token::TokenLists::Info::create(next_tok))));
        }
      }

      return output;
    });
    return type(z, opt);
  }

 protected:
  ParseFunctionInputs<TagName> params_;
  custom_func_type custom_func_{nullptr};
  basic::Bitset<NumElements> executed_mask_;
};

}  // namespace details

class Parser {

 public:
  explicit Parser() = default;
  void parse(uint32_t file_id);
};

}  // namespace lps::parser
