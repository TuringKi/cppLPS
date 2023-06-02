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

#include "lexer.h"
#include "src.h"
#include "token.h"

namespace lps::parser {

namespace details {

class Context {};

template <meta::Str TagName>
struct ParseFunctionOutputs {

  explicit ParseFunctionOutputs() = default;

#define SET_MOVE(A, B)                            \
  (A)->work_ = (B).work_;                         \
  (A)->file_id_ = (B).file_id_;                   \
  (A)->last_token_ = (B).last_token_;             \
  (A)->diag_inputs_(std::move((B).diag_inputs_)); \
  (A)->start_ = (B).start_;

#define SET(A, B)                       \
  (A)->work_ = (B).work_;               \
  (A)->file_id_ = (B).file_id_;         \
  (A)->last_token_ = (B).last_token_;   \
  (A)->diag_inputs_ = (B).diag_inputs_; \
  (A)->start_ = (B).start_;

  // template <meta::Str OtherTagName>
  // explicit ParseFunctionOutputs(
  //     const ParseFunctionOutputs<OtherTagName>& other) {
  //   SET(this, other);
  // }

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
#undef SET_MOVE
#undef SET

  void diag_record(diag::DiagInputs<TagName>&& a) {
    diag_inputs_.append(std::move(a));
  }
  template <meta::Str TagNameOther>
  void concat(ParseFunctionOutputs<TagNameOther>&& other, bool opt = true) {
    if (!opt) {
      for (const auto& a : other.diag_inputs_) {
        typename decltype(diag_inputs_)::ele_type b(a);
        this->diag_inputs_.append(b);
      }
      this->work_ = this->work_ && other.work_;
    }

    this->file_id_ = other.file_id_;
    this->start_ = other.start_;
    this->last_token_ = other.last_token_;
  }

  bool work_{false};
  const char* start_{nullptr};
  uint32_t file_id_{0};
  token::Token<TagName> last_token_;
  basic::Vector<4, diag::DiagInputs<TagName>, TagName + "_DiagInputs">
      diag_inputs_;
};

template <meta::Str TagName>
struct ParseFunctionInputs : virtual public basic::mem::TraceTag<TagName> {

  explicit ParseFunctionInputs() = default;
  explicit ParseFunctionInputs(
      bool opt, const char* start = nullptr, uint32_t file_id = 0,
      const token::Token<TagName>& tok = token::Token<TagName>())
      : opt_(opt), file_id_(file_id), start_(start), last_token_(tok) {}

  explicit ParseFunctionInputs(bool opt, const char* start, uint32_t file_id,
                               token::Token<TagName>&& tok)
      : opt_(opt),
        file_id_(file_id),
        start_(start),
        last_token_(std::move(tok)) {}

#define SET(A, B)                     \
  (A)->opt_ = (B).opt_;               \
  (A)->file_id_ = (B).file_id_;       \
  (A)->last_token_ = (B).last_token_; \
  (A)->start_ = (B).start_;

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
  const char* start_{nullptr};
  uint32_t file_id_{0};

  token::Token<TagName> last_token_;
};

template <meta::Str TagName>
class ParseFunction {
 public:
  using type = ParseFunction<TagName>;
  using output_type = ParseFunctionOutputs<TagName>;
  using custom_func_type = std::function<output_type(type*)>;

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
    if (custom_func_) {
      return custom_func_(this);
    }
    ParseFunctionOutputs<TagName> output;
    output.file_id_ = this->file_id();
    output.start_ = this->start();
    output.last_token_ = this->last_token();
    return output;
  };

  [[nodiscard]] bool opt() const { return params_.opt_; }
  [[nodiscard]] const char* start() const { return params_.start_; }
  [[nodiscard]] uint32_t file_id() const { return params_.file_id_; }
  [[nodiscard]] token::Token<TagName> last_token() const {
    return params_.last_token_;
  }

  [[nodiscard]] inline size_t len() const {
    return src::Manager::instance().size(params_.file_id_);
  }

  [[nodiscard]] inline const char* eof() const {
    auto content = src::Manager::instance().ref<TagName + "_file_contents">(
        params_.file_id_);
    return content.data() + len();
  }
  [[nodiscard]] inline bool valid() const { return params_.start_ < eof(); }

  void opt(bool opt) { params_.opt_ = opt; }
  void start(const char* start) { params_.start_ = start; }
  void file_id(uint32_t file_id) { params_.file_id_ = file_id; }
  void last_token(const token::Token<TagName>& tok) {
    params_.last_token_ = tok;
  }
  template <meta::Str TagNameOther>
  void last_token(const token::Token<TagNameOther>& tok) {
    params_.last_token_ = tok;
  }

  static type create_single_token_check(token::tok::TokenKind token_kind,
                                        diag::DiagKind diag_kind) {
    type::custom_func_type z([&token_kind, &diag_kind](type* func) {
      ParseFunctionOutputs<TagName> output;
      output.file_id_ = func->file_id();
      output.start_ = func->start();
      output.last_token_ = func->last_token();

      if (!func->valid()) {
        return output;
      }
      token::Token<TagName + "_first_token"> tok;
      lexer::Lexer lexer(func->file_id(), func->start());
      lexer.lex(tok);

      if (tok.kind() != token_kind) {
        if (!func->opt()) {
          output.diag_record(
              diag::record<TagName>(tok, diag_kind, output.last_token_));
        }
        output.work_ = false;
        return output;
      }

      {
        output.last_token_ = tok;
        output.start_ = lexer.cur();
        output.file_id_ = tok.file_id();
        output.work_ = true;
      }

      return output;
    });
    return type(z, false);
  }

 protected:
  ParseFunctionInputs<TagName> params_;
  custom_func_type custom_func_{nullptr};
};

}  // namespace details

class Parser {

 public:
  explicit Parser() = default;
  void parse(uint32_t file_id);
};

}  // namespace lps::parser
