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

namespace lps::parser {

namespace details {

class Context {};

template <meta::Str TagName>
struct ParseFunctionOutputs {

  explicit ParseFunctionOutputs() = default;

#define SET_MOVE(A, B)                            \
  (A)->work_ = (B).work_;                         \
  (A)->file_id_ = (B).file_id_;                   \
  (A)->diag_inputs_(std::move((B).diag_inputs_)); \
  (A)->start_ = (B).start_;

#define SET(A, B)                       \
  (A)->work_ = (B).work_;               \
  (A)->file_id_ = (B).file_id_;         \
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
  void concat_diag_inputs_and_remove(
      ParseFunctionOutputs<TagNameOther>& other) {
    for (const auto& a : other.diag_inputs_) {
      typename decltype(diag_inputs_)::ele_type b(a);
      this->diag_inputs_.append(b);
    }
    other.diag_inputs_.release();
  }

  bool work_{false};
  const char* start_{nullptr};
  uint32_t file_id_{0};
  basic::Vector<4, diag::DiagInputs<TagName>, TagName + "_DiagInputs">
      diag_inputs_;
};

template <meta::Str TagName>
struct ParseFunctionInputs {

  explicit ParseFunctionInputs() = default;

#define SET(A, B)               \
  (A)->opt_ = (B).opt_;         \
  (A)->file_id_ = (B).file_id_; \
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
};

template <meta::Str TagName>
class ParseFunction {
 public:
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

  ParseFunctionOutputs<TagName> operator()() {}

  [[nodiscard]] bool opt() const { return params_.opt_; }
  [[nodiscard]] const char* start() const { return params_.start_; }
  [[nodiscard]] uint32_t file_id() const { return params_.file_id_; }

 protected:
  ParseFunctionInputs<TagName> params_;
};

}  // namespace details

class Parser {

 public:
  explicit Parser() = default;
  void parse(uint32_t file_id);

 private:
  void translation_unit(uint32_t file_id);
  void declaration_seq();

  // declaration
  void block_declaration() {}
  void nodeclspec_function_declaration() {}
  void function_definition() {}
  void template_declaration() {}
  void deduction_guide() {}
  void explicit_instantiation() {}
  void explicit_specialization() {}
  void export_declaration() {}
  void linkage_specification() {}
  void namespace_definition() {}
  void empty_declaration() {}
  void attribute_declaration() {}
  void module_import_declaration() {}

  void module_partition() {}
  void attribute_specifier_seq() {}
};

}  // namespace lps::parser
