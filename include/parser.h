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
#include <utility>
#include "basic/bitset.h"
#include "basic/exception.h"
#include "basic/mem.h"
#include "basic/meta.h"
#include "lexer.h"
#include "src.h"
#include "token.h"

namespace lps::parser {

namespace details {

enum class ParseFunctionKind : uint16_t {
  kUnknown = 0,
  kExpectedToken = 1,
#define PARSE_FUNC(FUNC) FUNC,
#include "parse_function/kinds.def"
  kNum,
};

namespace kind {

static constexpr std::array<std::pair<ParseFunctionKind, const char*>,
                            static_cast<uint16_t>(ParseFunctionKind::kNum)>
    kLists = {{
#define PARSE_FUNC(X) {ParseFunctionKind::X, #X},
#include "parse_function/kinds.def"
    }};

static constexpr lps::basic::map::Map<ParseFunctionKind, const char*,
                                      static_cast<uint16_t>(
                                          ParseFunctionKind::kNum)>
    kMap{kLists};

}  // namespace kind

inline std::ostream& operator<<(std::ostream& s, ParseFunctionKind kind) {
  s << kind::kMap.at(kind);
  return s;
}

class Tree {
 public:
  static Tree& instance() {
    static Tree tree;
    return tree;
  }

  struct Node {
    using sub_nodes_type = basic::Vector<4, Node*>;
    using token_pts_type = basic::Vector<8, token::archived_type*>;
    ParseFunctionKind kind_{ParseFunctionKind::kUnknown};
    sub_nodes_type sub_nodes_;
    token_pts_type token_pts_;
    token::details::TokenKind expected_token_kind_{
        token::details::TokenKind::unknown};
  };

  Node* append(const Node& node) {
    nodes_.append(node);
    return &nodes_.back();
  }
  [[nodiscard]] size_t size() const { return nodes_.size(); }

 private:
  basic::Vector<4, Node> nodes_;
};

struct ParseFunctionOutputs {
  using diag_input_type = basic::Vector<4, diag::DiagInputs>;
  using token_list_infos_type =
      basic::Vector<16, const token::TokenLists::ele_type*>;
  explicit ParseFunctionOutputs() = default;

#define SET_MOVE(A, B)                             \
  (A)->work_ = (B).work_;                          \
  (A)->last_token_ = (B).last_token_;              \
  (A)->cur_token_ = (B).cur_token_;                \
  (A)->diag_inputs_ = std::move((B).diag_inputs_); \
  (A)->node_ = std::move((B).node_);               \
  (A)->token_list_infos_ = std::move((B).token_list_infos_);

#define SET(A, B)                       \
  (A)->work_ = (B).work_;               \
  (A)->last_token_ = (B).last_token_;   \
  (A)->cur_token_ = (B).cur_token_;     \
  (A)->diag_inputs_ = (B).diag_inputs_; \
  (A)->node_ = (B).node_;               \
  (A)->token_list_infos_ = (B).token_list_infos_;

  ParseFunctionOutputs(const ParseFunctionOutputs& other) {
    SET(this, other);
  }

  ParseFunctionOutputs(const ParseFunctionOutputs&& other) {
    SET_MOVE(this, other);
  }

  ParseFunctionOutputs& operator=(const ParseFunctionOutputs& other) {
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

  void diag_record(diag::DiagInputs&& a) {
    diag_inputs_.append(std::move(a));
  }

  void concat(ParseFunctionOutputs&& other, bool opt = true) {
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
  token::Token last_token_;
  token::Token cur_token_;
  diag_input_type diag_inputs_;
  token_list_infos_type token_list_infos_;
  Tree::Node node_;
};

struct ParseFunctionInputs {

  explicit ParseFunctionInputs() = default;
  explicit ParseFunctionInputs(bool opt, size_t calling_depth,
                               const token::Token& last_tok = token::Token(),
                               const token::Token& cur_tok = token::Token())
      : opt_(opt),
        last_token_(last_tok),
        cur_token_(cur_tok),
        calling_depth_(calling_depth) {}

  explicit ParseFunctionInputs(bool opt, size_t calling_depth,
                               token::Token&& last_tok, token::Token&& cur_tok)
      : opt_(opt),
        last_token_(std::move(last_tok)),
        cur_token_(std::move(cur_tok)),
        calling_depth_(calling_depth) {}

#define SET(A, B)                           \
  (A)->opt_ = (B).opt_;                     \
  (A)->last_token_ = (B).last_token_;       \
  (A)->calling_depth_ = (B).calling_depth_; \
  (A)->cur_token_ = (B).cur_token_;

  ParseFunctionInputs(const ParseFunctionInputs& other) {
    SET(this, other);
  }

  ParseFunctionInputs(ParseFunctionInputs&& other) {
    SET(this, other);
  }

  ParseFunctionInputs& operator=(const ParseFunctionInputs& other) {
    SET(this, other);
    return *this;
  }

#undef SET

  bool opt_{false};

  token::Token last_token_;
  token::Token cur_token_;
  size_t calling_depth_{0};
};

template <size_t NumElements = 1>
class ParseFunction {
 public:
  using type = ParseFunction<NumElements>;
  using output_type = ParseFunctionOutputs;
  using custom_func_type = std::function<output_type(type*)>;
  using bitset_type = basic::Bitset<NumElements>;
  static constexpr size_t kNumberOfElements = NumElements;
  const ParseFunctionKind kKind = ParseFunctionKind::kExpectedToken;

  template <typename... Params>
  explicit ParseFunction(const char* kName, Params... params)
      : kName_(kName), params_(params...) {}
  template <typename... Params>
  explicit ParseFunction(const char* kName, custom_func_type func,
                         token::details::TokenKind expected_token_kind,
                         Params... params)
      : kName_(kName),
        custom_func_(func),
        params_(params...),
        expected_token_kind_(expected_token_kind) {}
  explicit ParseFunction(const char* kName, ParseFunctionInputs param,
                         token::details::TokenKind expected_token_kind,
                         custom_func_type func)
      : kName_(kName),
        params_(std::move(param)),
        custom_func_(func),
        expected_token_kind_(expected_token_kind) {}
  explicit ParseFunction(const char* kName, const ParseFunctionInputs& param)
      : kName_(kName), params_(param) {}
  explicit ParseFunction(const char* kName, ParseFunctionInputs&& param)
      : kName_(kName), params_(std::move(param)) {}

  virtual output_type operator()() {
    if (custom_func_) {
      return custom_func_(this);
    }
    ParseFunctionOutputs output;
    output.last_token_ = this->last_token();
    output.cur_token_ = this->cur_token();
    return output;
  };
  [[nodiscard]] token::details::TokenKind expected_token_kind() const {
    return expected_token_kind_;
  }
  [[nodiscard]] bool opt() const { return params_.opt_; }
  [[nodiscard]] token::Token last_token() const { return params_.last_token_; }
  [[nodiscard]] token::Token cur_token() const { return params_.cur_token_; }

  [[nodiscard]] inline bool valid() const { return valid(params_.cur_token_); }

  [[nodiscard]] inline bool valid(const token::Token& tok) const {
    return tok.kind() != token::details::TokenKind::eof;
  }

  void opt(bool opt) { params_.opt_ = opt; }
  void last_token(const token::Token& tok) { params_.last_token_ = tok; }

  void cur_token(const token::Token& tok) { params_.cur_token_ = tok; }

  [[nodiscard]] size_t calling_depth() const { return params_.calling_depth_; }
  void calling_depth(size_t depth) { params_.calling_depth_ = depth; }
  [[nodiscard]] bitset_type executed_mask() const { return executed_mask_; }
  void executed_mask(const bitset_type& mask) { executed_mask_ = mask; }
  virtual void reset() { executed_mask_.reset(); }
  bool ok_to_try() {
    auto status = executed_mask_.all();
    return !status;
  }

  static type create_single_token_check(bool opt, size_t calling_depth,
                                        token::details::TokenKind token_kind,
                                        diag::DiagKind diag_kind) {
    typename type::custom_func_type z([token_kind, diag_kind](type* func) {
      func->executed_mask_.set();
      ParseFunctionOutputs output;
      output.last_token_ = func->last_token();
      output.cur_token_ = func->cur_token();
      if (!func->valid()) {
        return output;
      }
      token::Token next_tok;
      bool flg_use_lexer = true;
      if (token::TokenLists::instance().has(output.cur_token_)) {
        const auto* tok_ptr =
            token::TokenLists::instance().at(output.cur_token_).next();
        if (tok_ptr) {
          flg_use_lexer = false;
          next_tok = *tok_ptr;
        }
      }
      if (flg_use_lexer) {
        lexer::Lexer lexer(output.cur_token_.next_visitor().second,
                           output.cur_token_.next_visitor().first);
        lexer.lex(next_tok);
        if (next_tok.kind() == token::details::TokenKind::eof) {
          next_tok.file_id(output.cur_token_.file_id());
        }

        lps_assert("create_single_token_check",
                   next_tok.kind() != token::details::TokenKind::unknown);

        token::TokenLists::instance().append(
            next_tok, token::TokenLists::Info::create(output.cur_token_));
      }

      lps_assert("create_single_token_check",
                 output.cur_token_.ptr() != next_tok.ptr());

      if (output.cur_token_.kind() != token_kind) {
        if (!func->opt()) {
          output.diag_record(
              diag::record(output.cur_token_, diag_kind, {output.last_token_}));
        }
        output.work_ = false;
        return output;
      }

      {
        output.last_token_ = output.cur_token_;
        output.work_ = true;
        output.cur_token_ = next_tok;
        if (next_tok.kind() != token::details::TokenKind::unknown) {
          output.token_list_infos_.append(
              &(token::TokenLists::instance().at(next_tok)));
        }
      }

      return output;
    });
    return type(token::details::kMap.at(token_kind), z, token_kind, opt,
                calling_depth);
  }

 protected:
  ParseFunctionInputs params_;
  custom_func_type custom_func_{nullptr};
  bitset_type executed_mask_;
  const char* kName_;

 private:
  token::details::TokenKind expected_token_kind_{
      token::details::TokenKind::unknown};
};

}  // namespace details

class Parser {

 public:
  explicit Parser() = default;
  void parse(uint32_t file_id);
};

}  // namespace lps::parser
