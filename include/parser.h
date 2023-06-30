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

static constexpr lps::basic::map::Map<
    ParseFunctionKind, const char*,
    static_cast<uint16_t>(ParseFunctionKind::kNum), meta::S("token_kind_map")>
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
    using sub_nodes_type =
        basic::Vector<4, Node*,
                      meta::S("parser::details::Tree::Node::sub_nodes_")>;
    using token_pts_type =
        basic::Vector<8, token::archived_type*,
                      meta::S("parser::details::Tree::Node::token_pts_")>;
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
  size_t size() const { return nodes_.size(); }

 private:
  basic::Vector<4, Node, meta::S("parser::details::Tree::nodes_")> nodes_;
};

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
  (A)->node_ = std::move((B).node_);               \
  (A)->token_list_infos_ = std::move((B).token_list_infos_);

#define SET(A, B)                       \
  (A)->work_ = (B).work_;               \
  (A)->last_token_ = (B).last_token_;   \
  (A)->cur_token_ = (B).cur_token_;     \
  (A)->diag_inputs_ = (B).diag_inputs_; \
  (A)->node_ = (B).node_;               \
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

  ParseFunctionOutputs& operator=(const ParseFunctionOutputs& other) {
    SET(this, other);
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
  Tree::Node node_;
};

template <meta::Str TagName>
struct ParseFunctionInputs : virtual public basic::mem::TraceTag<TagName> {

  explicit ParseFunctionInputs() = default;
  explicit ParseFunctionInputs(
      bool opt, size_t calling_depth,
      const token::Token<TagName>& last_tok = token::Token<TagName>(),
      const token::Token<TagName>& cur_tok = token::Token<TagName>())
      : opt_(opt),
        last_token_(last_tok),
        cur_token_(cur_tok),
        calling_depth_(calling_depth) {}

  explicit ParseFunctionInputs(bool opt, size_t calling_depth,
                               token::Token<TagName>&& last_tok,
                               token::Token<TagName>&& cur_tok)
      : opt_(opt),
        last_token_(std::move(last_tok)),
        cur_token_(std::move(cur_tok)),
        calling_depth_(calling_depth) {}

#define SET(A, B)                           \
  (A)->opt_ = (B).opt_;                     \
  (A)->last_token_ = (B).last_token_;       \
  (A)->calling_depth_ = (B).calling_depth_; \
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
  size_t calling_depth_{0};
};

template <meta::Str TagName, size_t NumElements = 1>
class ParseFunction : virtual public basic::mem::TraceTag<TagName> {
 public:
  using type = ParseFunction<TagName, NumElements>;
  using output_type = ParseFunctionOutputs<TagName>;
  using custom_func_type = std::function<output_type(type*)>;
  using bitset_type = basic::Bitset<TagName, NumElements>;
  static constexpr size_t kNumberOfElements = NumElements;
  const ParseFunctionKind kKind = ParseFunctionKind::kExpectedToken;

  template <typename... Params>
  explicit ParseFunction(Params... params) : params_(params...) {}
  template <typename... Params>
  explicit ParseFunction(custom_func_type func,
                         token::details::TokenKind expected_token_kind,
                         Params... params)
      : custom_func_(func),
        params_(params...),
        expected_token_kind_(expected_token_kind) {}
  explicit ParseFunction(const ParseFunctionInputs<TagName>& param,
                         token::details::TokenKind expected_token_kind,
                         custom_func_type func)
      : params_(param),
        custom_func_(func),
        expected_token_kind_(expected_token_kind) {}
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
    output.last_token_ = this->last_token();
    output.cur_token_ = this->cur_token();
    return output;
  };
  [[nodiscard]] token::details::TokenKind expected_token_kind() const {
    return expected_token_kind_;
  }
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
  [[nodiscard]] inline bool valid() const { return valid(params_.cur_token_); }
  template <meta::Str TagNameOther>
  [[nodiscard]] inline bool valid(const token::Token<TagNameOther>& tok) const {
    return token::TokenLists::instance().next(tok).kind() !=
           token::details::TokenKind::eof;
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
  [[nodiscard]] size_t calling_depth() const { return params_.calling_depth_; }
  void calling_depth(size_t depth) const { params_.calling_depth_ = depth; }
  [[nodiscard]] bitset_type executed_mask() const { return executed_mask_; }
  void executed_mask(const bitset_type& mask) { executed_mask_ = mask; }
  void reset() { executed_mask_.reset(); }
  bool ok_to_try() { return !executed_mask_.all(); }

  static type create_single_token_check(bool opt, size_t calling_depth,
                                        token::details::TokenKind token_kind,
                                        diag::DiagKind diag_kind) {
    typename type::custom_func_type z([token_kind, diag_kind](type* func) {
      func->executed_mask_.set();
      ParseFunctionOutputs<TagName> output;
      output.last_token_ = func->last_token();
      output.cur_token_ = func->cur_token();
      if (!func->valid()) {
        return output;
      }
      token::Token<TagName + "_second_token"> next_tok;
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

        lps_assert(TagName,
                   next_tok.kind() != token::details::TokenKind::unknown);

        token::TokenLists::instance().append(
            next_tok, token::TokenLists::Info::create(output.cur_token_));
      }

      lps_assert(TagName, output.cur_token_.ptr() != next_tok.ptr());

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
        if (next_tok.kind() != token::details::TokenKind::unknown) {
          output.token_list_infos_.append(
              &(token::TokenLists::instance().at(next_tok)));
        }
      }

      return output;
    });
    return type(z, token_kind, opt, calling_depth);
  }

 protected:
  ParseFunctionInputs<TagName> params_;
  custom_func_type custom_func_{nullptr};
  bitset_type executed_mask_;

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
