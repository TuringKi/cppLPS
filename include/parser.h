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
#include <list>
#include <unordered_map>
#include <utility>
#include "ast.h"
#include "basic/bitset.h"
#include "basic/exception.h"
#include "basic/mem.h"
#include "basic/meta.h"
#include "lexer.h"
#include "src.h"
#include "token.h"

namespace lps::parser {

namespace details {
class Context {
 public:
  friend class ContextTrait;
  static constexpr basic::mem::TraceTag::tag_type kTag = "Context";
  using executed_func_type = std::function<void(Context*)>;
  void with(const executed_func_type& func) { func(this); }
  token::TokenLists& token_lists() { return token_lists_; }
  const token::Token& start_token() const { return start_token_; }
  void start_token(const token::Token& token) { start_token_ = token; }
  size_t len_from_start(const token::Token& cur_token) {
    return token_lists_.len(start_token_, cur_token);
  }
  const Line* paint(const Line& the_line) {
    if (path_.contains(the_line.start_)) {
      for (auto& line : path_[the_line.start_]) {
        if (line == the_line) {
          return &line;
        }
      }
    }
    path_[the_line.start_].push_back(the_line);
    lps_assert(kTag, path_[the_line.start_].back().len_ > 0);
    return &path_[the_line.start_].back();
  }

  Line longest_line(const auto& start) {
    size_t max_l = 0;
    const auto* p_start = &token_lists_.at(start);
    auto cmp = [](const Line& l, size_t b) {
      return l.len_ > b;
    };
    return find_line<cmp>(p_start, max_l);
  }

  Line longest_line_with_kind(const auto& start, ParseFunctionKind kind) {
    size_t max_l = 0;
    const auto* p_start = &token_lists_.at(start);
    auto cmp = [](const Line& l, size_t b, ParseFunctionKind kind) {
      return l.len_ > b && l.kind_ == kind;
    };
    return find_line<cmp>(p_start, max_l, kind);
  }

  Line shortest_line(const token::Token& start) {
    size_t min_l = std::numeric_limits<size_t>::max();
    const auto* p_start = &token_lists_.at(start);
    auto cmp = [](const Line& l, size_t b) {
      return l.len_ < b;
    };
    return find_line<cmp>(p_start, min_l);
  }

  Tree l2t(const Line& root_line);

 private:
  Line shortest_line(const token::Token* p_start, size_t min_l) {
    return find_line<[](const Line& l, size_t b) {
      return l.len_ < b;
    }>(p_start, min_l);
  }

  template <auto F, typename... Args>
  Line find_line(const token::archived_type* p_start, size_t the_l,
                 Args... args) {
    Line the_line;
    lps_assert(kTag, path_.contains(p_start));
    for (const auto& line : path_[p_start]) {
      if (F(line, the_l, args...)) {
        the_line = line;
      }
    }
    return the_line;
  }

  token::TokenLists token_lists_;
  token::Token start_token_;
  std::unordered_map<const token::Token*, std::list<Line>> path_;
};

class ContextTrait {
 public:
  explicit ContextTrait(Context* context) : context_(context) {}
  Context* context() { return context_; }

 protected:
  Context* context_;
};

struct ParseFunctionOutputs {
  explicit ParseFunctionOutputs() = default;

#define SET_MOVE(A, B)                \
  (A)->work_ = (B).work_;             \
  (A)->last_token_ = (B).last_token_; \
  (A)->cur_token_ = (B).cur_token_;   \
  (A)->line_ = (B).line_;             \
  (A)->len_ = (B).len_;

#define SET(A, B)                     \
  (A)->work_ = (B).work_;             \
  (A)->last_token_ = (B).last_token_; \
  (A)->cur_token_ = (B).cur_token_;   \
  (A)->line_ = (B).line_;             \
  (A)->len_ = (B).len_;

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

  void concat(ParseFunctionOutputs&& other, bool opt = true) {
    if (!opt) {
      this->work_ = other.work_;
    }
    if (other.work_) {
      this->last_token_ = other.last_token_;
      this->cur_token_ = other.cur_token_;
      this->work_ = other.work_;
      len_ += other.len_;
      this->line_ = other.line_;
    }
  }

  bool work_{false};
  token::Token last_token_;
  token::Token cur_token_;
  const Line* line_{nullptr};
  size_t len_{0};
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
class ParseFunction : public ContextTrait {
 public:
  using type = ParseFunction<NumElements>;
  using context_trait_type = ContextTrait;
  using output_type = ParseFunctionOutputs;
  using custom_func_type = std::function<output_type(type*)>;
  using bitset_type = basic::Bitset<NumElements>;
  static constexpr size_t kNumberOfElements = NumElements;

  virtual ParseFunctionKind kind() {
    static constexpr ParseFunctionKind kKind =
        ParseFunctionKind::kExpectedToken;
    return kKind;
  }
  virtual ~ParseFunction() = default;
  template <typename... Params>
  explicit ParseFunction(Context* context, const char* kName, Params... params)
      : context_trait_type(context), kName_(kName), params_(params...) {}
  template <typename... Params>
  explicit ParseFunction(Context* context, const char* kName,
                         custom_func_type func,
                         token::details::TokenKind expected_token_kind,
                         Params... params)
      : context_trait_type(context),
        kName_(kName),
        custom_func_(func),
        params_(params...),
        expected_token_kind_(expected_token_kind) {}
  explicit ParseFunction(Context* context, const char* kName,
                         ParseFunctionInputs param,
                         token::details::TokenKind expected_token_kind,
                         custom_func_type func)
      : context_trait_type(context),
        kName_(kName),
        params_(std::move(param)),
        custom_func_(func),
        expected_token_kind_(expected_token_kind) {}
  explicit ParseFunction(Context* context, const char* kName,
                         const ParseFunctionInputs& param)
      : context_trait_type(context), kName_(kName), params_(param) {}
  explicit ParseFunction(Context* context, const char* kName,
                         ParseFunctionInputs&& param)
      : context_trait_type(context), kName_(kName), params_(std::move(param)) {}

  output_type operator()() {
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

  static type create_single_token_check(Context* context, bool opt,
                                        size_t calling_depth,
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
      if (func->context()->token_lists().has(output.cur_token_)) {
        const auto* tok_ptr =
            func->context()->token_lists().at(output.cur_token_).next();
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

        func->context()->token_lists().append(
            next_tok, token::TokenLists::Info::create(output.cur_token_));
      }

      lps_assert("create_single_token_check",
                 output.cur_token_.ptr() != next_tok.ptr());

      if (output.cur_token_.kind() != token_kind) {
        if (!func->opt()) {
          //todo(@mxlol233): add diag.
        }
        output.work_ = false;
        return output;
      }

      {
        output.last_token_ = output.cur_token_;
        output.work_ = true;
        output.cur_token_ = next_tok;
        ++output.len_;
        {
          const auto* p_start =
              &func->context()->token_lists().at(output.last_token_);
          const auto* p_end = &func->context()->token_lists().at(next_tok);
          Line line{p_start,
                    p_end,
                    ParseFunctionKind::kExpectedToken,
                    token_kind,
                    token::TokenLists::len(p_start, p_end),
                    func->calling_depth(),
                    Line::segments_type()};
          output.line_ = func->context()->paint(line);
        }
      }

      return output;
    });
    return type(context, token::details::kMap.at(token_kind), z, token_kind,
                opt, calling_depth);
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
