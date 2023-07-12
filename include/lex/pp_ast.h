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

#include <cstring>
#include <memory>
#include "basic/exception.h"
#include "basic/vec.h"
#include "token.h"

namespace lps::lexer::details::pp::ast {

class Node {

 public:
  using tokens_type = typename token::Token::tokens_type;
  using macro_parameters_type = basic::Vector<3, tokens_type>;
  using token_type = token::Token;
  using ptr_type = std::unique_ptr<Node>;
  template <class... Args>
  explicit Node(token_type&& name, Args&&... /*args*/)
      : name_(std::move(name)){};

  virtual tokens_type expand(
      const macro_parameters_type& /*parameter_tokens*/) {
    return tokens_type{};
  };
  [[nodiscard]] const token_type& name() const { return name_; }

  [[nodiscard]] basic::StringRef name_str() const { return name_.str(); }

 protected:
  token_type name_;
};

class Define : public Node {

 public:
  using base = Node;
  template <class... Args>
  Define(typename base::token_type&& name, typename base::tokens_type&& body,
         Args... args)
      : base(std::move(name), std::forward<Args>(args)...),
        body_(std::move(body)) {}

  typename base::tokens_type expand(
      const typename base::macro_parameters_type& /*parameter_tokens*/)
      override {
    typename base::tokens_type out(body_);
    return out;
  }

 protected:
  typename base::tokens_type body_;
};

class DefineWithParameters : public Define {

 public:
  using base = Define;
  template <class... Args>
  DefineWithParameters(typename base::token_type&& name,
                       typename base::tokens_type&& parameters,
                       typename base::tokens_type&& body, Args... args)
      : base(std::move(name), std::move(body), std::forward<Args>(args)...),
        parameters_(std::move(parameters)) {}

  typename base::tokens_type expand(
      const typename base::macro_parameters_type& parameter_tokens) override {
    lps_assert("DefineWithParameters",
               parameter_tokens.size() == parameters_.size());
    typename base::tokens_type expanded_tokens;
    for (const auto& t : this->body_) {
      if (t.kind() == token::details::TokenKind::identifier) {
        bool matched = false;
        for (size_t i = 0; i < parameters_.size(); i++) {
          const auto& p = parameters_[i];
          if (std::strncmp(t.ptr(), p.ptr(), p.offset()) == 0) {
            // replace the `identifier` tokens with `parameters`
            for (const auto& t1 : parameter_tokens[i]) {
              expanded_tokens.append(t1);
            }
            matched = true;
            break;  // we only match once.
          }
        }
        if (matched) {
          continue;
        }
      }
      expanded_tokens.append(t);
    }

    return expanded_tokens;
  }

 protected:
  typename base::tokens_type parameters_;
};

class Include : public Node {

 public:
  using base = Node;
  template <class... Args>
  explicit Include(typename base::token_type&& name, Args... args)
      : base(std::move(name), std::forward<Args>(args)...) {}
};

class Factory {

 public:
  template <class Type, class... Args>
  static std::unique_ptr<Node> create(Args&&... args) {
    return std::make_unique<Type>(std::forward<Args>(args)...);
  }
};

}  // namespace lps::lexer::details::pp::ast
