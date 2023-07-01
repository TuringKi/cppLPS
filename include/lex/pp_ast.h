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
  using token_type = token::Token;
  using ptr_type = std::unique_ptr<Node>;
  template <class... Args>
  explicit Node(token_type&& name, Args&&... /*args*/)
      : name_(std::move(name)){};

  virtual const tokens_type& expand() { return expanded_tokens_; };
  [[nodiscard]] const token_type& name() const { return name_; }

  [[nodiscard]] basic::StringRef name_str() const { return name_.str(); }

 protected:
  token_type name_;
  tokens_type expanded_tokens_;
};

class Define : public Node {

 public:
  using base = Node;
  template <class... Args>
  Define(typename base::token_type&& name, typename base::tokens_type&& body,
         Args... args)
      : base(std::move(name), std::forward<Args>(args)...),
        body_(std::move(body)) {}

  const typename base::tokens_type& expand() override { return body_; }

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

  const typename base::tokens_type& expand() override {
    unreachable("DefineWithParameters");
    if (!this->expanded_tokens_.empty()) {
      return this->expanded_tokens_;
    }
    for (const auto& t : this->body_) {

      if (t.kind() == token::details::TokenKind::identifier) {
        bool matched = false;
        for (const auto& p : parameters_) {
          if (std::strncmp(t.ptr(), p.ptr(), p.offset()) == 0) {
            // replace the `identifier` tokens with `parameters`
            this->expanded_tokens_.append(p);
            matched = true;
            break;  // we only match once.
          }
        }
        if (matched) {
          continue;
        }
      }
      this->expanded_tokens_.append(t);
    }

    return this->expanded_tokens_;
  }

 protected:
  typename base::tokens_type parameters_;
};

class Factory {

 public:
  template <class Type, class... Args>
  static std::unique_ptr<Node> create(Args&&... args) {
    return std::make_unique<Type>(std::forward<Args>(args)...);
  }
};

}  // namespace lps::lexer::details::pp::ast
