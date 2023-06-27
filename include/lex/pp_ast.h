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

#include "basic/exception.h"
#include "token.h"

namespace lps::lexer::details::pp::ast {

template <meta::Str TagName>
class Node : virtual public basic::mem::TraceTag<TagName> {

 public:
  Node() = default;
  using tokens_type = typename token::Token<TagName>::tokens_type;
  virtual const tokens_type& expand() = 0;
};

template <meta::Str TagName>
class Define : public Node<TagName> {

 public:
  using base = Node<TagName>;
  const typename base::tokens_type& expand() override { return body_; }

 protected:
  token::Token<TagName> name_;
  typename base::tokens_type body_;
};

template <meta::Str TagName>
class DefineWithParameters : public Define<TagName> {

 public:
  using base = Node<TagName>;
  const typename base::tokens_type& expand() override {}

 protected:
  typename base::tokens_type parameters_;
};

}  // namespace lps::lexer::details::pp::ast
