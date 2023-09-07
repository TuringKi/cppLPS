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

#include <memory>
#include "basic/exception.h"
#include "basic/mem.h"
#include "parser.h"
#include "token.h"

namespace lps::sema {

namespace details {

class Unit {
 public:
  virtual void build() = 0;
  [[nodiscard]] parser::details::ParseFunctionKind kind() const {
    return kind_;
  }

  [[nodiscard]] token::details::TokenKind token_kind() const {
    return token_kind_;
  }

 protected:
  const parser::details::Tree::Node* gram_node_{nullptr};
  parser::details::ParseFunctionKind kind_{
      parser::details::ParseFunctionKind::kUnknown};
  token::details::TokenKind token_kind_{token::details::TokenKind::unknown};

 private:
  constexpr static const char* kTag = "lps::sema::details::Unit";
};

class Symbol {

 protected:
  const token::Token* token_{nullptr};

 private:
  constexpr static const char* kTag = "lps::sema::details::Symbol";
};

class Variable : public Symbol {
 private:
  constexpr static const char* kTag = "lps::sema::details::Variable";
};

class Literal : public Symbol {
 private:
  constexpr static const char* kTag = "lps::sema::details::Literal";
};

class Operator : public Symbol {
 private:
  constexpr static const char* kTag = "lps::sema::details::Operator";
};

}  // namespace details
}  // namespace lps::sema
