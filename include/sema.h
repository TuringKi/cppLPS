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

 protected:
  const parser::details::Tree::Node* gram_node_{nullptr};

 private:
  constexpr static const char* kTag = "lps::sema::details::Unit";
};

class TranslationUnit;
class Expression;
class AssignmentExpression;
class LogicalOrExpression;
class InitializerClause;

// A program consists of one or more translation units linked together.
// A `translation unit` consists of a sequence of declarations.
// A name is said to have linkage when it can denote the same object,
// reference, function, type, template, namespace or value as a name
// introduced by a declaration in another scope:

class TranslationUnit : public Unit {

 public:
  void build() override { LPS_ERROR(kTag, "not implemented"); }

 private:
  constexpr static const char* kTag = "lps::sema::details::TranslationUnit";
};

// A pair of expressions separated by a comma is evaluated left-to-right;
// the left expression is a `discarded-value expression`. The left expression
// is sequenced before the right expression.
// The type and value of the result are the type and value of the right operand;
// the result is of the same value category as its right operand,
// and is a bit-field if its right operand is a bit-field.
class Expression : public Unit {

 public:
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  virtual void constant_evaluate() { LPS_ERROR(kTag, "not implemented"); }

 private:
  constexpr static const char* kTag = "lps::sema::details::Expression";

  basic::Vector<2, std::shared_ptr<AssignmentExpression>> assignment_exprs_;
};

class AssignmentExpression : public Expression {

 public:
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void constant_evaluate() override { LPS_ERROR(kTag, "not implemented"); }

 private:
  constexpr static const char* kTag =
      "lps::sema::details::AssignmentExpression";

  std::shared_ptr<LogicalOrExpression> logical_or_expr_{nullptr};
  std::shared_ptr<InitializerClause> init_clause_{nullptr};
  const char* assignment_op_{nullptr};
};

}  // namespace details
}  // namespace lps::sema
