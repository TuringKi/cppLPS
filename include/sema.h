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

#include "basic/mem.h"
#include "parser.h"
#include "token.h"

namespace lps::sema {

namespace details {

template <meta::Str TagName>
class Unit : virtual public basic::mem::TraceTag<TagName> {
 public:
  virtual void build() = 0;

 protected:
  const parser::details::Tree::Node* gram_node_{nullptr};
};

// A program consists of one or more translation units linked together.
// A `translation unit` consists of a sequence of declarations.
// A name is said to have linkage when it can denote the same object,
// reference, function, type, template, namespace or value as a name
// introduced by a declaration in another scope:

class TranslationUnit : public Unit<meta::S("TranslationUnit")> {

 public:
  void build() override;
};

}  // namespace details
}  // namespace lps::sema
