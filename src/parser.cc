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

#include "parser.h"
#include "basic/exception.h"
#include "diag.h"
#include "lexer.h"
#include "parser/decl.h"
#include "parser/module.h"
#include "parser/top.h"
#include "token.h"

namespace lps::parser {

// cpp grammar: https://timsong-cpp.github.io/cppwp/n4868/gram
void Parser::parse(uint32_t file_id) {
  auto content =
      src::Manager::instance().ref<meta::S("file_contents")>(file_id);
  if (!content.empty()) {
    details::ParseFunctionInputs<meta::Str("translation_unit_param")> params;
    params.opt_ = false;
    params.file_id_ = file_id;
    params.start_ = content.data();
    details::TranslationUnit<meta::Str("translation_unit")> func(params);
    auto output = func();
    for (const auto& a : output.diag_inputs_) {
      diag::doing<meta::Str("translation_unit")>(a.main_token_, a.kind_,
                                                 a.context_tokens_);
    }
  }
}

}  // namespace lps::parser
