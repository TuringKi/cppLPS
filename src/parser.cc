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
#include "parse_function/function.h"
#include "token.h"

namespace lps::parser {

// cpp grammar: https://timsong-cpp.github.io/cppwp/n4868/gram
void Parser::parse(uint32_t file_id) {
  auto content = src::Manager::instance().ref_of_char_file(file_id);
  if (!content.empty()) {
    details::ParseFunctionInputs params;
    params.opt_ = false;
    token::Token next_tok;
    lexer::Lexer lexer(file_id, 0);
    lexer.lex(next_tok);
    params.cur_token_ = next_tok;
    if (next_tok.kind() != token::details::TokenKind::unknown) {
      token::TokenLists::instance().append(next_tok);
    }
    details::TranslationUnit func(params);
    auto output = func();
    for (const auto& a : output.diag_inputs_) {
      diag::doing(a.main_token_, a.kind_, a.context_tokens_);
    }
    if (output.work_) {
      for (const auto& node : output.node_.sub_nodes_) {
        if (node->kind_ != details::ParseFunctionKind::kUnknown) {
          // todo(@mxlol233): handle the recursive tree nodes...
        }
      }
    }
  }
}

}  // namespace lps::parser
