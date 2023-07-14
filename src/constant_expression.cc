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

#include "lex/pp.h"
#include "parse_function/function.h"

namespace lps::lexer::details::pp {

bool Preprocessing::lex_conditional_expression(
    const typename base::ptr_type& first_ptr, typename base::ptr_type& ptr,
    token::Token& tok) {

  {

    parser::details::Context context;
    context.with([first_ptr](parser::details::Context* context) {
      parser::details::ParseFunctionInputs params;
      params.opt_ = false;
      token::Token next_tok;
      lexer::Lexer lexer(first_ptr.file_id(), first_ptr.pos());
      lexer.lex(next_tok);
      if (next_tok.kind() != token::details::TokenKind::unknown) {
        context->token_lists().append(next_tok);
      }
      params.cur_token_ = next_tok;
      parser::details::ConditionalExpression func(context, params);
      auto output = func();
      int dummy = -1;
    });
  }

  return false;
}

}  // namespace lps::lexer::details::pp
