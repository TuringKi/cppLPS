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
#include "lexer.h"
#include "token.h"

namespace lps::parser {

void Parser::parse() {

  token::Token<meta::S("parse_start_token")> tok;

  lexer::Lexer lexer(content_.data());
  lexer.lex(tok);
}

// translation_unit:
//  declaration_seq
//  global_module_fragment[opt] module_declaration declaration_seq[opt] private_module_fragment[opt]
void Parser::translation_unit() {

  declaration_seq();
}

// declaration_seq:
//  declaration
//  declaration_seq declaration
void Parser::declaration_seq() {}

}  // namespace lps::parser
