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

#include "parser/function.h"

namespace lps::parser::details {

// translation_unit:
//  declaration_seq
//  global_module_fragment[opt] module_declaration declaration_seq[opt] private_module_fragment[opt]
template <meta::Str TagName>
ParseFunctionOutputs<TagName> TranslationUnit<TagName>::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  constexpr meta::Str kFistTokTag("translation_unit");
  token::Token<kFistTokTag> tok;

  lexer::Lexer lexer(this->file_id(), this->start());
  lexer.lex(tok);

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<meta::S("module_etc")>(
          true, nullptr, 0, token::Token<meta::S("module_etc")>()),
      GlobalModuleFragment<>(true), ModuleDeclaration<>(false),
      DeclarationSeq<>(true), PrivateModuleFragment<>(true));

  ParseFunctionInputs<kFistTokTag> b(true, lexer.cur(), tok.file_id(), tok);

  ParallelParseFunctions parallel_funcs(b, DeclarationSeq<>(true),
                                        std::move(serial_funcs));

  return parallel_funcs();
}

}  // namespace lps::parser::details
