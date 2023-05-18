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

#include <iostream>
#include "basic/exception.h"
#include "lexer.h"
#include "src.h"
int main(int argc, char** argv) {

  lps_assert(meta::S("test_lexer"), argc == 2);

  const char* file_path = argv[1];

  auto file_id = lps::src::Manager::instance().append(file_path);
  lps_assert(meta::S("test_lexer"), file_id > 0);

  auto contents =
      lps::src::Manager::instance().ref<meta::S("file_contents")>(file_id);
  lps_assert(meta::S("test_lexer"), contents.capacity() > 0);

  lps::lexer::Lexer lexer(file_id, contents.data());
  while (!lexer.finish(contents.capacity())) {
    lps::token::Token<meta::S("parse_start_token")> tok;
    lexer.lex(tok);
    std::cout << tok << "\n";
  }

  return 0;
}
