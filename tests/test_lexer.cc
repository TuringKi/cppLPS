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

#include <cstring>
#include <iostream>
#include "basic/exception.h"
#include "lexer.h"
#include "src.h"
#include "token.h"
#include "tu.h"
int main(int argc, char** argv) {
  lps::src::Manager::instance().exe_path(
      weakly_canonical(std::filesystem::path(argv[0])).parent_path());

  lps_assert("test_lexer", argc >= 2);

  const char* file_path = argv[1];
  if (argc >= 3) {
    for (int i = 2; i < argc; i++) {
      const char* include_path = argv[i];
      auto len = std::strlen(include_path);
      lps::tu::TU::instance().include_dir(
          lps::tu::TU::IdentStringRef(include_path, len));
    }
  }

  auto file_id = lps::src::Manager::instance().append(file_path);
  lps_assert("test_lexer", file_id > 0);

  auto contents = lps::src::Manager::instance().ref_of_char_file(file_id);
  lps_assert("test_lexer", contents.capacity() > 0);

  lps::token::Token tok;
  while (tok.kind() != lps::token::details::TokenKind::eof) {
    if (tok.kind() == lps::token::details::TokenKind::unknown) {
      lps::lexer::Lexer lexer(file_id, 0);
      lexer.lex(tok);
    } else {
      lps::lexer::Lexer lexer(tok.next_visitor().second,
                              tok.next_visitor().first);
      lexer.lex(tok);
    }

    std::cout << tok << "\n";
  }

  return 0;
}
