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
#include "basic/arg.h"
#include "basic/exception.h"
#include "basic/file.h"
#include "basic/vec.h"
#include "parser.h"
#include "version.h"

int main(int argc, char** argv) {

  argparse::ArgumentParser program("lps", lps::version::git_hash);

  program.add_argument("-c").required().default_value("-").help(
      "specify the source file.");

  program.add_description(
      "A Lexer, Parser and Semantics Engine for the C++ Programming Language.");

  try {
    program.parse_args(argc, argv);
  } catch (const std::runtime_error& err) {
    std::cerr << err.what() << std::endl;
    std::cerr << program;
    std::exit(1);
  }

  auto filename = program.get<std::string>("-c");

  auto file = lps::basic::File<meta::S("main_file")>::create(filename.c_str());

  if (file->empty()) {
    return 0;
  }

  auto contents = file->ref<meta::S("file_contents")>();

  using lps::basic::str::details::operator<<;
  std::cout << contents;

  lps::parser::Parser parser(contents);

  parser.parse();

  return 0;
}
