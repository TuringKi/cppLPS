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
#include "parser.h"
#include "src.h"
int main(int argc, char** argv) {

  lps::parser::details::ParseFunctionOutputs o1;
  {

    lps::parser::details::ParseFunctionOutputs o0;
    decltype(o0.diag_inputs_)::ele_type a;
    decltype(a.context_tokens_)::ele_type token;
    a.context_tokens_.append(token);
    a.context_tokens_.append(token);
    a.context_tokens_.append(token);
    a.context_tokens_.append(token);
    a.context_tokens_.append(token);
    a.context_tokens_.append(token);

    {
      decltype(o0.diag_inputs_)::ele_type b;
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      o0.diag_inputs_.append(std::move(b));
    }

    {
      decltype(o0.diag_inputs_)::ele_type b;
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      o0.diag_inputs_.append(std::move(b));
    }

    {
      decltype(o0.diag_inputs_)::ele_type b;
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      o0.diag_inputs_.append(std::move(b));
    }
    {
      decltype(o0.diag_inputs_)::ele_type b;
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      b.context_tokens_.append(token);
      o0.diag_inputs_.append(std::move(b));
    }

    o0.diag_inputs_.append(std::move(a));

    o1.concat(std::move(o0), false);
  }

  std::cout << o1.diag_inputs_.size() << "\n";
  return 0;
}
