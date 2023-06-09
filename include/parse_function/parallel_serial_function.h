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

#include <stack>
#include <type_traits>
#include "basic/exception.h"
#include "parser.h"

namespace lps::parser::details {

template <size_t NumElements, typename... ParseFuncs>
class ParallelParseFunctions : public ParseFunction<NumElements> {

 public:
  using base = ParseFunction<NumElements>;
  ParallelParseFunctions(const ParallelParseFunctions& other) = default;
  explicit ParallelParseFunctions(const ParseFunctionInputs& param,
                                  ParseFuncs&&... funcs)
      : base(param), parse_functions_(funcs...) {}
  ParseFunctionOutputs operator()() override;

 protected:
  std::tuple<ParseFuncs...> parse_functions_;
};

template <typename... ParseFuncs>
class SerialParseFunctions : public ParseFunction<1> {

 public:
  using base = ParseFunction<1>;
  SerialParseFunctions(const SerialParseFunctions& other) = default;
  explicit SerialParseFunctions(const ParseFunctionInputs& param,
                                ParseFuncs&&... funcs)
      : base(param), parse_functions_(funcs...) {}
  ParseFunctionOutputs operator()() override;

 protected:
  std::tuple<ParseFuncs...> parse_functions_;
};
}  // namespace lps::parser::details
