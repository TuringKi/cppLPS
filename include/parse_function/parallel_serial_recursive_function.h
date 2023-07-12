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
  explicit ParallelParseFunctions(const char* kName,
                                  const ParseFunctionInputs& param,
                                  ParseFuncs&&... funcs)
      : base(kName, param), parse_functions_(funcs...) {}
  ParseFunctionOutputs operator()() override;
  void reset() override;

 protected:
  std::tuple<ParseFuncs...> parse_functions_;
};

template <typename... ParseFuncs>
class SerialParseFunctions : public ParseFunction<1> {

 public:
  using base = ParseFunction<1>;
  explicit SerialParseFunctions(const char* kName,
                                const ParseFunctionInputs& param,
                                ParseFuncs&&... funcs)
      : base(kName, param), parse_functions_(funcs...) {}
  ParseFunctionOutputs operator()() override;
  void reset() override;

 protected:
  std::tuple<ParseFuncs...> parse_functions_;
};

template <typename... ParseFuncs>
class RecursiveParseFunctions : public ParseFunction<1> {

 public:
  static constexpr int kNFuncs = sizeof...(ParseFuncs);
  using working_list_type = basic::Bitset<kNFuncs>;
  using working_list_stack_type = std::stack<working_list_type>;
  using base = ParseFunction<1>;
  explicit RecursiveParseFunctions(const char* kName,
                                   const ParseFunctionInputs& param,
                                   ParseFuncs&&... funcs)
      : base(kName, param), parse_functions_(funcs...) {}
  ParseFunctionOutputs operator()() override;

 protected:
  void execute(ParseFunctionOutputs&, working_list_type& executed_mask);
  bool regret(const working_list_type& executed_mask);
  void reset() override;

  std::tuple<ParseFuncs...> parse_functions_;
  working_list_stack_type working_list_stack_;
};
}  // namespace lps::parser::details
