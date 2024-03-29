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
#include "ast.h"
#include "basic/exception.h"
#include "parser.h"

namespace lps::parser::details {

template <ParseFunctionKind Kind, typename... ParseFuncs>
class SpecialParseFunctions {
 public:
  explicit SpecialParseFunctions(ParseFuncs&&... funcs)
      : parse_functions_(funcs...) {}
  [[nodiscard]] const basic::Vector<4, ParseFunctionOutputs>& valid_outputs()
      const {
    return valid_outputs_;
  }

 protected:
  std::tuple<ParseFuncs...> parse_functions_;
  basic::Vector<4, ParseFunctionOutputs> valid_outputs_;
};

template <ParseFunctionKind Kind, typename... ParseFuncs>
class ParallelParseFunctions
    : public ParseFunction,
      public SpecialParseFunctions<Kind, ParseFuncs...> {

 public:
  using base = ParseFunction;
  using special_functions_type = SpecialParseFunctions<Kind, ParseFuncs...>;
  explicit ParallelParseFunctions(Context* context, const char* kName,
                                  const ParseFunctionInputs& param,
                                  ParseFuncs&&... funcs)
      : base(context, kName, param),
        special_functions_type(std::move(funcs)...) {}
  virtual ~ParallelParseFunctions() = default;
  ParseFunctionOutputs operator()();
  void reset() override;
  ParseFunctionKind kind() override { return Kind; }
};

template <ParseFunctionKind Kind, typename... ParseFuncs>
class SerialParseFunctions : public ParseFunction,
                             public SpecialParseFunctions<Kind, ParseFuncs...> {

 public:
  using base = ParseFunction;
  using special_functions_type = SpecialParseFunctions<Kind, ParseFuncs...>;
  explicit SerialParseFunctions(Context* context, const char* kName,
                                const ParseFunctionInputs& param,
                                ParseFuncs&&... funcs)
      : base(context, kName, param),
        special_functions_type(std::move(funcs)...) {}
  virtual ~SerialParseFunctions() = default;
  ParseFunctionOutputs operator()();
  void reset() override;
  ParseFunctionKind kind() override { return Kind; }
};

template <ParseFunctionKind Kind, typename... ParseFuncs>
class RecursiveParseFunctions
    : public ParseFunction,
      public SpecialParseFunctions<Kind, ParseFuncs...> {

 public:
  static constexpr int kNFuncs = sizeof...(ParseFuncs);
  using working_list_type = basic::Bitset<kNFuncs>;
  using working_list_stack_type = std::stack<working_list_type>;
  using base = ParseFunction;
  using special_functions_type = SpecialParseFunctions<Kind, ParseFuncs...>;
  explicit RecursiveParseFunctions(Context* context, const char* kName,
                                   const ParseFunctionInputs& param,
                                   ParseFuncs&&... funcs)
      : base(context, kName, param),
        special_functions_type(std::move(funcs)...) {}
  virtual ~RecursiveParseFunctions() = default;
  ParseFunctionOutputs operator()();
  ParseFunctionKind kind() override { return Kind; }

 protected:
  void execute(ParseFunctionOutputs&, const Line::segments_type& saved_lines);
  void reset() override;
  working_list_stack_type working_list_stack_;
};
}  // namespace lps::parser::details
