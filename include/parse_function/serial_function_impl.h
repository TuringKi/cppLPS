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
#include "diag.h"
#include "parse_function/parallel_serial_function.h"
#include "token.h"

namespace lps::parser::details {

template <meta::Str TagName, typename... ParseFuncs>
ParseFunctionOutputs<TagName>
SerialParseFunctions<TagName, ParseFuncs...>::operator()() {

  ParseFunctionOutputs<TagName> output;
  token::Token<TagName + "_first_token"> tok;
  output.last_token_ = this->last_token();
  output.cur_token_ = this->cur_token();

  bool flg_continue = true;

  uint32_t running_sub_func_stack_top_idx = 0;

  uint32_t running_sub_func_size = 0;
  std::apply(
      [&running_sub_func_size](ParseFuncs&... funcs) {
        (
            [&running_sub_func_size](auto& /*func*/) {
              running_sub_func_size++;
            }(funcs),
            ...);
      },
      parse_functions_);

  std::stack<decltype(output)> path_stack;
  path_stack.push(output);
  while (flg_continue) {
    std::apply(
        [this, &running_sub_func_size, &path_stack, &flg_continue,
         &running_sub_func_stack_top_idx](ParseFuncs&... funcs) {
          bool flg_not_continue = false;
          uint32_t running_sub_func_idx = 0;
          (
              [this, &running_sub_func_size, &path_stack, &flg_not_continue,
               &flg_continue, &running_sub_func_idx,
               &running_sub_func_stack_top_idx](auto& func) {
                if (flg_not_continue || !flg_continue) {
                  return;
                }
                if (running_sub_func_idx++ < running_sub_func_stack_top_idx) {
                  return;
                }
                auto make_try_again =
                    [&](basic::Vector<4, diag::DiagInputs<TagName>,
                                      TagName + "_DiagInputs">&& diag_inputs) {
                      if ((running_sub_func_idx - 1) == 0) {
                        flg_continue = false;
                        return;
                      }
                      flg_not_continue = true;
                      running_sub_func_stack_top_idx =
                          (running_sub_func_idx - 1) - 1;
                      func.reset();
                      auto tmp_output(path_stack.top());
                      tmp_output.work_ = false;
                      lps_assert(TagName, path_stack.size() > 0);

                      path_stack.pop();

                      path_stack.top().concat(std::move(tmp_output), false);

                      if (!diag_inputs.empty()) {
                        decltype(tmp_output) tmp_output0;
                        tmp_output0.diag_inputs_ = std::move(diag_inputs);
                        path_stack.top().concat(std::move(tmp_output0), false);
                      }
                      return;
                    };
                if (flg_continue) {
                  func.last_token(path_stack.top().last_token_);
                  func.cur_token(path_stack.top().cur_token_);

                  if (!func.ok_to_try()) {
                    make_try_again(basic::Vector<4, diag::DiagInputs<TagName>,
                                                 TagName + "_DiagInputs">());
                    return;
                  }

                  auto local_output = func();
                  if (!local_output.work_ && !func.opt()) {
                    basic::Vector<4, diag::DiagInputs<TagName>,
                                  TagName + "_DiagInputs">
                        a;
                    a = std::move(local_output.diag_inputs_);
                    make_try_again(std::move(a));
                    return;
                  }

                  auto output(path_stack.top());
                  output.concat(std::move(local_output), func.opt());
                  path_stack.push(output);
                  LPS_NOTE(TagName, "[ ", func.tag(),
                           " ] local_output.work = ", local_output.work_,
                           ", output.work = ", output.work_);
                  if (running_sub_func_idx == running_sub_func_size &&
                      output.work_) {
                    flg_continue = false;
                    return;
                  }
                }
              }(funcs),
              ...);
        },
        parse_functions_);
  }
  lps_assert(TagName, path_stack.size() > 0);
  if (path_stack.top().work_) {
    LPS_NOTE(TagName, "output ok");
  }
  return path_stack.top();
}

}  // namespace lps::parser::details
