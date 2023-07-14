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
#include "parse_function/parallel_serial_recursive_function.h"
#include "parser.h"
#include "token.h"

namespace lps::parser::details {

template <typename... ParseFuncs>
ParseFunctionOutputs RecursiveParseFunctions<ParseFuncs...>::operator()() {
  lps_assert("RecursiveParseFunctions", this->ok_to_try());
  auto output = base::operator()();
  this->executed_mask_.set();
  if (!this->valid()) {
    return output;
  }
  uint32_t recursive_depth = 0;
  working_list_type executed_mask;
  auto tmp_output = output;
  do {

    execute(tmp_output, executed_mask);

    if (!tmp_output.work_) {
      if (recursive_depth > 0) {
        lps_assert("RecursiveParseFunctions", !working_list_stack_.empty());
        executed_mask = working_list_stack_.top();
        if (!regret(executed_mask)) {
          break;
        }
        working_list_stack_.pop();
        --recursive_depth;
        continue;
      }
      break;
    }
    working_list_stack_.push(executed_mask);
    executed_mask.reset();
    ++recursive_depth;
    output = tmp_output;
  } while (true);

  return output;
}
template <typename... ParseFuncs>
bool RecursiveParseFunctions<ParseFuncs...>::regret(
    const working_list_type& executed_mask) {
  return !executed_mask.all();
}

template <typename... ParseFuncs>
void RecursiveParseFunctions<ParseFuncs...>::reset() {
  std::apply(
      [](ParseFuncs&... funcs) {
        (
            [](auto& func) {
              func.reset();
            }(funcs),
            ...);
      },
      parse_functions_);
}

template <typename... ParseFuncs>
void RecursiveParseFunctions<ParseFuncs...>::execute(
    ParseFunctionOutputs& output, working_list_type& executed_mask) {
  bool flg_continue = true;
  uint32_t running_sub_func_idx = 0;
  ParseFunctionOutputs max_len_match_output;
  max_len_match_output.last_token_ = this->last_token();
  max_len_match_output.cur_token_ = this->cur_token();
  uint32_t max_len_match_idx = -1;

  reset();

  std::apply(
      [this, &flg_continue, &output, &max_len_match_output,
       &running_sub_func_idx, &max_len_match_idx,
       &executed_mask](ParseFuncs&... funcs) {
        (
            [this, &flg_continue, &output, &max_len_match_output,
             &running_sub_func_idx, &max_len_match_idx,
             &executed_mask](auto& func) {
              if (!executed_mask.at(running_sub_func_idx)) {
                func.last_token(output.last_token_);
                func.cur_token(output.cur_token_);
                auto local_output = func();
                while (!local_output.work_ && func.ok_to_try()) {
                  auto local_output = func();
                }
                if (local_output.work_) {  // only allow one of them work
                  if (local_output.len_ > max_len_match_output.len_) {
                    max_len_match_output = std::move(local_output);
                    max_len_match_idx = running_sub_func_idx;
                    if (!this->valid(
                            local_output.cur_token_)) {  // no more chance
                      executed_mask.set();
                      return;
                    }
                  }
                } else {
                  output.concat(std::move(local_output), false);
                  executed_mask.set(running_sub_func_idx);
                }
                running_sub_func_idx++;
              }
            }(funcs),
            ...);
      },
      parse_functions_);
  if (max_len_match_output.work_) {
    diag::infos() << basic::str::from(
        std::string(this->calling_depth(), '>'), " ", this->kName_,
        basic::tui::color::Shell::colorize(
            basic::str::from(" ok, matched idx = ", max_len_match_idx, ".\n"),
            basic::tui::color::Shell::fgreen()));
  } else {
  }
  output.concat(std::move(max_len_match_output), false);
}

}  // namespace lps::parser::details
