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

#include "basic/tui.h"
#include "diag.h"
#include "parse_function/parallel_serial_recursive_function.h"
#include "token.h"

namespace lps::parser::details {

template <ParseFunctionKind Kind, size_t NumElements, typename... ParseFuncs>
ParseFunctionOutputs
ParallelParseFunctions<Kind, NumElements, ParseFuncs...>::operator()() {
  lps_assert("ParallelParseFunctions", this->ok_to_try());
  auto output = base::operator()();
  if (!this->valid()) {
    this->executed_mask_.set();
    return output;
  }

  bool flg_continue = true;
  uint32_t running_sub_func_idx = 0;
  ParseFunctionOutputs max_len_match_output;
  max_len_match_output.last_token_ = this->last_token();
  max_len_match_output.cur_token_ = this->cur_token();
  uint32_t max_len_match_idx = -1;
  std::apply(
      [this, &flg_continue, &output, &max_len_match_output,
       &running_sub_func_idx, &max_len_match_idx](ParseFuncs&... funcs) {
        (
            [this, &flg_continue, &output, &max_len_match_output,
             &running_sub_func_idx, &max_len_match_idx](auto& func) {
              if (!this->executed_mask_.at(running_sub_func_idx)) {
                func.last_token(this->last_token());
                func.cur_token(this->cur_token());
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
                      this->executed_mask_.set();
                      return;
                    }
                  }
                } else {
                  output.concat(std::move(local_output), false);
                  this->executed_mask_.set(running_sub_func_idx);
                }
                running_sub_func_idx++;
              }
            }(funcs),
            ...);
      },
      parse_functions_);
  if (max_len_match_output.work_) {
    this->executed_mask_.set(max_len_match_idx);
    diag::infos() << basic::str::from(
        std::string(this->calling_depth(), '>'), " ", this->kName_,
        basic::tui::color::Shell::colorize(
            basic::str::from(" ok, matched idx = ", max_len_match_idx, ".\n"),
            basic::tui::color::Shell::fgreen()));
  } else {
    this->executed_mask_.set();
    diag::infos() << basic::str::from(
        std::string(this->calling_depth(), '>'), " ", this->kName_,
        basic::tui::color::Shell::colorize(basic::str::from(" failed\n"),
                                           basic::tui::color::Shell::fred()));
  }
  output.concat(std::move(max_len_match_output), false);
  return output;
}

template <ParseFunctionKind Kind, size_t NumElements, typename... ParseFuncs>
void ParallelParseFunctions<Kind, NumElements, ParseFuncs...>::reset() {
  base::reset();
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

}  // namespace lps::parser::details
