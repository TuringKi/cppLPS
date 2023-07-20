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

#include <algorithm>
#include "basic/exception.h"
#include "basic/tui.h"
#include "diag.h"
#include "parse_function/parallel_serial_recursive_function.h"
#include "parser.h"
#include "token.h"

namespace lps::parser::details {

template <ParseFunctionKind Kind, typename... ParseFuncs>
ParseFunctionOutputs ParallelParseFunctions<Kind, ParseFuncs...>::operator()() {
  lps_assert("ParallelParseFunctions", this->ok_to_try());
  auto output = base::operator()();
  if (!this->valid()) {
    this->opt_idx(-1);
    return output;
  }

  bool flg_continue = true;
  uint32_t running_sub_func_idx = 0;
  ParseFunctionOutputs max_len_match_output;
  max_len_match_output.last_token_ = this->last_token();
  max_len_match_output.cur_token_ = this->cur_token();
  uint32_t max_len_match_idx = -1;
  lps_assert(this->kName_, this->valid_outputs_.empty());
  std::apply(
      [this, &flg_continue, &output, &max_len_match_output,
       &running_sub_func_idx, &max_len_match_idx](ParseFuncs&... funcs) {
        (
            [this, &flg_continue, &output, &max_len_match_output,
             &running_sub_func_idx, &max_len_match_idx](auto& func) {
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
                    this->valid_outputs_.append(local_output);
                    return;
                  }
                }
                this->valid_outputs_.append(local_output);
              } else {
                output.concat(std::move(local_output), false);
              }
              running_sub_func_idx++;
            }(funcs),
            ...);
      },
      this->parse_functions_);
  if (!this->valid_outputs_.empty()) {
    std::sort(this->valid_outputs_.begin(), this->valid_outputs_.end(),
              [](const ParseFunctionOutputs& a, const ParseFunctionOutputs& b) {
                return a.len_ < b.len_;
              });
    // for (const auto& a : valid_outputs) {
    //   this->context_->save(this->cur_token(), a.cur_token_, this->kind(),
    //                        a.line_, a.len_);
    // }
    this->opt_idx(this->valid_outputs_.size() - 2);
    diag::infos() << basic::str::from(
        std::string(this->calling_depth(), '>'), ":", this->calling_depth(),
        " ", this->kName_,
        basic::tui::color::Shell::colorize(
            basic::str::from(" ok, matched idx = ", max_len_match_idx, ".\n"),
            basic::tui::color::Shell::fgreen()));
  } else {
    this->opt_idx(-1);
  }

  output.concat(std::move(max_len_match_output), false);
  return output;
}

template <ParseFunctionKind Kind, typename... ParseFuncs>
void ParallelParseFunctions<Kind, ParseFuncs...>::reset() {
  this->valid_outputs_.clear();
  if (this->cur_token().file_id() == 0) {
    opt_idx_ = 0;
    return;
  }
  auto r = this->context_->saved(this->cur_token(), kind());
  if (r >= 0) {
    opt_idx_ = r;
  } else if (r == -2 || r == -1) {
    opt_idx_ = 0;
  } else {
    unreachable(this->kName_);
  }
  std::apply(
      [](ParseFuncs&... funcs) {
        (
            [](auto& func) {
              func.reset();
            }(funcs),
            ...);
      },
      this->parse_functions_);
}

}  // namespace lps::parser::details
