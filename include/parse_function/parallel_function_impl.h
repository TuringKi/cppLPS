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

#include "diag.h"
#include "parse_function/parallel_serial_function.h"
#include "token.h"

namespace lps::parser::details {

template <meta::Str TagName, size_t NumElements, typename... ParseFuncs>
ParseFunctionOutputs<TagName>
ParallelParseFunctions<TagName, NumElements, ParseFuncs...>::operator()() {
  lps_assert(TagName, this->ok_to_try());
  ParseFunctionOutputs<TagName> output;
  output.last_token_ = this->last_token();
  output.cur_token_ = this->cur_token();

  bool flg_continue = true;
  uint32_t running_sub_func_idx = 0;
  ParseFunctionOutputs<TagName> max_len_match_output;
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
                  if (local_output.len() > max_len_match_output.len()) {
                    max_len_match_output = std::move(local_output);
                    max_len_match_idx = running_sub_func_idx;
                  }
                  LPS_NOTE(TagName, " [ ", func.tag(),
                           " ] local_output.work = ", local_output.work_);
                } else {
                  max_len_match_output.concat(std::move(local_output), false);
                  LPS_NOTE(TagName, " [ ", func.tag(),
                           " ] output.work = ", output.work_);
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

    static int space = 0;
    LPS_NOTE(TagName, std::string(space++, ' '), "output ok");
  } else {
    this->executed_mask_.set();
  }
  output.concat(std::move(max_len_match_output), false);
  return output;
}

}  // namespace lps::parser::details
