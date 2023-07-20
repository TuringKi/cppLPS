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

#include <cstdint>
#include <stack>
#include "diag.h"
#include "parse_function/parallel_serial_recursive_function.h"
#include "parser.h"
#include "token.h"

namespace lps::parser::details {

template <ParseFunctionKind Kind, typename... ParseFuncs>
ParseFunctionOutputs
RecursiveParseFunctions<Kind, ParseFuncs...>::operator()() {
  lps_assert("RecursiveParseFunctions", this->ok_to_try());
  auto output = base::operator()();
  this->opt_idx(-1);
  if (!this->valid()) {
    return output;
  }
  uint32_t recursive_depth = 0;
  working_list_type executed_mask;
  auto tmp_output = output;

  Line::segments_type saved_lines;
  basic::Vector<4, ParseFunctionOutputs> valid_outputs;
  int64_t valid_idx = 0;
  do {
    tmp_output.len_ = 0;
    execute(tmp_output, saved_lines);

    if (!tmp_output.work_) {
      if (valid_idx > 0) {
        --valid_idx;
        while (valid_idx >= 0 && this->valid_outputs_[valid_idx].cur_token_ !=
                                     tmp_output.cur_token_) {
          --valid_idx;
        }
        if (valid_idx >= 0) {
          tmp_output = this->valid_outputs_[valid_idx];
          continue;
        }
      }
      break;
    }

    valid_idx = this->valid_outputs_.size() - 1;
    saved_lines.append(tmp_output.line_);
  } while (true);

  if (!this->valid_outputs_.empty()) {
    this->opt_idx(this->valid_outputs_.size() - 2);
    return this->valid_outputs_.back();
  }
  this->opt_idx(-1);
  return output;
}

template <ParseFunctionKind Kind, typename... ParseFuncs>
void RecursiveParseFunctions<Kind, ParseFuncs...>::reset() {
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

template <ParseFunctionKind Kind, typename... ParseFuncs>
void RecursiveParseFunctions<Kind, ParseFuncs...>::execute(
    ParseFunctionOutputs& output, const Line::segments_type& saved_lines) {
  bool flg_continue = true;
  uint32_t running_sub_func_idx = 0;
  ParseFunctionOutputs max_len_match_output;
  max_len_match_output.last_token_ = this->last_token();
  max_len_match_output.cur_token_ = this->cur_token();
  uint32_t max_len_match_idx = -1;

  std::apply(
      [&output](ParseFuncs&... funcs) {
        (
            [&output](auto& func) {
              func.last_token(output.last_token_);
              func.cur_token(output.cur_token_);
            }(funcs),
            ...);
      },
      this->parse_functions_);

  reset();

  basic::Vector<4, ParseFunctionOutputs> local_saved_outputs;

  std::apply(
      [this, &flg_continue, &output, &max_len_match_output,
       &running_sub_func_idx, &max_len_match_idx,
       &local_saved_outputs](ParseFuncs&... funcs) {
        (
            [this, &flg_continue, &output, &max_len_match_output,
             &running_sub_func_idx, &max_len_match_idx,
             &local_saved_outputs](auto& func) {
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
                    local_saved_outputs.append(local_output);
                    return;
                  }
                }
                local_saved_outputs.append(local_output);
              } else {
                output.concat(std::move(local_output), false);
              }
              running_sub_func_idx++;
            }(funcs),
            ...);
      },
      this->parse_functions_);
  if (max_len_match_output.work_) {
    diag::infos() << basic::str::from(
        std::string(this->calling_depth(), '>'), ":", this->calling_depth(),
        " ", this->kName_,
        basic::tui::color::Shell::colorize(
            basic::str::from(" ok, matched idx = ", max_len_match_idx, ".\n"),
            basic::tui::color::Shell::fgreen()));
  } else {
  }
  if (!local_saved_outputs.empty()) {
    std::sort(local_saved_outputs.begin(), local_saved_outputs.end(),
              [](const ParseFunctionOutputs& a, const ParseFunctionOutputs& b) {
                return a.len_ < b.len_;
              });

    for (auto& a : local_saved_outputs) {
      Line::segments_type tmp_saved_lines(saved_lines);
      tmp_saved_lines.append(a.line_);
      const auto* p_start = &context_->token_lists().at(this->cur_token());
      const auto* p_end = a.line_->end_;
      Line line{
          p_start,
          p_end,
          this->kind(),
          token::details::TokenKind::unknown,
          token::TokenLists::len(p_start, p_end),
          this->calling_depth(),
          std::move(tmp_saved_lines),
      };
      a.line_ = context_->paint(line);
      this->valid_outputs_.append(a);
    }
  }
  output.concat(std::move(max_len_match_output), false);
}

}  // namespace lps::parser::details
