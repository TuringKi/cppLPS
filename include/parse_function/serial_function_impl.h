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

template <ParseFunctionKind Kind, typename... ParseFuncs>
ParseFunctionOutputs SerialParseFunctions<Kind, ParseFuncs...>::operator()() {
  lps_assert("SerialParseFunctions", this->ok_to_try());
  this->executed_mask_.set();
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

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

  Line::segments_type saved_lines(running_sub_func_size, nullptr);

  std::stack<decltype(output)> path_stack;
  path_stack.push(output);

  while (flg_continue) {
    std::apply(
        [this, &running_sub_func_size, &path_stack, &flg_continue,
         &running_sub_func_stack_top_idx, &saved_lines](ParseFuncs&... funcs) {
          bool flg_not_continue = false;
          uint32_t running_sub_func_idx = 0;
          (
              [this, &running_sub_func_size, &path_stack, &flg_not_continue,
               &flg_continue, &running_sub_func_idx,
               &running_sub_func_stack_top_idx, &saved_lines](auto& func) {
                if (flg_not_continue || !flg_continue) {
                  return;
                }
                if (running_sub_func_idx++ < running_sub_func_stack_top_idx) {
                  return;
                }
                auto make_try_again = [&]() {
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
                  lps_assert("make_try_again", path_stack.size() > 0);

                  path_stack.pop();

                  path_stack.top().concat(std::move(tmp_output), false);
                  return;
                };
                if (flg_continue) {
                  func.last_token(path_stack.top().last_token_);
                  func.cur_token(path_stack.top().cur_token_);

                  if (!func.ok_to_try() || (!func.valid() && !func.opt())) {
                    make_try_again();
                    return;
                  }

                  auto local_output = func();
                  if (!local_output.work_ && !func.opt()) {
                    make_try_again();
                    return;
                  }
                  if (local_output.work_) {
                    lps_assert(this->kName_, local_output.line_ != nullptr);
                    saved_lines[running_sub_func_idx - 1] = local_output.line_;
                  }

                  auto output(path_stack.top());
                  output.concat(std::move(local_output), func.opt());
                  path_stack.push(output);
                  if ((running_sub_func_idx == running_sub_func_size &&
                       output.work_) ||
                      !func.valid()) {
                    flg_continue = false;
                    return;
                  }
                }
              }(funcs),
              ...);
        },
        parse_functions_);
  }
  lps_assert(this->kName_, path_stack.size() > 0);
  if (path_stack.top().work_) {
    const auto* p_start = &context_->token_lists().at(this->cur_token());
    const auto* p_end =
        &context_->token_lists().at(path_stack.top().cur_token_);
    Line line{
        p_start,
        p_end,
        this->kind(),
        token::details::TokenKind::unknown,
        token::TokenLists::len(p_start, p_end),
        this->calling_depth(),
        std::move(saved_lines),
    };
    path_stack.top().line_ = context_->paint(line);

    diag::infos() << basic::str::from(
        std::string(this->calling_depth(), '>'), " ", this->kName_,
        basic::tui::color::Shell::colorize(basic::str::from(" ok.\n"),
                                           basic::tui::color::Shell::fgreen()));
  } else {
    diag::infos() << basic::str::from(
        std::string(this->calling_depth(), '>'), " ", this->kName_,
        basic::tui::color::Shell::colorize(basic::str::from(" failed\n"),
                                           basic::tui::color::Shell::fred()));
  }
  return path_stack.top();
}

template <ParseFunctionKind Kind, typename... ParseFuncs>
void SerialParseFunctions<Kind, ParseFuncs...>::reset() {
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
