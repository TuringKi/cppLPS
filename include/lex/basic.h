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

#include "lex/base.h"
#include "token.h"
#include "tu.h"

namespace lps::lexer::details {

class Basic : public Base {
  using base = Base;

 public:
  static bool jump_include_stack(ptr_type& ptr) {

    auto jump_include_stack_impl = [](ptr_type& ptr, auto func) -> bool {
      if (*ptr != 0) {
        return false;
      }
      if (ptr.file_id() == tu::TU::instance().include_stack_top_file_id()) {
        auto next_file_info = tu::TU::instance().include_stack_top();
        tu::TU::instance().include_stack_pop();
        auto offset = ptr.pos() - ptr.size() - 1;
        ptr = src::Manager::instance().visitor_of_char_file(
            next_file_info.parent_info_.second);
        ptr += next_file_info.parent_info_.first + offset;

        do {
          if (!func(ptr, func)) {
            break;
          }
        } while (true);
        return true;
      }
      return false;
    };

    return jump_include_stack_impl(ptr, jump_include_stack_impl);
  }

  explicit Basic(const typename base::ptr_type& ptr)
      : base(ptr, MethodType::kBasic) {}

  // All the lex processing can be modeled as a state machine.
  // The states are following:
  // *start*: the start of this lex function.
  // *next*: temp state.
  // *skip_comments*: skip comments, these characters are useless for parser.
  // *skip_horz_ws*: skip line-changing characters, like `'\n'`.
  void lex_impl(lps::token::Token& tok) override;

 private:
  inline bool line_comment_skipping() { return false; }
  inline bool block_comment_skipping() { return false; }
};

}  // namespace lps::lexer::details
