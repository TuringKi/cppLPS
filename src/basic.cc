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

#include "basic/file.h"
#include "src.h"
#include "tu.h"

namespace lps::basic {

const char* FileVisitor::cur_() {
  if (pos_ > len() || start_ > end_) {
    if (file_id_ == tu::TU::instance().include_stack_top_file_id()) {
      auto next_file_info = tu::TU::instance().include_stack_top();
      auto offset = pos_ - len() - 1;
      *this = src::Manager::instance().visitor_of_char_file(
          next_file_info.parent_info_.second);
      file_id_ = next_file_info.parent_info_.second;
      pos_ = next_file_info.parent_info_.first + offset;
      return cur_();
    }
    FileVisitor tmp(*this);
    tmp.pos_ = 0;
    this->check_eof_callback_(&tmp);
    return &eof_;
  }
  return start_ + pos_;
}
}  // namespace lps::basic
