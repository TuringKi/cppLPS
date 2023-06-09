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
#include "lex/base.h"
#include "src.h"
#include "tu.h"

namespace lps::basic {

FileVisitor::FileVisitor(
    const char* start, const char* end,
    const base::check_eof_callback_type& check_eof_callback, uint32_t file_id)
    : base(start, end, std::move(check_eof_callback), file_id) {
  eof_ = 0;
}

FileVisitor::~FileVisitor() {}
const char* FileVisitor::cur() const {
  if (pos_ > len() || start_ > end_) {
    return &eof_;
  }
  return start_ + pos_;
}

void FileVisitor::vertws_skipping() {
  if (flg_skip_vertws_) {
    skipping();
    return;
  }
  vertws_skip(true);
  skipping();
  vertws_skip(false);
}
void FileVisitor::horzws_skipping() {
  if (flg_skip_horzws_) {
    skipping();
    return;
  }
  horzws_skip(true);
  skipping();
  horzws_skip(false);
}
}  // namespace lps::basic
