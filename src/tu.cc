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

#include "tu.h"
#include "basic/exception.h"
#include "basic/mem.h"
#include "src.h"
#include "token.h"

namespace lps::tu {

uint32_t TU::record_expanded_tokens_as_virtual_file(
    const char* cur_tok_data_ptr, uint32_t cur_tok_file_id,
    token::TokenContainer::tokens_type&& tokens) {
  constexpr basic::mem::TraceTag::tag_type kTag =
      "record_expanded_tokens_as_virtual_file";
  return src::Manager::instance().append(std::move(tokens));
}

token::TokenListsVisitor TU::get_visitor_of_token_file(uint32_t file_id) {
  lps_assert(kTag, src::Manager::instance().template has<1>(file_id));
  return src::Manager::instance().visitor_of_token_file(file_id);
}

uint32_t TU::record_include_as_char_file(const char* path) {
  return src::Manager::instance().append(path);
}

basic::FileVisitor TU::get_visitor_of_char_file(uint32_t file_id) {
  lps_assert(kTag, src::Manager::instance().template has<0>(file_id));
  return src::Manager::instance().visitor_of_char_file(file_id);
}

}  // namespace lps::tu
