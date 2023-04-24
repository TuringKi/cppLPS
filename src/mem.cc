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

#include "mem.h"
#include "excption.h"

namespace lps::basic::mem {

void* malloc(size_t sz) {
  void* result = std::malloc(sz);
  if (result == nullptr) {
    if (sz == 0)
      return lps::basic::mem::malloc(1);
    LPS_ERROR("alloc failed");
  }
  return result;
}

void* realloc(void* ptr, size_t sz) {
  void* result = std::realloc(ptr, sz);
  if (result == nullptr) {
    if (sz == 0)
      return lps::basic::mem::malloc(1);
    LPS_ERROR("alloc failed");
  }
  return result;
}

}  // namespace lps::basic::mem
