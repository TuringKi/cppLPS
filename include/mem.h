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

#include <memory>

namespace lps::basic::mem {

namespace details {
template <auto TagName, size_t N, typename T>
class Buffer {
  using type = Buffer<TagName, N, T>;

 public:
  static std::unique_ptr<type> create() { return std::make_unique(type()); }
  char* ptr() { return data_; }

 private:
  alignas(T) char data_[N * sizeof(T)];
};
}  // namespace details

template <typename Buffer>
class MemoryBuffer {
  using buffer_type = std::unique_ptr<Buffer>;

 public:
  MemoryBuffer() : buffer_(Buffer::create()) {}
  MemoryBuffer(const MemoryBuffer&) = delete;
  MemoryBuffer& operator=(const MemoryBuffer&) = delete;
  virtual ~MemoryBuffer();

 private:
  buffer_type buffer_;
};

void* malloc(size_t sz);
void* realloc(void* ptr, size_t sz);

}  // namespace lps::basic::mem
