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
#include <fstream>
#include "basic/exception.h"
#include "vec.h"

namespace lps::basic {

template <meta::Str TagName = "file">
class File {

  using type = File<TagName>;
  using ptr_type = std::unique_ptr<type>;
  using buffer_type =
      mem::MemoryBuffer<char, 0, SizeType<char>, TagName + "_memory_buffer">;
  using buffer_ptr_type = std::unique_ptr<buffer_type>;
  template <meta::Str TagNameOther>
  using str_type = StringRef<TagNameOther>;

 public:
  template <meta::Str TagNameOther>
  explicit File(const StringRef<TagNameOther>& path) {
    set(path.data());
  }

  explicit File(const char* path) { set(path); }

  File(File&& file) noexcept {
    this->buffer_ = std::move(file.buffer_);
    file.buffer_ = nullptr;
  }

  template <meta::Str TagNameOther>
  static ptr_type create(const StringRef<TagNameOther>& path) {
    return std::make_unique<type>(path);
  }

  static ptr_type create(const char* path) {
    return std::make_unique<type>(path);
  }

  template <meta::Str TagNameOther>
  StringRef<TagNameOther> ref() {
    lps_assert(TagName, buffer_->top());
    return StringRef<TagNameOther>(buffer_->top(), buffer_->capacity());
  }

  bool empty() { return buffer_->capacity() == 0; }

 private:
  size_t set(const char* path) {
    std::ifstream the_file(path);
    LPS_CHECK_ERROR(TagName, the_file.is_open(),
                    "the path is not exists:", path);

    the_file.seekg(0, std::ios::end);
    std::streamsize size = the_file.tellg();
    if (size == 0) {
      buffer_ = buffer_type::create();
      return size;
    }
    the_file.seekg(0, std::ios::beg);
    buffer_ = buffer_type::create(size);
    the_file.read(buffer_->top(), size);
    return size;
  }

  buffer_ptr_type buffer_;
};

}  // namespace lps::basic
