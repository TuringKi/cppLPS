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
#include <filesystem>
#include <fstream>
#include "vec.h"
#include "vfile.h"

namespace lps::basic {

class File : public vfile::File<char> {
 public:
  using type = File;
  using ptr_type = std::unique_ptr<type>;
  using buffer_type =
      mem::MemoryBuffer<char, 0, SizeType<char>, meta::S("file_memory_buffer")>;
  using buffer_ptr_type = std::unique_ptr<buffer_type>;
  template <meta::Str TagNameOther>
  using str_type = StringRef<TagNameOther>;

  template <meta::Str TagNameOther>
  explicit File(const StringRef<TagNameOther>& path) {
    set(path.data());
  }

  explicit File(const char* path) { set(path); }

  File(File&& file) {
    this->buffer_ = std::move(file.buffer_);
    file.buffer_ = nullptr;
    this->file_id_ = file.file_id_;
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
    lps_assert(meta::S("file"), buffer_->top());
    return StringRef<TagNameOther>(buffer_->top(), buffer_->capacity());
  }

  [[nodiscard]] const std::filesystem::path& path() const { return path_; }
  [[nodiscard]] uint32_t file_id() const { return file_id_; }

 private:
  size_t set(const char* path) {
    std::ifstream the_file(path);
    LPS_CHECK_ERROR(meta::S("file"), the_file.is_open(),
                    "the path is not exists:", path);
    path_ = std::filesystem::path(path);
    the_file.seekg(0, std::ios::end);
    std::streamsize size = the_file.tellg();
    static uint32_t k_file_id = 0;
    file_id_ = ++k_file_id;
    if (size == 0) {
      buffer_ = buffer_type::create();
      this->first_ = buffer_->top();
      this->size_ = 0;
      return size;
    }
    the_file.seekg(0, std::ios::beg);
    buffer_ = buffer_type::create(size);
    the_file.read(buffer_->top(), size);

    this->first_ = buffer_->top();
    this->size_ = size;

    return size;
  }

  buffer_ptr_type buffer_;
  std::filesystem::path path_;
  uint32_t file_id_;
};

class FileVisitor : public vfile::Visitor<char>,
                    public vfile::Operator<FileVisitor> {

 public:
  using base = vfile::Visitor<char>;
  static int strncmp(const FileVisitor& a, const FileVisitor& b, size_t n) {
    return std::strncmp(a.start_, b.start_, n);
  }
  static int strncmp(const char* a, const char* b, size_t n) {
    return std::strncmp(a, b, n);
  }
  explicit FileVisitor(const char* start, const char* end, uint32_t file_id = 0)
      : base(start, end), file_id_(file_id) {}

  bool operator>=(const char* other) const { return start_ >= other; }
  bool operator<=(const char* other) const { return start_ <= other; }
  bool operator<(const char* other) const { return start_ < other; }
  bool operator>(const char* other) const { return start_ > other; }
  bool operator!=(const char* other) const { return start_ != other; }
  bool operator==(const char* other) const { return start_ == other; }

  void vertws_skip(bool flg) { flg_skip_vertws_ = flg; }
  void horzws_skip(bool flg) { flg_skip_horzws_ = flg; }
  void ws_skip(bool flg) {
    vertws_skip(flg);
    horzws_skip(flg);
  }

  void vertws_skipping() {
    if (flg_skip_vertws_) {
      (*this)++;
      return;
    }
    vertws_skip(true);
    (*this)++;
    vertws_skip(false);
  }
  void horzws_skipping() {
    if (flg_skip_horzws_) {
      (*this)++;
      return;
    }
    horzws_skip(true);
    (*this)++;
    horzws_skip(false);
  }

  void ws_skipping() {
    horzws_skipping();
    vertws_skipping();
  }

 private:
  void skipping() {
    if (flg_skip_vertws_ && flg_skip_horzws_) {
      while (str::ascii::is::Ws(*start_)) {
        next();
      }
    } else if (flg_skip_vertws_) {
      while (str::ascii::is::VertWs(*start_)) {
        next();
      }
    } else if (flg_skip_horzws_) {
      while (str::ascii::is::HorzWs(*start_)) {
        next();
      }
    }
  }

  uint32_t file_id_{0};
  bool flg_skip_vertws_{false};
  bool flg_skip_horzws_{false};
};

}  // namespace lps::basic
