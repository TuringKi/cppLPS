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
#include <utility>
#include "vec.h"
#include "vfile.h"

namespace lps::basic {

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
  explicit FileVisitor(const char* start, const char* end,
                       const base::check_eof_callback_type& check_eof_callback,
                       uint32_t file_id = 0)
      : base(start, end, std::move(check_eof_callback), file_id) {}

  [[nodiscard]] const char* cur_() override;
  [[nodiscard]] const char* cur() const override {
    auto a = *this;
    return a.cur_();
  }
  [[nodiscard]] bool same_file(size_t offset) const {
    return start_ + pos_ + offset <= end_;
  }
  char operator[](size_t idx) const { return *(*this + idx); }
  void vertws_skip(bool flg) { flg_skip_vertws_ = flg; }
  void horzws_skip(bool flg) { flg_skip_horzws_ = flg; }
  void ws_skip(bool flg) {
    vertws_skip(flg);
    horzws_skip(flg);
  }

  void vertws_skipping() {
    if (flg_skip_vertws_) {
      next();
      return;
    }
    vertws_skip(true);
    next();
    vertws_skip(false);
  }
  void horzws_skipping() {
    if (flg_skip_horzws_) {
      next();
      return;
    }
    horzws_skip(true);
    next();
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

  bool flg_skip_vertws_{false};
  bool flg_skip_horzws_{false};
};

class File : public vfile::File<char> {
 public:
  using base = vfile::File<char>;
  using type = File;
  using ptr_type = std::unique_ptr<type>;
  using buffer_type = mem::MemoryBuffer<char, 0, SizeType<char>>;
  using buffer_ptr_type = std::unique_ptr<buffer_type>;

  using str_type = StringRef;

  explicit File(const StringRef& path, uint32_t file_id) {
    set(path.data(), file_id);
  }

  explicit File(const char* path, uint32_t file_id) { set(path, file_id); }

  File(File&& file) {
    this->buffer_ = std::move(file.buffer_);
    file.buffer_ = nullptr;
    this->file_id_ = file.file_id_;
    this->first_ = file.first_;
    this->size_ = file.size_;
  }

  static ptr_type create(const StringRef& path, uint32_t file_id) {
    return std::make_unique<type>(path, file_id);
  }

  static ptr_type create(const char* path, uint32_t file_id) {
    return std::make_unique<type>(path, file_id);
  }

  virtual FileVisitor visitor() {
    if (size_ == 0) {
      return FileVisitor(nullptr, nullptr,
                         [](const basic::vfile::Visitor<char>* visitor_ptr) {
                           lps_assert(kTagName, visitor_ptr);
                           throw basic::vfile::Eof();
                         });
    }
    lps_assert(kTagName, first_ != nullptr);

    return FileVisitor(
        first_, first_ + size_ - 1,
        [this](const basic::vfile::Visitor<char>* visitor_ptr) {
          lps_assert(kTagName, visitor_ptr);
          throw basic::vfile::Eof();
        },
        file_id_);
  }

  StringRef ref() {
    if (size_ == 0) {
      return StringRef();
    }
    lps_assert("file", buffer_->top());
    return StringRef(buffer_->top(), buffer_->capacity());
  }

  [[nodiscard]] const std::filesystem::path& path() const { return path_; }

 private:
  size_t set(const char* path, uint32_t file_id) {
    std::ifstream the_file(path);
    LPS_CHECK_ERROR("file", the_file.is_open(),
                    "the path is not exists:", path);
    path_ = std::filesystem::path(path);
    the_file.seekg(0, std::ios::end);
    std::streamsize size = the_file.tellg();
    if (size == 0) {
      buffer_ = nullptr;
      this->first_ = nullptr;
      this->size_ = 0;
      this->file_id_ = file_id;
      return size;
    }
    the_file.seekg(0, std::ios::beg);
    buffer_ = buffer_type::create("file", size);
    the_file.read(buffer_->top(), size);

    this->first_ = buffer_->top();
    this->size_ = size;
    this->file_id_ = file_id;

    return size;
  }

  buffer_ptr_type buffer_;
  std::filesystem::path path_;
};

}  // namespace lps::basic
