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
#include "basic/exception.h"
#include "vec.h"

namespace lps::basic {

class File {
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

  bool empty() { return buffer_->capacity() == 0; }
  [[nodiscard]] size_t size() const { return buffer_->capacity(); }
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
    if (size == 0) {
      buffer_ = buffer_type::create();
      return size;
    }
    the_file.seekg(0, std::ios::beg);
    buffer_ = buffer_type::create(size);
    the_file.read(buffer_->top(), size);

    static uint32_t k_file_id = 0;
    file_id_ = ++k_file_id;

    return size;
  }

  buffer_ptr_type buffer_;
  std::filesystem::path path_;
  uint32_t file_id_;
};

class FileVisitor {

 public:
  static int strncmp(const FileVisitor& a, const FileVisitor& b, size_t n) {
    return std::strncmp(a.ptr_, b.ptr_, n);
  }
  static int strncmp(const char* a, const char* b, size_t n) {
    return std::strncmp(a, b, n);
  }
  explicit FileVisitor(const char* ptr = nullptr, uint32_t file_id = 0)
      : ptr_(ptr), file_id_(file_id) {}
  [[nodiscard]] const char* ptr() const { return ptr_; }
  FileVisitor& operator++() {
    skipping();
    ptr_++;
    return *this;
  }
  FileVisitor operator++(int) {
    FileVisitor tmp = *this;
    skipping();
    ptr_++;
    return tmp;
  }
  FileVisitor& operator--() {
    ptr_--;
    return *this;
  }
  FileVisitor operator--(int) {
    FileVisitor tmp = *this;
    ptr_--;
    return tmp;
  }

  FileVisitor& operator+=(size_t idx) {
    ptr_ += idx;
    return *this;
  }

  size_t operator-(const FileVisitor& other) const { return ptr_ - other.ptr_; }

  FileVisitor operator+(size_t idx) const {
    FileVisitor tmp;
    tmp.ptr_ = ptr_ + idx;
    return tmp;
  }
  FileVisitor operator-(size_t idx) const {
    FileVisitor tmp;
    tmp.ptr_ = ptr_ - idx;
    return tmp;
  }
  bool operator>=(const FileVisitor& other) const { return ptr_ >= other.ptr_; }
  bool operator<=(const FileVisitor& other) const { return ptr_ <= other.ptr_; }
  bool operator<(const FileVisitor& other) const { return ptr_ < other.ptr_; }
  bool operator>(const FileVisitor& other) const { return ptr_ > other.ptr_; }
  bool operator!=(const FileVisitor& other) const { return ptr_ != other.ptr_; }
  bool operator==(const FileVisitor& other) const { return ptr_ == other.ptr_; }

  bool operator>=(const char* other) const { return ptr_ >= other; }
  bool operator<=(const char* other) const { return ptr_ <= other; }
  bool operator<(const char* other) const { return ptr_ < other; }
  bool operator>(const char* other) const { return ptr_ > other; }
  bool operator!=(const char* other) const { return ptr_ != other; }
  bool operator==(const char* other) const { return ptr_ == other; }

  char operator*() { return *ptr_; }

  char operator[](size_t idx) const { return ptr_[idx]; }

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
      while (str::ascii::is::Ws(*ptr_)) {
        ptr_++;
      }
    } else if (flg_skip_vertws_) {
      while (str::ascii::is::VertWs(*ptr_)) {
        ptr_++;
      }
    } else if (flg_skip_horzws_) {
      while (str::ascii::is::HorzWs(*ptr_)) {
        ptr_++;
      }
    }
  }
  const char* ptr_{nullptr};
  uint32_t file_id_{0};
  bool flg_skip_vertws_{false};
  bool flg_skip_horzws_{false};
};

}  // namespace lps::basic
