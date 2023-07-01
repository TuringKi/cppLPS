
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

#include <functional>
#include <utility>
#include "basic/mem.h"
#include "exception.h"

namespace lps::basic::vfile {

template <typename T>
class Operator {
 public:
  constexpr static mem::TraceTag::tag_type kTagName = "vfile::Operator";
  T operator++(int) {
    T a(*static_cast<T*>(this));
    operator++();
    return a;
  }
  T& operator++() {
    T* a = static_cast<T*>(this);
    a->next();
    return *a;
  }
  T& operator--() {
    T* a = static_cast<T*>(this);
    a->next(-1);
    return *a;
  }
  T operator--(int) {
    Operator<T> a(*this);
    operator++();
    return dynamic_cast<T>(a);
  }
  T& operator+=(size_t idx) {
    T* a = static_cast<T*>(this);
    a->next(idx);
    return *a;
  }

  T operator+(size_t idx) const {
    T a(*static_cast<const T*>(this));
    a.next(idx);
    return a;
  }

  T operator-(size_t idx) const {
    T a(*static_cast<const T*>(this));
    a.next(-idx);
    return a;
  }

  size_t operator-(const T& other) const {
    const T* a = static_cast<const T*>(this);
    lps_assert(kTagName, a->end_ == other.end_);
    return a->cur() - other.cur();
  }
  bool operator>=(const T& other) const {
    const T* a = static_cast<const T*>(this);
    lps_assert(kTagName, a->end_ == other.end_);
    return a->cur() >= other.cur();
  }
  bool operator<=(const T& other) const {
    const T* a = static_cast<const T*>(this);
    lps_assert(kTagName, a->end_ == other.end_);
    return a->cur() <= other.cur();
  }
  bool operator<(const T& other) const {
    const T* a = static_cast<const T*>(this);
    lps_assert(kTagName, a->end_ == other.end_);
    return a->cur() < other.cur();
  }
  bool operator>(const T& other) const {
    const T* a = static_cast<const T*>(this);
    lps_assert(kTagName, a->end_ == other.end_);
    return a->cur() > other->cur();
  }
  bool operator!=(const T& other) const {
    const T* a = static_cast<const T*>(this);
    lps_assert(kTagName, a->end_ == other.end_);
    return a->cur() != other->cur();
  }
  bool operator==(const T& other) const {
    const T* a = static_cast<const T*>(this);
    lps_assert(kTagName, a->end_ == other.end_);
    return a->cur() == other->cur();
  }
};

class Eof : public std::exception {
 public:
  explicit Eof() = default;
};

template <typename VisitedType>
class Visitor {
 public:
  template <typename T>
  friend class Operator;
  constexpr static mem::TraceTag::tag_type kTagName = "vfile::Visitor";
  using check_eof_callback_type = std::function<void()>;
  Visitor(
      const VisitedType* start, const VisitedType* end,
      check_eof_callback_type check_eof_callback = []() {},
      uint32_t file_id = 0)
      : start_(start),
        end_(end),
        check_eof_callback_(std::move(check_eof_callback)),
        file_id_(file_id) {
    lps_assert(kTagName, start <= end);
  }

  bool operator>=(const VisitedType* other) const { return start_ >= other; }
  bool operator<=(const VisitedType* other) const { return start_ <= other; }
  bool operator<(const VisitedType* other) const { return start_ < other; }
  bool operator>(const VisitedType* other) const { return start_ > other; }
  bool operator!=(const VisitedType* other) const { return start_ != other; }
  bool operator==(const VisitedType* other) const { return start_ == other; }

  explicit operator bool() { return !eof() && start_; }

  VisitedType operator*() const { return *cur(); }

  VisitedType operator[](size_t idx) const {
    if ((pos_ + idx) > len() || start_ > end_) {
      check_eof_callback_();
      return eof_;
    }
    return *(start_ + pos_ + idx);
  }

  [[nodiscard]] bool eof() const { return cur() == &eof_; }
  const VisitedType* cur() const {
    if (pos_ > len() || start_ > end_) {
      check_eof_callback_();
      return &eof_;
    }
    return start_ + pos_;
  }

 protected:
  [[nodiscard]] size_t len() const { return end_ - start_; }
  const VisitedType* next(size_t idx = 1) {
    pos_ += idx;
    return cur();
  }
  const VisitedType* start_{nullptr};
  const VisitedType* end_{nullptr};
  size_t pos_{0};
  VisitedType eof_;
  check_eof_callback_type check_eof_callback_;
  uint32_t file_id_{0};
};

template <typename StoredType>
class File {
 public:
  constexpr static mem::TraceTag::tag_type kTagName = "vfile::File";

  bool empty() { return size_ == 0; }
  [[nodiscard]] size_t size() const { return size_; }
  [[nodiscard]] uint32_t file_id() const { return file_id_; }

 protected:
  const StoredType* first_{nullptr};
  size_t size_{0};
  uint32_t file_id_{0};
};

};  // namespace lps::basic::vfile
