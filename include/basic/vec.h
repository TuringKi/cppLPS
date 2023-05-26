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
#include <cstring>
#include <iterator>
#include <memory>
#include <type_traits>
#include "basic/exception.h"
#include "basic/mem.h"

template <typename T>
using SizeType =
    std::conditional_t<sizeof(T) < 4 && sizeof(void*) >= 8, uint64_t, uint32_t>;

namespace lps::basic::vec::details {

template <meta::Str TagName, typename T>
class Base : virtual public mem::TraceTag<TagName> {
  using pointer = T*;
  using const_pointer = const T*;
  using Size = SizeType<T>;

 public:
  Base() = default;

  [[nodiscard]] size_t size() const { return size_; }
  void size(size_t n) { size_ = n; }
  [[nodiscard]] size_t capacity() const { return capacity_; }
  [[nodiscard]] bool empty() const { return !size_; }

  pointer first_;
  Size size_{0};
  Size capacity_;
};

template <meta::Str TagName, typename T>
class ConstCommon : public Base<TagName, T> {
 public:
  using base_type = Base<TagName, T>;
  using const_iterator = const T*;
  using const_reference = const T&;
  using const_pointer = const T*;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;
  using size_type = size_t;

  ConstCommon() : base_type() {}

  const_iterator begin() const { return this->first_; }
  const_iterator end() const { return this->first_ + this->size(); }

  const_reverse_iterator rbegin() const {
    return const_reverse_iterator(end());
  }
  const_reverse_iterator rend() const {
    return const_reverse_iterator(begin());
  }

  const_pointer data() const { return const_pointer(begin()); }

  const_reference operator[](size_type idx) const {
    lps_assert(TagName, idx < this->size());
    return begin()[idx];
  }

  const_reference front() const {
    lps_assert(TagName, !this->empty());
    return begin()[0];
  }

  const_reference back() const {
    lps_assert(TagName, !this->empty());
    return end()[-1];
  }
};

template <meta::Str TagName, typename T>
class Common : public Base<TagName, T> {
 public:
  using base_type = Base<TagName, T>;
  using iterator = T*;
  using reference = T&;
  using pointer = T*;
  using rvalue_reference = T&&;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using size_type = size_t;
  using const_iterator = const T*;
  using const_reference = const T&;
  using const_pointer = const T*;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;

  Common() : base_type() {}

  iterator begin() { return this->first_; }
  iterator end() { return this->first_ + this->size(); }

  reverse_iterator rbegin() { return reverse_iterator(end()); }
  reverse_iterator rend() { return reverse_iterator(begin()); }

  pointer data() { return pointer(begin()); }

  reference operator[](size_type idx) {
    lps_assert(TagName, idx < this->size());
    return begin()[idx];
  }

  reference front() {
    lps_assert(TagName, !this->empty());
    return begin()[0];
  }

  reference back() {
    lps_assert(TagName, !this->empty());
    return end()[-1];
  }

  const_iterator begin() const { return this->first_; }
  const_iterator end() const { return this->first_ + this->size(); }

  const_reverse_iterator rbegin() const {
    return const_reverse_iterator(end());
  }
  const_reverse_iterator rend() const {
    return const_reverse_iterator(begin());
  }

  const_pointer data() const { return const_pointer(begin()); }

  const_reference operator[](size_type idx) const {
    lps_assert(TagName, idx < this->size());
    return begin()[idx];
  }

  const_reference front() const {
    lps_assert(TagName, !this->empty());
    return begin()[0];
  }

  const_reference back() const {
    lps_assert(TagName, !this->empty());
    return end()[-1];
  }

  void clear() { this->size_ = 0; }
};

template <meta::Str TagName, typename T>
class TemplateCommon : public Common<TagName, T> {
  using common = Common<TagName, T>;

 public:
  inline void append(typename common::const_reference a) {
    need_grow();
    *(this->end()) = a;
    size_inc();
  }

  inline void append(typename common::rvalue_reference a) {
    need_grow();
    *(this->end()) = std::forward<T>(a);
    size_inc();
  }

 protected:
  TemplateCommon() : common() {}

  inline void need_grow() {
    if (this->size() >= this->capacity()) {
      grow();
    }
  }
  inline void size_inc() { this->size(this->size() + 1); }
  virtual void grow() = 0;
};

template <meta::Str TagName, size_t N, typename T>
class Impl : public TemplateCommon<TagName, T> {
  using base_type = TemplateCommon<TagName, T>;
  using buffer_type =
      mem::MemoryBuffer<T, N, SizeType<T>, TagName + "_memory_buffer">;
  using buffer_ptr_type = std::unique_ptr<buffer_type>;
  using block_type = mem::Block<TagName + "_block", SizeType<T>, T>;
  using block_ptr_type = std::unique_ptr<block_type>;

 public:
  void release() {
    this->size_ = 0;
    this->buffer_ = nullptr;
    this->capacity_ = 0;
    this->first_ = nullptr;
  }

 protected:
  explicit Impl(const T (&d)[N]) : base_type() {
    buffer_ = Impl<TagName, N, T>::buffer_type::create();
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
    this->size_ = N;
    lps_assert(TagName, this->data() != nullptr);
    std::copy_n(d, N, this->data());
  }
  Impl& operator=(const T (&d)[N]) {
    this->size_ = N;
    this->need_grow();
    lps_assert(TagName, this->data() != nullptr);
    std::copy_n(d, N, this->data());
    return *this;
  }

  void grow() override {
    buffer_->grow();
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
  }

  Impl() : base_type() { init(); }

  void init() {
    buffer_ = Impl<TagName, N, T>::buffer_type::create();
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
    this->size_ = 0;
  }

  buffer_ptr_type buffer_{nullptr};
};

}  // namespace lps::basic::vec::details

namespace lps::basic {
template <size_t N, typename T, meta::Str TagName = meta::S("vector")>
class Vector : public vec::details::Impl<TagName, N, T> {
 public:
  using base_type = vec::details::Impl<TagName, N, T>;
  using common = vec::details::Common<TagName, T>;
  using ele_type = T;
#define SET()                           \
  this->buffer_ = std::move(v.buffer_); \
  this->first_ = this->buffer_->top();  \
  this->capacity_ = v.capacity_;        \
  this->size_ = v.size_;                \
  v.release()

  explicit Vector() : base_type() {}
  Vector(Vector&& v) noexcept {
    SET();
  }
  template <meta::Str TagNameOther>
  explicit Vector(Vector<N, T, TagNameOther>&& v) noexcept {
    SET();
  }

  template <meta::Str TagNameOther>
  explicit Vector(const Vector<N, T, TagNameOther>& v) : base_type() {
    for (const T& a : v) {
      this->append(a);
    }
  }

  template <typename T2, meta::Str TagNameOther>
  explicit Vector(const Vector<N, T2, TagNameOther>& v) : base_type() {
    for (const T2& a : v) {
      ele_type b(a);
      this->append(std::move(b));
    }
  }

  template <typename T2, meta::Str TagNameOther>
  explicit Vector(Vector<N, T2, TagNameOther>&& v) noexcept : base_type() {
    for (const T2& a : v) {
      ele_type b(std::move(a));
      this->append(std::move(b));
    }
  }

  Vector& operator=(const Vector& v) {
    this->init();
    for (const T& a : v) {
      this->append(a);
    }
    return *this;
  }

  template <meta::Str TagNameOther>
  Vector& operator=(const Vector<N, T, TagNameOther>& v) {
    this->init();
    for (const T& a : v) {
      this->append(a);
    }
    return *this;
  }

  template <typename T2, meta::Str TagNameOther>
  Vector& operator=(const Vector<N, T2, TagNameOther>& v) {
    this->init();
    for (const T2& a : v) {
      ele_type b(a);
      this->append(std::move(b));
    }
    return *this;
  }

  Vector& operator=(Vector&& v) noexcept {
    SET();
    return *this;
  }

  template <meta::Str TagNameOther>
  Vector& operator=(Vector<N, T, TagNameOther>&& v) noexcept {
    SET();
    return *this;
  }

  template <typename T2, meta::Str TagNameOther>
  Vector& operator=(Vector<N, T2, TagNameOther>&& v) noexcept {
    this->init();
    for (const T2& a : v) {
      ele_type b(std::move(a));
      this->append(std::move(b));
    }
    return *this;
  }

#undef SET
  explicit Vector(const T (&d)[N]) : base_type(d) {}
  Vector& operator=(const T (&d)[N]) {
    base_type::operator=(d);
    return *this;
  }
};

template <size_t N, meta::Str TagName = meta::S("string")>
class String : public Vector<N, char, TagName> {
  using base_type = Vector<N, char, TagName>;

 public:
  explicit String() : base_type() {}
  String(String&& a) noexcept : base_type(a) {}
  explicit String(const char (&d)[N]) : base_type(d) {}
  String& operator=(const char (&d)[N]) {
    base_type::operator=(d);
    return *this;
  }
};

template <typename T, meta::Str TagName>
class StaticVector : public vec::details::ConstCommon<TagName, T> {
  using base_type = vec::details::ConstCommon<TagName, char>;
  using buffer_type =
      mem::MemoryBuffer<T, 0, SizeType<T>, TagName + "_memory_buffer">;
  using buffer_ptr_type = std::unique_ptr<buffer_type>;

 public:
  explicit StaticVector() {
    this->buffer_ = nullptr;
    this->first_ = nullptr;
    this->size_ = 0;
    this->capacity_ = 0;
  }

  template <size_t N, meta::Str TagName1>
  static StaticVector from(const Vector<N, T, TagName1>& vec) {
    return StaticVector(vec);
  }

  template <size_t N>
  static StaticVector from(const T (&d)[N]) {
    return StaticVector(d);
  }

  explicit StaticVector(const T& val, size_t N) : base_type() {
    lps_assert(TagName, N >= 1);
    buffer_ = buffer_type::create(N);
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
    this->size_ = N;
    lps_assert(TagName, this->first_);
    for (int i = 0; i < N; i++) {
      *(this->first_ + i) = val;
    }
  }

  StaticVector& operator=(StaticVector&& v) noexcept {
    this->buffer_ = std::move(v.buffer_);
    this->first_ = this->buffer_->top();
    this->capacity_ = v.capacity_;
    this->size_ = v.size_;
    v.buffer_ = nullptr;
    v.first_ = nullptr;
    v.size_ = 0;
    v.capacity_ = 0;
    return *this;
  }

  StaticVector(StaticVector&& v) noexcept {
    this->buffer_ = std::move(v.buffer_);
    this->first_ = this->buffer_->top();
    this->capacity_ = v.capacity_;
    this->size_ = v.size_;
    v.buffer_ = nullptr;
    v.first_ = nullptr;
    v.size_ = 0;
    v.capacity_ = 0;
  }

 protected:
  template <size_t N, meta::Str TagName1>
  explicit StaticVector(const Vector<N, T, TagName1>& vec)
      : base_type(), buffer_(buffer_type::create(vec.size())) {
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
    this->size_ = vec.size();
    lps_assert(TagName, vec.data());
    lps_assert(TagName, this->first_);
    std::copy_n(vec.data(), vec.size(), this->first_);
  }

  template <size_t N>
  explicit StaticVector(const T (&d)[N]) : base_type() {
    lps_assert(TagName, N >= 1);
    buffer_ = buffer_type::create(N - 1);
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
    this->size_ = N - 1;
    lps_assert(TagName, this->first_);
    std::copy_n(d, N - 1, this->first_);
  }

  explicit StaticVector(const T* ptr, size_t N) {
    lps_assert(TagName, N >= 1);
    lps_assert(TagName, ptr != nullptr);
    buffer_ = buffer_type::create(N);
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
    this->size_ = N;
    std::copy_n(ptr, N, this->first_);
  }

  buffer_ptr_type buffer_{nullptr};
};

template <meta::Str TagName = meta::S("string")>
class StaticString : public StaticVector<char, TagName> {
  using base_type = StaticVector<char, TagName>;

 public:
  explicit StaticString() : base_type() {}
  StaticString(StaticString&& str) noexcept : base_type(std::move(str)) {}

  explicit StaticString(const char& val, size_t N) : base_type(val, N) {}

  explicit operator std::string() const { return std(); }
  [[nodiscard]] std::string std() const {
    std::string a(this->data(), this->size());
    return a;
  }

  StaticString& operator=(StaticString&& v) noexcept {
    this->buffer_ = std::move(v.buffer_);
    this->first_ = this->buffer_->top();
    this->capacity_ = v.capacity_;
    this->size_ = v.size_;
    v.buffer_ = nullptr;
    v.first_ = nullptr;
    v.size_ = 0;
    v.capacity_ = 0;
    return *this;
  }

  template <size_t N, meta::Str TagName1>
  static StaticString from(const String<N, TagName1>& str) {
    return StaticString(str);
  }

  static StaticString from(const std::string& str) { return StaticString(str); }

  template <size_t N>
  static StaticString from(const char (&d)[N]) {
    return StaticString(d);
  }

 private:
  template <size_t N, meta::Str TagName1>
  explicit StaticString(const String<N, TagName1>& str) : base_type(str) {}
  template <size_t N>
  explicit StaticString(const char (&d)[N]) : base_type(d) {}
  explicit StaticString(const std::string& str)
      : base_type(str.c_str(), str.size()) {}
};

template <meta::Str TagName = "string_ref">
class StringRef : public vec::details::ConstCommon<TagName, char> {
  using base_type = vec::details::ConstCommon<TagName, char>;

 public:
  StringRef& operator=(const StringRef& s) {
    lps_assert(TagName, s.data() != nullptr);
    this->first_ = (char*)s.data();
    this->size_ = s.size();
    this->capacity_ = s.capacity();
    return *this;
  }
  explicit operator std::string() const { return std(); }
  std::string std() {
    std::string a(this->data(), this->size());
    return a;
  }

  bool operator==(const StringRef<TagName>& b) const {
    if (this->empty() || b.empty()) {
      return false;
    }
    if (this->capacity() != b.capacity()) {
      return false;
    }
    return std::strncmp(this->data(), b.data(), this->capacity()) == 0;
  }

  template <size_t N, meta::Str TagNameOther>
  explicit StringRef(const String<N, TagNameOther>& s) : base_type() {
    lps_assert(TagName, s.data() != nullptr);
    this->first_ = s.data();
    this->size_ = s.size();
    this->capacity_ = s.capacity();
  }

  template <meta::Str TagNameOther>
  explicit StringRef(const StringRef<TagNameOther>& s) : base_type() {
    lps_assert(TagName, s.data() != nullptr);
    this->first_ = (char*)s.data();
    this->size_ = s.size();
    this->capacity_ = s.capacity();
  }

  StringRef(const StringRef<TagName>& s) : base_type() {
    lps_assert(TagName, s.data() != nullptr);
    this->first_ = (char*)s.data();
    this->size_ = s.size();
    this->capacity_ = s.capacity();
  }

  template <meta::Str TagNameOther>
  explicit StringRef(StringRef<TagNameOther>&& s) : base_type() {
    lps_assert(TagName, s.data() != nullptr);
    this->first_ = (char*)s.data();
    this->size_ = s.size();
    this->capacity_ = s.capacity();
  }

  StringRef(StringRef<TagName>&& s) noexcept : base_type() {
    lps_assert(TagName, s.data() != nullptr);
    this->first_ = (char*)s.data();
    this->size_ = s.size();
    this->capacity_ = s.capacity();
  }

  explicit StringRef(const char* data, size_t size) : base_type() {
    lps_assert(TagName, data != nullptr);
    this->first_ = (char*)data;
    this->size_ = size;
    this->capacity_ = size;
  }

  template <size_t N>
  explicit StringRef(const char (&s)[N]) : base_type() {
    lps_assert(TagName, N >= 1);
    this->first_ = (char*)s;
    this->size_ = N - 1;
    this->capacity_ = N - 1;
  }

  explicit StringRef() : base_type() {
    this->first_ = nullptr;
    this->size_ = 0;
    this->capacity_ = 0;
  }

  template <meta::Str TagNameOther>
  explicit StringRef(const StaticString<TagNameOther>& s) : base_type() {
    lps_assert(TagNameOther, s.data() != nullptr);
    this->first_ = (char*)s.data();
    this->size_ = s.size();
    this->capacity_ = s.capacity();
  }

 private:
};

template <size_t N, typename T, meta::Str TagName>
std::ostream& operator<<(std::ostream& s, const Vector<N, T, TagName>& arr) {

  if (arr.empty()) {
    return s;
  }
  const size_t max = 32;
  size_t max_idx = std::min(max, arr.size());
  s << "[";
  for (size_t i = 0; i < max_idx; i++) {
    if (i < max_idx - 1)
      s << arr[i] << ", ";
    else {
      if (arr.size() > max_idx) {
        s << arr[i] << ", ...";
      } else {
        s << arr[i];
      }
    }
  }
  s << "]";
  return s;
}

template <meta::Str TagName>
std::ostream& operator<<(std::ostream& s, const StaticString<TagName>& arr) {
  s << std::string(arr.data(), arr.size());
  return s;
}

template <meta::Str TagName>
std::string& operator+=(std::string& s, const StaticString<TagName>& arr) {
  s += std::string(arr.data(), arr.size());
  return s;
}

template <size_t N, meta::Str TagName>
std::ostream& operator<<(std::ostream& s, const String<N, TagName>& arr) {
  s << std::string(arr.data(), arr.size());
  return s;
}

template <meta::Str TagName>
std::ostream& operator<<(std::ostream& s, const StringRef<TagName>& arr) {
  if (arr.empty()) {
    return s;
  }
  s << std::string(arr.data(), arr.size());
  return s;
}

namespace str::ascii::is {

template <meta::Str TagName, bool AllowDollar = false>
inline bool ValidIdent(const StringRef<TagName>& s) {
  if (s.empty() || !IdentStart<AllowDollar>(s[0])) {
    return false;
  }
  for (typename StringRef<TagName>::iterator i = s.begin(), e = s.end(); i != e;
       ++i) {
    if (!IdentContinue<AllowDollar>(*i)) {
      return false;
    }
  }
  return true;
}
}  // namespace str::ascii::is

}  // namespace lps::basic
