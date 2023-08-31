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
using SizeType = std::conditional_t<sizeof(void*) >= 8, uint64_t, uint32_t>;

namespace lps::basic::vec::details {

template <typename T>
class Base : public mem::TraceTag {
  using pointer = T*;
  using const_pointer = const T*;
  using Size = SizeType<T>;

 public:
  explicit constexpr Base(basic::mem::TraceTag::tag_type tag)
      : basic::mem::TraceTag(tag) {}
  explicit constexpr Base() : basic::mem::TraceTag("vec::details::Base") {}
  [[nodiscard]] size_t size() const { return size_; }
  void size(size_t n) { size_ = n; }
  [[nodiscard]] size_t capacity() const { return capacity_; }
  [[nodiscard]] bool empty() const { return size_ == 0U; }

  pointer first_;
  Size size_{0};
  Size capacity_;
};

template <typename T>
class ConstCommon : public Base<T> {
 public:
  using base_type = Base<T>;
  using const_iterator = const T*;
  using const_reference = const T&;
  using const_pointer = const T*;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;
  using size_type = size_t;

  constexpr explicit ConstCommon(basic::mem::TraceTag::tag_type tag)
      : base_type(tag) {}

  constexpr explicit ConstCommon() = default;

  [[nodiscard]] const_iterator begin() const { return this->first_; }
  [[nodiscard]] const_iterator end() const {
    return this->first_ + this->size();
  }

  [[nodiscard]] const_reverse_iterator rbegin() const {
    return const_reverse_iterator(end());
  }
  [[nodiscard]] const_reverse_iterator rend() const {
    return const_reverse_iterator(begin());
  }

  [[nodiscard]] const_pointer data() const {
    return static_cast<const_pointer>(begin());
  }

  const_reference operator[](size_type idx) const {
    lps_assert(this->tag_, idx < this->size());
    return begin()[idx];
  }

  [[nodiscard]] const_reference front() const {
    lps_assert(this->tag_, !this->empty());
    return begin()[0];
  }

  [[nodiscard]] const_reference back() const {
    lps_assert(this->tag_, !this->empty());
    return end()[-1];
  }
};

template <typename T>
class Common : public Base<T> {
 public:
  using base_type = Base<T>;
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

  Common(basic::mem::TraceTag::tag_type tag) : base_type(tag) {}

  iterator begin() { return this->first_; }
  iterator end() { return this->first_ + this->size(); }

  reverse_iterator rbegin() { return reverse_iterator(end()); }
  reverse_iterator rend() { return reverse_iterator(begin()); }

  pointer data() { return pointer(begin()); }

  reference operator[](size_type idx) {
    lps_assert(this->tag_, idx < this->size());
    return begin()[idx];
  }

  reference front() {
    lps_assert(this->tag_, !this->empty());
    return begin()[0];
  }

  reference back() {
    lps_assert(this->tag_, !this->empty());
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
    lps_assert(this->tag_, idx < this->size());
    return begin()[idx];
  }

  const_reference front() const {
    lps_assert(this->tag_, !this->empty());
    return begin()[0];
  }

  const_reference back() const {
    lps_assert(this->tag_, !this->empty());
    return end()[-1];
  }

  void clear() { this->size_ = 0; }
};

template <typename T>
class TemplateCommon : public Common<T> {
  using common = Common<T>;

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

  inline void append(uint64_t sz, typename common::const_reference a) {
    // fixme(@mxlol233): it's too slow!!
    for (uint64_t i = 0; i < sz; i++) {
      append(a);
    }
  }

  inline typename common::iterator erase(typename common::const_iterator cs,
                                         typename common::const_iterator ce) {
    auto s = const_cast<typename common::iterator>(cs);
    auto e = const_cast<typename common::iterator>(ce);

    LPS_CHECK_ERROR(this->tag_, is_range_in_storage(s, e),
                    "range to erase is out of bounds.");

    typename common::iterator n = s;
    auto i = std::move(e, this->end(), s);
    destroy_range(i, this->end());
    this->size_ = i - this->begin();
    return (n);
  }

 protected:
  bool is_range_in_storage(const void* first, const void* last) const {
    std::less<> lt;
    return !lt(first, this->begin()) && !lt(last, first) &&
           !lt(this->end(), last);
  }

  static void destroy_range(T* s, T* e) {
    while (s != e) {
      --e;
      e->~T();
    }
  }

  TemplateCommon(basic::mem::TraceTag::tag_type tag) : common(tag) {}

  inline void need_grow() {
    if (this->size() >= this->capacity()) {
      grow();
    }
  }
  inline void size_inc() { this->size(this->size() + 1); }
  virtual void grow() = 0;
};

template <size_t N, typename T>
class Impl : public TemplateCommon<T> {
  using base_type = TemplateCommon<T>;
  using buffer_type = mem::MemoryBuffer<T, N, SizeType<T>>;
  using buffer_ptr_type = std::unique_ptr<buffer_type>;
  using block_type = mem::Block<SizeType<T>, T>;
  using block_ptr_type = std::unique_ptr<block_type>;

 public:
  void release() {
    this->size_ = 0;
    this->buffer_ = nullptr;
    this->capacity_ = 0;
    this->first_ = nullptr;
  }

 protected:
  explicit Impl(basic::mem::TraceTag::tag_type tag, const T (&d)[N])
      : base_type(tag) {
    buffer_ = Impl<N, T>::buffer_type::create(tag);
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
    this->size_ = N;
    lps_assert(this->tag_, this->data() != nullptr);
    std::copy_n(d, N, this->data());
  }
  Impl& operator=(const T (&d)[N]) {
    this->size_ = N;
    this->need_grow();
    lps_assert(this->tag_, this->data() != nullptr);
    std::copy_n(d, N, this->data());
    return *this;
  }

  void grow() override {
    buffer_->grow();
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
  }

  Impl(basic::mem::TraceTag::tag_type tag) : base_type(tag) { init(tag); }

  void init(basic::mem::TraceTag::tag_type tag) {
    buffer_ = Impl<N, T>::buffer_type::create(tag);
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
    this->size_ = 0;
  }

  buffer_ptr_type buffer_{nullptr};
};

}  // namespace lps::basic::vec::details

namespace lps::basic {
template <size_t N, typename T>
class Vector : public vec::details::Impl<N, T> {
 public:
  using base_type = vec::details::Impl<N, T>;
  using common = vec::details::Common<T>;
  using ele_type = T;
#define SET()                           \
  this->buffer_ = std::move(v.buffer_); \
  this->first_ = this->buffer_->top();  \
  this->capacity_ = v.capacity_;        \
  this->size_ = v.size_;                \
  this->tag_ = v.tag_;                  \
  v.release()

  explicit Vector() : base_type("Vector") {}
  Vector(Vector&& v) : base_type("Vector") {
    SET();
  }

  Vector(size_t N_hat, const ele_type& val = ele_type()) : base_type("Vector") {
    for (size_t i = 0; i < N_hat; i++) {
      this->append(val);
    }
  }

  explicit Vector(const Vector& v) : base_type(v.tag_) {
    for (const T& a : v) {
      this->append(a);
    }
  }

  Vector& operator=(const Vector& v) {
    this->init(v.tag_);
    for (const T& a : v) {
      this->append(a);
    }
    return *this;
  }

  Vector& operator=(Vector&& v) {
    this->init(v.tag_);
    SET();
    return *this;
  }

  bool operator==(const Vector& v) const {
    if (this->size() != v.size()) {
      return false;
    }
    for (size_t i = 0; i < this->size_; ++i) {
      if (this->first_[i] != v.first_[i]) {
        return false;
      }
    }
    return true;
  }

  bool operator!=(const Vector& v) const {
    if (this->size() != v.size()) {
      return true;
    }
    for (size_t i = 0; i < this->size_; ++i) {
      if (this->first_[i] != v.first_[i]) {
        return true;
      }
    }
    return false;
  }

#undef SET
  explicit Vector(const T (&d)[N]) : base_type(d) {}
  Vector& operator=(const T (&d)[N]) {
    base_type::operator=(d);
    return *this;
  }
};

template <size_t N>
class String : public Vector<N, char> {
  using base_type = Vector<N, char>;

 public:
  explicit String() : base_type() {}
  String(String&& a) : base_type(a) {}
  explicit String(const char (&d)[N]) : base_type(d) {}
  String& operator=(const char (&d)[N]) {
    base_type::operator=(d);
    return *this;
  }
  explicit operator std::string() const { return std(); }
  [[nodiscard]] std::string std() const {
    std::string a(this->data(), this->size());
    return a;
  }
};

template <typename T>
class StaticVector : public vec::details::ConstCommon<T> {
  using base_type = vec::details::ConstCommon<char>;
  using buffer_type = mem::MemoryBuffer<T, 0, SizeType<T>>;
  using buffer_ptr_type = std::unique_ptr<buffer_type>;

 public:
  explicit StaticVector() : base_type("StaticVector") {
    this->buffer_ = nullptr;
    this->first_ = nullptr;
    this->size_ = 0;
    this->capacity_ = 0;
  }

  template <size_t N>
  static StaticVector from(const Vector<N, T>& vec) {
    return StaticVector(vec);
  }

  template <size_t N>
  static StaticVector from(const T (&d)[N]) {
    return StaticVector(d);
  }

  explicit StaticVector(const T& val, size_t N) : base_type("StaticVector") {
    lps_assert(this->tag_, N >= 1);
    buffer_ = buffer_type::create(this->tag_, N);
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
    this->size_ = N;
    lps_assert(this->tag_, this->first_);
    for (int i = 0; i < N; i++) {
      *(this->first_ + i) = val;
    }
  }

  StaticVector& operator=(StaticVector&& v) {
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

  StaticVector(StaticVector&& v) : base_type("StaticVector") {
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
  template <size_t N>
  explicit StaticVector(const Vector<N, T>& vec)
      : base_type("StaticVector"),
        buffer_(buffer_type::create("StaticVector", vec.size())) {
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
    this->size_ = vec.size();
    lps_assert(this->tag_, vec.data());
    lps_assert(this->tag_, this->first_);
    std::copy_n(vec.data(), vec.size(), this->first_);
  }

  template <size_t N>
  explicit StaticVector(const T (&d)[N]) : base_type("StaticVector") {
    lps_assert(this->tag_, N >= 1);
    buffer_ = buffer_type::create("StaticVector", N - 1);
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
    this->size_ = N - 1;
    lps_assert(this->tag_, this->first_);
    std::copy_n(d, N - 1, this->first_);
  }

  explicit StaticVector(const T* ptr, size_t N) : base_type("StaticVector") {
    lps_assert(this->tag_, N >= 1);
    lps_assert(this->tag_, ptr != nullptr);
    buffer_ = buffer_type::create(this->tag_, N);
    this->first_ = buffer_->top();
    this->capacity_ = buffer_->capacity();
    this->size_ = N;
    std::copy_n(ptr, N, this->first_);
  }

  buffer_ptr_type buffer_{nullptr};
};

class StaticString : public StaticVector<char> {
  using base_type = StaticVector<char>;

 public:
  explicit StaticString() {}
  StaticString(StaticString&& str) : base_type(std::move(str)) {}

  explicit StaticString(const char& val, size_t N) : base_type(val, N) {}

  explicit operator std::string() const { return std(); }
  [[nodiscard]] std::string std() const {
    std::string a(this->data(), this->size());
    return a;
  }

  StaticString& operator=(StaticString&& v) {
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

  static StaticString from(const std::string& str) { return StaticString(str); }

  template <size_t N>
  static StaticString from(const char (&d)[N]) {
    return StaticString(d);
  }

 private:
  template <size_t N>
  explicit StaticString(const String<N>& str) : base_type(str) {}
  template <size_t N>
  explicit StaticString(const char (&d)[N]) : base_type(d) {}
  explicit StaticString(const std::string& str)
      : base_type(str.c_str(), str.size()) {}
};

namespace apn {
class Int;
}

class StringRef : public vec::details::ConstCommon<char> {
  using base_type = vec::details::ConstCommon<char>;

 public:
  explicit operator std::string() const { return std(); }
  [[nodiscard]] std::string std() const {
    std::string a(this->data(), this->size());
    return a;
  }

  bool operator==(const StringRef& b) const {
    if (this->empty() || b.empty()) {
      return false;
    }
    if (this->capacity() != b.capacity()) {
      return false;
    }
    return std::strncmp(this->data(), b.data(), this->capacity()) == 0;
  }

  bool operator==(const char* b) const { return operator==(StringRef(b)); }
  bool eq(const char* b) const { return operator==(b); }

  constexpr StringRef(const char* data) : base_type("StringRef") {
    lps_assert(this->tag_, data != nullptr);
    this->first_ = (char*)data;
    this->size_ = std::char_traits<char>::length(this->first_);
    this->capacity_ = this->size_;
  }

  constexpr StringRef(const char* data, size_t size) : base_type("StringRef") {
    lps_assert(this->tag_, data != nullptr);
    this->first_ = (char*)data;
    this->size_ = size;
    this->capacity_ = size;
  }

  constexpr explicit StringRef(const std::string& s) : base_type("StringRef") {
    this->first_ = (char*)s.data();
    this->size_ = s.size();
    this->capacity_ = this->size_;
  }

  StringRef(const StaticString& str) : base_type("StringRef") {
    lps_assert(this->tag_, str.data() != nullptr);
    this->first_ = (char*)str.data();
    this->size_ = str.size();
    this->capacity_ = str.size();
  }

  template <size_t N>
  constexpr StringRef(const char (&s)[N]) : base_type("StringRef") {
    lps_assert(this->tag_, N >= 1);
    this->first_ = (char*)s;
    this->size_ = N - 1;
    this->capacity_ = N - 1;
  }

  constexpr StringRef() : base_type("StringRef") {
    this->first_ = nullptr;
    this->size_ = 0;
    this->capacity_ = 0;
  }
  StringRef(std::nullptr_t) = delete;

  static constexpr size_t kNpos = ~static_cast<size_t>(0);
  [[nodiscard]] constexpr StringRef substr(size_t start,
                                           size_t n = kNpos) const {
    start = std::min(start, size_);
    return StringRef(first_ + start, std::min(n, size_ - start));
  }

  [[nodiscard]] StringRef drop_front(size_t N = 1) const {
    LPS_CHECK_ERROR(tag_, size() >= N, "dropping more elements than exist");
    return substr(N);
  }
  [[nodiscard]] StringRef drop_back(size_t N = 1) const {
    LPS_CHECK_ERROR(tag_, size() >= N, "dropping more elements than exist");
    return substr(0, size() - N);
  }

  [[nodiscard]] bool starts_with(StringRef prefix) const {
    return size_ >= prefix.size_ &&
           str::compare(first_, prefix.first_, prefix.size_) == 0;
  }

  [[nodiscard]] StringRef slice(size_t start, size_t end) const {
    start = std::min(start, size_);
    end = std::min(std::max(start, end), size_);
    return StringRef(first_ + start, end - start);
  }

  bool as_int(uint64_t, apn::Int&) const;

 private:
};

template <size_t N, typename T>
std::ostream& operator<<(std::ostream& s, const Vector<N, T>& arr) {

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

inline std::ostream& operator<<(std::ostream& s, const StaticString& arr) {
  s << std::string(arr.data(), arr.size());
  return s;
}

template <size_t N>
std::ostream& operator<<(std::ostream& s, const String<N>& arr) {
  s << std::string(arr.data(), arr.size());
  return s;
}

inline std::ostream& operator<<(std::ostream& s, const StringRef& arr) {
  if (arr.empty()) {
    return s;
  }
  s << std::string(arr.data(), arr.size());
  return s;
}

namespace str::ascii::is {

template <bool AllowDollar = false>
inline bool ValidIdent(const StringRef& s) {
  if (s.empty() || !IdentStart<AllowDollar>(s[0])) {
    return false;
  }
  for (char i : s) {
    if (!IdentContinue<AllowDollar>(i)) {
      return false;
    }
  }
  return true;
}
}  // namespace str::ascii::is

}  // namespace lps::basic

inline std::string& operator+=(std::string& buffer,
                               lps::basic::StringRef string) {
  return buffer.append(string.data(), string.size());
}
