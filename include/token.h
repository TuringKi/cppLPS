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
#include <unordered_map>
#include "basic/exception.h"
#include "basic/map.h"
#include "basic/vec.h"
namespace lps::token {

template <meta::Str TagName = meta::S("location")>
class Location {

 public:
  using id_type = uint32_t;
  [[nodiscard]] id_type offset() const { return offset_id_ & ~kMask; }
  [[nodiscard]] id_type file_id() const { return file_id_; }
  void offset(uint32_t offset) { offset_id_ = offset; }
  void file_id(uint32_t file_id) { file_id_ = file_id; }
  void clear() {
    offset_id_ = 0;
    file_id_ = 0;
  }
  template <meta::Str OtherTagName>
  Location& operator=(const Location<OtherTagName>& other) {
    this->file_id_ = other.file_id();
    this->offset_id_ = other.offset();
    return *this;
  }
  template <meta::Str OtherTagName>
  explicit Location(const Location<OtherTagName>& other) {
    this->file_id_ = other.file_id();
    this->offset_id_ = other.offset();
  }
  Location() {}

 private:
  id_type offset_id_{0};
  id_type file_id_{0};
  static const id_type kMask = 1ULL << (8 * sizeof(id_type) - 1);
};

namespace tok {

enum class TokenKind : uint16_t {
#define TOK(X) X,
#include "token/kinds.def"
#undef TOK
  kNum
};

static constexpr std::array<std::pair<TokenKind, const char*>,
                            static_cast<uint16_t>(TokenKind::kNum)>
    kLists = {{
#define TOK(X) {TokenKind::X, #X},
#include "token/kinds.def"
#undef TOK
    }};

class IdentInfo {
 public:
  using IdentStringRef = lps::basic::StringRef<meta::S("ident_str")>;

  static const IdentInfo& instance() {
    static IdentInfo info;
    return info;
  }
  template <meta::Str TagName>
  bool kw_has(const basic::StringRef<TagName>& s) const {
    IdentStringRef a(s.data(), s.size());
    return kw_map_.contains(a);
  }
  bool kw_has(const IdentStringRef& s) const { return kw_map_.contains(s); }

  template <meta::Str TagName>
  TokenKind kw_at(const basic::StringRef<TagName>& s) const {
    LPS_CHECK_ERROR(meta::S("indent_at"), kw_has(s), "s = ", s);
    IdentStringRef a(s.data(), s.size());
    return kw_map_.at(a);
  }

 private:
  struct IdentHash {
    std::size_t operator()(const IdentStringRef& k) const {
      if (k.empty()) {
        return 0;
      }
      return std::_Hash_impl::hash(k.data(), k.size());
    }
  };

  IdentInfo() {
    kw_map_ = {
#define KEYWORD(NAME, FLAGS) {IdentStringRef(#NAME), TokenKind::kw_##NAME},
#define ALIAS(NAME, TOK, FLAGS) {IdentStringRef(NAME), TokenKind::kw_##TOK},
#include "token/kinds.def"
#undef KEYWORD
#undef ALIAS
    };
  }

  std::unordered_map<IdentStringRef, TokenKind, IdentHash> kw_map_;
  std::unordered_map<IdentStringRef, TokenKind, IdentHash> ident_map_;
};

static constexpr lps::basic::map::Map<TokenKind, const char*,
                                      static_cast<uint16_t>(TokenKind::kNum),
                                      meta::S("token_kind_map")>
    kMap{kLists};

inline std::ostream& operator<<(std::ostream& s, TokenKind kind) {
  s << kMap.at(kind);
  return s;
}

};  // namespace tok

enum Flag : uint16_t {
  kStartOfLine = 0x01,
  kLeadingSpace = 0x02,
  kNeedClean = 0x04
};

template <meta::Str TagName>
struct Token : virtual public basic::mem::TraceTag<TagName> {

 public:
#define SET(A, B)             \
  (A)->data_ = (B).data();    \
  (A)->loc_ = (B).location(); \
  (A)->kind_ = (B).kind();    \
  (A)->flags_ = (B).flags();

  template <meta::Str OtherTagName>
  Token& operator=(const Token<OtherTagName>& other) {
    SET(this, other);
    return *this;
  }
  Token() {}
  Token(const Token& other) {
    SET(this, other);
  }

  template <meta::Str OtherTagName>
  explicit Token(const Token<OtherTagName>& other) {
    SET(this, other);
  }
#undef SET
  void clear() {
    this->data_ = nullptr;
    this->loc_.clear();
    this->kind_ = tok::TokenKind::unknown;
    this->flags_ = 0;
  }
  void set_flag(Flag flg) {
    flags_ |= flg;
  }
  void clear_flag(Flag flg) {
    flags_ &= ~flg;
  }

  void offset(uint32_t offset) {
    loc_.offset(offset);
  }
  void file_id(uint32_t file_id) {
    loc_.file_id(file_id);
  }
  uint16_t flags() const {
    return flags_;
  }
  [[nodiscard]] uint32_t offset() const {
    return loc_.offset();
  }
  [[nodiscard]] uint32_t file_id() const {
    return loc_.file_id();
  }
  [[nodiscard]] tok::TokenKind kind() const {
    return kind_;
  }
  void kind(tok::TokenKind kind) {
    kind_ = kind;
  }
  void location(const Location<TagName + "_location">& loc) {
    loc_ = loc;
  }
  Location<TagName + "_location"> location() const {
    return loc_;
  }
  void data(const char* ptr) {
    data_ = const_cast<char*>(ptr);
  }
  [[nodiscard]] const char* ptr() const {
    return static_cast<char*>(data_);
  }
  [[nodiscard]] void* data() const {
    return data_;
  }
  template <meta::Str TagNameOther>
  basic::StringRef<TagNameOther> str() const {
    if (data_ == nullptr) {
      return basic::StringRef<TagNameOther>();
    }
    return basic::StringRef<TagNameOther>(static_cast<char*>(data_), offset());
  }

 private:
  Location<TagName + "_location"> loc_;
  tok::TokenKind kind_{tok::TokenKind::unknown};
  void* data_{nullptr};

  uint16_t flags_{0};
};

template <meta::Str TagName>
std::ostream& operator<<(std::ostream& s, const Token<TagName>& tok) {

  s << "kind:" << tok.kind();
  s << ", file_id:" << tok.file_id();
  s << ", offset:" << tok.offset();
  s << ", data:";
  using basic::str::details::operator<<;
  s << tok.template str<meta::S("operator<<_str")>();
  return s;
}
}  // namespace lps::token
