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
#include "basic/exception.h"
#include "basic/map.h"
#include "basic/vec.h"
namespace lps::token {

template <meta::Str TagName = meta::S("location")>
class Location {

 public:
  using id_type = uint32_t;
  [[nodiscard]] id_type offset() const { return offset_id_ & ~kMask_; }
  [[nodiscard]] id_type file_id() const { return file_id_; }
  void offset(uint32_t offset) { offset_id_ = offset; }
  void file_id(uint32_t file_id) { file_id_ = file_id; }

 private:
  id_type offset_id_{0};
  id_type file_id_{0};
  const id_type kMask_ = 1ULL << (8 * sizeof(id_type) - 1);
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
struct Token {

 public:
  void set_flag(Flag flg) { flags_ |= flg; }
  void clear_flag(Flag flg) { flags_ &= ~flg; }

  void offset(uint32_t offset) { loc_.offset(offset); }
  void file_id(uint32_t file_id) { loc_.file_id(file_id); }
  [[nodiscard]] uint32_t offset() const { return loc_.offset(); }
  [[nodiscard]] uint32_t file_id() const { return loc_.file_id(); }
  [[nodiscard]] tok::TokenKind kind() const { return kind_; }
  void kind(tok::TokenKind kind) { kind_ = kind; }
  void location(const Location<TagName + "_location">& loc) { loc_ = loc; }
  Location<TagName + "_location"> location() const { return loc_; }
  void data(const char* ptr) { data_ = const_cast<char*>(ptr); }
  [[nodiscard]] void* data() const { return data_; }
  basic::StringRef<TagName + "_data"> str() const {
    if (data_ == nullptr) {
      return basic::StringRef<TagName + "_data">();
    }
    return basic::StringRef<TagName + "_data">(static_cast<char*>(data_),
                                               offset());
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
  s << tok.str();
  return s;
}
}  // namespace lps::token
