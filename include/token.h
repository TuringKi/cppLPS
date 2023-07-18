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
#include "basic/file.h"
#include "basic/map.h"
#include "basic/mem.h"

namespace lps::src {
class Manager;
}
namespace lps::token {

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

  explicit Location() = default;

 private:
  id_type offset_id_{0};
  id_type file_id_{0};
  static const id_type kMask = 1ULL << (8 * sizeof(id_type) - 1);
};

namespace details {

enum class TokenKind : uint16_t {
#define TOK(X) X,
#define CXX_KEYWORD_OPERATOR(X, Y) kw_##X,
#include "token/kinds.def"
#undef CXX_KEYWORD_OPERATOR
#undef TOK
  kNum
};

static constexpr std::array<std::pair<TokenKind, const char*>,
                            static_cast<uint16_t>(TokenKind::kNum)>
    kLists = {{
#define TOK(X) {TokenKind::X, #X},
#define CXX_KEYWORD_OPERATOR(X, Y) {TokenKind::kw_##X, #X},
#include "token/kinds.def"
#undef CXX_KEYWORD_OPERATOR
#undef TOK
    }};

class IdentInfo {
 public:
  using IdentStringRef = lps::basic::StringRef;
  template <typename String>
  struct IdentHash {
    std::size_t operator()(const String& k) const {
      if (k.empty()) {
        return 0;
      }
      return std::hash<std::string>()(std::string(k.data(), k.size()));
    }
  };

  static const IdentInfo& instance() {
    static IdentInfo info;
    return info;
  }

  bool kw_has(const basic::StringRef& s) const { return kw_map_.contains(s); }

  TokenKind kw_at(const basic::StringRef& s) const {
    LPS_CHECK_ERROR("indent_at", kw_has(s), "s = ", s);
    IdentStringRef a(s.data(), s.size());
    return kw_map_.at(a);
  }

 private:
  IdentInfo() {
    kw_map_ = {
#define KEYWORD(NAME, FLAGS) {IdentStringRef(#NAME), TokenKind::kw_##NAME},
#define KEYPP(NAME, FLAGS) {IdentStringRef("#" #NAME), TokenKind::kw_##NAME},
#define ALIAS(NAME, TOK, FLAGS) {IdentStringRef(NAME), TokenKind::kw_##TOK},
#define CXX_KEYWORD_OPERATOR(X, Y) {IdentStringRef(#X), TokenKind::kw_##X},
#include "token/kinds.def"
#undef CXX_KEYWORD_OPERATOR
#undef KEYWORD
#undef KEYPP
#undef ALIAS
    };
  }

  std::unordered_map<IdentStringRef, TokenKind, IdentHash<IdentStringRef>>
      kw_map_;
  std::unordered_map<IdentStringRef, TokenKind, IdentHash<IdentStringRef>>
      ident_map_;
};

static constexpr lps::basic::map::Map<TokenKind, const char*,
                                      static_cast<uint16_t>(TokenKind::kNum)>
    kMap{kLists};

inline std::ostream& operator<<(std::ostream& s, TokenKind kind) {
  s << kMap.at(kind);
  return s;
}

};  // namespace details

enum Flag : uint16_t {
  kStartOfLine = 0x01,
  kLeadingSpace = 0x02,
  kNeedClean = 0x04
};

struct Token;

using archived_type = Token;

struct Token {

 public:
  using tokens_type = basic::Vector<4, Token>;
  using next_info_type = basic::vfile::next_info_type;
  void clear() {
    this->data_ = nullptr;
    this->char_store_ = nullptr;
    this->loc_.clear();
    this->kind_ = details::TokenKind::unknown;
    this->flags_ = 0;
  }
  void set_flag(Flag flg) { flags_ |= flg; }
  void clear_flag(Flag flg) { flags_ &= ~flg; }

  void offset(uint32_t offset) { loc_.offset(offset); }
  void file_id(uint32_t file_id) { loc_.file_id(file_id); }
  [[nodiscard]] uint16_t flags() const { return flags_; }
  [[nodiscard]] uint32_t offset() const { return loc_.offset(); }
  [[nodiscard]] uint32_t file_id() const { return loc_.file_id(); }
  [[nodiscard]] details::TokenKind kind() const { return kind_; }
  void kind(details::TokenKind kind) { kind_ = kind; }
  void location(const Location& loc) { loc_ = loc; }
  [[nodiscard]] Location location() const { return loc_; }
  void data(const basic::FileVisitor& ptr_) {
    lps_assert("token::Token", !ptr_.eof());
    auto ptr = ptr_;
    if (!ptr.same_file(offset())) {
      char_store_.reset(new basic::String<8>);
      auto tmp_ptr = ptr;

      for (size_t i = 0; i < offset(); i++, ++tmp_ptr) {
        char_store_->append(*tmp_ptr);
      }
    } else {
      data_ = ptr.cur();
    }
  }
  [[nodiscard]] const char* ptr() const {
    if (!data_ && !char_store_) {
      return nullptr;
    }
    if (data_) {
      return data_;
    }
    return char_store_->data();
  }

  [[nodiscard]] basic::StringRef str() const {
    if (!data_ && !char_store_) {
      return basic::StringRef();
    }
    if (data_) {
      return basic::StringRef(data_, offset());
    }
    return basic::StringRef(char_store_->data(), offset());
  }

  [[nodiscard]] const archived_type* next() const { return next_token_; }

  [[nodiscard]] const archived_type* last() const { return last_token_; }

  void next(const archived_type* other) { next_token_ = other; }

  void last(const archived_type* other) { last_token_ = other; }

  void next_visitor_offset(size_t offset) { next_visitor_.offset_ = offset; }

  void next_visitor_file_id(uint32_t file_id) {
    next_visitor_.file_id_ = file_id;
  }

  void next_visitor(size_t offset, uint32_t file_id) {
    next_visitor_ = {offset, file_id};
  }

  [[nodiscard]] next_info_type next_visitor() const {
    return {next_visitor_.offset_, next_visitor_.file_id_};
  }

 private:
  Location loc_;
  details::TokenKind kind_{details::TokenKind::unknown};
  const char* data_{nullptr};
  std::shared_ptr<basic::String<8>> char_store_{nullptr};

  uint16_t flags_{0};
  const archived_type* last_token_{nullptr};
  const archived_type* next_token_{nullptr};

  struct {
    size_t offset_;
    uint32_t file_id_;
  } next_visitor_{0, 0};
};

inline bool operator==(const Token& a, const Token& b) {
  return (a.file_id() == b.file_id()) && (a.offset() == b.offset()) &&
         (a.kind() == b.kind());
}

inline bool operator!=(const Token& a, const Token& b) {
  return !(a == b);
}

inline std::ostream& operator<<(std::ostream& s, const Token& tok) {

  s << "kind:" << tok.kind();
  s << ", file_id:" << tok.file_id();
  s << ", offset:" << tok.offset();
  s << ", data:";
  using basic::str::details::operator<<;
  s << tok.str();
  return s;
}

class TokenListsVisitor : public basic::vfile::Visitor<archived_type>,
                          public basic::vfile::Operator<TokenListsVisitor> {
 public:
  using base = basic::vfile::Visitor<archived_type>;
  explicit TokenListsVisitor(
      const archived_type* start, const archived_type* end,
      const base::check_eof_callback_type& check_eof_callback =
          [](const basic::vfile::Visitor<archived_type>*) {},
      uint32_t file_id = 0)
      : base(start, end, check_eof_callback, file_id) {}

  archived_type operator[](size_t idx) const { return *(*this + idx); }
  [[nodiscard]] const archived_type* cur() const override {
    if (pos_ > len() || start_ > end_) {
      TokenListsVisitor tmp(*this);
      tmp.pos_ = 0;
      this->check_eof_callback_(&tmp);
      return &eof_;
    }
    return start_ + pos_;
  }
};

class TokenContainer : public basic::vfile::File<archived_type> {

 public:
  friend class src::Manager;
  using tokens_type = basic::Vector<4, archived_type>;
  using base = basic::vfile::File<archived_type>;
  using type = TokenContainer;
  using ptr_type = std::unique_ptr<type>;

  virtual TokenListsVisitor visitor() {
    lps_assert(kTagName, first_ != nullptr);
    lps_assert(kTagName, size_ > 0);
    return TokenListsVisitor(
        first_, first_ + size_ - 1,
        [this](const basic::vfile::Visitor<archived_type>* visitor_ptr) {
          lps_assert(kTagName, visitor_ptr);
          auto next_info = tokens_.back().next_visitor();
          throw basic::vfile::Eof(next_info.second, next_info.first);
        });
  }

  static ptr_type create(tokens_type&& tokens, uint32_t file_id) {
    return std::make_unique<type>(std::move(tokens), file_id);
  }

  explicit TokenContainer(tokens_type&& tokens, uint32_t file_id)
      : tokens_(std::move(tokens)) {
    first_ = tokens_.data();
    size_ = tokens_.size();
    file_id_ = file_id;
  }

  TokenContainer(TokenContainer&& file) {
    this->tokens_ = std::move(file.tokens_);
    this->file_id_ = file.file_id_;
    this->first_ = file.first_;
    this->size_ = file.size_;
  }

 private:
  tokens_type tokens_;
};

}  // namespace lps::token
