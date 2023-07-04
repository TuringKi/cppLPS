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

#include <filesystem>
#include <limits>
#include <map>
#include "basic/exception.h"
#include "basic/file.h"
#include "basic/mem.h"
#include "basic/vec.h"
#include "token.h"

namespace lps::src {

class Manager {

 public:
  static constexpr basic::mem::TraceTag::tag_type kTag = "lps::src::Manager";
  using FilePathStaticString = basic::StaticString;
  using FilePathStringRef = basic::StringRef;

  static Manager& instance() {
    static Manager mng;
    return mng;
  }
  uint32_t append(const char* path) {
    auto file = lps::basic::File::create(path, ++file_count_);
    lps_assert(kTag, file_count_ < std::numeric_limits<uint32_t>::max());
    if (file == nullptr) {
      --file_count_;
      return 0;
    }
    auto file_id = file->file_id();
    char_files_[file_id] = std::move(file);

    return file_id;
  }

  uint32_t append(token::TokenContainer::tokens_type&& tokens) {
    auto file = token::TokenContainer::create(std::move(tokens), ++file_count_);
    lps_assert(kTag, file_count_ < std::numeric_limits<uint32_t>::max());
    if (file == nullptr) {
      --file_count_;
      return 0;
    }
    auto file_id = file->file_id();
    token_files_[file_id] = std::move(file);

    fill_next_info_for_token_file(file_id);

    return file_id;
  }

  template <uint8_t CheckWhich = 0>
  [[nodiscard]] bool has(uint32_t file_id) const {
    if (CheckWhich == 0) {
      return char_files_.contains(file_id);
    }
    if (CheckWhich == 1) {
      return token_files_.contains(file_id);
    }
    return char_files_.contains(file_id) || token_files_.contains(file_id);
  }

  [[nodiscard]] FilePathStringRef path(uint32_t file_id) {
    if (!has(file_id)) {
      LPS_ERROR(kTag, "file_id = ", file_id, "not exists");
      return FilePathStringRef();
    }
    if (!abs_file_paths_.contains(file_id)) {
      auto the_path =
          std::filesystem::canonical(
              std::filesystem::absolute(char_files_.at(file_id)->path()))
              .string();
      abs_file_paths_[file_id] = FilePathStaticString::from(the_path);
    }
    if (!abs_file_paths_.contains(file_id)) {
      LPS_ERROR(kTag, "file_id = ", file_id, "not exists");
      return FilePathStringRef();
    }

    return FilePathStringRef(abs_file_paths_[file_id]);
  }

  basic::StringRef visitor_of_char_file(uint32_t file_id) const {
    return ref_char_file(file_id);
  }

  token::TokenListsVisitor visitor_of_token_file(uint32_t file_id) const {
    return ref_token_file(file_id);
  }

  [[nodiscard]] size_t size(uint32_t file_id) const {
    if (!has(file_id)) {
      LPS_ERROR(kTag, "file_id = ", file_id, "not exists");
      return -1;
    }
    if (char_files_.contains(file_id)) {
      return char_files_.at(file_id)->size();
    }
    return token_files_.at(file_id)->size();
  }

 private:
  basic::StringRef ref_char_file(uint32_t file_id) const {
    if (!char_files_.contains(file_id)) {
      LPS_ERROR(kTag, "file_id = ", file_id, "not exists");
      return basic::StringRef();
    }
    return char_files_.at(file_id)->ref();
  }

  token::TokenListsVisitor ref_token_file(uint32_t file_id) const {
    if (!token_files_.contains(file_id)) {
      LPS_ERROR(kTag, "file_id = ", file_id, "not exists");
      return token::TokenListsVisitor(nullptr, nullptr);
    }
    return token_files_.at(file_id)->visitor();
  }

  void fill_next_info_for_token_file(uint32_t file_id) {
    if (!token_files_.contains(file_id)) {
      LPS_ERROR(kTag, "file_id = ", file_id, "not exists");
      return;
    }
    auto len = token_files_[file_id]->tokens_.size();
    for (size_t i = 0; i < len; i++) {
      auto& tok = token_files_[file_id]->tokens_[i];
      auto next_info = tok.next_visitor();
      lps_assert(kTag, next_info.first > 0);
      if (i < len - 1) {
        tok.next_visitor_file_id(file_id);
      } else {
        lps_assert(kTag, next_info.second > 0);
        lps_assert(kTag, has(next_info.second));  // must be a char_file
      }
    }
  }

  explicit Manager() = default;
  std::unordered_map<uint32_t, basic::File::ptr_type> char_files_;
  std::unordered_map<uint32_t, token::TokenContainer::ptr_type> token_files_;
  std::unordered_map<uint32_t, FilePathStaticString> abs_file_paths_;
  uint32_t file_count_{0};
};

}  // namespace lps::src

namespace lps::token {
class TokenLists {
 public:
  static constexpr basic::mem::TraceTag::tag_type kTag =
      "lps::token::TokenLists";
  struct Info {

    static Info create(const Token& tok) {
      auto content =
          src::Manager::instance().visitor_of_char_file(tok.file_id());
      const auto* start = content.data();
      uint64_t offset = tok.ptr() - start;
      lps_assert(kTag, offset >= 0);
      return {tok.file_id(), offset};
    }
    static const char* start(uint32_t file_id) {
      auto content = src::Manager::instance().visitor_of_char_file(file_id);
      return content.data();
    }
    uint32_t file_id_{0};
    uint64_t offset_{0};
  };

  using ele_type = token::archived_type;
  static TokenLists& instance() {
    static TokenLists lists;
    return lists;
  }
  bool has(uint32_t file_id, uint64_t offset) const {
    if (lists_.contains(file_id)) {
      return lists_.at(file_id).contains(offset);
    }
    return false;
  }
  const ele_type& at(uint32_t file_id, uint64_t offset) const {
    lps_assert(kTag, has(file_id, offset));
    return lists_.at(file_id).at(offset);
  }

  bool has(const Info& info) const {
    if (lists_.contains(info.file_id_)) {
      return lists_.at(info.file_id_).contains(info.offset_);
    }
    return false;
  }
  const ele_type& at(const Info& info) const {
    lps_assert(kTag, has(info.file_id_, info.offset_));
    return lists_.at(info.file_id_).at(info.offset_);
  }

  ele_type& at(const Token& tok) {
    lps_assert(kTag, has(tok));
    auto info = Info::create(tok);
    return lists_.at(info.file_id_).at(info.offset_);
  }

  const ele_type& last(const Token& tok) const {
    lps_assert(kTag, has(tok));
    auto info = Info::create(tok);
    const auto* ptr = token::TokenLists::instance().at(tok).last();
    if (!ptr) {
      static const ele_type kEmptyTok;
      return kEmptyTok;
    }
    return *ptr;
  }

  const ele_type& next(const Token& tok) const {
    lps_assert(kTag, has(tok));
    auto info = Info::create(tok);
    const auto* ptr = token::TokenLists::instance().at(tok).next();
    if (!ptr) {
      static const ele_type kEmptyTok;
      return kEmptyTok;
    }
    return *ptr;
  }

  bool has(const Token& tok) const {
    auto info = Info::create(tok);
    return has(info);
  }

  void append(const Token& tok, Info last_tok_info = {0, 0}) {
    auto info = Info::create(tok);
    if (lists_.contains(tok.file_id())) {
      lps_assert(kTag, !lists_.at(info.file_id_).contains(info.offset_));
      lists_.at(info.file_id_)[info.offset_] = tok;
    } else {
      lists_[info.file_id_][info.offset_] = tok;
    }

    if (has(last_tok_info)) {
      lists_[last_tok_info.file_id_][last_tok_info.offset_].next(
          &lists_[info.file_id_][info.offset_]);
      lists_[info.file_id_][info.offset_].last(
          &lists_[last_tok_info.file_id_][last_tok_info.offset_]);
    }
  }

 private:
  std::unordered_map<uint32_t, std::unordered_map<uint64_t, ele_type>> lists_;
};
}  // namespace lps::token
