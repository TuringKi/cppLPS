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
#include <map>
#include <unordered_map>
#include "basic/exception.h"
#include "basic/map.h"
#include "basic/tui.h"
#include "basic/vec.h"
#include "token.h"

namespace lps::diag {

enum class Level : uint8_t { kNote = 0, kWarning, kError, kNum };

static constexpr std::array<std::pair<Level, const char*>,
                            static_cast<uint16_t>(Level::kNum)>
    kLevelLists = {{
        {Level::kNote, "Note"},
        {Level::kWarning, "Warning"},
        {Level::kError, "Error"},
    }};

static constexpr lps::basic::map::Map<Level, const char*,
                                      static_cast<uint16_t>(Level::kNum),
                                      meta::S("diag_level_map")>
    kLevelMap{kLevelLists};

inline std::ostream& operator<<(std::ostream& s, Level kind) {
  s << kLevelMap.at(kind);
  return s;
}

enum DiagKind : uint16_t {
  kNone = 0,
#define DIAG(K, D, NOTE, LEVEL) K,
#include "./diag/kinds.def"
  kNum
};
inline std::ostream& operator<<(std::ostream& s, const DiagKind& kind) {
  s << static_cast<uint16_t>(kind);
  return s;
}

namespace details {

inline lps::basic::tui::color::Shell level2color(Level level) {
  switch (level) {
    case Level::kNote:
      return lps::basic::tui::color::Shell::fdefault();
    case Level::kWarning:
      return lps::basic::tui::color::Shell::fyellow();
    case Level::kError:
      return lps::basic::tui::color::Shell::fred();
    default:
      unreachable(meta::Str("level2color"));
      break;
  }

  unreachable(meta::Str("level2color"));
  return lps::basic::tui::color::Shell::fdefault();
}

using DescStringRef = basic::StringRef<meta::S("diag_info_desc")>;
struct Info {

  DescStringRef desc;
  Level level;
};

struct TokenInfo {
  uint32_t file_id{0};
  const char* ptr{nullptr};
  uint32_t offset{0};
  static std::pair<size_t, size_t> line_col(uint32_t file_id, const char* ptr);
};

using VecTokenInfos =
    basic::Vector<2, details::TokenInfo, meta::Str("context_token_infos")>;

void table(DiagKind kind, uint32_t file_id, const char* ptr, uint32_t token_len,
           const VecTokenInfos& context_token_infos);

std::string underline(uint32_t print_offset,
                      lps::basic::tui::color::Shell&& color, uint32_t file_id,
                      const char* ptr, uint32_t token_len,
                      const VecTokenInfos& context_token_infos);

class Summarize {
 public:
  struct Info {
    using FilePath = basic::StringRef<meta::Str("Counter_abs_file_path")>;
    FilePath file_path;
    DiagKind kind;
    Level level;
    uint32_t file_id;
    size_t line_n;
    size_t col_n;
    Info& operator=(const Info&) = default;
  };

  static Summarize& instance() {
    static Summarize counter;
    return counter;
  }
  void append(Info&& info) { infos_.append(info); }
  ~Summarize();

 private:
  void summary();
  Summarize() = default;
  basic::Vector<8, Info, meta::Str("Counter_infos")> infos_;
};

}  // namespace details

template <meta::Str TagName>
struct DiagInputs {

#define SET_R(A, B)                                      \
  (A)->main_token_ = std::move((B).main_token_);         \
  (A)->context_tokens_ = std::move((B).context_tokens_); \
  (A)->kind_ = (B).kind_;

#define SET_L(A, B)                           \
  (A)->main_token_ = (B).main_token_;         \
  (A)->context_tokens_ = (B).context_tokens_; \
  (A)->kind_ = (B).kind_;

  DiagInputs& operator=(DiagInputs&& other) {
    SET_R(this, other);
    return *this;
  }

  DiagInputs(const DiagInputs& other) {
    SET_L(this, other);
  }

  template <meta::Str TagNameOther>
  explicit DiagInputs(const DiagInputs<TagNameOther>& other) {
    SET_L(this, other);
  }

  template <meta::Str TagNameOther>
  explicit DiagInputs(DiagInputs<TagNameOther> const&& other) {
    SET_R(this, other);
  }

  template <meta::Str TagNameOther>
  DiagInputs& operator=(DiagInputs<TagNameOther> const&& other) {
    SET_R(this, other);
    return *this;
  }

  DiagInputs& operator=(const DiagInputs& other) {
    SET_L(this, other);
    return *this;
  }

  DiagInputs() = default;

#undef SET
  using main_token_type =
      token::Token<TagName + "DiagContextTokens_main_token">;
  using context_token_type =
      token::Token<TagName + "DiagContextTokens_context_token">;
  using tokens_type = basic::Vector<3, context_token_type,
                                    TagName + "DiagContextTokens_tokens">;
  main_token_type main_token_;
  tokens_type context_tokens_;
  DiagKind kind_{DiagKind::kNone};
};

class Information {

 public:
  static const Information& instance() {
    static Information info;
    return info;
  }

  bool has(DiagKind kind) const { return map_.contains(kind); }

  const details::Info& at(DiagKind kind) const {
    LPS_CHECK_ERROR(meta::S("Information.at"), has(kind), "");
    return map_.at(kind);
  }

 private:
  explicit Information() {
    map_ = {
#define DIAG(K, D, NOTE, LEVEL) \
  {DiagKind::K, {details::DescStringRef(D), LEVEL}},
#include "./diag/kinds.def"
    };
  }

  std::unordered_map<DiagKind, details::Info> map_;
};

std::ostream& infos();
std::ostream& warnings();
std::ostream& errs();

template <meta::Str TagName, meta::Str TagNameMain, meta::Str... TagNames>
DiagInputs<TagName> record(
    const lps::token::Token<TagNameMain>& tok, DiagKind kind,
    const lps::token::Token<TagNames>&... context_tokens) {
  LPS_CHECK_ERROR(meta::S("diag_record"), Information::instance().has(kind),
                  "kind:", kind, " not valid");
  DiagInputs<TagName> diag_inputs;
  diag_inputs.kind_ = kind;
  typename DiagInputs<TagName>::main_token_type the_main_tok;
  the_main_tok = tok;
  diag_inputs.main_token_ = the_main_tok;
  [&diag_inputs](const lps::token::Token<TagNames>&... tokens) {
    (
        [&diag_inputs](const auto& tok) {
          typename decltype(diag_inputs.context_tokens_)::ele_type tok_a;
          tok_a = tok;
          diag_inputs.context_tokens_.append(tok_a);
        }(tokens),
        ...);
  }(context_tokens...);
  return diag_inputs;
}

template <meta::Str TagName>
void doing(const typename DiagInputs<TagName>::main_token_type& tok,
           DiagKind kind,
           const typename DiagInputs<TagName>::tokens_type& context_tokens) {

  LPS_CHECK_ERROR(meta::S("diag_doing"), Information::instance().has(kind),
                  "kind not valid");
  details::VecTokenInfos token_infos;

  for (const auto& a : context_tokens) {
    token_infos.append({a.file_id(), a.ptr(), a.offset()});
  }

  details::table(kind, tok.file_id(), tok.ptr(), tok.offset(), token_infos);
}

};  // namespace lps::diag
