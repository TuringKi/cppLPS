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
#include "basic/mem.h"
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
                                      static_cast<uint16_t>(Level::kNum)>
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
      unreachable("level2color");
      break;
  }

  unreachable("level2color");
  return lps::basic::tui::color::Shell::fdefault();
}

using DescStringRef = basic::StringRef;
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

using VecTokenInfos = basic::Vector<2, details::TokenInfo>;

void table(DiagKind kind, uint32_t file_id, const char* ptr, uint32_t token_len,
           const VecTokenInfos& context_token_infos);

std::string underline(uint32_t print_offset,
                      lps::basic::tui::color::Shell&& color, uint32_t file_id,
                      const char* ptr, uint32_t token_len,
                      const VecTokenInfos& context_token_infos);

class Summarize {
 public:
  struct Info {
    using FilePath = basic::StringRef;
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
  basic::Vector<8, Info> infos_;
};

}  // namespace details

struct DiagInputs {

  explicit DiagInputs() = default;

  using main_token_type = token::Token;
  using context_token_type = token::Token;
  using tokens_type = basic::Vector<3, context_token_type>;
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
    LPS_CHECK_ERROR("Information.at", has(kind), "");
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

inline DiagInputs record(const lps::token::Token& tok, DiagKind kind,
                         const std::vector<lps::token::Token>& context_tokens) {
  LPS_CHECK_ERROR("diag_record", Information::instance().has(kind),
                  "kind:", kind, " not valid");
  DiagInputs diag_inputs;
  diag_inputs.kind_ = kind;
  typename DiagInputs::main_token_type the_main_tok;
  the_main_tok = tok;
  diag_inputs.main_token_ = the_main_tok;
  for (const auto& t : context_tokens) {
    diag_inputs.context_tokens_.append(t);
  }

  return diag_inputs;
}

inline void doing(const typename DiagInputs::main_token_type& tok,
                  DiagKind kind,
                  const typename DiagInputs::tokens_type& context_tokens) {

  LPS_CHECK_ERROR("diag_doing", Information::instance().has(kind),
                  "kind not valid");
  details::VecTokenInfos token_infos;

  for (const auto& a : context_tokens) {
    token_infos.append({a.file_id(), a.ptr(), a.offset()});
  }

  details::table(kind, tok.file_id(), tok.ptr(), tok.offset(), token_infos);
}

};  // namespace lps::diag
