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

#include "diag.h"
#include <algorithm>
#include "basic/exception.h"
#include "basic/str.h"
#include "basic/tui.h"
#include "basic/tui/table.h"
#include "basic/vec.h"
#include "src.h"

namespace {}  // namespace

namespace lps::diag {

namespace details {

std::pair<size_t, size_t> TokenInfo::line_col(uint32_t file_id,
                                              const char* ptr) {
  const char* content_begin =
      src::Manager::instance().ref_of_char_file(file_id).data();
  const char* p = content_begin;
  size_t line_n = 1;
  const char* cur_line_start_ptr = content_begin;
  while (p < ptr) {
    if (lps::basic::str::ascii::is::VertWs(*p)) {
      cur_line_start_ptr = p;
      line_n++;
    }
    p++;
  }
  p = cur_line_start_ptr;
  size_t col_n = 1;
  while (p < ptr) {
    col_n++;
    p++;
  }
  return {line_n, col_n};
}

void table(DiagKind kind, uint32_t file_id, const char* ptr, uint32_t token_len,
           const VecTokenInfos& context_token_infos) {

  const auto& info = Information::instance().at(kind);

  std::ostream& s = warnings();

  switch (info.level) {
    case Level::kWarning:
      break;
    case Level::kError: {
      std::ostream& s = errs();
      break;
    }
    default:
      unreachable("diag_table");
      break;
  }

  // The definition of this data structure can be found in the RFC(https://discourse.llvm.org/t/rfc-improving-clang-s-diagnostics/62584) for an reference:
  // *Diagnostic category*	Error, warning, remark
  // *Source location*
  // *Summary*	Similar to current diagnostics, states what the problem is.
  // *Reason*	Explains why the diagnostic was generated in a friendly manner.
  // *Context*	Relevant source info such as considered overload resolution candidates, template backtraces, etc. These should be structured, rather than appearing as plaintext.
  // *Potential fixes*
  // *Reference material*	cppreference, C++ standard drafts, etc.
  // *Glossary*	Separates identifying type aliases and template parameters from the message.

  const std::string str_diag_cat("*Diagnostic category*");
  const std::string str_src_loc("*Source location*");
  const std::string str_summary("*Summary*");
  const std::string str_reason("*Reason*");
  const std::string str_context("*Context*");
  const std::string str_potential("*Potential fixes*");
  const std::string str_ref_mat("*Reference material*");
  const std::string str_glo("*Glossary*");
  const std::vector<std::string> titles = {str_summary, str_reason,
                                           str_context, str_potential,
                                           str_ref_mat, str_glo};
  size_t max_len = 0;
  std::for_each(titles.begin(), titles.end(),
                [&max_len](const std::string& str) {
                  if (str.size() > max_len) {
                    max_len = str.size();
                  }
                });

  auto title_before_ws_len = strlen(kLevelMap.at(info.level)) + 1;
  auto create_title = [&max_len, &s,
                       &title_before_ws_len](const std::string& str) {
    std::string ws(title_before_ws_len, ' ');
    return lps::basic::tui::color::Shell::colorize(
        ws + str + std::string(max_len - str.size(), ' '),
        lps::basic::tui::color::Shell::fblue());
  };

  auto ul = underline(title_before_ws_len + max_len + 3,
                      details::level2color(info.level), file_id, ptr, token_len,
                      context_token_infos);

  // calculate line number:
  auto line_col = TokenInfo::line_col(file_id, ptr);
  size_t line_n = line_col.first;
  size_t col_n = line_col.second;

  s << "["
    << lps::basic::tui::color::Shell::colorize(kLevelMap.at(info.level),
                                               level2color(info.level))
    << "]"
    << lps::basic::tui::color::Shell::colorize(
           src::Manager::instance().path(file_id).std(),
           lps::basic::tui::color::Shell::fgreen())
    << "["
    << lps::basic::tui::color::Shell::colorize(
           lps::basic::str::from("Line:", line_n, ", Col:", col_n),
           lps::basic::tui::color::Shell::fyellow())
    << "]:\n";

  s << create_title(str_summary) << " : " << info.desc << "\n";
  s << create_title(str_reason) << " :\n";
  s << create_title(str_context) << " : " << ul << " \n";
  s << create_title(str_potential) << " :\n";
  s << create_title(str_ref_mat) << " :\n";
  s << create_title(str_glo) << " :\n";

  details::Summarize::Info::FilePath file_path(
      src::Manager::instance().path(file_id));
  details::Summarize::instance().append(
      {file_path, kind, info.level, file_id, line_n, col_n});
  //s << styled_table << std::endl;
}

std::string underline(uint32_t print_offset,
                      lps::basic::tui::color::Shell&& color, uint32_t file_id,
                      const char* ptr, uint32_t token_len,
                      const VecTokenInfos& context_token_infos) {
  const char* content_begin =
      src::Manager::instance().ref_of_char_file(file_id).data();
  int64_t content_size = src::Manager::instance().size(file_id);

  const char* content_end = content_begin + content_size;

  // find the beginning of the line:
  auto find_offset_before =
      [](const char* ptr,
         const char* content_begin) -> std::pair<size_t, const char*> {
    const char* p = ptr;
    while (p != content_begin) {
      if (lps::basic::str::ascii::is::VertWs(*p)) {
        break;
      }
      p--;
    }

    const char* start_of_line = p;
    if (lps::basic::str::ascii::is::VertWs(*start_of_line)) {
      start_of_line++;
    }
    int64_t offset_before = ptr - start_of_line;
    offset_before = std::min<int64_t>(offset_before, 64);

    lps_assert("diag_underline", offset_before > 0);
    start_of_line = ptr - offset_before;
    return {offset_before, start_of_line};
  };

  auto offset_before_and_start_of_line = find_offset_before(ptr, content_begin);
  auto offset_before = offset_before_and_start_of_line.first;
  const char* start_of_line = offset_before_and_start_of_line.second;

  // find the ending of the line:
  auto find_offset_after =
      [](const char* ptr,
         const char* content_end) -> std::pair<size_t, const char*> {
    const char* p = ptr;
    while (p != content_end) {
      if (lps::basic::str::ascii::is::VertWs(*p)) {
        break;
      }
      p++;
    }
    const char* end_of_line = p;
    int64_t offset_after = end_of_line - ptr;
    offset_after = std::min<int64_t>(offset_after, 64);
    lps_assert("diag_underline", offset_after > 0);
    end_of_line = ptr + offset_after;
    return {offset_after, end_of_line};
  };

  auto offset_after_and_end_of_line = find_offset_after(ptr, content_end);
  auto offset_after = offset_after_and_end_of_line.first;
  const char* end_of_line = offset_after_and_end_of_line.second;

  auto out = std::string(start_of_line, end_of_line - start_of_line) + "\n";

  // first, we need `underline` the context tokens.
  using UlString = basic::StaticString;
  using PosUlStr =
      basic::Pair<basic::Pair<size_t, UlString>, basic::tui::color::Shell>;
  basic::Vector<4, PosUlStr> context_uls;
  std::string context_ul(print_offset, ' ');
  for (const auto& a : context_token_infos) {
    const char* content_begin =
        src::Manager::instance().ref_of_char_file(a.file_id).data();
    auto offset_before_and_start_of_line =
        find_offset_before(a.ptr, content_begin);
    auto offset_before = offset_before_and_start_of_line.first;
    const char* start_of_line = offset_before_and_start_of_line.second;

    int64_t content_size = src::Manager::instance().size(file_id);
    const char* content_end = content_begin + content_size;

    auto offset_after_and_end_of_line = find_offset_after(ptr, content_end);
    auto offset_after = offset_after_and_end_of_line.first;
    const char* end_of_line = offset_after_and_end_of_line.second;
    UlString ul('~', std::min<uint64_t>(a.offset, offset_after));
    context_uls.append({{std::move(offset_before), std::move(ul)},
                        basic::tui::color::Shell::fgreen()});
  }

  context_uls.append({{offset_before + 0, UlString('^', 1)},
                      basic::tui::color::Shell(color.code())});
  context_uls.append(
      {{offset_before + 1,
        UlString('~', std::min<uint64_t>(token_len, offset_after - 1))},
       basic::tui::color::Shell(color.code())});

  if (!context_uls.empty()) {
    std::sort(context_uls.begin(), context_uls.end(),
              [](const PosUlStr& a, const PosUlStr& b) {
                return a.t0_.t0_ < b.t0_.t0_;
              });
    auto last_offset = context_uls[0].t0_.t0_;
    size_t idx = 0;
    for (const auto& a : context_uls) {
      if (idx > 0) {
        last_offset = a.t0_.t0_ - last_offset;
      }
      context_ul += std::string(last_offset, ' ') +
                    lps::basic::tui::color::Shell::colorize(
                        std::move(a.t0_.t1_.std()),
                        basic::tui::color::Shell(a.t1_.code()));
      last_offset = a.t0_.t0_ + a.t0_.t1_.size();
      idx++;
    }
  }

  out = out + context_ul + "\n";
  return out;
}
void Summarize::summary() {
  tabulate::Table summary;
  summary.add_row({"*Diagnostic category*", "*Numbers*", "*Locations*"});
  for (auto& a : summary[0]) {
    a.format()
        .font_color(tabulate::Color::blue)
        .font_align(tabulate::FontAlign::center)
        .font_style({tabulate::FontStyle::bold});
  }
  using VecCntIdx = basic::Vector<4, size_t>;
  VecCntIdx cnt_errors;
  VecCntIdx cnt_warnings;
  size_t idx = 0;
  for (const auto& a : infos_) {
    if (a.level == Level::kError) {
      cnt_errors.append(idx);
    } else if (a.level == Level::kWarning) {
      cnt_warnings.append(idx);
    }
    idx++;
  }

  auto add_loc = [this](const VecCntIdx& cnt) {
    std::string locations;
    for (const auto& i : cnt) {
      const auto& info = infos_[i];
      locations = basic::str::from(
          locations, "[", i,
          "]:", src::Manager::instance().path(info.file_id).std(), "[",
          lps::basic::str::from("Line:", info.line_n, ", Col:", info.col_n),
          "]\n");
    }
    return locations;
  };
  size_t row_idx = 0;
  if (!cnt_errors.empty()) {
    std::string locations = add_loc(cnt_errors);

    summary.add_row({"Errors", basic::str::from(cnt_errors.size()), locations});

    summary[++row_idx][0]
        .format()
        .font_color(tabulate::Color::red)
        .font_align(tabulate::FontAlign::center)
        .font_style({tabulate::FontStyle::bold});
  }

  if (!cnt_warnings.empty()) {
    std::string locations = add_loc(cnt_warnings);
    summary.add_row(
        {"Warnings", basic::str::from(cnt_warnings.size()), locations});
    summary[++row_idx][0]
        .format()
        .font_color(tabulate::Color::yellow)
        .font_align(tabulate::FontAlign::center)
        .font_style({tabulate::FontStyle::bold});
  }
  if (!cnt_errors.empty()) {
    errs() << summary << "\n";
  } else if (!cnt_warnings.empty()) {
    warnings() << summary << "\n";
  }
}
Summarize::~Summarize() {
  summary();
}

}  // namespace details

std::ostream& infos() {
  return std::cout;
}

std::ostream& warnings() {
  return std::cout;
}

std::ostream& errs() {
  return std::cerr;
}

}  // namespace lps::diag
