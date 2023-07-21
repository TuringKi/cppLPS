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

#include "dump.h"
#include <filesystem>
#include <regex>
#include <sstream>
#include "ast.h"
#include "basic/exception.h"
#include "basic/fmt/inja.h"
#include "src.h"
#include "token.h"

namespace lps::dump::parse_tree {

namespace {
void html_escape(std::string& str) {
  str = std::regex_replace(str, std::regex("<"), "&lt");
  str = std::regex_replace(str, std::regex(">"), "&gt");
}

std::string concat_token_chars(const lps::token::Token* start,
                               const lps::token::Token* end) {
  lps_assert("dump::concat_token_chars", start && end);
  std::stringstream ss;
  ss << start->str();
  const auto* p = start->next();
  while (p != end && p != nullptr) {
    ss << " " << p->str();
    p = p->next();
  }

  return ss.str();
}
}  // namespace

void html(const parser::details::Tree& tree, const char* saved_path_str) {
  token::Token start = *(tree.root().start_);
  auto the_content = src::Manager::instance().ref_of_char_file(start.file_id());
  inja::Environment env;
  auto template_file_path =
      lps::src::Manager::instance().share_path() / "dump_parsing_results.html";
  LPS_CHECK_ERROR("dump::html", std::filesystem::exists(template_file_path),
                  "the template file does not exists: ", template_file_path,
                  ", do `make install`.");
  auto dump = [](const parser::details::Tree::Node& n) -> std::string {
    auto get_kind = [](const parser::details::Tree::Node& n) {
      if (n.kind_ == parser::details::ParseFunctionKind::kExpectedToken) {

        lps_assert("dump::json",
                   n.token_kind_ != token::details::TokenKind::unknown);
        return token::details::kMap.at(n.token_kind_);
      }
      lps_assert("dump::json",
                 n.token_kind_ == token::details::TokenKind::unknown);
      return parser::details::kind::kMap.at(n.kind_);
    };

    std::stringstream ss;
    int graph_idx = 0;
    auto dump_impl = [](std::stringstream& ss,
                        const parser::details::Tree::Node& n,
                        auto& func) -> void {
      ss << "<div class=\"type_";
      std::string kind;
      if (n.kind_ == parser::details::ParseFunctionKind::kExpectedToken) {

        lps_assert("dump::json",
                   n.token_kind_ != token::details::TokenKind::unknown);
        kind = "Token";
      } else {
        kind = parser::details::kind::kMap.at(n.kind_);
        lps_assert("dump::json",
                   n.token_kind_ == token::details::TokenKind::unknown);
      }
      ss << kind << "\">";
      std::string code = concat_token_chars(n.start_, n.end_);
      html_escape(code);
      if (n.len_ == 0) {
        return;
      }
      if (n.children_.empty()) {
        ss << code << "\n";
      } else {
        ss << "<p style=\"display: flex; flex-direction: column;\">" << kind
           << "</p>\n";
      }

      for (const auto& n0 : n.children_) {
        func(ss, n0, func);
      }
      ss << "</div>\n";
    };
    dump_impl(ss, n, dump_impl);
    return ss.str();
  };

  inja::json data;
  data["source_code"] = dump(tree.root());

  auto html_results = env.render_file(template_file_path.c_str(), data);
  std::filesystem::path saved_path(saved_path_str);
  LPS_CHECK_ERROR("dump::html", std::filesystem::exists(saved_path),
                  "the dump path does not exists:", saved_path);
  {
    std::ofstream dump_file(saved_path / "dump.html");
    dump_file << html_results;
  }
}

void json(const parser::details::Tree& tree, const char* saved_path_str) {
  inja::json root;
  auto dump = [](inja::json& data,
                 const parser::details::Tree::Node& n) -> void {
    auto dump_impl = [](inja::json& data, const parser::details::Tree::Node& n,
                        auto& func) -> void {
      if (n.kind_ == parser::details::ParseFunctionKind::kExpectedToken) {
        data["kind"] = "none";
        lps_assert("dump::json",
                   n.token_kind_ != token::details::TokenKind::unknown);
        data["token_kind"] = token::details::kMap.at(n.token_kind_);
      } else {
        data["kind"] = parser::details::kind::kMap.at(n.kind_);
        lps_assert("dump::json",
                   n.token_kind_ == token::details::TokenKind::unknown);
        data["token_kind"] = "none";
      }
      std::string code = concat_token_chars(n.start_, n.end_);
      html_escape(code);
      data["code"] = code;
      if (n.len_ == 0) {
        return;
      }
      int i = 0;
      for (const auto& n0 : n.children_) {
        func(data["children"][i++], n0, func);
      }
    };
  };

  dump(root, tree.root());

  std::filesystem::path saved_path(saved_path_str);
  LPS_CHECK_ERROR("dump::json", std::filesystem::exists(saved_path),
                  "the dump path does not exists:", saved_path);
  {
    std::ofstream dump_file(saved_path / "dump.json");
    dump_file << root;
  }
  int dummy = -1;
}

void graphviz(const parser::details::Tree& tree, const char* saved_path) {
  unreachable("graphviz");
}

}  // namespace lps::dump::parse_tree
