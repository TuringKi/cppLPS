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
#include <unordered_map>
#include "basic/exception.h"
#include "basic/mem.h"
#include "basic/vfile.h"
#include "diag.h"
#include "lex/pp_ast.h"
#include "token.h"

namespace lps::tu {

// The `compiler`'s target: `TranslationUnit`
class TU {
 public:
  constexpr static basic::mem::TraceTag::tag_type kTag = "TU";
  using IdentStringRef = lps::basic::StringRef;
  using defined_token_type = token::Token;
  constexpr static basic::mem::TraceTag::tag_type kMacroInfoTagName =
      "TU::MacroInfo";
  using pp_ast_node_type = lexer::details::pp::ast::Node;

  struct MacroInfo {
    pp_ast_node_type::ptr_type node_;
  };
  using macro_info_type = MacroInfo;
  using defined_tokens_type =
      std::unordered_map<IdentStringRef, macro_info_type,
                         token::details::IdentInfo::IdentHash<IdentStringRef>>;
  using included_tokens_type = std::unordered_map<size_t, macro_info_type>;
  static TU& instance() {
    static TU tu;
    return tu;
  }

  bool defined(const token::Token& tok) {
    check_define(tok);
    return already_defined(tok);
  }

  void define(const token::Token& tok,
              typename lps::token::Token::tokens_type&& parameter_tokens =
                  typename lps::token::Token::tokens_type{},
              typename lps::token::Token::tokens_type&& expand_tokens =
                  typename lps::token::Token::tokens_type{}) {
    check_define(tok);
    if (already_defined(tok)) {
      diag(tok, diag::DiagKind::redefine_ident_in_preprocessing);
    }

    using namespace lexer::details::pp::ast;
    pp_ast_node_type::ptr_type node = nullptr;
    if (!parameter_tokens.empty() && !expand_tokens.empty()) {
      auto tmp_tok = tok;
      node = Factory::create<DefineWithParameters>(std::move(tmp_tok),
                                                   std::move(parameter_tokens),
                                                   std::move(expand_tokens));
    } else {
      auto tmp_tok = tok;
      node =
          Factory::create<Define>(std::move(tmp_tok), std::move(expand_tokens));
    }

    lps_assert(kTag, "node can not be nullptr");

    define_tokens_[str(tok)] = {std::move(node)};
  }

  void include(const token::Token& tok) {
    lps_assert(kTag, tok.kind() == token::details::TokenKind::header_name);

    auto path = std::filesystem::absolute(tok.str().data());
    std::string path_str = path.string();
    auto hash_val = std::hash<std::string>()(path_str);
    if (included_.contains(hash_val)) {
      return;
    }
    if (!std::filesystem::exists(path)) {
      diag(tok, diag::DiagKind::included_path_no_exists);
      return;
    }
    using namespace lexer::details::pp::ast;
    auto tmp_tok = tok;
    included_[hash_val] = {Factory::create<Include>(std::move(tmp_tok))};
  }

  bool already_defined(const token::Token& tok) {
    check_define(tok);
    return define_tokens_.contains(str(tok));
  }

  void undef(const token::Token& tok) {
    check_define(tok);
    if (!already_defined(tok)) {
      diag(tok, diag::DiagKind::undef_on_no_defined_ident);
    } else {
      define_tokens_.erase(str(tok));
    }
  }

  token::Token expand(
      const token::Token& tok,
      const typename lps::token::Token::tokens_type& parameter_tokens =
          typename lps::token::Token::tokens_type{}) {
    check_define(tok);
    lps_assert(kTag, already_defined(tok));
    const auto& node = define_tokens_[str(tok)].node_;
    const auto& expanded_tokens = node->expand();
    using expanded_tokens_type = decltype(expanded_tokens);
    token::TokenContainer::tokens_type full_expanded_tokens;
    [this](expanded_tokens_type tokens,
           token::TokenContainer::tokens_type& out_tokens) -> void {
      auto expand_impl = [this](expanded_tokens_type tokens,
                                token::TokenContainer::tokens_type& out_tokens,
                                auto func) -> void {
        for (const auto& t : tokens) {
          if (t.kind() == token::details::TokenKind::identifier) {
            if (defined(t)) {
              const auto& tokens_1 = define_tokens_[str(t)].node_->expand();
              if (tokens.empty()) {
                diag(t, diag::DiagKind::unexpected_empty_macro_expand);
              } else {
                func(tokens_1, out_tokens, func);
                continue;
              }
            }
          }
          token::TokenContainer::tokens_type::ele_type t0;
          t0 = t;
          out_tokens.append(t0);
          if (out_tokens.size() > 1) {
            // set `next` information
            out_tokens[out_tokens.size() - 2].next_visitor_offset(
                out_tokens.size() - 1);
          }
        }
      };
      expand_impl(tokens, out_tokens, expand_impl);
    }(expanded_tokens, full_expanded_tokens);
    token::Token returned_tok;
    if (!full_expanded_tokens.empty()) {

      full_expanded_tokens.back().next_visitor(tok.offset(), tok.file_id());

      auto file_id = record_expanded_tokens_as_virtual_file(
          tok.ptr(), tok.file_id(), std::move(full_expanded_tokens));
      lps_assert(kTag, file_id > 0);
      // the expanded list is valid, and recorded by `src::Manager`,
      // now we can return the first element of the expanded list.
      auto visitor = get_visitor_of_token_file(file_id);
      lps_assert(kTag, !visitor.eof());

      returned_tok = *visitor.cur();

    } else {
      // todo(@mxlol233): what if the expanded list is empty?
      // Should we just skip this token?
    }

    return returned_tok;
  }

 private:
  static uint32_t record_expanded_tokens_as_virtual_file(
      const char* cur_tok_data_ptr, uint32_t cur_tok_file_id,
      token::TokenContainer::tokens_type&& tokens);
  static token::TokenListsVisitor get_visitor_of_token_file(uint32_t file_id);

  static IdentStringRef str(const token::Token& tok) { return tok.str(); }

  static void check_define(const token::Token& tok) {
    lps_assert(kTag, tok.kind() == token::details::TokenKind::identifier);
    lps_assert(kTag, tok.ptr() != nullptr && tok.offset() > 0);
  }

  static inline void diag(const token::Token& tok, diag::DiagKind kind) {
    diag::DiagInputs diag_input;
    diag_input.kind_ = kind;
    diag_input.main_token_ = tok;
    diag::doing(diag_input.main_token_, diag_input.kind_,
                diag_input.context_tokens_);
  }

  defined_tokens_type define_tokens_;
  included_tokens_type included_;
};

}  // namespace lps::tu
