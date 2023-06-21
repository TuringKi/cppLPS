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

#include "basic/exception.h"
#include "diag.h"

namespace lps::tu {

// The `compiler`'s target: `TranslationUnit`
class TU : virtual public basic::mem::TraceTag<meta::S("TU")> {
 public:
  using trace_tag_type = basic::mem::TraceTag<meta::S("TU")>;
  using IdentStringRef = lps::basic::StringRef<meta::S("TU::ident_str")>;
  using defined_token_type = token::Token<meta::S("TU::defined_token")>;
  using defined_tokens_type =
      std::unordered_map<IdentStringRef, defined_token_type,
                         token::tok::IdentInfo::IdentHash<IdentStringRef>>;

  static TU& instance() {
    static TU tu;
    return tu;
  }
  template <meta::Str TagName>
  bool defined(const token::Token<TagName>& tok) {
    check_define(tok);
    return already_defined(tok);
  }

  template <meta::Str TagName>
  void define(const token::Token<TagName>& tok) {
    check_define(tok);
    if (already_defined(tok)) {
      diag(tok, diag::DiagKind::redefine_ident_in_preprocessing);
    }
    define_tokens_[str(tok)] = tok;
  }
  template <meta::Str TagName>
  bool already_defined(const token::Token<TagName>& tok) {
    check_define(tok);
    return define_tokens_.contains(str(tok));
  }

  template <meta::Str TagName>
  void undef(const token::Token<TagName>& tok) {
    check_define(tok);
    if (!already_defined(tok)) {
      diag(tok, diag::DiagKind::undef_on_no_defined_ident);
    } else {
      define_tokens_.erase(str(tok));
    }
  }

 private:
  template <meta::Str TagName>
  IdentStringRef str(const token::Token<TagName>& tok) {
    return tok.template str<meta::S("TU::ident_str")>();
  }
  template <meta::Str TagName>
  void check_define(const token::Token<TagName>& tok) {
    constexpr auto kAssertTag = trace_tag_type::kTag + "_" + TagName;
    lps_assert(kAssertTag, tok.kind() == token::tok::TokenKind::identifier);
    lps_assert(kAssertTag, tok.ptr() != nullptr && tok.offset() > 0);
  }
  template <meta::Str TagName>
  inline void diag(const token::Token<TagName>& tok, diag::DiagKind kind) {
    diag::DiagInputs<TagName> diag_input;
    diag_input.kind_ = kind;
    diag_input.main_token_ = tok;
    diag::doing<TagName>(diag_input.main_token_, diag_input.kind_,
                         diag_input.context_tokens_);
  }

  defined_tokens_type define_tokens_;
};

}  // namespace lps::tu
