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

#include "parser.h"
#include "basic/exception.h"
#include "lexer.h"
#include "token.h"

namespace lps::parser {

namespace details {

template <meta::Str TagName>
class TranslationUnit : public ParseFunction<TagName> {
 public:
  using base = ParseFunction<TagName>;
  explicit TranslationUnit(const ParseFunctionInputs<TagName>& param)
      : base(param) {}
  template <meta::Str TagNameOther>
  explicit TranslationUnit(const ParseFunctionInputs<TagNameOther>& param)
      : base(param) {}
  ParseFunctionOutputs<TagName> operator()();
};
template <meta::Str TagName>
class DeclarationSeq : public ParseFunction<TagName> {
 public:
  using base = ParseFunction<TagName>;
  explicit DeclarationSeq(const ParseFunctionInputs<TagName>& param)
      : base(param) {}
  template <meta::Str TagNameOther>
  explicit DeclarationSeq(const ParseFunctionInputs<TagNameOther>& param)
      : base(param) {}
  ParseFunctionOutputs<TagName> operator()();
};

template <meta::Str TagName>
class ModulePartition : public ParseFunction<TagName> {
 public:
  using base = ParseFunction<TagName>;
  explicit ModulePartition(const ParseFunctionInputs<TagName>& param)
      : base(param) {}
  template <meta::Str TagNameOther>
  explicit ModulePartition(const ParseFunctionInputs<TagNameOther>& param)
      : base(param) {}
  ParseFunctionOutputs<TagName> operator()();
};

template <meta::Str TagName>
class AttributeSpecifierSeq : public ParseFunction<TagName> {
 public:
  using base = ParseFunction<TagName>;
  explicit AttributeSpecifierSeq(const ParseFunctionInputs<TagName>& param)
      : base(param) {}
  template <meta::Str TagNameOther>
  explicit AttributeSpecifierSeq(const ParseFunctionInputs<TagNameOther>& param)
      : base(param) {}
  ParseFunctionOutputs<TagName> operator()();
};

template <meta::Str TagName, meta::Str LastTokenTagName>
class GlobalModuleFragment : public ParseFunction<TagName> {
 public:
  using base = ParseFunction<TagName>;
  explicit GlobalModuleFragment(const ParseFunctionInputs<TagName>& param)
      : base(param) {}
  template <meta::Str TagNameOther>
  explicit GlobalModuleFragment(const ParseFunctionInputs<TagNameOther>& param)
      : base(param) {}
  ParseFunctionOutputs<TagName> operator()(
      const token::Token<LastTokenTagName>& tok);
};

template <meta::Str TagName, meta::Str LastTokenTagName>
class ModuleDeclaration : public ParseFunction<TagName> {
 public:
  using base = ParseFunction<TagName>;
  explicit ModuleDeclaration(const ParseFunctionInputs<TagName>& param)
      : base(param) {}
  template <meta::Str TagNameOther>
  explicit ModuleDeclaration(const ParseFunctionInputs<TagNameOther>& param)
      : base(param) {}
  ParseFunctionOutputs<TagName> operator()(
      const token::Token<LastTokenTagName>& tok);
};

// global_module_fragment:
//  `module` `;` declaration-seq[opt]
template <meta::Str TagName, meta::Str LastTokenTagName>
ParseFunctionOutputs<TagName>
GlobalModuleFragment<TagName, LastTokenTagName>::operator()(
    const token::Token<LastTokenTagName>& cur_tok) {
  ParseFunctionOutputs<TagName> output;
  output.file_id_ = this->file_id();
  output.start_ = this->start();

  lexer::Lexer lexer(this->file_id(), this->start());
  if (cur_tok.kind() == token::tok::TokenKind::kw_module) {

    token::Token<meta::S("global_module_fragment_semi")> tok_semi;

    if (tok_semi.kind() != token::tok::TokenKind::semi) {
      if (!this->opt()) {
        output.diag_record(diag::record<TagName>(
            tok_semi, diag::DiagKind::global_module_expect_semi, cur_tok));
      }

      output.work_ = false;
      return output;
    }

    {
      ParseFunctionInputs<meta::Str("DeclarationSeq")> params;
      params.opt_ = true;
      params.file_id_ = this->file_id();

      DeclarationSeq<meta::Str("DeclarationSeq")> func(params);
      auto out0 = func();
      if (!out0.work_) {
        output.concat_diag_inputs_and_remove(out0);
      }
    }
  }

  if (!this->opt() && !output.work_) {
    unreachable(TagName);
  }

  return output;
}

// module-declaration:
//  `export`[opt] `module` module-name module_partition[opt] attribute_specifier_seq[opt] `;`
template <meta::Str TagName, meta::Str LastTokenTagName>
ParseFunctionOutputs<TagName>
ModuleDeclaration<TagName, LastTokenTagName>::operator()(
    const token::Token<LastTokenTagName>& cur_tok) {

  ParseFunctionOutputs<TagName> output;
  output.file_id_ = this->file_id();
  output.start_ = this->start();

  lexer::Lexer lexer(this->file_id(), this->start());
  token::Token<meta::S("module-declaration")> tok;
  tok = cur_tok;

  // `export`[opt]
  if (tok.kind() == token::tok::TokenKind::kw_export) {
    lexer.lex(tok);
  }

  // `module`
  if (tok.kind() != token::tok::TokenKind::kw_module) {
    if (!this->opt()) {
      output.diag_record(diag::record<TagName>(
          tok, diag::DiagKind::module_decl_expect_kw_module));
    }

    output.work_ = false;
    return output;
  }

  token::Token<meta::S("module-declaration_kw_module")> ident_tok;
  lexer.lex(ident_tok);
  // module-name
  if (ident_tok.kind() != token::tok::TokenKind::identifier) {
    if (!this->opt()) {
      output.diag_record(diag::record<TagName>(
          ident_tok, diag::DiagKind::module_decl_expect_ident, tok));
    }
    output.work_ = false;
    return output;
  }

  //module_partition[opt]
  {
    ParseFunctionInputs<meta::Str("")> params;
    params.opt_ = true;
    params.file_id_ = this->file_id();

    ModulePartition<meta::Str("")> func(params);
    auto out0 = func();
    if (!out0.work_) {
      output.concat_diag_inputs_and_remove(out0);
    } else {
      output = out0;
    }
  }

  //attribute_specifier_seq[opt]
  {
    ParseFunctionInputs<meta::Str("")> params;
    params.opt_ = true;
    params.start_ = output.start_;
    params.file_id_ = output.file_id_;

    AttributeSpecifierSeq<meta::Str("")> func(params);
    output = func();
  }

  return output;
}

// translation_unit:
//  declaration_seq
//  global_module_fragment[opt] module_declaration declaration_seq[opt] private_module_fragment[opt]
template <meta::Str TagName>
ParseFunctionOutputs<TagName> TranslationUnit<TagName>::operator()() {

  ParseFunctionOutputs<TagName> output;
  output.file_id_ = this->file_id();
  output.start_ = this->start();

  constexpr meta::Str kFistTokTag("translation_unit");
  token::Token<kFistTokTag> tok;

  lexer::Lexer lexer(this->file_id(), this->start());
  lexer.lex(tok);

  // declaration_seq
  {
    ParseFunctionInputs<meta::S("declaration_seq0")> params;
    params.opt_ = false;
    params.start_ = lexer.cur();
    DeclarationSeq<meta::S("declaration_seq0")> func(params);
    output = func();
  }
  if (!output.work_) {

    // global_module_fragment[opt]
    {
      ParseFunctionInputs<meta::S("global_module_fragment")> params;
      params.opt_ = true;
      params.start_ = lexer.cur();
      params.file_id_ = this->file_id();
      GlobalModuleFragment<meta::S("global_module_fragment"), kFistTokTag> func(
          params);
      output = func(tok);
    }

    // module_declaration
    {
      ParseFunctionInputs<meta::S("module_declaration")> params;
      params.opt_ = false;
      if (output.work_) {
        params.start_ = output.start_;
        params.file_id_ = output.file_id_;
      } else {
        params.start_ = lexer.cur();
        params.file_id_ = this->file_id();
      }

      ModuleDeclaration<meta::S("module_declaration"), kFistTokTag> func(
          params);
      output = func(tok);
    }
  }

  return output;
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> DeclarationSeq<TagName>::operator()() {
  return ParseFunctionOutputs<TagName>();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> ModulePartition<TagName>::operator()() {
  return ParseFunctionOutputs<TagName>();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> AttributeSpecifierSeq<TagName>::operator()() {
  return ParseFunctionOutputs<TagName>();
}

}  // namespace details

// cpp grammar: https://timsong-cpp.github.io/cppwp/n4868/gram
void Parser::parse(uint32_t file_id) {
  auto content =
      src::Manager::instance().ref<meta::S("file_contents")>(file_id);
  if (!content.empty()) {
    details::ParseFunctionInputs<meta::Str("translation_unit_param")> params;
    params.opt_ = false;
    params.file_id_ = file_id;
    params.start_ = content.data();
    details::TranslationUnit<meta::Str("translation_unit")> func(params);
    func();
  }
}

void Parser::translation_unit(uint32_t file_id) {}

// declaration_seq:
//  declaration
//  declaration_seq declaration
void Parser::declaration_seq() {}

}  // namespace lps::parser
