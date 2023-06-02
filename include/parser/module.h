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

#include "parser/function.h"

namespace lps::parser::details {

// global_module_fragment:
//  `module` `;` declaration-seq[opt]
template <meta::Str TagName>
ParseFunctionOutputs<TagName> GlobalModuleFragment<TagName>::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  auto cur_tok = this->params_.last_token_;

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
      output.last_token_ = tok_semi;
      output.file_id_ = tok_semi.file_id();
      output.start_ = lexer.cur();
      output.work_ = true;
    }
    {
      ParseFunctionInputs<TagName + "_serial_funcs"> a(
          true, output.start_, output.file_id_,
          token::Token<TagName + "_serial_funcs">(output.last_token_));
      SerialParseFunctions serial_funcs(a, DeclarationSeq<>(true));

      output.concat(serial_funcs(), true);
    }
  }

  return output;
}

// module_declaration:
//  `export`[opt] `module` module_name module_partition[opt] attribute_specifier_seq[opt] `;`
template <meta::Str TagName>
ParseFunctionOutputs<TagName> ModuleDeclaration<TagName>::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  auto cur_tok = this->params_.last_token_;

  // `export`[opt] `module` module_name
  {
    lexer::Lexer lexer(this->file_id(), this->start());
    // `export`[opt] `module`
    {
      token::Token<TagName + "_module_declaration"> tok;
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
    }

    // module_name
    {
      token::Token<TagName + "_module_name"> tok;
      lexer.lex(tok);
      if (tok.kind() != token::tok::TokenKind::identifier) {
        if (!this->opt()) {
          output.diag_record(diag::record<TagName>(
              tok, diag::DiagKind::module_decl_expect_ident));
        }

        output.work_ = false;
        return output;
      }

      {
        output.last_token_ = tok;
        output.file_id_ = tok.file_id();
        output.start_ = lexer.cur();
      }
    }
  }

  // module_partition[opt] attribute_specifier_seq[opt]
  {
    ParseFunctionInputs<TagName + "_serial_funcs"> a(
        true, output.start_, output.file_id_,
        token::Token<TagName + "_serial_funcs">(output.last_token_));
    SerialParseFunctions serial_funcs(a, ModulePartition<>(true),
                                      AttributeSpecifierSeq<>(true));

    output.concat(serial_funcs(), true);
  }

  {
    lexer::Lexer lexer(output.file_id_, output.start_);
    token::Token<TagName + "_semi"> tok;
    lexer.lex(tok);
    if (tok.kind() != token::tok::TokenKind::semi) {
      if (!this->opt()) {
        output.diag_record(diag::record<TagName>(
            tok, diag::DiagKind::global_module_expect_semi,
            output.last_token_));
      }

      output.work_ = false;
      return output;
    }

    {
      output.last_token_ = tok;
      output.file_id_ = tok.file_id();
      output.start_ = lexer.cur();
      output.work_ = true;
    }
    return output;
  }
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> ModulePartition<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> PrivateModuleFragment<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> AttributeSpecifierSeq<TagName>::operator()() {
  return base::operator()();
}

}  // namespace lps::parser::details
