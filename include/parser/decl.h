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

#include "diag.h"
#include "parser/function.h"
#include "token.h"

namespace lps::parser::details {

// declaration_seq:
//  declaration
//  declaration_seq declaration
template <meta::Str TagName>
ParseFunctionOutputs<TagName> DeclarationSeq<TagName>::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  auto cur_token = output.last_token_;

  token::Token<TagName + "_first_token"> tok;

  lexer::Lexer lexer(this->file_id(), this->start());
  lexer.lex(tok);

  SerialParseFunctions serial_funcs(ParseFunctionInputs<TagName>(false),
                                    DeclarationSeq<>(false),
                                    Declaration<>(false));

  ParallelParseFunctions parallel_funcs(
      ParseFunctionInputs<TagName>(false, lexer.cur(), this->file_id(),
                                   token::Token<TagName>(tok)),
      Declaration<>(false), std::move(serial_funcs));

  return parallel_funcs();
}

// declaration:
//   block_declaration
//   nodeclspec_function_declaration
//   function_definition
//   template_declaration
//   deduction_guide
//   explicit_instantiation
//   explicit_specialization
//   export_declaration
//   linkage_specification
//   namespace_definition
//   empty_declaration
//   attribute_declaration
//   module_import_declaration
template <meta::Str TagName>
ParseFunctionOutputs<TagName> Declaration<TagName>::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  token::Token<TagName + "_first_token"> tok;

  lexer::Lexer lexer(this->file_id(), this->start());
  lexer.lex(tok);

  ParallelParseFunctions parallel_funcs(
      ParseFunctionInputs<TagName>(false, lexer.cur(), this->file_id(),
                                   token::Token<TagName>(tok)),
      BlockDeclaration<>(false), NodeclspecFunctionDeclaration<>(false),
      FunctionDefinition<>(false), TemplateDeclaration<>(false),
      DeductionGuide<>(false), ExplicitInstantiation<>(false),
      ExplicitSpecialization<>(false), ExportDeclaration<>(false),
      LinkageSpecification<>(false), NamespaceDefinition<>(false),
      EmptyDeclaration<>(false), AttributeDeclaration<>(false),
      ModuleImportDeclaration<>(false));

  return parallel_funcs();
}

// block_declaration:
// simple-declaration
// asm-declaration
// namespace-alias-definition
// using-declaration
// using-enum-declaration
// using-directive
// static_assert-declaration
// alias-declaration
// opaque-enum-declaration
template <meta::Str TagName>
ParseFunctionOutputs<TagName> BlockDeclaration<TagName>::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  token::Token<TagName + "_first_token"> tok;

  lexer::Lexer lexer(this->file_id(), this->start());
  lexer.lex(tok);

  ParallelParseFunctions parallel_funcs(
      ParseFunctionInputs<TagName>(false, lexer.cur(), this->file_id(),
                                   token::Token<TagName>(tok)),
      SimpleDeclaration<>(false), AsmDeclaration<>(false),
      NamespaceAliasDefinition<>(false), UsingDeclaration<>(false),
      UsingEnumDeclaration<>(false), UsingDirective<>(false),
      StaticAssertDeclaration<>(false), AliasDeclaration<>(false),
      OpaqueEnumDeclaration<>(false));

  return parallel_funcs();
}

// simple_declaration:
//  decl_specifier_seq init_declarator_list[opt] `;`
//  attribute_specifier_seq decl_specifier_seq init_declarator_list `;`
//  attribute_specifier_seq[opt] decl-specifier-seq ref-qualifier[opt] `[` identifier_list `]` initializer `;`
template <meta::Str TagName>
ParseFunctionOutputs<TagName> SimpleDeclaration<TagName>::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  token::Token<TagName + "_first_token"> tok;

  lexer::Lexer lexer(this->file_id(), this->start());
  lexer.lex(tok);

  SerialParseFunctions serial_funcs0(
      ParseFunctionInputs<TagName + "_serial_funcs0">(
          false, lexer.cur(), this->file_id(),
          token::Token<TagName + "_serial_funcs0">(tok)),
      DeclSpecifierSeq<>(false), InitDeclaratorList<>(true),
      ParseFunction<TagName + "_serial_funcs0" + "_check `;`">::
          create_single_token_check(
              token::tok::TokenKind::semi,
              diag::DiagKind::simple_declaration_expect_semi));

  SerialParseFunctions serial_funcs1(
      ParseFunctionInputs<TagName + "_serial_funcs1">(
          false, lexer.cur(), this->file_id(),
          token::Token<TagName + "_serial_funcs1">(tok)),
      AttributeSpecifierSeq<>(false), DeclSpecifierSeq<>(false),
      InitDeclaratorList<>(false),
      ParseFunction<TagName + "_serial_funcs1" + "_check `;`">::
          create_single_token_check(
              token::tok::TokenKind::semi,
              diag::DiagKind::simple_declaration_expect_semi));

  SerialParseFunctions serial_funcs2(
      ParseFunctionInputs<TagName + "_serial_funcs2">(
          false, lexer.cur(), this->file_id(),
          token::Token<TagName + "_serial_funcs2">(tok)),
      AttributeSpecifierSeq<>(true), DeclSpecifierSeq<>(false),
      RefQualifier<>(true),
      ParseFunction<TagName + "_serial_funcs2" + "_check `[`">::
          create_single_token_check(
              token::tok::TokenKind::l_square,
              diag::DiagKind::simple_declaration_expect_semi),
      IdentifierList<>(false),
      ParseFunction<TagName + "_serial_funcs2" + "_check `]`">::
          create_single_token_check(
              token::tok::TokenKind::r_square,
              diag::DiagKind::simple_declaration_expect_semi),
      Initializer<>(false),
      ParseFunction<TagName + "_serial_funcs2" + "_check `;`">::
          create_single_token_check(
              token::tok::TokenKind::semi,
              diag::DiagKind::simple_declaration_expect_semi));

  ParallelParseFunctions parallel_funcs(
      ParseFunctionInputs<TagName>(true, lexer.cur(), this->file_id(),
                                   token::Token<TagName>(tok)),
      std::move(serial_funcs0), std::move(serial_funcs1),
      std::move(serial_funcs2));

  return parallel_funcs();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> RefQualifier<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> IdentifierList<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> InitDeclaratorList<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> Initializer<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> DeclSpecifierSeq<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> AsmDeclaration<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> NamespaceAliasDefinition<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> UsingEnumDeclaration<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> UsingDirective<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> StaticAssertDeclaration<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> AliasDeclaration<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> OpaqueEnumDeclaration<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> UsingDeclaration<TagName>::operator()() {
  return base::operator()();
}

// nodeclspec_function_declaration:
//  attribute_specifier_seq[opt] declarator `;`
template <meta::Str TagName>
ParseFunctionOutputs<TagName>
NodeclspecFunctionDeclaration<TagName>::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  token::Token<TagName + "_first_token"> tok;

  lexer::Lexer lexer(this->file_id(), this->start());
  lexer.lex(tok);

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<TagName>(false),
      AttributeSpecifierSeq<TagName + "_AttributeSpecifierSeq">(true),
      Declarator<TagName + "_Declarator">(false));
  return serial_funcs();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> FunctionDefinition<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> TemplateDeclaration<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> DeductionGuide<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> ExplicitInstantiation<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> ExplicitSpecialization<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> ExportDeclaration<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> LinkageSpecification<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> NamespaceDefinition<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> EmptyDeclaration<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> AttributeDeclaration<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> ModuleImportDeclaration<TagName>::operator()() {
  return base::operator()();
}

// declarator:
//  ptr_declarator
//  noptr_declarator parameters_and_qualifiers trailing_return_type
template <meta::Str TagName>
ParseFunctionOutputs<TagName> Declarator<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> PtrDeclarator<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> NoptrDeclarator<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> ParametersAndQualifiers<TagName>::operator()() {
  return base::operator()();
}

template <meta::Str TagName>
ParseFunctionOutputs<TagName> TrailingReturnType<TagName>::operator()() {
  return base::operator()();
}

}  // namespace lps::parser::details
