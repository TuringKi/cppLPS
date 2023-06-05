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
#include "parse_function/function.h"
#include "parse_function/parallel_function_impl.h"
#include "parse_function/serial_function_impl.h"
#include "token.h"

namespace lps::parser::details {

// translation-unit:
// 	declaration_seq[opt]
// 	global_module_fragment[opt], module_declaration, declaration_seq[opt], private_module_fragment[opt]
inline ParseFunctionOutputs<TranslationUnit::kTag>
TranslationUnit::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      TranslationUnit::kTag, 2,
      SerialParseFunctions<TranslationUnit::kTag, DeclarationSeq>,
      SerialParseFunctions<TranslationUnit::kTag, GlobalModuleFragment,
                           ModuleDeclaration, DeclarationSeq,
                           PrivateModuleFragment>>
      parallel_funcs_0(ParseFunctionInputs<TranslationUnit::kTag>(
                           false, output.last_token_, output.cur_token_),
                       SerialParseFunctions(
                           ParseFunctionInputs<TranslationUnit::kTag>(false),
                           DeclarationSeq(true)),
                       SerialParseFunctions(
                           ParseFunctionInputs<TranslationUnit::kTag>(false),
                           GlobalModuleFragment(true), ModuleDeclaration(false),
                           DeclarationSeq(true), PrivateModuleFragment(true)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// primary-expression:
// 	literal
// 	`this`
// 	`(`, expression, `)`
// 	id_expression
// 	lambda_expression
// 	fold_expression
// 	requires_expression
inline ParseFunctionOutputs<PrimaryExpression::kTag>
PrimaryExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      PrimaryExpression::kTag, 7,
      SerialParseFunctions<PrimaryExpression::kTag, Literal>,
      SerialParseFunctions<PrimaryExpression::kTag,
                           ParseFunction<PrimaryExpression::kTag>>,
      SerialParseFunctions<PrimaryExpression::kTag,
                           ParseFunction<PrimaryExpression::kTag>, Expression,
                           ParseFunction<PrimaryExpression::kTag>>,
      SerialParseFunctions<PrimaryExpression::kTag, IdExpression>,
      SerialParseFunctions<PrimaryExpression::kTag, LambdaExpression>,
      SerialParseFunctions<PrimaryExpression::kTag, FoldExpression>,
      SerialParseFunctions<PrimaryExpression::kTag, RequiresExpression>>
      parallel_funcs_0(
          ParseFunctionInputs<PrimaryExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<PrimaryExpression::kTag>(false),
              Literal(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PrimaryExpression::kTag>(false),
              ParseFunction<PrimaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_this,
                  diag::DiagKind::primary_expression_expect_kw_this)),
          SerialParseFunctions(
              ParseFunctionInputs<PrimaryExpression::kTag>(false),
              ParseFunction<PrimaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::primary_expression_expect_l_paren),
              Expression(false),
              ParseFunction<PrimaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::primary_expression_expect_r_paren)),
          SerialParseFunctions(
              ParseFunctionInputs<PrimaryExpression::kTag>(false),
              IdExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PrimaryExpression::kTag>(false),
              LambdaExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PrimaryExpression::kTag>(false),
              FoldExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PrimaryExpression::kTag>(false),
              RequiresExpression(false)));

  static_assert(base::kNumberOfElements >= 7);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// id-expression:
// 	unqualified_id
// 	qualified_id
inline ParseFunctionOutputs<IdExpression::kTag> IdExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      IdExpression::kTag, 2,
      SerialParseFunctions<IdExpression::kTag, UnqualifiedId>,
      SerialParseFunctions<IdExpression::kTag, QualifiedId>>
      parallel_funcs_0(
          ParseFunctionInputs<IdExpression::kTag>(false, output.last_token_,
                                                  output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<IdExpression::kTag>(false),
                               UnqualifiedId(false)),
          SerialParseFunctions(ParseFunctionInputs<IdExpression::kTag>(false),
                               QualifiedId(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// unqualified-id:
// 	`identifier`
// 	operator_function_id
// 	conversion_function_id
// 	literal_operator_id
// 	`~`, type_name
// 	`~`, decltype_specifier
// 	`~`, template_id
inline ParseFunctionOutputs<UnqualifiedId::kTag> UnqualifiedId::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      UnqualifiedId::kTag, 7,
      SerialParseFunctions<UnqualifiedId::kTag,
                           ParseFunction<UnqualifiedId::kTag>>,
      SerialParseFunctions<UnqualifiedId::kTag, OperatorFunctionId>,
      SerialParseFunctions<UnqualifiedId::kTag, ConversionFunctionId>,
      SerialParseFunctions<UnqualifiedId::kTag, LiteralOperatorId>,
      SerialParseFunctions<UnqualifiedId::kTag,
                           ParseFunction<UnqualifiedId::kTag>, TypeName>,
      SerialParseFunctions<UnqualifiedId::kTag,
                           ParseFunction<UnqualifiedId::kTag>,
                           DecltypeSpecifier>,
      SerialParseFunctions<UnqualifiedId::kTag,
                           ParseFunction<UnqualifiedId::kTag>, TemplateId>>
      parallel_funcs_0(
          ParseFunctionInputs<UnqualifiedId::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<UnqualifiedId::kTag>(false),
              ParseFunction<UnqualifiedId::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::unqualified_id_expect_identifier)),
          SerialParseFunctions(ParseFunctionInputs<UnqualifiedId::kTag>(false),
                               OperatorFunctionId(false)),
          SerialParseFunctions(ParseFunctionInputs<UnqualifiedId::kTag>(false),
                               ConversionFunctionId(false)),
          SerialParseFunctions(ParseFunctionInputs<UnqualifiedId::kTag>(false),
                               LiteralOperatorId(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UnqualifiedId::kTag>(false),
              ParseFunction<UnqualifiedId::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::tilde,
                  diag::DiagKind::unqualified_id_expect_tilde),
              TypeName(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UnqualifiedId::kTag>(false),
              ParseFunction<UnqualifiedId::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::tilde,
                  diag::DiagKind::unqualified_id_expect_tilde),
              DecltypeSpecifier(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UnqualifiedId::kTag>(false),
              ParseFunction<UnqualifiedId::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::tilde,
                  diag::DiagKind::unqualified_id_expect_tilde),
              TemplateId(false)));

  static_assert(base::kNumberOfElements >= 7);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// qualified-id:
// 	nested_name_specifier, `template`[opt], unqualified_id
inline ParseFunctionOutputs<QualifiedId::kTag> QualifiedId::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<QualifiedId::kTag>(false, output.last_token_,
                                             output.cur_token_),
      NestedNameSpecifier(false),
      ParseFunction<QualifiedId::kTag>::create_single_token_check(
          true, token::tok::TokenKind::kw_template,
          diag::DiagKind::qualified_id_expect_kw_template),
      UnqualifiedId(false));
  return serial_funcs();
}

// nested-name-specifier:
// 	`::`
// 	type_name, `::`
// 	namespace_name, `::`
// 	decltype_specifier, `::`
// 	nested_name_specifier, `identifier`, `::`
// 	nested_name_specifier, `template`[opt], simple_template_id, `::`
inline ParseFunctionOutputs<NestedNameSpecifier::kTag>
NestedNameSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        NestedNameSpecifier::kTag, 4,
        SerialParseFunctions<NestedNameSpecifier::kTag,
                             ParseFunction<NestedNameSpecifier::kTag>>,
        SerialParseFunctions<NestedNameSpecifier::kTag, TypeName,
                             ParseFunction<NestedNameSpecifier::kTag>>,
        SerialParseFunctions<NestedNameSpecifier::kTag, NamespaceName,
                             ParseFunction<NestedNameSpecifier::kTag>>,
        SerialParseFunctions<NestedNameSpecifier::kTag, DecltypeSpecifier,
                             ParseFunction<NestedNameSpecifier::kTag>>>
        parallel_funcs_0(
            ParseFunctionInputs<NestedNameSpecifier::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<NestedNameSpecifier::kTag>(false),
                ParseFunction<NestedNameSpecifier::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::coloncolon,
                        diag::DiagKind::
                            nested_name_specifier_expect_coloncolon)),
            SerialParseFunctions(
                ParseFunctionInputs<NestedNameSpecifier::kTag>(false),
                TypeName(false),
                ParseFunction<NestedNameSpecifier::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::coloncolon,
                        diag::DiagKind::
                            nested_name_specifier_expect_coloncolon)),
            SerialParseFunctions(
                ParseFunctionInputs<NestedNameSpecifier::kTag>(false),
                NamespaceName(false),
                ParseFunction<NestedNameSpecifier::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::coloncolon,
                        diag::DiagKind::
                            nested_name_specifier_expect_coloncolon)),
            SerialParseFunctions(
                ParseFunctionInputs<NestedNameSpecifier::kTag>(false),
                DecltypeSpecifier(false),
                ParseFunction<NestedNameSpecifier::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::coloncolon,
                        diag::DiagKind::
                            nested_name_specifier_expect_coloncolon)));

    static_assert(base::kNumberOfElements >= 4);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      NestedNameSpecifier::kTag, 2,
      SerialParseFunctions<NestedNameSpecifier::kTag, NestedNameSpecifier,
                           ParseFunction<NestedNameSpecifier::kTag>,
                           ParseFunction<NestedNameSpecifier::kTag>>,
      SerialParseFunctions<NestedNameSpecifier::kTag, NestedNameSpecifier,
                           ParseFunction<NestedNameSpecifier::kTag>,
                           SimpleTemplateId,
                           ParseFunction<NestedNameSpecifier::kTag>>>
      parallel_funcs_1(
          ParseFunctionInputs<NestedNameSpecifier::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<NestedNameSpecifier::kTag>(false),
              NestedNameSpecifier(false),
              ParseFunction<NestedNameSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::identifier,
                      diag::DiagKind::nested_name_specifier_expect_identifier),
              ParseFunction<NestedNameSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::coloncolon,
                      diag::DiagKind::nested_name_specifier_expect_coloncolon)),
          SerialParseFunctions(
              ParseFunctionInputs<NestedNameSpecifier::kTag>(false),
              NestedNameSpecifier(false),
              ParseFunction<NestedNameSpecifier::kTag>::
                  create_single_token_check(
                      true, token::tok::TokenKind::kw_template,
                      diag::DiagKind::nested_name_specifier_expect_kw_template),
              SimpleTemplateId(false),
              ParseFunction<NestedNameSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::coloncolon,
                      diag::DiagKind::
                          nested_name_specifier_expect_coloncolon)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// lambda-expression:
// 	lambda_introducer, lambda_declarator[opt], compound_statement
// 	lambda_introducer, `<`, template_parameter_list, `>`, requires_clause[opt], lambda_declarator[opt], compound_statement
inline ParseFunctionOutputs<LambdaExpression::kTag>
LambdaExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      LambdaExpression::kTag, 2,
      SerialParseFunctions<LambdaExpression::kTag, LambdaIntroducer,
                           LambdaDeclarator, CompoundStatement>,
      SerialParseFunctions<LambdaExpression::kTag, LambdaIntroducer,
                           ParseFunction<LambdaExpression::kTag>,
                           TemplateParameterList,
                           ParseFunction<LambdaExpression::kTag>,
                           RequiresClause, LambdaDeclarator, CompoundStatement>>
      parallel_funcs_0(
          ParseFunctionInputs<LambdaExpression::kTag>(false, output.last_token_,
                                                      output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<LambdaExpression::kTag>(false),
              LambdaIntroducer(false), LambdaDeclarator(true),
              CompoundStatement(false)),
          SerialParseFunctions(
              ParseFunctionInputs<LambdaExpression::kTag>(false),
              LambdaIntroducer(false),
              ParseFunction<LambdaExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::less,
                  diag::DiagKind::lambda_expression_expect_less),
              TemplateParameterList(false),
              ParseFunction<LambdaExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::greater,
                  diag::DiagKind::lambda_expression_expect_greater),
              RequiresClause(true), LambdaDeclarator(true),
              CompoundStatement(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// lambda-introducer:
// 	`[`, lambda_capture[opt], `]`
inline ParseFunctionOutputs<LambdaIntroducer::kTag>
LambdaIntroducer::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<LambdaIntroducer::kTag>(false, output.last_token_,
                                                  output.cur_token_),
      ParseFunction<LambdaIntroducer::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_square,
          diag::DiagKind::lambda_introducer_expect_l_square),
      LambdaCapture(true),
      ParseFunction<LambdaIntroducer::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_square,
          diag::DiagKind::lambda_introducer_expect_r_square));
  return serial_funcs();
}

// lambda-declarator:
// 	`(`, parameter_declaration_clause, `)`, decl_specifier_seq[opt]
// 	noexcept_specifier[opt], attribute_specifier_seq[opt], trailing_return_type[opt], requires_clause[opt]
inline ParseFunctionOutputs<LambdaDeclarator::kTag>
LambdaDeclarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      LambdaDeclarator::kTag, 2,
      SerialParseFunctions<
          LambdaDeclarator::kTag, ParseFunction<LambdaDeclarator::kTag>,
          ParameterDeclarationClause, ParseFunction<LambdaDeclarator::kTag>,
          DeclSpecifierSeq>,
      SerialParseFunctions<LambdaDeclarator::kTag, NoexceptSpecifier,
                           AttributeSpecifierSeq, TrailingReturnType,
                           RequiresClause>>
      parallel_funcs_0(
          ParseFunctionInputs<LambdaDeclarator::kTag>(false, output.last_token_,
                                                      output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<LambdaDeclarator::kTag>(false),
              ParseFunction<LambdaDeclarator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::lambda_declarator_expect_l_paren),
              ParameterDeclarationClause(false),
              ParseFunction<LambdaDeclarator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::lambda_declarator_expect_r_paren),
              DeclSpecifierSeq(true)),
          SerialParseFunctions(
              ParseFunctionInputs<LambdaDeclarator::kTag>(false),
              NoexceptSpecifier(true), AttributeSpecifierSeq(true),
              TrailingReturnType(true), RequiresClause(true)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// lambda-capture:
// 	capture_default
// 	capture_list
// 	capture_default, `,`, capture_list
inline ParseFunctionOutputs<LambdaCapture::kTag> LambdaCapture::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      LambdaCapture::kTag, 3,
      SerialParseFunctions<LambdaCapture::kTag, CaptureDefault>,
      SerialParseFunctions<LambdaCapture::kTag, CaptureList>,
      SerialParseFunctions<LambdaCapture::kTag, CaptureDefault,
                           ParseFunction<LambdaCapture::kTag>, CaptureList>>
      parallel_funcs_0(
          ParseFunctionInputs<LambdaCapture::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<LambdaCapture::kTag>(false),
                               CaptureDefault(false)),
          SerialParseFunctions(ParseFunctionInputs<LambdaCapture::kTag>(false),
                               CaptureList(false)),
          SerialParseFunctions(
              ParseFunctionInputs<LambdaCapture::kTag>(false),
              CaptureDefault(false),
              ParseFunction<LambdaCapture::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::comma,
                  diag::DiagKind::lambda_capture_expect_comma),
              CaptureList(false)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// capture-default:
// 	`&`
// 	`=`
inline ParseFunctionOutputs<CaptureDefault::kTag> CaptureDefault::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      CaptureDefault::kTag, 2,
      SerialParseFunctions<CaptureDefault::kTag,
                           ParseFunction<CaptureDefault::kTag>>,
      SerialParseFunctions<CaptureDefault::kTag,
                           ParseFunction<CaptureDefault::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<CaptureDefault::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<CaptureDefault::kTag>(false),
              ParseFunction<CaptureDefault::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::amp,
                  diag::DiagKind::capture_default_expect_amp)),
          SerialParseFunctions(
              ParseFunctionInputs<CaptureDefault::kTag>(false),
              ParseFunction<CaptureDefault::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::equal,
                  diag::DiagKind::capture_default_expect_equal)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// capture-list:
// 	capture
// 	capture_list, `,`, capture
inline ParseFunctionOutputs<CaptureList::kTag> CaptureList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<CaptureList::kTag, 1,
                           SerialParseFunctions<CaptureList::kTag, Capture>>
        parallel_funcs_0(
            ParseFunctionInputs<CaptureList::kTag>(false, output.last_token_,
                                                   output.cur_token_),
            SerialParseFunctions(ParseFunctionInputs<CaptureList::kTag>(false),
                                 Capture(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      CaptureList::kTag, 1,
      SerialParseFunctions<CaptureList::kTag, CaptureList,
                           ParseFunction<CaptureList::kTag>, Capture>>
      parallel_funcs_1(
          ParseFunctionInputs<CaptureList::kTag>(false, output.last_token_,
                                                 output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<CaptureList::kTag>(false), CaptureList(false),
              ParseFunction<CaptureList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::comma,
                  diag::DiagKind::capture_list_expect_comma),
              Capture(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// capture:
// 	simple_capture
// 	init_capture
inline ParseFunctionOutputs<Capture::kTag> Capture::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<Capture::kTag, 2,
                         SerialParseFunctions<Capture::kTag, SimpleCapture>,
                         SerialParseFunctions<Capture::kTag, InitCapture>>
      parallel_funcs_0(
          ParseFunctionInputs<Capture::kTag>(false, output.last_token_,
                                             output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<Capture::kTag>(false),
                               SimpleCapture(false)),
          SerialParseFunctions(ParseFunctionInputs<Capture::kTag>(false),
                               InitCapture(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// simple-capture:
// 	`identifier`, `...`[opt]
// 	`&`, `identifier`, `...`[opt]
// 	`this`
// 	`*`, `this`
inline ParseFunctionOutputs<SimpleCapture::kTag> SimpleCapture::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      SimpleCapture::kTag, 4,
      SerialParseFunctions<SimpleCapture::kTag,
                           ParseFunction<SimpleCapture::kTag>,
                           ParseFunction<SimpleCapture::kTag>>,
      SerialParseFunctions<SimpleCapture::kTag,
                           ParseFunction<SimpleCapture::kTag>,
                           ParseFunction<SimpleCapture::kTag>,
                           ParseFunction<SimpleCapture::kTag>>,
      SerialParseFunctions<SimpleCapture::kTag,
                           ParseFunction<SimpleCapture::kTag>>,
      SerialParseFunctions<SimpleCapture::kTag,
                           ParseFunction<SimpleCapture::kTag>,
                           ParseFunction<SimpleCapture::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<SimpleCapture::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleCapture::kTag>(false),
              ParseFunction<SimpleCapture::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::simple_capture_expect_identifier),
              ParseFunction<SimpleCapture::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::simple_capture_expect_ellipsis)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleCapture::kTag>(false),
              ParseFunction<SimpleCapture::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::amp,
                  diag::DiagKind::simple_capture_expect_amp),
              ParseFunction<SimpleCapture::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::simple_capture_expect_identifier),
              ParseFunction<SimpleCapture::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::simple_capture_expect_ellipsis)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleCapture::kTag>(false),
              ParseFunction<SimpleCapture::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_this,
                  diag::DiagKind::simple_capture_expect_kw_this)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleCapture::kTag>(false),
              ParseFunction<SimpleCapture::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::star,
                  diag::DiagKind::simple_capture_expect_star),
              ParseFunction<SimpleCapture::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_this,
                  diag::DiagKind::simple_capture_expect_kw_this)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// init-capture:
// 	`...`[opt], `identifier`, initializer
// 	`&`, `...`[opt], `identifier`, initializer
inline ParseFunctionOutputs<InitCapture::kTag> InitCapture::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      InitCapture::kTag, 2,
      SerialParseFunctions<InitCapture::kTag, ParseFunction<InitCapture::kTag>,
                           ParseFunction<InitCapture::kTag>, Initializer>,
      SerialParseFunctions<InitCapture::kTag, ParseFunction<InitCapture::kTag>,
                           ParseFunction<InitCapture::kTag>,
                           ParseFunction<InitCapture::kTag>, Initializer>>
      parallel_funcs_0(
          ParseFunctionInputs<InitCapture::kTag>(false, output.last_token_,
                                                 output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<InitCapture::kTag>(false),
              ParseFunction<InitCapture::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::init_capture_expect_ellipsis),
              ParseFunction<InitCapture::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::init_capture_expect_identifier),
              Initializer(false)),
          SerialParseFunctions(
              ParseFunctionInputs<InitCapture::kTag>(false),
              ParseFunction<InitCapture::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::amp,
                  diag::DiagKind::init_capture_expect_amp),
              ParseFunction<InitCapture::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::init_capture_expect_ellipsis),
              ParseFunction<InitCapture::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::init_capture_expect_identifier),
              Initializer(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// fold-expression:
// 	`(`, cast_expression, fold_operator, `...`, `)`
// 	`(`, `...`, fold_operator, cast_expression, `)`
// 	`(`, cast_expression, fold_operator, `...`, fold_operator, cast_expression, `)`
inline ParseFunctionOutputs<FoldExpression::kTag> FoldExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      FoldExpression::kTag, 3,
      SerialParseFunctions<FoldExpression::kTag,
                           ParseFunction<FoldExpression::kTag>, CastExpression,
                           FoldOperator, ParseFunction<FoldExpression::kTag>,
                           ParseFunction<FoldExpression::kTag>>,
      SerialParseFunctions<FoldExpression::kTag,
                           ParseFunction<FoldExpression::kTag>,
                           ParseFunction<FoldExpression::kTag>, FoldOperator,
                           CastExpression, ParseFunction<FoldExpression::kTag>>,
      SerialParseFunctions<
          FoldExpression::kTag, ParseFunction<FoldExpression::kTag>,
          CastExpression, FoldOperator, ParseFunction<FoldExpression::kTag>,
          FoldOperator, CastExpression, ParseFunction<FoldExpression::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<FoldExpression::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<FoldExpression::kTag>(false),
              ParseFunction<FoldExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::fold_expression_expect_l_paren),
              CastExpression(false), FoldOperator(false),
              ParseFunction<FoldExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::fold_expression_expect_ellipsis),
              ParseFunction<FoldExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::fold_expression_expect_r_paren)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldExpression::kTag>(false),
              ParseFunction<FoldExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::fold_expression_expect_l_paren),
              ParseFunction<FoldExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::fold_expression_expect_ellipsis),
              FoldOperator(false), CastExpression(false),
              ParseFunction<FoldExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::fold_expression_expect_r_paren)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldExpression::kTag>(false),
              ParseFunction<FoldExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::fold_expression_expect_l_paren),
              CastExpression(false), FoldOperator(false),
              ParseFunction<FoldExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::fold_expression_expect_ellipsis),
              FoldOperator(false), CastExpression(false),
              ParseFunction<FoldExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::fold_expression_expect_r_paren)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// fold-operator:
// 	`+`
// 	`_`
// 	`*`
// 	`/`
// 	`%`
// 	`^`
// 	`&`
// 	`|`
// 	`<<`
// 	`>>`
// 	`+=`
// 	`_=`
// 	`*=`
// 	`/=`
// 	`%=`
// 	`^=`
// 	`&=`
// 	`|=`
// 	`<<=`
// 	`>>=`
// 	`=`
// 	`==`
// 	`!=`
// 	`<`
// 	`>`
// 	`<=`
// 	`>=`
// 	`&&`
// 	`||`
// 	`,`
// 	`.*`
// 	`_>*`
inline ParseFunctionOutputs<FoldOperator::kTag> FoldOperator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      FoldOperator::kTag, 32,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>,
      SerialParseFunctions<FoldOperator::kTag,
                           ParseFunction<FoldOperator::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<FoldOperator::kTag>(false, output.last_token_,
                                                  output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::plus,
                  diag::DiagKind::fold_operator_expect_plus)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::minus,
                  diag::DiagKind::fold_operator_expect_minus)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::star,
                  diag::DiagKind::fold_operator_expect_star)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::slash,
                  diag::DiagKind::fold_operator_expect_slash)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::percent,
                  diag::DiagKind::fold_operator_expect_percent)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::caret,
                  diag::DiagKind::fold_operator_expect_caret)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::amp,
                  diag::DiagKind::fold_operator_expect_amp)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::pipe,
                  diag::DiagKind::fold_operator_expect_pipe)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::lessless,
                  diag::DiagKind::fold_operator_expect_lessless)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::greatergreater,
                  diag::DiagKind::fold_operator_expect_greatergreater)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::plusequal,
                  diag::DiagKind::fold_operator_expect_plusequal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::minusequal,
                  diag::DiagKind::fold_operator_expect_minusequal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::starequal,
                  diag::DiagKind::fold_operator_expect_starequal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::slashequal,
                  diag::DiagKind::fold_operator_expect_slashequal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::percentequal,
                  diag::DiagKind::fold_operator_expect_percentequal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::caretequal,
                  diag::DiagKind::fold_operator_expect_caretequal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::ampequal,
                  diag::DiagKind::fold_operator_expect_ampequal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::pipeequal,
                  diag::DiagKind::fold_operator_expect_pipeequal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::lesslessequal,
                  diag::DiagKind::fold_operator_expect_lesslessequal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::greatergreaterequal,
                  diag::DiagKind::fold_operator_expect_greatergreaterequal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::equal,
                  diag::DiagKind::fold_operator_expect_equal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::equalequal,
                  diag::DiagKind::fold_operator_expect_equalequal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::exclaimequal,
                  diag::DiagKind::fold_operator_expect_exclaimequal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::less,
                  diag::DiagKind::fold_operator_expect_less)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::greater,
                  diag::DiagKind::fold_operator_expect_greater)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::lessequal,
                  diag::DiagKind::fold_operator_expect_lessequal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::greaterequal,
                  diag::DiagKind::fold_operator_expect_greaterequal)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::ampamp,
                  diag::DiagKind::fold_operator_expect_ampamp)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::pipepipe,
                  diag::DiagKind::fold_operator_expect_pipepipe)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::comma,
                  diag::DiagKind::fold_operator_expect_comma)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::periodstar,
                  diag::DiagKind::fold_operator_expect_periodstar)),
          SerialParseFunctions(
              ParseFunctionInputs<FoldOperator::kTag>(false),
              ParseFunction<FoldOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::arrowstar,
                  diag::DiagKind::fold_operator_expect_arrowstar)));

  static_assert(base::kNumberOfElements >= 32);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// requires-expression:
// 	`requires`, requirement_parameter_list[opt], requirement_body
inline ParseFunctionOutputs<RequiresExpression::kTag>
RequiresExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<RequiresExpression::kTag>(false, output.last_token_,
                                                    output.cur_token_),
      ParseFunction<RequiresExpression::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_requires,
          diag::DiagKind::requires_expression_expect_kw_requires),
      RequirementParameterList(true), RequirementBody(false));
  return serial_funcs();
}

// requirement-parameter-list:
// 	`(`, parameter_declaration_clause[opt], `)`
inline ParseFunctionOutputs<RequirementParameterList::kTag>
RequirementParameterList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<RequirementParameterList::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<RequirementParameterList::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_paren,
          diag::DiagKind::requirement_parameter_list_expect_l_paren),
      ParameterDeclarationClause(true),
      ParseFunction<RequirementParameterList::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_paren,
          diag::DiagKind::requirement_parameter_list_expect_r_paren));
  return serial_funcs();
}

// requirement-body:
// 	`{`, requirement_seq, `}`
inline ParseFunctionOutputs<RequirementBody::kTag>
RequirementBody::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<RequirementBody::kTag>(false, output.last_token_,
                                                 output.cur_token_),
      ParseFunction<RequirementBody::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_brace,
          diag::DiagKind::requirement_body_expect_l_brace),
      RequirementSeq(false),
      ParseFunction<RequirementBody::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_brace,
          diag::DiagKind::requirement_body_expect_r_brace));
  return serial_funcs();
}

// requirement-seq:
// 	requirement
// 	requirement_seq, requirement
inline ParseFunctionOutputs<RequirementSeq::kTag> RequirementSeq::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        RequirementSeq::kTag, 1,
        SerialParseFunctions<RequirementSeq::kTag, Requirement>>
        parallel_funcs_0(ParseFunctionInputs<RequirementSeq::kTag>(
                             false, output.last_token_, output.cur_token_),
                         SerialParseFunctions(
                             ParseFunctionInputs<RequirementSeq::kTag>(false),
                             Requirement(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      RequirementSeq::kTag, 1,
      SerialParseFunctions<RequirementSeq::kTag, RequirementSeq, Requirement>>
      parallel_funcs_1(
          ParseFunctionInputs<RequirementSeq::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<RequirementSeq::kTag>(false),
                               RequirementSeq(false), Requirement(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// requirement:
// 	simple_requirement
// 	type_requirement
// 	compound_requirement
// 	nested_requirement
inline ParseFunctionOutputs<Requirement::kTag> Requirement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      Requirement::kTag, 4,
      SerialParseFunctions<Requirement::kTag, SimpleRequirement>,
      SerialParseFunctions<Requirement::kTag, TypeRequirement>,
      SerialParseFunctions<Requirement::kTag, CompoundRequirement>,
      SerialParseFunctions<Requirement::kTag, NestedRequirement>>
      parallel_funcs_0(
          ParseFunctionInputs<Requirement::kTag>(false, output.last_token_,
                                                 output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<Requirement::kTag>(false),
                               SimpleRequirement(false)),
          SerialParseFunctions(ParseFunctionInputs<Requirement::kTag>(false),
                               TypeRequirement(false)),
          SerialParseFunctions(ParseFunctionInputs<Requirement::kTag>(false),
                               CompoundRequirement(false)),
          SerialParseFunctions(ParseFunctionInputs<Requirement::kTag>(false),
                               NestedRequirement(false)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// simple-requirement:
// 	expression, `;`
inline ParseFunctionOutputs<SimpleRequirement::kTag>
SimpleRequirement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<SimpleRequirement::kTag>(false, output.last_token_,
                                                   output.cur_token_),
      Expression(false),
      ParseFunction<SimpleRequirement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::simple_requirement_expect_semi));
  return serial_funcs();
}

// type-requirement:
// 	`typename`, nested_name_specifier[opt], type_name, `;`
inline ParseFunctionOutputs<TypeRequirement::kTag>
TypeRequirement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<TypeRequirement::kTag>(false, output.last_token_,
                                                 output.cur_token_),
      ParseFunction<TypeRequirement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_typename,
          diag::DiagKind::type_requirement_expect_kw_typename),
      NestedNameSpecifier(true), TypeName(false),
      ParseFunction<TypeRequirement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::type_requirement_expect_semi));
  return serial_funcs();
}

// compound-requirement:
// 	`{`, expression, `}`, `noexcept`[opt], return_type_requirement[opt], `;`
inline ParseFunctionOutputs<CompoundRequirement::kTag>
CompoundRequirement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<CompoundRequirement::kTag>(false, output.last_token_,
                                                     output.cur_token_),
      ParseFunction<CompoundRequirement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_brace,
          diag::DiagKind::compound_requirement_expect_l_brace),
      Expression(false),
      ParseFunction<CompoundRequirement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_brace,
          diag::DiagKind::compound_requirement_expect_r_brace),
      ParseFunction<CompoundRequirement::kTag>::create_single_token_check(
          true, token::tok::TokenKind::kw_noexcept,
          diag::DiagKind::compound_requirement_expect_kw_noexcept),
      ReturnTypeRequirement(true),
      ParseFunction<CompoundRequirement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::compound_requirement_expect_semi));
  return serial_funcs();
}

// return-type-requirement:
// 	`_>`, type_constraint
inline ParseFunctionOutputs<ReturnTypeRequirement::kTag>
ReturnTypeRequirement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ReturnTypeRequirement::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<ReturnTypeRequirement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::arrow,
          diag::DiagKind::return_type_requirement_expect_arrow),
      TypeConstraint(false));
  return serial_funcs();
}

// nested-requirement:
// 	`requires`, constraint_expression, `;`
inline ParseFunctionOutputs<NestedRequirement::kTag>
NestedRequirement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<NestedRequirement::kTag>(false, output.last_token_,
                                                   output.cur_token_),
      ParseFunction<NestedRequirement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_requires,
          diag::DiagKind::nested_requirement_expect_kw_requires),
      ConstraintExpression(false),
      ParseFunction<NestedRequirement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::nested_requirement_expect_semi));
  return serial_funcs();
}

// postfix-expression:
// 	primary_expression
// 	postfix_expression, `[`, expr_or_braced_init_list, `]`
// 	postfix_expression, `(`, expression_list[opt], `)`
// 	simple_type_specifier, `(`, expression_list[opt], `)`
// 	typename_specifier, `(`, expression_list[opt], `)`
// 	simple_type_specifier, braced_init_list
// 	typename_specifier, braced_init_list
// 	postfix_expression, `.`, `template`[opt], id_expression
// 	postfix_expression, `_>`, `template`[opt], id_expression
// 	postfix_expression, `++`
// 	postfix_expression, `__`
// 	`dynamic_cast`, `<`, type_id, `>`, `(`, expression, `)`
// 	`static_cast`, `<`, type_id, `>`, `(`, expression, `)`
// 	`reinterpret_cast`, `<`, type_id, `>`, `(`, expression, `)`
// 	`const_cast`, `<`, type_id, `>`, `(`, expression, `)`
// 	`typeid`, `(`, expression, `)`
// 	`typeid`, `(`, type_id, `)`
inline ParseFunctionOutputs<PostfixExpression::kTag>
PostfixExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        PostfixExpression::kTag, 11,
        SerialParseFunctions<PostfixExpression::kTag, PrimaryExpression>,
        SerialParseFunctions<PostfixExpression::kTag, SimpleTypeSpecifier,
                             ParseFunction<PostfixExpression::kTag>,
                             ExpressionList,
                             ParseFunction<PostfixExpression::kTag>>,
        SerialParseFunctions<PostfixExpression::kTag, TypenameSpecifier,
                             ParseFunction<PostfixExpression::kTag>,
                             ExpressionList,
                             ParseFunction<PostfixExpression::kTag>>,
        SerialParseFunctions<PostfixExpression::kTag, SimpleTypeSpecifier,
                             BracedInitList>,
        SerialParseFunctions<PostfixExpression::kTag, TypenameSpecifier,
                             BracedInitList>,
        SerialParseFunctions<PostfixExpression::kTag,
                             ParseFunction<PostfixExpression::kTag>,
                             ParseFunction<PostfixExpression::kTag>, TypeId,
                             ParseFunction<PostfixExpression::kTag>,
                             ParseFunction<PostfixExpression::kTag>, Expression,
                             ParseFunction<PostfixExpression::kTag>>,
        SerialParseFunctions<PostfixExpression::kTag,
                             ParseFunction<PostfixExpression::kTag>,
                             ParseFunction<PostfixExpression::kTag>, TypeId,
                             ParseFunction<PostfixExpression::kTag>,
                             ParseFunction<PostfixExpression::kTag>, Expression,
                             ParseFunction<PostfixExpression::kTag>>,
        SerialParseFunctions<PostfixExpression::kTag,
                             ParseFunction<PostfixExpression::kTag>,
                             ParseFunction<PostfixExpression::kTag>, TypeId,
                             ParseFunction<PostfixExpression::kTag>,
                             ParseFunction<PostfixExpression::kTag>, Expression,
                             ParseFunction<PostfixExpression::kTag>>,
        SerialParseFunctions<PostfixExpression::kTag,
                             ParseFunction<PostfixExpression::kTag>,
                             ParseFunction<PostfixExpression::kTag>, TypeId,
                             ParseFunction<PostfixExpression::kTag>,
                             ParseFunction<PostfixExpression::kTag>, Expression,
                             ParseFunction<PostfixExpression::kTag>>,
        SerialParseFunctions<PostfixExpression::kTag,
                             ParseFunction<PostfixExpression::kTag>,
                             ParseFunction<PostfixExpression::kTag>, Expression,
                             ParseFunction<PostfixExpression::kTag>>,
        SerialParseFunctions<PostfixExpression::kTag,
                             ParseFunction<PostfixExpression::kTag>,
                             ParseFunction<PostfixExpression::kTag>, TypeId,
                             ParseFunction<PostfixExpression::kTag>>>
        parallel_funcs_0(
            ParseFunctionInputs<PostfixExpression::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<PostfixExpression::kTag>(false),
                PrimaryExpression(false)),
            SerialParseFunctions(
                ParseFunctionInputs<PostfixExpression::kTag>(false),
                SimpleTypeSpecifier(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::l_paren,
                        diag::DiagKind::postfix_expression_expect_l_paren),
                ExpressionList(true),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::r_paren,
                        diag::DiagKind::postfix_expression_expect_r_paren)),
            SerialParseFunctions(
                ParseFunctionInputs<PostfixExpression::kTag>(false),
                TypenameSpecifier(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::l_paren,
                        diag::DiagKind::postfix_expression_expect_l_paren),
                ExpressionList(true),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::r_paren,
                        diag::DiagKind::postfix_expression_expect_r_paren)),
            SerialParseFunctions(
                ParseFunctionInputs<PostfixExpression::kTag>(false),
                SimpleTypeSpecifier(false), BracedInitList(false)),
            SerialParseFunctions(
                ParseFunctionInputs<PostfixExpression::kTag>(false),
                TypenameSpecifier(false), BracedInitList(false)),
            SerialParseFunctions(
                ParseFunctionInputs<PostfixExpression::kTag>(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::kw_dynamic_cast,
                        diag::DiagKind::
                            postfix_expression_expect_kw_dynamic_cast),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::less,
                        diag::DiagKind::postfix_expression_expect_less),
                TypeId(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::greater,
                        diag::DiagKind::postfix_expression_expect_greater),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::l_paren,
                        diag::DiagKind::postfix_expression_expect_l_paren),
                Expression(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::r_paren,
                        diag::DiagKind::postfix_expression_expect_r_paren)),
            SerialParseFunctions(
                ParseFunctionInputs<PostfixExpression::kTag>(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::kw_static_cast,
                        diag::DiagKind::
                            postfix_expression_expect_kw_static_cast),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::less,
                        diag::DiagKind::postfix_expression_expect_less),
                TypeId(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::greater,
                        diag::DiagKind::postfix_expression_expect_greater),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::l_paren,
                        diag::DiagKind::postfix_expression_expect_l_paren),
                Expression(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::r_paren,
                        diag::DiagKind::postfix_expression_expect_r_paren)),
            SerialParseFunctions(
                ParseFunctionInputs<PostfixExpression::kTag>(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::kw_reinterpret_cast,
                        diag::DiagKind::
                            postfix_expression_expect_kw_reinterpret_cast),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::less,
                        diag::DiagKind::postfix_expression_expect_less),
                TypeId(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::greater,
                        diag::DiagKind::postfix_expression_expect_greater),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::l_paren,
                        diag::DiagKind::postfix_expression_expect_l_paren),
                Expression(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::r_paren,
                        diag::DiagKind::postfix_expression_expect_r_paren)),
            SerialParseFunctions(
                ParseFunctionInputs<PostfixExpression::kTag>(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::kw_const_cast,
                        diag::DiagKind::
                            postfix_expression_expect_kw_const_cast),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::less,
                        diag::DiagKind::postfix_expression_expect_less),
                TypeId(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::greater,
                        diag::DiagKind::postfix_expression_expect_greater),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::l_paren,
                        diag::DiagKind::postfix_expression_expect_l_paren),
                Expression(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::r_paren,
                        diag::DiagKind::postfix_expression_expect_r_paren)),
            SerialParseFunctions(
                ParseFunctionInputs<PostfixExpression::kTag>(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::kw_typeid,
                        diag::DiagKind::postfix_expression_expect_kw_typeid),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::l_paren,
                        diag::DiagKind::postfix_expression_expect_l_paren),
                Expression(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::r_paren,
                        diag::DiagKind::postfix_expression_expect_r_paren)),
            SerialParseFunctions(
                ParseFunctionInputs<PostfixExpression::kTag>(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::kw_typeid,
                        diag::DiagKind::postfix_expression_expect_kw_typeid),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::l_paren,
                        diag::DiagKind::postfix_expression_expect_l_paren),
                TypeId(false),
                ParseFunction<PostfixExpression::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::r_paren,
                        diag::DiagKind::postfix_expression_expect_r_paren)));

    static_assert(base::kNumberOfElements >= 11);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      PostfixExpression::kTag, 6,
      SerialParseFunctions<PostfixExpression::kTag, PostfixExpression,
                           ParseFunction<PostfixExpression::kTag>,
                           ExprOrBracedInitList,
                           ParseFunction<PostfixExpression::kTag>>,
      SerialParseFunctions<PostfixExpression::kTag, PostfixExpression,
                           ParseFunction<PostfixExpression::kTag>,
                           ExpressionList,
                           ParseFunction<PostfixExpression::kTag>>,
      SerialParseFunctions<PostfixExpression::kTag, PostfixExpression,
                           ParseFunction<PostfixExpression::kTag>,
                           ParseFunction<PostfixExpression::kTag>,
                           IdExpression>,
      SerialParseFunctions<PostfixExpression::kTag, PostfixExpression,
                           ParseFunction<PostfixExpression::kTag>,
                           ParseFunction<PostfixExpression::kTag>,
                           IdExpression>,
      SerialParseFunctions<PostfixExpression::kTag, PostfixExpression,
                           ParseFunction<PostfixExpression::kTag>>,
      SerialParseFunctions<PostfixExpression::kTag, PostfixExpression,
                           ParseFunction<PostfixExpression::kTag>>>
      parallel_funcs_1(
          ParseFunctionInputs<PostfixExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<PostfixExpression::kTag>(false),
              PostfixExpression(false),
              ParseFunction<PostfixExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_square,
                  diag::DiagKind::postfix_expression_expect_l_square),
              ExprOrBracedInitList(false),
              ParseFunction<PostfixExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_square,
                  diag::DiagKind::postfix_expression_expect_r_square)),
          SerialParseFunctions(
              ParseFunctionInputs<PostfixExpression::kTag>(false),
              PostfixExpression(false),
              ParseFunction<PostfixExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::postfix_expression_expect_l_paren),
              ExpressionList(true),
              ParseFunction<PostfixExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::postfix_expression_expect_r_paren)),
          SerialParseFunctions(
              ParseFunctionInputs<PostfixExpression::kTag>(false),
              PostfixExpression(false),
              ParseFunction<PostfixExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::period,
                  diag::DiagKind::postfix_expression_expect_period),
              ParseFunction<PostfixExpression::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::kw_template,
                  diag::DiagKind::postfix_expression_expect_kw_template),
              IdExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PostfixExpression::kTag>(false),
              PostfixExpression(false),
              ParseFunction<PostfixExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::arrow,
                  diag::DiagKind::postfix_expression_expect_arrow),
              ParseFunction<PostfixExpression::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::kw_template,
                  diag::DiagKind::postfix_expression_expect_kw_template),
              IdExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PostfixExpression::kTag>(false),
              PostfixExpression(false),
              ParseFunction<PostfixExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::plusplus,
                  diag::DiagKind::postfix_expression_expect_plusplus)),
          SerialParseFunctions(
              ParseFunctionInputs<PostfixExpression::kTag>(false),
              PostfixExpression(false),
              ParseFunction<PostfixExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::minusminus,
                  diag::DiagKind::postfix_expression_expect_minusminus)));

  static_assert(base::kNumberOfElements >= 6);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// expression-list:
// 	initializer_list
inline ParseFunctionOutputs<ExpressionList::kTag> ExpressionList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ExpressionList::kTag>(false, output.last_token_,
                                                output.cur_token_),
      InitializerList(false));
  return serial_funcs();
}

// unary-expression:
// 	postfix_expression
// 	unary_operator, cast_expression
// 	`++`, cast_expression
// 	`__`, cast_expression
// 	await_expression
// 	`sizeof`, unary_expression
// 	`sizeof`, `(`, type_id, `)`
// 	`sizeof`, `...`, `(`, `identifier`, `)`
// 	`alignof`, `(`, type_id, `)`
// 	noexcept_expression
// 	new_expression
// 	delete_expression
inline ParseFunctionOutputs<UnaryExpression::kTag>
UnaryExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      UnaryExpression::kTag, 12,
      SerialParseFunctions<UnaryExpression::kTag, PostfixExpression>,
      SerialParseFunctions<UnaryExpression::kTag, UnaryOperator,
                           CastExpression>,
      SerialParseFunctions<UnaryExpression::kTag,
                           ParseFunction<UnaryExpression::kTag>,
                           CastExpression>,
      SerialParseFunctions<UnaryExpression::kTag,
                           ParseFunction<UnaryExpression::kTag>,
                           CastExpression>,
      SerialParseFunctions<UnaryExpression::kTag, AwaitExpression>,
      SerialParseFunctions<UnaryExpression::kTag,
                           ParseFunction<UnaryExpression::kTag>,
                           UnaryExpression>,
      SerialParseFunctions<UnaryExpression::kTag,
                           ParseFunction<UnaryExpression::kTag>,
                           ParseFunction<UnaryExpression::kTag>, TypeId,
                           ParseFunction<UnaryExpression::kTag>>,
      SerialParseFunctions<UnaryExpression::kTag,
                           ParseFunction<UnaryExpression::kTag>,
                           ParseFunction<UnaryExpression::kTag>,
                           ParseFunction<UnaryExpression::kTag>,
                           ParseFunction<UnaryExpression::kTag>,
                           ParseFunction<UnaryExpression::kTag>>,
      SerialParseFunctions<UnaryExpression::kTag,
                           ParseFunction<UnaryExpression::kTag>,
                           ParseFunction<UnaryExpression::kTag>, TypeId,
                           ParseFunction<UnaryExpression::kTag>>,
      SerialParseFunctions<UnaryExpression::kTag, NoexceptExpression>,
      SerialParseFunctions<UnaryExpression::kTag, NewExpression>,
      SerialParseFunctions<UnaryExpression::kTag, DeleteExpression>>
      parallel_funcs_0(
          ParseFunctionInputs<UnaryExpression::kTag>(false, output.last_token_,
                                                     output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryExpression::kTag>(false),
              PostfixExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryExpression::kTag>(false),
              UnaryOperator(false), CastExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryExpression::kTag>(false),
              ParseFunction<UnaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::plusplus,
                  diag::DiagKind::unary_expression_expect_plusplus),
              CastExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryExpression::kTag>(false),
              ParseFunction<UnaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::minusminus,
                  diag::DiagKind::unary_expression_expect_minusminus),
              CastExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryExpression::kTag>(false),
              AwaitExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryExpression::kTag>(false),
              ParseFunction<UnaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_sizeof,
                  diag::DiagKind::unary_expression_expect_kw_sizeof),
              UnaryExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryExpression::kTag>(false),
              ParseFunction<UnaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_sizeof,
                  diag::DiagKind::unary_expression_expect_kw_sizeof),
              ParseFunction<UnaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::unary_expression_expect_l_paren),
              TypeId(false),
              ParseFunction<UnaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::unary_expression_expect_r_paren)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryExpression::kTag>(false),
              ParseFunction<UnaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_sizeof,
                  diag::DiagKind::unary_expression_expect_kw_sizeof),
              ParseFunction<UnaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::unary_expression_expect_ellipsis),
              ParseFunction<UnaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::unary_expression_expect_l_paren),
              ParseFunction<UnaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::unary_expression_expect_identifier),
              ParseFunction<UnaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::unary_expression_expect_r_paren)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryExpression::kTag>(false),
              ParseFunction<UnaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_alignof,
                  diag::DiagKind::unary_expression_expect_kw_alignof),
              ParseFunction<UnaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::unary_expression_expect_l_paren),
              TypeId(false),
              ParseFunction<UnaryExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::unary_expression_expect_r_paren)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryExpression::kTag>(false),
              NoexceptExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryExpression::kTag>(false),
              NewExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryExpression::kTag>(false),
              DeleteExpression(false)));

  static_assert(base::kNumberOfElements >= 12);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// unary-operator:
// 	`*`
// 	`&`
// 	`+`
// 	`_`
// 	`!`
// 	`~`
inline ParseFunctionOutputs<UnaryOperator::kTag> UnaryOperator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      UnaryOperator::kTag, 6,
      SerialParseFunctions<UnaryOperator::kTag,
                           ParseFunction<UnaryOperator::kTag>>,
      SerialParseFunctions<UnaryOperator::kTag,
                           ParseFunction<UnaryOperator::kTag>>,
      SerialParseFunctions<UnaryOperator::kTag,
                           ParseFunction<UnaryOperator::kTag>>,
      SerialParseFunctions<UnaryOperator::kTag,
                           ParseFunction<UnaryOperator::kTag>>,
      SerialParseFunctions<UnaryOperator::kTag,
                           ParseFunction<UnaryOperator::kTag>>,
      SerialParseFunctions<UnaryOperator::kTag,
                           ParseFunction<UnaryOperator::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<UnaryOperator::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryOperator::kTag>(false),
              ParseFunction<UnaryOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::star,
                  diag::DiagKind::unary_operator_expect_star)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryOperator::kTag>(false),
              ParseFunction<UnaryOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::amp,
                  diag::DiagKind::unary_operator_expect_amp)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryOperator::kTag>(false),
              ParseFunction<UnaryOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::plus,
                  diag::DiagKind::unary_operator_expect_plus)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryOperator::kTag>(false),
              ParseFunction<UnaryOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::minus,
                  diag::DiagKind::unary_operator_expect_minus)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryOperator::kTag>(false),
              ParseFunction<UnaryOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::exclaim,
                  diag::DiagKind::unary_operator_expect_exclaim)),
          SerialParseFunctions(
              ParseFunctionInputs<UnaryOperator::kTag>(false),
              ParseFunction<UnaryOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::tilde,
                  diag::DiagKind::unary_operator_expect_tilde)));

  static_assert(base::kNumberOfElements >= 6);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// await-expression:
// 	`co_await`, cast_expression
inline ParseFunctionOutputs<AwaitExpression::kTag>
AwaitExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<AwaitExpression::kTag>(false, output.last_token_,
                                                 output.cur_token_),
      ParseFunction<AwaitExpression::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_co_await,
          diag::DiagKind::await_expression_expect_kw_co_await),
      CastExpression(false));
  return serial_funcs();
}

// noexcept-expression:
// 	`noexcept`, `(`, expression, `)`
inline ParseFunctionOutputs<NoexceptExpression::kTag>
NoexceptExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<NoexceptExpression::kTag>(false, output.last_token_,
                                                    output.cur_token_),
      ParseFunction<NoexceptExpression::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_noexcept,
          diag::DiagKind::noexcept_expression_expect_kw_noexcept),
      ParseFunction<NoexceptExpression::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_paren,
          diag::DiagKind::noexcept_expression_expect_l_paren),
      Expression(false),
      ParseFunction<NoexceptExpression::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_paren,
          diag::DiagKind::noexcept_expression_expect_r_paren));
  return serial_funcs();
}

// new-expression:
// 	`::`[opt], `new`, new_placement[opt], new_type_id, new_initializer[opt]
// 	`::`[opt], `new`, new_placement[opt], `(`, type_id, `)`, new_initializer[opt]
inline ParseFunctionOutputs<NewExpression::kTag> NewExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      NewExpression::kTag, 2,
      SerialParseFunctions<NewExpression::kTag,
                           ParseFunction<NewExpression::kTag>,
                           ParseFunction<NewExpression::kTag>, NewPlacement,
                           NewTypeId, NewInitializer>,
      SerialParseFunctions<NewExpression::kTag,
                           ParseFunction<NewExpression::kTag>,
                           ParseFunction<NewExpression::kTag>, NewPlacement,
                           ParseFunction<NewExpression::kTag>, TypeId,
                           ParseFunction<NewExpression::kTag>, NewInitializer>>
      parallel_funcs_0(
          ParseFunctionInputs<NewExpression::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<NewExpression::kTag>(false),
              ParseFunction<NewExpression::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::coloncolon,
                  diag::DiagKind::new_expression_expect_coloncolon),
              ParseFunction<NewExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_new,
                  diag::DiagKind::new_expression_expect_kw_new),
              NewPlacement(true), NewTypeId(false), NewInitializer(true)),
          SerialParseFunctions(
              ParseFunctionInputs<NewExpression::kTag>(false),
              ParseFunction<NewExpression::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::coloncolon,
                  diag::DiagKind::new_expression_expect_coloncolon),
              ParseFunction<NewExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_new,
                  diag::DiagKind::new_expression_expect_kw_new),
              NewPlacement(true),
              ParseFunction<NewExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::new_expression_expect_l_paren),
              TypeId(false),
              ParseFunction<NewExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::new_expression_expect_r_paren),
              NewInitializer(true)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// new-placement:
// 	`(`, expression_list, `)`
inline ParseFunctionOutputs<NewPlacement::kTag> NewPlacement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<NewPlacement::kTag>(false, output.last_token_,
                                              output.cur_token_),
      ParseFunction<NewPlacement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_paren,
          diag::DiagKind::new_placement_expect_l_paren),
      ExpressionList(false),
      ParseFunction<NewPlacement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_paren,
          diag::DiagKind::new_placement_expect_r_paren));
  return serial_funcs();
}

// new-type-id:
// 	type_specifier_seq, new_declarator[opt]
inline ParseFunctionOutputs<NewTypeId::kTag> NewTypeId::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<NewTypeId::kTag>(false, output.last_token_,
                                           output.cur_token_),
      TypeSpecifierSeq(false), NewDeclarator(true));
  return serial_funcs();
}

// new-declarator:
// 	ptr_operator, new_declarator[opt]
// 	noptr_new_declarator
inline ParseFunctionOutputs<NewDeclarator::kTag> NewDeclarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      NewDeclarator::kTag, 2,
      SerialParseFunctions<NewDeclarator::kTag, PtrOperator, NewDeclarator>,
      SerialParseFunctions<NewDeclarator::kTag, NoptrNewDeclarator>>
      parallel_funcs_0(
          ParseFunctionInputs<NewDeclarator::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<NewDeclarator::kTag>(false),
                               PtrOperator(false), NewDeclarator(true)),
          SerialParseFunctions(ParseFunctionInputs<NewDeclarator::kTag>(false),
                               NoptrNewDeclarator(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// noptr-new-declarator:
// 	`[`, expression[opt], `]`, attribute_specifier_seq[opt]
// 	noptr_new_declarator, `[`, constant_expression, `]`, attribute_specifier_seq[opt]
inline ParseFunctionOutputs<NoptrNewDeclarator::kTag>
NoptrNewDeclarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        NoptrNewDeclarator::kTag, 1,
        SerialParseFunctions<
            NoptrNewDeclarator::kTag, ParseFunction<NoptrNewDeclarator::kTag>,
            Expression, ParseFunction<NoptrNewDeclarator::kTag>,
            AttributeSpecifierSeq>>
        parallel_funcs_0(
            ParseFunctionInputs<NoptrNewDeclarator::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<NoptrNewDeclarator::kTag>(false),
                ParseFunction<NoptrNewDeclarator::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::l_square,
                        diag::DiagKind::noptr_new_declarator_expect_l_square),
                Expression(true),
                ParseFunction<NoptrNewDeclarator::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::r_square,
                        diag::DiagKind::noptr_new_declarator_expect_r_square),
                AttributeSpecifierSeq(true)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      NoptrNewDeclarator::kTag, 1,
      SerialParseFunctions<
          NoptrNewDeclarator::kTag, NoptrNewDeclarator,
          ParseFunction<NoptrNewDeclarator::kTag>, ConstantExpression,
          ParseFunction<NoptrNewDeclarator::kTag>, AttributeSpecifierSeq>>
      parallel_funcs_1(
          ParseFunctionInputs<NoptrNewDeclarator::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<NoptrNewDeclarator::kTag>(false),
              NoptrNewDeclarator(false),
              ParseFunction<NoptrNewDeclarator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_square,
                      diag::DiagKind::noptr_new_declarator_expect_l_square),
              ConstantExpression(false),
              ParseFunction<NoptrNewDeclarator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_square,
                      diag::DiagKind::noptr_new_declarator_expect_r_square),
              AttributeSpecifierSeq(true)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// new-initializer:
// 	`(`, expression_list[opt], `)`
// 	braced_init_list
inline ParseFunctionOutputs<NewInitializer::kTag> NewInitializer::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      NewInitializer::kTag, 2,
      SerialParseFunctions<NewInitializer::kTag,
                           ParseFunction<NewInitializer::kTag>, ExpressionList,
                           ParseFunction<NewInitializer::kTag>>,
      SerialParseFunctions<NewInitializer::kTag, BracedInitList>>
      parallel_funcs_0(
          ParseFunctionInputs<NewInitializer::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<NewInitializer::kTag>(false),
              ParseFunction<NewInitializer::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::new_initializer_expect_l_paren),
              ExpressionList(true),
              ParseFunction<NewInitializer::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::new_initializer_expect_r_paren)),
          SerialParseFunctions(ParseFunctionInputs<NewInitializer::kTag>(false),
                               BracedInitList(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// delete-expression:
// 	`::`[opt], `delete`, cast_expression
// 	`::`[opt], `delete`, `[`, `]`, cast_expression
inline ParseFunctionOutputs<DeleteExpression::kTag>
DeleteExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      DeleteExpression::kTag, 2,
      SerialParseFunctions<
          DeleteExpression::kTag, ParseFunction<DeleteExpression::kTag>,
          ParseFunction<DeleteExpression::kTag>, CastExpression>,
      SerialParseFunctions<
          DeleteExpression::kTag, ParseFunction<DeleteExpression::kTag>,
          ParseFunction<DeleteExpression::kTag>,
          ParseFunction<DeleteExpression::kTag>,
          ParseFunction<DeleteExpression::kTag>, CastExpression>>
      parallel_funcs_0(
          ParseFunctionInputs<DeleteExpression::kTag>(false, output.last_token_,
                                                      output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<DeleteExpression::kTag>(false),
              ParseFunction<DeleteExpression::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::coloncolon,
                  diag::DiagKind::delete_expression_expect_coloncolon),
              ParseFunction<DeleteExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_delete,
                  diag::DiagKind::delete_expression_expect_kw_delete),
              CastExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<DeleteExpression::kTag>(false),
              ParseFunction<DeleteExpression::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::coloncolon,
                  diag::DiagKind::delete_expression_expect_coloncolon),
              ParseFunction<DeleteExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_delete,
                  diag::DiagKind::delete_expression_expect_kw_delete),
              ParseFunction<DeleteExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_square,
                  diag::DiagKind::delete_expression_expect_l_square),
              ParseFunction<DeleteExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_square,
                  diag::DiagKind::delete_expression_expect_r_square),
              CastExpression(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// cast-expression:
// 	unary_expression
// 	`(`, type_id, `)`, cast_expression
inline ParseFunctionOutputs<CastExpression::kTag> CastExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      CastExpression::kTag, 2,
      SerialParseFunctions<CastExpression::kTag, UnaryExpression>,
      SerialParseFunctions<CastExpression::kTag,
                           ParseFunction<CastExpression::kTag>, TypeId,
                           ParseFunction<CastExpression::kTag>, CastExpression>>
      parallel_funcs_0(
          ParseFunctionInputs<CastExpression::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<CastExpression::kTag>(false),
                               UnaryExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<CastExpression::kTag>(false),
              ParseFunction<CastExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::cast_expression_expect_l_paren),
              TypeId(false),
              ParseFunction<CastExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::cast_expression_expect_r_paren),
              CastExpression(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// pm-expression:
// 	cast_expression
// 	pm_expression, `.*`, cast_expression
// 	pm_expression, `_>*`, cast_expression
inline ParseFunctionOutputs<PmExpression::kTag> PmExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        PmExpression::kTag, 1,
        SerialParseFunctions<PmExpression::kTag, CastExpression>>
        parallel_funcs_0(
            ParseFunctionInputs<PmExpression::kTag>(false, output.last_token_,
                                                    output.cur_token_),
            SerialParseFunctions(ParseFunctionInputs<PmExpression::kTag>(false),
                                 CastExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      PmExpression::kTag, 2,
      SerialParseFunctions<PmExpression::kTag, PmExpression,
                           ParseFunction<PmExpression::kTag>, CastExpression>,
      SerialParseFunctions<PmExpression::kTag, PmExpression,
                           ParseFunction<PmExpression::kTag>, CastExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<PmExpression::kTag>(false, output.last_token_,
                                                  output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<PmExpression::kTag>(false),
              PmExpression(false),
              ParseFunction<PmExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::periodstar,
                  diag::DiagKind::pm_expression_expect_periodstar),
              CastExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PmExpression::kTag>(false),
              PmExpression(false),
              ParseFunction<PmExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::arrowstar,
                  diag::DiagKind::pm_expression_expect_arrowstar),
              CastExpression(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// multiplicative-expression:
// 	pm_expression
// 	multiplicative_expression, `*`, pm_expression
// 	multiplicative_expression, `/`, pm_expression
// 	multiplicative_expression, `%`, pm_expression
inline ParseFunctionOutputs<MultiplicativeExpression::kTag>
MultiplicativeExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        MultiplicativeExpression::kTag, 1,
        SerialParseFunctions<MultiplicativeExpression::kTag, PmExpression>>
        parallel_funcs_0(
            ParseFunctionInputs<MultiplicativeExpression::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<MultiplicativeExpression::kTag>(false),
                PmExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      MultiplicativeExpression::kTag, 3,
      SerialParseFunctions<
          MultiplicativeExpression::kTag, MultiplicativeExpression,
          ParseFunction<MultiplicativeExpression::kTag>, PmExpression>,
      SerialParseFunctions<
          MultiplicativeExpression::kTag, MultiplicativeExpression,
          ParseFunction<MultiplicativeExpression::kTag>, PmExpression>,
      SerialParseFunctions<
          MultiplicativeExpression::kTag, MultiplicativeExpression,
          ParseFunction<MultiplicativeExpression::kTag>, PmExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<MultiplicativeExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<MultiplicativeExpression::kTag>(false),
              MultiplicativeExpression(false),
              ParseFunction<MultiplicativeExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::star,
                      diag::DiagKind::multiplicative_expression_expect_star),
              PmExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<MultiplicativeExpression::kTag>(false),
              MultiplicativeExpression(false),
              ParseFunction<MultiplicativeExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::slash,
                      diag::DiagKind::multiplicative_expression_expect_slash),
              PmExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<MultiplicativeExpression::kTag>(false),
              MultiplicativeExpression(false),
              ParseFunction<MultiplicativeExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::percent,
                      diag::DiagKind::multiplicative_expression_expect_percent),
              PmExpression(false)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// additive-expression:
// 	multiplicative_expression
// 	additive_expression, `+`, multiplicative_expression
// 	additive_expression, `_`, multiplicative_expression
inline ParseFunctionOutputs<AdditiveExpression::kTag>
AdditiveExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<AdditiveExpression::kTag, 1,
                           SerialParseFunctions<AdditiveExpression::kTag,
                                                MultiplicativeExpression>>
        parallel_funcs_0(
            ParseFunctionInputs<AdditiveExpression::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<AdditiveExpression::kTag>(false),
                MultiplicativeExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      AdditiveExpression::kTag, 2,
      SerialParseFunctions<AdditiveExpression::kTag, AdditiveExpression,
                           ParseFunction<AdditiveExpression::kTag>,
                           MultiplicativeExpression>,
      SerialParseFunctions<AdditiveExpression::kTag, AdditiveExpression,
                           ParseFunction<AdditiveExpression::kTag>,
                           MultiplicativeExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<AdditiveExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<AdditiveExpression::kTag>(false),
              AdditiveExpression(false),
              ParseFunction<AdditiveExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::plus,
                      diag::DiagKind::additive_expression_expect_plus),
              MultiplicativeExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<AdditiveExpression::kTag>(false),
              AdditiveExpression(false),
              ParseFunction<AdditiveExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::minus,
                      diag::DiagKind::additive_expression_expect_minus),
              MultiplicativeExpression(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// shift-expression:
// 	additive_expression
// 	shift_expression, `<<`, additive_expression
// 	shift_expression, `>>`, additive_expression
inline ParseFunctionOutputs<ShiftExpression::kTag>
ShiftExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        ShiftExpression::kTag, 1,
        SerialParseFunctions<ShiftExpression::kTag, AdditiveExpression>>
        parallel_funcs_0(ParseFunctionInputs<ShiftExpression::kTag>(
                             false, output.last_token_, output.cur_token_),
                         SerialParseFunctions(
                             ParseFunctionInputs<ShiftExpression::kTag>(false),
                             AdditiveExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      ShiftExpression::kTag, 2,
      SerialParseFunctions<ShiftExpression::kTag, ShiftExpression,
                           ParseFunction<ShiftExpression::kTag>,
                           AdditiveExpression>,
      SerialParseFunctions<ShiftExpression::kTag, ShiftExpression,
                           ParseFunction<ShiftExpression::kTag>,
                           AdditiveExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<ShiftExpression::kTag>(false, output.last_token_,
                                                     output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ShiftExpression::kTag>(false),
              ShiftExpression(false),
              ParseFunction<ShiftExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::lessless,
                  diag::DiagKind::shift_expression_expect_lessless),
              AdditiveExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ShiftExpression::kTag>(false),
              ShiftExpression(false),
              ParseFunction<ShiftExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::greatergreater,
                  diag::DiagKind::shift_expression_expect_greatergreater),
              AdditiveExpression(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// compare-expression:
// 	shift_expression
// 	compare_expression, `<=>`, shift_expression
inline ParseFunctionOutputs<CompareExpression::kTag>
CompareExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        CompareExpression::kTag, 1,
        SerialParseFunctions<CompareExpression::kTag, ShiftExpression>>
        parallel_funcs_0(
            ParseFunctionInputs<CompareExpression::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<CompareExpression::kTag>(false),
                ShiftExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      CompareExpression::kTag, 1,
      SerialParseFunctions<CompareExpression::kTag, CompareExpression,
                           ParseFunction<CompareExpression::kTag>,
                           ShiftExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<CompareExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<CompareExpression::kTag>(false),
              CompareExpression(false),
              ParseFunction<CompareExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::spaceship,
                  diag::DiagKind::compare_expression_expect_spaceship),
              ShiftExpression(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// relational-expression:
// 	compare_expression
// 	relational_expression, `<`, compare_expression
// 	relational_expression, `>`, compare_expression
// 	relational_expression, `<=`, compare_expression
// 	relational_expression, `>=`, compare_expression
inline ParseFunctionOutputs<RelationalExpression::kTag>
RelationalExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        RelationalExpression::kTag, 1,
        SerialParseFunctions<RelationalExpression::kTag, CompareExpression>>
        parallel_funcs_0(
            ParseFunctionInputs<RelationalExpression::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<RelationalExpression::kTag>(false),
                CompareExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      RelationalExpression::kTag, 4,
      SerialParseFunctions<RelationalExpression::kTag, RelationalExpression,
                           ParseFunction<RelationalExpression::kTag>,
                           CompareExpression>,
      SerialParseFunctions<RelationalExpression::kTag, RelationalExpression,
                           ParseFunction<RelationalExpression::kTag>,
                           CompareExpression>,
      SerialParseFunctions<RelationalExpression::kTag, RelationalExpression,
                           ParseFunction<RelationalExpression::kTag>,
                           CompareExpression>,
      SerialParseFunctions<RelationalExpression::kTag, RelationalExpression,
                           ParseFunction<RelationalExpression::kTag>,
                           CompareExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<RelationalExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<RelationalExpression::kTag>(false),
              RelationalExpression(false),
              ParseFunction<RelationalExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::less,
                      diag::DiagKind::relational_expression_expect_less),
              CompareExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<RelationalExpression::kTag>(false),
              RelationalExpression(false),
              ParseFunction<RelationalExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::greater,
                      diag::DiagKind::relational_expression_expect_greater),
              CompareExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<RelationalExpression::kTag>(false),
              RelationalExpression(false),
              ParseFunction<RelationalExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::lessequal,
                      diag::DiagKind::relational_expression_expect_lessequal),
              CompareExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<RelationalExpression::kTag>(false),
              RelationalExpression(false),
              ParseFunction<RelationalExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::greaterequal,
                      diag::DiagKind::
                          relational_expression_expect_greaterequal),
              CompareExpression(false)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// equality-expression:
// 	relational_expression
// 	equality_expression, `==`, relational_expression
// 	equality_expression, `!=`, relational_expression
inline ParseFunctionOutputs<EqualityExpression::kTag>
EqualityExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        EqualityExpression::kTag, 1,
        SerialParseFunctions<EqualityExpression::kTag, RelationalExpression>>
        parallel_funcs_0(
            ParseFunctionInputs<EqualityExpression::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<EqualityExpression::kTag>(false),
                RelationalExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      EqualityExpression::kTag, 2,
      SerialParseFunctions<EqualityExpression::kTag, EqualityExpression,
                           ParseFunction<EqualityExpression::kTag>,
                           RelationalExpression>,
      SerialParseFunctions<EqualityExpression::kTag, EqualityExpression,
                           ParseFunction<EqualityExpression::kTag>,
                           RelationalExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<EqualityExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<EqualityExpression::kTag>(false),
              EqualityExpression(false),
              ParseFunction<EqualityExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::equalequal,
                      diag::DiagKind::equality_expression_expect_equalequal),
              RelationalExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<EqualityExpression::kTag>(false),
              EqualityExpression(false),
              ParseFunction<EqualityExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::exclaimequal,
                      diag::DiagKind::equality_expression_expect_exclaimequal),
              RelationalExpression(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// and-expression:
// 	equality_expression
// 	and_expression, `&`, equality_expression
inline ParseFunctionOutputs<AndExpression::kTag> AndExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        AndExpression::kTag, 1,
        SerialParseFunctions<AndExpression::kTag, EqualityExpression>>
        parallel_funcs_0(ParseFunctionInputs<AndExpression::kTag>(
                             false, output.last_token_, output.cur_token_),
                         SerialParseFunctions(
                             ParseFunctionInputs<AndExpression::kTag>(false),
                             EqualityExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      AndExpression::kTag, 1,
      SerialParseFunctions<AndExpression::kTag, AndExpression,
                           ParseFunction<AndExpression::kTag>,
                           EqualityExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<AndExpression::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<AndExpression::kTag>(false),
              AndExpression(false),
              ParseFunction<AndExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::amp,
                  diag::DiagKind::and_expression_expect_amp),
              EqualityExpression(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// exclusive-or-expression:
// 	and_expression
// 	exclusive_or_expression, `^`, and_expression
inline ParseFunctionOutputs<ExclusiveOrExpression::kTag>
ExclusiveOrExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        ExclusiveOrExpression::kTag, 1,
        SerialParseFunctions<ExclusiveOrExpression::kTag, AndExpression>>
        parallel_funcs_0(
            ParseFunctionInputs<ExclusiveOrExpression::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<ExclusiveOrExpression::kTag>(false),
                AndExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      ExclusiveOrExpression::kTag, 1,
      SerialParseFunctions<ExclusiveOrExpression::kTag, ExclusiveOrExpression,
                           ParseFunction<ExclusiveOrExpression::kTag>,
                           AndExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<ExclusiveOrExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ExclusiveOrExpression::kTag>(false),
              ExclusiveOrExpression(false),
              ParseFunction<ExclusiveOrExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::caret,
                      diag::DiagKind::exclusive_or_expression_expect_caret),
              AndExpression(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// inclusive-or-expression:
// 	exclusive_or_expression
// 	inclusive_or_expression, `|`, exclusive_or_expression
inline ParseFunctionOutputs<InclusiveOrExpression::kTag>
InclusiveOrExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<InclusiveOrExpression::kTag, 1,
                           SerialParseFunctions<InclusiveOrExpression::kTag,
                                                ExclusiveOrExpression>>
        parallel_funcs_0(
            ParseFunctionInputs<InclusiveOrExpression::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<InclusiveOrExpression::kTag>(false),
                ExclusiveOrExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      InclusiveOrExpression::kTag, 1,
      SerialParseFunctions<InclusiveOrExpression::kTag, InclusiveOrExpression,
                           ParseFunction<InclusiveOrExpression::kTag>,
                           ExclusiveOrExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<InclusiveOrExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<InclusiveOrExpression::kTag>(false),
              InclusiveOrExpression(false),
              ParseFunction<InclusiveOrExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::pipe,
                      diag::DiagKind::inclusive_or_expression_expect_pipe),
              ExclusiveOrExpression(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// logical-and-expression:
// 	inclusive_or_expression
// 	logical_and_expression, `&&`, exclusive_or_expression
inline ParseFunctionOutputs<LogicalAndExpression::kTag>
LogicalAndExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        LogicalAndExpression::kTag, 1,
        SerialParseFunctions<LogicalAndExpression::kTag, InclusiveOrExpression>>
        parallel_funcs_0(
            ParseFunctionInputs<LogicalAndExpression::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<LogicalAndExpression::kTag>(false),
                InclusiveOrExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      LogicalAndExpression::kTag, 1,
      SerialParseFunctions<LogicalAndExpression::kTag, LogicalAndExpression,
                           ParseFunction<LogicalAndExpression::kTag>,
                           ExclusiveOrExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<LogicalAndExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<LogicalAndExpression::kTag>(false),
              LogicalAndExpression(false),
              ParseFunction<LogicalAndExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::ampamp,
                      diag::DiagKind::logical_and_expression_expect_ampamp),
              ExclusiveOrExpression(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// logical-or-expression:
// 	logical_and_expression
// 	logical_or_expression, `||`, logical_and_expression
inline ParseFunctionOutputs<LogicalOrExpression::kTag>
LogicalOrExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        LogicalOrExpression::kTag, 1,
        SerialParseFunctions<LogicalOrExpression::kTag, LogicalAndExpression>>
        parallel_funcs_0(
            ParseFunctionInputs<LogicalOrExpression::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<LogicalOrExpression::kTag>(false),
                LogicalAndExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      LogicalOrExpression::kTag, 1,
      SerialParseFunctions<LogicalOrExpression::kTag, LogicalOrExpression,
                           ParseFunction<LogicalOrExpression::kTag>,
                           LogicalAndExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<LogicalOrExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<LogicalOrExpression::kTag>(false),
              LogicalOrExpression(false),
              ParseFunction<LogicalOrExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::pipepipe,
                      diag::DiagKind::logical_or_expression_expect_pipepipe),
              LogicalAndExpression(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// conditional-expression:
// 	logical_or_expression
// 	logical_or_expression, `?`, expression, `:`, assignment_expression
inline ParseFunctionOutputs<ConditionalExpression::kTag>
ConditionalExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ConditionalExpression::kTag, 2,
      SerialParseFunctions<ConditionalExpression::kTag, LogicalOrExpression>,
      SerialParseFunctions<
          ConditionalExpression::kTag, LogicalOrExpression,
          ParseFunction<ConditionalExpression::kTag>, Expression,
          ParseFunction<ConditionalExpression::kTag>, AssignmentExpression>>
      parallel_funcs_0(
          ParseFunctionInputs<ConditionalExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ConditionalExpression::kTag>(false),
              LogicalOrExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ConditionalExpression::kTag>(false),
              LogicalOrExpression(false),
              ParseFunction<ConditionalExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::question,
                      diag::DiagKind::conditional_expression_expect_question),
              Expression(false),
              ParseFunction<ConditionalExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::colon,
                      diag::DiagKind::conditional_expression_expect_colon),
              AssignmentExpression(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// yield-expression:
// 	`co_yield`, assignment_expression
// 	`co_yield`, braced_init_list
inline ParseFunctionOutputs<YieldExpression::kTag>
YieldExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      YieldExpression::kTag, 2,
      SerialParseFunctions<YieldExpression::kTag,
                           ParseFunction<YieldExpression::kTag>,
                           AssignmentExpression>,
      SerialParseFunctions<YieldExpression::kTag,
                           ParseFunction<YieldExpression::kTag>,
                           BracedInitList>>
      parallel_funcs_0(
          ParseFunctionInputs<YieldExpression::kTag>(false, output.last_token_,
                                                     output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<YieldExpression::kTag>(false),
              ParseFunction<YieldExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_co_yield,
                  diag::DiagKind::yield_expression_expect_kw_co_yield),
              AssignmentExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<YieldExpression::kTag>(false),
              ParseFunction<YieldExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_co_yield,
                  diag::DiagKind::yield_expression_expect_kw_co_yield),
              BracedInitList(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// throw-expression:
// 	`throw`, assignment_expression[opt]
inline ParseFunctionOutputs<ThrowExpression::kTag>
ThrowExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ThrowExpression::kTag, 1,
      SerialParseFunctions<ThrowExpression::kTag,
                           ParseFunction<ThrowExpression::kTag>,
                           AssignmentExpression>>
      parallel_funcs_0(
          ParseFunctionInputs<ThrowExpression::kTag>(false, output.last_token_,
                                                     output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ThrowExpression::kTag>(false),
              ParseFunction<ThrowExpression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_throw,
                  diag::DiagKind::throw_expression_expect_kw_throw),
              AssignmentExpression(true)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// assignment-expression:
// 	conditional_expression
// 	yield_expression
// 	throw_expression
// 	logical_or_expression, assignment_operator, initializer_clause
inline ParseFunctionOutputs<AssignmentExpression::kTag>
AssignmentExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      AssignmentExpression::kTag, 4,
      SerialParseFunctions<AssignmentExpression::kTag, ConditionalExpression>,
      SerialParseFunctions<AssignmentExpression::kTag, YieldExpression>,
      SerialParseFunctions<AssignmentExpression::kTag, ThrowExpression>,
      SerialParseFunctions<AssignmentExpression::kTag, LogicalOrExpression,
                           AssignmentOperator, InitializerClause>>
      parallel_funcs_0(
          ParseFunctionInputs<AssignmentExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentExpression::kTag>(false),
              ConditionalExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentExpression::kTag>(false),
              YieldExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentExpression::kTag>(false),
              ThrowExpression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentExpression::kTag>(false),
              LogicalOrExpression(false), AssignmentOperator(false),
              InitializerClause(false)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// assignment-operator:
// 	`=`
// 	`*=`
// 	`/=`
// 	`%=`
// 	`+=`
// 	`_=`
// 	`>>=`
// 	`<<=`
// 	`&=`
// 	`^=`
// 	`|=`
inline ParseFunctionOutputs<AssignmentOperator::kTag>
AssignmentOperator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      AssignmentOperator::kTag, 11,
      SerialParseFunctions<AssignmentOperator::kTag,
                           ParseFunction<AssignmentOperator::kTag>>,
      SerialParseFunctions<AssignmentOperator::kTag,
                           ParseFunction<AssignmentOperator::kTag>>,
      SerialParseFunctions<AssignmentOperator::kTag,
                           ParseFunction<AssignmentOperator::kTag>>,
      SerialParseFunctions<AssignmentOperator::kTag,
                           ParseFunction<AssignmentOperator::kTag>>,
      SerialParseFunctions<AssignmentOperator::kTag,
                           ParseFunction<AssignmentOperator::kTag>>,
      SerialParseFunctions<AssignmentOperator::kTag,
                           ParseFunction<AssignmentOperator::kTag>>,
      SerialParseFunctions<AssignmentOperator::kTag,
                           ParseFunction<AssignmentOperator::kTag>>,
      SerialParseFunctions<AssignmentOperator::kTag,
                           ParseFunction<AssignmentOperator::kTag>>,
      SerialParseFunctions<AssignmentOperator::kTag,
                           ParseFunction<AssignmentOperator::kTag>>,
      SerialParseFunctions<AssignmentOperator::kTag,
                           ParseFunction<AssignmentOperator::kTag>>,
      SerialParseFunctions<AssignmentOperator::kTag,
                           ParseFunction<AssignmentOperator::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<AssignmentOperator::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentOperator::kTag>(false),
              ParseFunction<AssignmentOperator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::equal,
                      diag::DiagKind::assignment_operator_expect_equal)),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentOperator::kTag>(false),
              ParseFunction<AssignmentOperator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::starequal,
                      diag::DiagKind::assignment_operator_expect_starequal)),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentOperator::kTag>(false),
              ParseFunction<AssignmentOperator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::slashequal,
                      diag::DiagKind::assignment_operator_expect_slashequal)),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentOperator::kTag>(false),
              ParseFunction<AssignmentOperator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::percentequal,
                      diag::DiagKind::assignment_operator_expect_percentequal)),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentOperator::kTag>(false),
              ParseFunction<AssignmentOperator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::plusequal,
                      diag::DiagKind::assignment_operator_expect_plusequal)),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentOperator::kTag>(false),
              ParseFunction<AssignmentOperator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::minusequal,
                      diag::DiagKind::assignment_operator_expect_minusequal)),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentOperator::kTag>(false),
              ParseFunction<AssignmentOperator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::greatergreaterequal,
                      diag::DiagKind::
                          assignment_operator_expect_greatergreaterequal)),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentOperator::kTag>(false),
              ParseFunction<AssignmentOperator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::lesslessequal,
                      diag::DiagKind::
                          assignment_operator_expect_lesslessequal)),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentOperator::kTag>(false),
              ParseFunction<AssignmentOperator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::ampequal,
                      diag::DiagKind::assignment_operator_expect_ampequal)),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentOperator::kTag>(false),
              ParseFunction<AssignmentOperator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::caretequal,
                      diag::DiagKind::assignment_operator_expect_caretequal)),
          SerialParseFunctions(
              ParseFunctionInputs<AssignmentOperator::kTag>(false),
              ParseFunction<AssignmentOperator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::pipeequal,
                      diag::DiagKind::assignment_operator_expect_pipeequal)));

  static_assert(base::kNumberOfElements >= 11);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// expression:
// 	assignment_expression
// 	expression, `,`, assignment_expression
inline ParseFunctionOutputs<Expression::kTag> Expression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        Expression::kTag, 1,
        SerialParseFunctions<Expression::kTag, AssignmentExpression>>
        parallel_funcs_0(
            ParseFunctionInputs<Expression::kTag>(false, output.last_token_,
                                                  output.cur_token_),
            SerialParseFunctions(ParseFunctionInputs<Expression::kTag>(false),
                                 AssignmentExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<Expression::kTag, 1,
                         SerialParseFunctions<Expression::kTag, Expression,
                                              ParseFunction<Expression::kTag>,
                                              AssignmentExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<Expression::kTag>(false, output.last_token_,
                                                output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<Expression::kTag>(false), Expression(false),
              ParseFunction<Expression::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::comma,
                  diag::DiagKind::expression_expect_comma),
              AssignmentExpression(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// constant-expression:
// 	conditional_expression
inline ParseFunctionOutputs<ConstantExpression::kTag>
ConstantExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ConstantExpression::kTag>(false, output.last_token_,
                                                    output.cur_token_),
      ConditionalExpression(false));
  return serial_funcs();
}

// statement:
// 	labeled_statement
// 	attribute_specifier_seq[opt], expression_statement
// 	attribute_specifier_seq[opt], compound_statement
// 	attribute_specifier_seq[opt], selection_statement
// 	attribute_specifier_seq[opt], iteration_statement
// 	attribute_specifier_seq[opt], jump_statement
// 	declaration_statement
// 	attribute_specifier_seq[opt], try_block
inline ParseFunctionOutputs<Statement::kTag> Statement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      Statement::kTag, 8,
      SerialParseFunctions<Statement::kTag, LabeledStatement>,
      SerialParseFunctions<Statement::kTag, AttributeSpecifierSeq,
                           ExpressionStatement>,
      SerialParseFunctions<Statement::kTag, AttributeSpecifierSeq,
                           CompoundStatement>,
      SerialParseFunctions<Statement::kTag, AttributeSpecifierSeq,
                           SelectionStatement>,
      SerialParseFunctions<Statement::kTag, AttributeSpecifierSeq,
                           IterationStatement>,
      SerialParseFunctions<Statement::kTag, AttributeSpecifierSeq,
                           JumpStatement>,
      SerialParseFunctions<Statement::kTag, DeclarationStatement>,
      SerialParseFunctions<Statement::kTag, AttributeSpecifierSeq, TryBlock>>
      parallel_funcs_0(
          ParseFunctionInputs<Statement::kTag>(false, output.last_token_,
                                               output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<Statement::kTag>(false),
                               LabeledStatement(false)),
          SerialParseFunctions(ParseFunctionInputs<Statement::kTag>(false),
                               AttributeSpecifierSeq(true),
                               ExpressionStatement(false)),
          SerialParseFunctions(ParseFunctionInputs<Statement::kTag>(false),
                               AttributeSpecifierSeq(true),
                               CompoundStatement(false)),
          SerialParseFunctions(ParseFunctionInputs<Statement::kTag>(false),
                               AttributeSpecifierSeq(true),
                               SelectionStatement(false)),
          SerialParseFunctions(ParseFunctionInputs<Statement::kTag>(false),
                               AttributeSpecifierSeq(true),
                               IterationStatement(false)),
          SerialParseFunctions(ParseFunctionInputs<Statement::kTag>(false),
                               AttributeSpecifierSeq(true),
                               JumpStatement(false)),
          SerialParseFunctions(ParseFunctionInputs<Statement::kTag>(false),
                               DeclarationStatement(false)),
          SerialParseFunctions(ParseFunctionInputs<Statement::kTag>(false),
                               AttributeSpecifierSeq(true), TryBlock(false)));

  static_assert(base::kNumberOfElements >= 8);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// init-statement:
// 	expression_statement
// 	simple_declaration
inline ParseFunctionOutputs<InitStatement::kTag> InitStatement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      InitStatement::kTag, 2,
      SerialParseFunctions<InitStatement::kTag, ExpressionStatement>,
      SerialParseFunctions<InitStatement::kTag, SimpleDeclaration>>
      parallel_funcs_0(
          ParseFunctionInputs<InitStatement::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<InitStatement::kTag>(false),
                               ExpressionStatement(false)),
          SerialParseFunctions(ParseFunctionInputs<InitStatement::kTag>(false),
                               SimpleDeclaration(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// condition:
// 	expression
// 	attribute_specifier_seq[opt], decl_specifier_seq, declarator, brace_or_equal_initializer
inline ParseFunctionOutputs<Condition::kTag> Condition::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      Condition::kTag, 2, SerialParseFunctions<Condition::kTag, Expression>,
      SerialParseFunctions<Condition::kTag, AttributeSpecifierSeq,
                           DeclSpecifierSeq, Declarator,
                           BraceOrEqualInitializer>>
      parallel_funcs_0(
          ParseFunctionInputs<Condition::kTag>(false, output.last_token_,
                                               output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<Condition::kTag>(false),
                               Expression(false)),
          SerialParseFunctions(ParseFunctionInputs<Condition::kTag>(false),
                               AttributeSpecifierSeq(true),
                               DeclSpecifierSeq(false), Declarator(false),
                               BraceOrEqualInitializer(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// labeled-statement:
// 	attribute_specifier_seq[opt], `identifier`, `:`, statement
// 	attribute_specifier_seq[opt], `case`, constant_expression, `:`, statement
// 	attribute_specifier_seq[opt], `default`, `:`, statement
inline ParseFunctionOutputs<LabeledStatement::kTag>
LabeledStatement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      LabeledStatement::kTag, 3,
      SerialParseFunctions<LabeledStatement::kTag, AttributeSpecifierSeq,
                           ParseFunction<LabeledStatement::kTag>,
                           ParseFunction<LabeledStatement::kTag>, Statement>,
      SerialParseFunctions<LabeledStatement::kTag, AttributeSpecifierSeq,
                           ParseFunction<LabeledStatement::kTag>,
                           ConstantExpression,
                           ParseFunction<LabeledStatement::kTag>, Statement>,
      SerialParseFunctions<LabeledStatement::kTag, AttributeSpecifierSeq,
                           ParseFunction<LabeledStatement::kTag>,
                           ParseFunction<LabeledStatement::kTag>, Statement>>
      parallel_funcs_0(
          ParseFunctionInputs<LabeledStatement::kTag>(false, output.last_token_,
                                                      output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<LabeledStatement::kTag>(false),
              AttributeSpecifierSeq(true),
              ParseFunction<LabeledStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::labeled_statement_expect_identifier),
              ParseFunction<LabeledStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::colon,
                  diag::DiagKind::labeled_statement_expect_colon),
              Statement(false)),
          SerialParseFunctions(
              ParseFunctionInputs<LabeledStatement::kTag>(false),
              AttributeSpecifierSeq(true),
              ParseFunction<LabeledStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_case,
                  diag::DiagKind::labeled_statement_expect_kw_case),
              ConstantExpression(false),
              ParseFunction<LabeledStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::colon,
                  diag::DiagKind::labeled_statement_expect_colon),
              Statement(false)),
          SerialParseFunctions(
              ParseFunctionInputs<LabeledStatement::kTag>(false),
              AttributeSpecifierSeq(true),
              ParseFunction<LabeledStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_default,
                  diag::DiagKind::labeled_statement_expect_kw_default),
              ParseFunction<LabeledStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::colon,
                  diag::DiagKind::labeled_statement_expect_colon),
              Statement(false)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// expression-statement:
// 	expression[opt], `;`
inline ParseFunctionOutputs<ExpressionStatement::kTag>
ExpressionStatement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ExpressionStatement::kTag>(false, output.last_token_,
                                                     output.cur_token_),
      Expression(true),
      ParseFunction<ExpressionStatement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::expression_statement_expect_semi));
  return serial_funcs();
}

// compound-statement:
// 	`{`, statement_seq[opt], `}`
inline ParseFunctionOutputs<CompoundStatement::kTag>
CompoundStatement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<CompoundStatement::kTag>(false, output.last_token_,
                                                   output.cur_token_),
      ParseFunction<CompoundStatement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_brace,
          diag::DiagKind::compound_statement_expect_l_brace),
      StatementSeq(true),
      ParseFunction<CompoundStatement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_brace,
          diag::DiagKind::compound_statement_expect_r_brace));
  return serial_funcs();
}

// statement-seq:
// 	statement
// 	statement_seq, statement
inline ParseFunctionOutputs<StatementSeq::kTag> StatementSeq::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<StatementSeq::kTag, 1,
                           SerialParseFunctions<StatementSeq::kTag, Statement>>
        parallel_funcs_0(
            ParseFunctionInputs<StatementSeq::kTag>(false, output.last_token_,
                                                    output.cur_token_),
            SerialParseFunctions(ParseFunctionInputs<StatementSeq::kTag>(false),
                                 Statement(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      StatementSeq::kTag, 1,
      SerialParseFunctions<StatementSeq::kTag, StatementSeq, Statement>>
      parallel_funcs_1(
          ParseFunctionInputs<StatementSeq::kTag>(false, output.last_token_,
                                                  output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<StatementSeq::kTag>(false),
                               StatementSeq(false), Statement(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// selection-statement:
// 	`if`, `constexpr`[opt], `(`, init_statement[opt], condition, `)`, statement
// 	`if`, `constexpr`[opt], `(`, init_statement[opt], condition, `)`, statement, `else`, statement
// 	`switch`, `(`, init_statement[opt], condition, `)`, statement
inline ParseFunctionOutputs<SelectionStatement::kTag>
SelectionStatement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      SelectionStatement::kTag, 3,
      SerialParseFunctions<
          SelectionStatement::kTag, ParseFunction<SelectionStatement::kTag>,
          ParseFunction<SelectionStatement::kTag>,
          ParseFunction<SelectionStatement::kTag>, InitStatement, Condition,
          ParseFunction<SelectionStatement::kTag>, Statement>,
      SerialParseFunctions<
          SelectionStatement::kTag, ParseFunction<SelectionStatement::kTag>,
          ParseFunction<SelectionStatement::kTag>,
          ParseFunction<SelectionStatement::kTag>, InitStatement, Condition,
          ParseFunction<SelectionStatement::kTag>, Statement,
          ParseFunction<SelectionStatement::kTag>, Statement>,
      SerialParseFunctions<
          SelectionStatement::kTag, ParseFunction<SelectionStatement::kTag>,
          ParseFunction<SelectionStatement::kTag>, InitStatement, Condition,
          ParseFunction<SelectionStatement::kTag>, Statement>>
      parallel_funcs_0(
          ParseFunctionInputs<SelectionStatement::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<SelectionStatement::kTag>(false),
              ParseFunction<SelectionStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_if,
                      diag::DiagKind::selection_statement_expect_kw_if),
              ParseFunction<SelectionStatement::kTag>::
                  create_single_token_check(
                      true, token::tok::TokenKind::kw_constexpr,
                      diag::DiagKind::selection_statement_expect_kw_constexpr),
              ParseFunction<SelectionStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::selection_statement_expect_l_paren),
              InitStatement(true), Condition(false),
              ParseFunction<SelectionStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::selection_statement_expect_r_paren),
              Statement(false)),
          SerialParseFunctions(
              ParseFunctionInputs<SelectionStatement::kTag>(false),
              ParseFunction<SelectionStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_if,
                      diag::DiagKind::selection_statement_expect_kw_if),
              ParseFunction<SelectionStatement::kTag>::
                  create_single_token_check(
                      true, token::tok::TokenKind::kw_constexpr,
                      diag::DiagKind::selection_statement_expect_kw_constexpr),
              ParseFunction<SelectionStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::selection_statement_expect_l_paren),
              InitStatement(true), Condition(false),
              ParseFunction<SelectionStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::selection_statement_expect_r_paren),
              Statement(false),
              ParseFunction<SelectionStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_else,
                      diag::DiagKind::selection_statement_expect_kw_else),
              Statement(false)),
          SerialParseFunctions(
              ParseFunctionInputs<SelectionStatement::kTag>(false),
              ParseFunction<SelectionStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_switch,
                      diag::DiagKind::selection_statement_expect_kw_switch),
              ParseFunction<SelectionStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::selection_statement_expect_l_paren),
              InitStatement(true), Condition(false),
              ParseFunction<SelectionStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::selection_statement_expect_r_paren),
              Statement(false)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// iteration-statement:
// 	`while`, `(`, condition, `)`, statement
// 	`do`, statement, `while`, `(`, expression, `)`, `;`
// 	`for`, `(`, init_statement, condition[opt], `;`, expression[opt], `)`, statement
// 	`for`, `(`, init_statement[opt], for_range_declaration, `:`, for_range_initializer, `)`, statement
inline ParseFunctionOutputs<IterationStatement::kTag>
IterationStatement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      IterationStatement::kTag, 4,
      SerialParseFunctions<IterationStatement::kTag,
                           ParseFunction<IterationStatement::kTag>,
                           ParseFunction<IterationStatement::kTag>, Condition,
                           ParseFunction<IterationStatement::kTag>, Statement>,
      SerialParseFunctions<IterationStatement::kTag,
                           ParseFunction<IterationStatement::kTag>, Statement,
                           ParseFunction<IterationStatement::kTag>,
                           ParseFunction<IterationStatement::kTag>, Expression,
                           ParseFunction<IterationStatement::kTag>,
                           ParseFunction<IterationStatement::kTag>>,
      SerialParseFunctions<
          IterationStatement::kTag, ParseFunction<IterationStatement::kTag>,
          ParseFunction<IterationStatement::kTag>, InitStatement, Condition,
          ParseFunction<IterationStatement::kTag>, Expression,
          ParseFunction<IterationStatement::kTag>, Statement>,
      SerialParseFunctions<
          IterationStatement::kTag, ParseFunction<IterationStatement::kTag>,
          ParseFunction<IterationStatement::kTag>, InitStatement,
          ForRangeDeclaration, ParseFunction<IterationStatement::kTag>,
          ForRangeInitializer, ParseFunction<IterationStatement::kTag>,
          Statement>>
      parallel_funcs_0(
          ParseFunctionInputs<IterationStatement::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<IterationStatement::kTag>(false),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_while,
                      diag::DiagKind::iteration_statement_expect_kw_while),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::iteration_statement_expect_l_paren),
              Condition(false),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::iteration_statement_expect_r_paren),
              Statement(false)),
          SerialParseFunctions(
              ParseFunctionInputs<IterationStatement::kTag>(false),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_do,
                      diag::DiagKind::iteration_statement_expect_kw_do),
              Statement(false),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_while,
                      diag::DiagKind::iteration_statement_expect_kw_while),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::iteration_statement_expect_l_paren),
              Expression(false),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::iteration_statement_expect_r_paren),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::semi,
                      diag::DiagKind::iteration_statement_expect_semi)),
          SerialParseFunctions(
              ParseFunctionInputs<IterationStatement::kTag>(false),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_for,
                      diag::DiagKind::iteration_statement_expect_kw_for),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::iteration_statement_expect_l_paren),
              InitStatement(false), Condition(true),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::semi,
                      diag::DiagKind::iteration_statement_expect_semi),
              Expression(true),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::iteration_statement_expect_r_paren),
              Statement(false)),
          SerialParseFunctions(
              ParseFunctionInputs<IterationStatement::kTag>(false),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_for,
                      diag::DiagKind::iteration_statement_expect_kw_for),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::iteration_statement_expect_l_paren),
              InitStatement(true), ForRangeDeclaration(false),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::colon,
                      diag::DiagKind::iteration_statement_expect_colon),
              ForRangeInitializer(false),
              ParseFunction<IterationStatement::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::iteration_statement_expect_r_paren),
              Statement(false)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// for-range-declaration:
// 	attribute_specifier_seq[opt], decl_specifier_seq, declarator
// 	attribute_specifier_seq[opt], decl_specifier_seq, ref_qualifier[opt], `[`, identifier_list, `]`
inline ParseFunctionOutputs<ForRangeDeclaration::kTag>
ForRangeDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ForRangeDeclaration::kTag, 2,
      SerialParseFunctions<ForRangeDeclaration::kTag, AttributeSpecifierSeq,
                           DeclSpecifierSeq, Declarator>,
      SerialParseFunctions<
          ForRangeDeclaration::kTag, AttributeSpecifierSeq, DeclSpecifierSeq,
          RefQualifier, ParseFunction<ForRangeDeclaration::kTag>,
          IdentifierList, ParseFunction<ForRangeDeclaration::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<ForRangeDeclaration::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ForRangeDeclaration::kTag>(false),
              AttributeSpecifierSeq(true), DeclSpecifierSeq(false),
              Declarator(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ForRangeDeclaration::kTag>(false),
              AttributeSpecifierSeq(true), DeclSpecifierSeq(false),
              RefQualifier(true),
              ParseFunction<ForRangeDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_square,
                      diag::DiagKind::for_range_declaration_expect_l_square),
              IdentifierList(false),
              ParseFunction<ForRangeDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_square,
                      diag::DiagKind::for_range_declaration_expect_r_square)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// for-range-initializer:
// 	expr_or_braced_init_list
inline ParseFunctionOutputs<ForRangeInitializer::kTag>
ForRangeInitializer::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ForRangeInitializer::kTag>(false, output.last_token_,
                                                     output.cur_token_),
      ExprOrBracedInitList(false));
  return serial_funcs();
}

// jump-statement:
// 	`break`, `;`
// 	`continue`, `;`
// 	`return`, expr_or_braced_init_list[opt], `;`
// 	coroutine_return_statement
// 	`goto`, `identifier`, `;`
inline ParseFunctionOutputs<JumpStatement::kTag> JumpStatement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      JumpStatement::kTag, 5,
      SerialParseFunctions<JumpStatement::kTag,
                           ParseFunction<JumpStatement::kTag>,
                           ParseFunction<JumpStatement::kTag>>,
      SerialParseFunctions<JumpStatement::kTag,
                           ParseFunction<JumpStatement::kTag>,
                           ParseFunction<JumpStatement::kTag>>,
      SerialParseFunctions<
          JumpStatement::kTag, ParseFunction<JumpStatement::kTag>,
          ExprOrBracedInitList, ParseFunction<JumpStatement::kTag>>,
      SerialParseFunctions<JumpStatement::kTag, CoroutineReturnStatement>,
      SerialParseFunctions<JumpStatement::kTag,
                           ParseFunction<JumpStatement::kTag>,
                           ParseFunction<JumpStatement::kTag>,
                           ParseFunction<JumpStatement::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<JumpStatement::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<JumpStatement::kTag>(false),
              ParseFunction<JumpStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_break,
                  diag::DiagKind::jump_statement_expect_kw_break),
              ParseFunction<JumpStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::semi,
                  diag::DiagKind::jump_statement_expect_semi)),
          SerialParseFunctions(
              ParseFunctionInputs<JumpStatement::kTag>(false),
              ParseFunction<JumpStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_continue,
                  diag::DiagKind::jump_statement_expect_kw_continue),
              ParseFunction<JumpStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::semi,
                  diag::DiagKind::jump_statement_expect_semi)),
          SerialParseFunctions(
              ParseFunctionInputs<JumpStatement::kTag>(false),
              ParseFunction<JumpStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_return,
                  diag::DiagKind::jump_statement_expect_kw_return),
              ExprOrBracedInitList(true),
              ParseFunction<JumpStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::semi,
                  diag::DiagKind::jump_statement_expect_semi)),
          SerialParseFunctions(ParseFunctionInputs<JumpStatement::kTag>(false),
                               CoroutineReturnStatement(false)),
          SerialParseFunctions(
              ParseFunctionInputs<JumpStatement::kTag>(false),
              ParseFunction<JumpStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_goto,
                  diag::DiagKind::jump_statement_expect_kw_goto),
              ParseFunction<JumpStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::jump_statement_expect_identifier),
              ParseFunction<JumpStatement::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::semi,
                  diag::DiagKind::jump_statement_expect_semi)));

  static_assert(base::kNumberOfElements >= 5);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// coroutine-return-statement:
// 	`co_return`, expr_or_braced_init_list[opt], `;`
inline ParseFunctionOutputs<CoroutineReturnStatement::kTag>
CoroutineReturnStatement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<CoroutineReturnStatement::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<CoroutineReturnStatement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_co_return,
          diag::DiagKind::coroutine_return_statement_expect_kw_co_return),
      ExprOrBracedInitList(true),
      ParseFunction<CoroutineReturnStatement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::coroutine_return_statement_expect_semi));
  return serial_funcs();
}

// declaration-statement:
// 	block_declaration
inline ParseFunctionOutputs<DeclarationStatement::kTag>
DeclarationStatement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<DeclarationStatement::kTag>(false, output.last_token_,
                                                      output.cur_token_),
      BlockDeclaration(false));
  return serial_funcs();
}

// declaration-seq:
// 	declaration
// 	declaration_seq, declaration
inline ParseFunctionOutputs<DeclarationSeq::kTag> DeclarationSeq::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        DeclarationSeq::kTag, 1,
        SerialParseFunctions<DeclarationSeq::kTag, Declaration>>
        parallel_funcs_0(ParseFunctionInputs<DeclarationSeq::kTag>(
                             false, output.last_token_, output.cur_token_),
                         SerialParseFunctions(
                             ParseFunctionInputs<DeclarationSeq::kTag>(false),
                             Declaration(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      DeclarationSeq::kTag, 1,
      SerialParseFunctions<DeclarationSeq::kTag, DeclarationSeq, Declaration>>
      parallel_funcs_1(
          ParseFunctionInputs<DeclarationSeq::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<DeclarationSeq::kTag>(false),
                               DeclarationSeq(false), Declaration(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// declaration:
// 	block_declaration
// 	nodeclspec_function_declaration
// 	function_definition
// 	template_declaration
// 	deduction_guide
// 	explicit_instantiation
// 	explicit_specialization
// 	export_declaration
// 	linkage_specification
// 	namespace_definition
// 	empty_declaration
// 	attribute_declaration
// 	module_import_declaration
inline ParseFunctionOutputs<Declaration::kTag> Declaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      Declaration::kTag, 13,
      SerialParseFunctions<Declaration::kTag, BlockDeclaration>,
      SerialParseFunctions<Declaration::kTag, NodeclspecFunctionDeclaration>,
      SerialParseFunctions<Declaration::kTag, FunctionDefinition>,
      SerialParseFunctions<Declaration::kTag, TemplateDeclaration>,
      SerialParseFunctions<Declaration::kTag, DeductionGuide>,
      SerialParseFunctions<Declaration::kTag, ExplicitInstantiation>,
      SerialParseFunctions<Declaration::kTag, ExplicitSpecialization>,
      SerialParseFunctions<Declaration::kTag, ExportDeclaration>,
      SerialParseFunctions<Declaration::kTag, LinkageSpecification>,
      SerialParseFunctions<Declaration::kTag, NamespaceDefinition>,
      SerialParseFunctions<Declaration::kTag, EmptyDeclaration>,
      SerialParseFunctions<Declaration::kTag, AttributeDeclaration>,
      SerialParseFunctions<Declaration::kTag, ModuleImportDeclaration>>
      parallel_funcs_0(
          ParseFunctionInputs<Declaration::kTag>(false, output.last_token_,
                                                 output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<Declaration::kTag>(false),
                               BlockDeclaration(false)),
          SerialParseFunctions(ParseFunctionInputs<Declaration::kTag>(false),
                               NodeclspecFunctionDeclaration(false)),
          SerialParseFunctions(ParseFunctionInputs<Declaration::kTag>(false),
                               FunctionDefinition(false)),
          SerialParseFunctions(ParseFunctionInputs<Declaration::kTag>(false),
                               TemplateDeclaration(false)),
          SerialParseFunctions(ParseFunctionInputs<Declaration::kTag>(false),
                               DeductionGuide(false)),
          SerialParseFunctions(ParseFunctionInputs<Declaration::kTag>(false),
                               ExplicitInstantiation(false)),
          SerialParseFunctions(ParseFunctionInputs<Declaration::kTag>(false),
                               ExplicitSpecialization(false)),
          SerialParseFunctions(ParseFunctionInputs<Declaration::kTag>(false),
                               ExportDeclaration(false)),
          SerialParseFunctions(ParseFunctionInputs<Declaration::kTag>(false),
                               LinkageSpecification(false)),
          SerialParseFunctions(ParseFunctionInputs<Declaration::kTag>(false),
                               NamespaceDefinition(false)),
          SerialParseFunctions(ParseFunctionInputs<Declaration::kTag>(false),
                               EmptyDeclaration(false)),
          SerialParseFunctions(ParseFunctionInputs<Declaration::kTag>(false),
                               AttributeDeclaration(false)),
          SerialParseFunctions(ParseFunctionInputs<Declaration::kTag>(false),
                               ModuleImportDeclaration(false)));

  static_assert(base::kNumberOfElements >= 13);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// block-declaration:
// 	simple_declaration
// 	asm_declaration
// 	namespace_alias_definition
// 	using_declaration
// 	using_enum_declaration
// 	using_directive
// 	static_assert_declaration
// 	alias_declaration
// 	opaque_enum_declaration
inline ParseFunctionOutputs<BlockDeclaration::kTag>
BlockDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      BlockDeclaration::kTag, 9,
      SerialParseFunctions<BlockDeclaration::kTag, SimpleDeclaration>,
      SerialParseFunctions<BlockDeclaration::kTag, AsmDeclaration>,
      SerialParseFunctions<BlockDeclaration::kTag, NamespaceAliasDefinition>,
      SerialParseFunctions<BlockDeclaration::kTag, UsingDeclaration>,
      SerialParseFunctions<BlockDeclaration::kTag, UsingEnumDeclaration>,
      SerialParseFunctions<BlockDeclaration::kTag, UsingDirective>,
      SerialParseFunctions<BlockDeclaration::kTag, StaticAssertDeclaration>,
      SerialParseFunctions<BlockDeclaration::kTag, AliasDeclaration>,
      SerialParseFunctions<BlockDeclaration::kTag, OpaqueEnumDeclaration>>
      parallel_funcs_0(ParseFunctionInputs<BlockDeclaration::kTag>(
                           false, output.last_token_, output.cur_token_),
                       SerialParseFunctions(
                           ParseFunctionInputs<BlockDeclaration::kTag>(false),
                           SimpleDeclaration(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<BlockDeclaration::kTag>(false),
                           AsmDeclaration(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<BlockDeclaration::kTag>(false),
                           NamespaceAliasDefinition(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<BlockDeclaration::kTag>(false),
                           UsingDeclaration(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<BlockDeclaration::kTag>(false),
                           UsingEnumDeclaration(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<BlockDeclaration::kTag>(false),
                           UsingDirective(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<BlockDeclaration::kTag>(false),
                           StaticAssertDeclaration(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<BlockDeclaration::kTag>(false),
                           AliasDeclaration(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<BlockDeclaration::kTag>(false),
                           OpaqueEnumDeclaration(false)));

  static_assert(base::kNumberOfElements >= 9);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// nodeclspec-function-declaration:
// 	attribute_specifier_seq[opt], declarator, `;`
inline ParseFunctionOutputs<NodeclspecFunctionDeclaration::kTag>
NodeclspecFunctionDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<NodeclspecFunctionDeclaration::kTag>(
          false, output.last_token_, output.cur_token_),
      AttributeSpecifierSeq(true), Declarator(false),
      ParseFunction<NodeclspecFunctionDeclaration::kTag>::
          create_single_token_check(
              false, token::tok::TokenKind::semi,
              diag::DiagKind::nodeclspec_function_declaration_expect_semi));
  return serial_funcs();
}

// alias-declaration:
// 	`using`, `identifier`, attribute_specifier_seq[opt], `=`, defining_type_id, `;`
inline ParseFunctionOutputs<AliasDeclaration::kTag>
AliasDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<AliasDeclaration::kTag>(false, output.last_token_,
                                                  output.cur_token_),
      ParseFunction<AliasDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_using,
          diag::DiagKind::alias_declaration_expect_kw_using),
      ParseFunction<AliasDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::alias_declaration_expect_identifier),
      AttributeSpecifierSeq(true),
      ParseFunction<AliasDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::equal,
          diag::DiagKind::alias_declaration_expect_equal),
      DefiningTypeId(false),
      ParseFunction<AliasDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::alias_declaration_expect_semi));
  return serial_funcs();
}

// simple-declaration:
// 	decl_specifier_seq, init_declarator_list[opt], `;`
// 	attribute_specifier_seq, decl_specifier_seq, init_declarator_list, `;`
// 	attribute_specifier_seq[opt], decl_specifier_seq, ref_qualifier[opt], `[`, identifier_list, `]`, initializer, `;`
inline ParseFunctionOutputs<SimpleDeclaration::kTag>
SimpleDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      SimpleDeclaration::kTag, 3,
      SerialParseFunctions<SimpleDeclaration::kTag, DeclSpecifierSeq,
                           InitDeclaratorList,
                           ParseFunction<SimpleDeclaration::kTag>>,
      SerialParseFunctions<SimpleDeclaration::kTag, AttributeSpecifierSeq,
                           DeclSpecifierSeq, InitDeclaratorList,
                           ParseFunction<SimpleDeclaration::kTag>>,
      SerialParseFunctions<
          SimpleDeclaration::kTag, AttributeSpecifierSeq, DeclSpecifierSeq,
          RefQualifier, ParseFunction<SimpleDeclaration::kTag>, IdentifierList,
          ParseFunction<SimpleDeclaration::kTag>, Initializer,
          ParseFunction<SimpleDeclaration::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<SimpleDeclaration::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleDeclaration::kTag>(false),
              DeclSpecifierSeq(false), InitDeclaratorList(true),
              ParseFunction<SimpleDeclaration::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::semi,
                  diag::DiagKind::simple_declaration_expect_semi)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleDeclaration::kTag>(false),
              AttributeSpecifierSeq(false), DeclSpecifierSeq(false),
              InitDeclaratorList(false),
              ParseFunction<SimpleDeclaration::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::semi,
                  diag::DiagKind::simple_declaration_expect_semi)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleDeclaration::kTag>(false),
              AttributeSpecifierSeq(true), DeclSpecifierSeq(false),
              RefQualifier(true),
              ParseFunction<SimpleDeclaration::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_square,
                  diag::DiagKind::simple_declaration_expect_l_square),
              IdentifierList(false),
              ParseFunction<SimpleDeclaration::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_square,
                  diag::DiagKind::simple_declaration_expect_r_square),
              Initializer(false),
              ParseFunction<SimpleDeclaration::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::semi,
                  diag::DiagKind::simple_declaration_expect_semi)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// static_assert-declaration:
// 	`static_assert`, `(`, constant_expression, `)`, `;`
// 	`static_assert`, `(`, constant_expression, `,`, string_literal, `)`, `;`
inline ParseFunctionOutputs<StaticAssertDeclaration::kTag>
StaticAssertDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      StaticAssertDeclaration::kTag, 2,
      SerialParseFunctions<StaticAssertDeclaration::kTag,
                           ParseFunction<StaticAssertDeclaration::kTag>,
                           ParseFunction<StaticAssertDeclaration::kTag>,
                           ConstantExpression,
                           ParseFunction<StaticAssertDeclaration::kTag>,
                           ParseFunction<StaticAssertDeclaration::kTag>>,
      SerialParseFunctions<
          StaticAssertDeclaration::kTag,
          ParseFunction<StaticAssertDeclaration::kTag>,
          ParseFunction<StaticAssertDeclaration::kTag>, ConstantExpression,
          ParseFunction<StaticAssertDeclaration::kTag>, StringLiteral,
          ParseFunction<StaticAssertDeclaration::kTag>,
          ParseFunction<StaticAssertDeclaration::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<StaticAssertDeclaration::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<StaticAssertDeclaration::kTag>(false),
              ParseFunction<StaticAssertDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_static_assert,
                      diag::DiagKind::
                          static_assert_declaration_expect_kw_static_assert),
              ParseFunction<StaticAssertDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::static_assert_declaration_expect_l_paren),
              ConstantExpression(false),
              ParseFunction<StaticAssertDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::static_assert_declaration_expect_r_paren),
              ParseFunction<StaticAssertDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::semi,
                      diag::DiagKind::static_assert_declaration_expect_semi)),
          SerialParseFunctions(
              ParseFunctionInputs<StaticAssertDeclaration::kTag>(false),
              ParseFunction<StaticAssertDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_static_assert,
                      diag::DiagKind::
                          static_assert_declaration_expect_kw_static_assert),
              ParseFunction<StaticAssertDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::static_assert_declaration_expect_l_paren),
              ConstantExpression(false),
              ParseFunction<StaticAssertDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::comma,
                      diag::DiagKind::static_assert_declaration_expect_comma),
              StringLiteral(false),
              ParseFunction<StaticAssertDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::static_assert_declaration_expect_r_paren),
              ParseFunction<StaticAssertDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::semi,
                      diag::DiagKind::static_assert_declaration_expect_semi)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// empty-declaration:
// 	`;`
inline ParseFunctionOutputs<EmptyDeclaration::kTag>
EmptyDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<EmptyDeclaration::kTag>(false, output.last_token_,
                                                  output.cur_token_),
      ParseFunction<EmptyDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::empty_declaration_expect_semi));
  return serial_funcs();
}

// attribute-declaration:
// 	attribute_specifier_seq, `;`
inline ParseFunctionOutputs<AttributeDeclaration::kTag>
AttributeDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<AttributeDeclaration::kTag>(false, output.last_token_,
                                                      output.cur_token_),
      AttributeSpecifierSeq(false),
      ParseFunction<AttributeDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::attribute_declaration_expect_semi));
  return serial_funcs();
}

// decl-specifier:
// 	storage_class_specifier
// 	defining_type_specifier
// 	function_specifier
// 	`friend`
// 	`typedef`
// 	`constexpr`
// 	`consteval`
// 	`constinit`
// 	`inline`
inline ParseFunctionOutputs<DeclSpecifier::kTag> DeclSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      DeclSpecifier::kTag, 9,
      SerialParseFunctions<DeclSpecifier::kTag, StorageClassSpecifier>,
      SerialParseFunctions<DeclSpecifier::kTag, DefiningTypeSpecifier>,
      SerialParseFunctions<DeclSpecifier::kTag, FunctionSpecifier>,
      SerialParseFunctions<DeclSpecifier::kTag,
                           ParseFunction<DeclSpecifier::kTag>>,
      SerialParseFunctions<DeclSpecifier::kTag,
                           ParseFunction<DeclSpecifier::kTag>>,
      SerialParseFunctions<DeclSpecifier::kTag,
                           ParseFunction<DeclSpecifier::kTag>>,
      SerialParseFunctions<DeclSpecifier::kTag,
                           ParseFunction<DeclSpecifier::kTag>>,
      SerialParseFunctions<DeclSpecifier::kTag,
                           ParseFunction<DeclSpecifier::kTag>>,
      SerialParseFunctions<DeclSpecifier::kTag,
                           ParseFunction<DeclSpecifier::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<DeclSpecifier::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<DeclSpecifier::kTag>(false),
                               StorageClassSpecifier(false)),
          SerialParseFunctions(ParseFunctionInputs<DeclSpecifier::kTag>(false),
                               DefiningTypeSpecifier(false)),
          SerialParseFunctions(ParseFunctionInputs<DeclSpecifier::kTag>(false),
                               FunctionSpecifier(false)),
          SerialParseFunctions(
              ParseFunctionInputs<DeclSpecifier::kTag>(false),
              ParseFunction<DeclSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_friend,
                  diag::DiagKind::decl_specifier_expect_kw_friend)),
          SerialParseFunctions(
              ParseFunctionInputs<DeclSpecifier::kTag>(false),
              ParseFunction<DeclSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_typedef,
                  diag::DiagKind::decl_specifier_expect_kw_typedef)),
          SerialParseFunctions(
              ParseFunctionInputs<DeclSpecifier::kTag>(false),
              ParseFunction<DeclSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_constexpr,
                  diag::DiagKind::decl_specifier_expect_kw_constexpr)),
          SerialParseFunctions(
              ParseFunctionInputs<DeclSpecifier::kTag>(false),
              ParseFunction<DeclSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_consteval,
                  diag::DiagKind::decl_specifier_expect_kw_consteval)),
          SerialParseFunctions(
              ParseFunctionInputs<DeclSpecifier::kTag>(false),
              ParseFunction<DeclSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_constinit,
                  diag::DiagKind::decl_specifier_expect_kw_constinit)),
          SerialParseFunctions(
              ParseFunctionInputs<DeclSpecifier::kTag>(false),
              ParseFunction<DeclSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_inline,
                  diag::DiagKind::decl_specifier_expect_kw_inline)));

  static_assert(base::kNumberOfElements >= 9);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// decl-specifier-seq:
// 	decl_specifier, attribute_specifier_seq[opt]
// 	decl_specifier, decl_specifier_seq
inline ParseFunctionOutputs<DeclSpecifierSeq::kTag>
DeclSpecifierSeq::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      DeclSpecifierSeq::kTag, 2,
      SerialParseFunctions<DeclSpecifierSeq::kTag, DeclSpecifier,
                           AttributeSpecifierSeq>,
      SerialParseFunctions<DeclSpecifierSeq::kTag, DeclSpecifier,
                           DeclSpecifierSeq>>
      parallel_funcs_0(ParseFunctionInputs<DeclSpecifierSeq::kTag>(
                           false, output.last_token_, output.cur_token_),
                       SerialParseFunctions(
                           ParseFunctionInputs<DeclSpecifierSeq::kTag>(false),
                           DeclSpecifier(false), AttributeSpecifierSeq(true)),
                       SerialParseFunctions(
                           ParseFunctionInputs<DeclSpecifierSeq::kTag>(false),
                           DeclSpecifier(false), DeclSpecifierSeq(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// storage-class-specifier:
// 	`static`
// 	`thread_local`
// 	`extern`
// 	`mutable`
inline ParseFunctionOutputs<StorageClassSpecifier::kTag>
StorageClassSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      StorageClassSpecifier::kTag, 4,
      SerialParseFunctions<StorageClassSpecifier::kTag,
                           ParseFunction<StorageClassSpecifier::kTag>>,
      SerialParseFunctions<StorageClassSpecifier::kTag,
                           ParseFunction<StorageClassSpecifier::kTag>>,
      SerialParseFunctions<StorageClassSpecifier::kTag,
                           ParseFunction<StorageClassSpecifier::kTag>>,
      SerialParseFunctions<StorageClassSpecifier::kTag,
                           ParseFunction<StorageClassSpecifier::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<StorageClassSpecifier::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<StorageClassSpecifier::kTag>(false),
              ParseFunction<StorageClassSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_static,
                      diag::DiagKind::
                          storage_class_specifier_expect_kw_static)),
          SerialParseFunctions(
              ParseFunctionInputs<StorageClassSpecifier::kTag>(false),
              ParseFunction<StorageClassSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_thread_local,
                      diag::DiagKind::
                          storage_class_specifier_expect_kw_thread_local)),
          SerialParseFunctions(
              ParseFunctionInputs<StorageClassSpecifier::kTag>(false),
              ParseFunction<StorageClassSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_extern,
                      diag::DiagKind::
                          storage_class_specifier_expect_kw_extern)),
          SerialParseFunctions(
              ParseFunctionInputs<StorageClassSpecifier::kTag>(false),
              ParseFunction<StorageClassSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_mutable,
                      diag::DiagKind::
                          storage_class_specifier_expect_kw_mutable)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// function-specifier:
// 	`virtual`
// 	explicit_specifier
inline ParseFunctionOutputs<FunctionSpecifier::kTag>
FunctionSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      FunctionSpecifier::kTag, 2,
      SerialParseFunctions<FunctionSpecifier::kTag,
                           ParseFunction<FunctionSpecifier::kTag>>,
      SerialParseFunctions<FunctionSpecifier::kTag, ExplicitSpecifier>>
      parallel_funcs_0(
          ParseFunctionInputs<FunctionSpecifier::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<FunctionSpecifier::kTag>(false),
              ParseFunction<FunctionSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_virtual,
                  diag::DiagKind::function_specifier_expect_kw_virtual)),
          SerialParseFunctions(
              ParseFunctionInputs<FunctionSpecifier::kTag>(false),
              ExplicitSpecifier(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// explicit-specifier:
// 	`explicit`, `(`, constant_expression, `)`
// 	`explicit`
inline ParseFunctionOutputs<ExplicitSpecifier::kTag>
ExplicitSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ExplicitSpecifier::kTag, 2,
      SerialParseFunctions<
          ExplicitSpecifier::kTag, ParseFunction<ExplicitSpecifier::kTag>,
          ParseFunction<ExplicitSpecifier::kTag>, ConstantExpression,
          ParseFunction<ExplicitSpecifier::kTag>>,
      SerialParseFunctions<ExplicitSpecifier::kTag,
                           ParseFunction<ExplicitSpecifier::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<ExplicitSpecifier::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ExplicitSpecifier::kTag>(false),
              ParseFunction<ExplicitSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_explicit,
                  diag::DiagKind::explicit_specifier_expect_kw_explicit),
              ParseFunction<ExplicitSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::explicit_specifier_expect_l_paren),
              ConstantExpression(false),
              ParseFunction<ExplicitSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::explicit_specifier_expect_r_paren)),
          SerialParseFunctions(
              ParseFunctionInputs<ExplicitSpecifier::kTag>(false),
              ParseFunction<ExplicitSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_explicit,
                  diag::DiagKind::explicit_specifier_expect_kw_explicit)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// typedef-name:
// 	`identifier`
// 	simple_template_id
inline ParseFunctionOutputs<TypedefName::kTag> TypedefName::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      TypedefName::kTag, 2,
      SerialParseFunctions<TypedefName::kTag, ParseFunction<TypedefName::kTag>>,
      SerialParseFunctions<TypedefName::kTag, SimpleTemplateId>>
      parallel_funcs_0(
          ParseFunctionInputs<TypedefName::kTag>(false, output.last_token_,
                                                 output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<TypedefName::kTag>(false),
              ParseFunction<TypedefName::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::typedef_name_expect_identifier)),
          SerialParseFunctions(ParseFunctionInputs<TypedefName::kTag>(false),
                               SimpleTemplateId(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// type-specifier:
// 	simple_type_specifier
// 	elaborated_type_specifier
// 	typename_specifier
// 	cv_qualifier
inline ParseFunctionOutputs<TypeSpecifier::kTag> TypeSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      TypeSpecifier::kTag, 4,
      SerialParseFunctions<TypeSpecifier::kTag, SimpleTypeSpecifier>,
      SerialParseFunctions<TypeSpecifier::kTag, ElaboratedTypeSpecifier>,
      SerialParseFunctions<TypeSpecifier::kTag, TypenameSpecifier>,
      SerialParseFunctions<TypeSpecifier::kTag, CvQualifier>>
      parallel_funcs_0(
          ParseFunctionInputs<TypeSpecifier::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<TypeSpecifier::kTag>(false),
                               SimpleTypeSpecifier(false)),
          SerialParseFunctions(ParseFunctionInputs<TypeSpecifier::kTag>(false),
                               ElaboratedTypeSpecifier(false)),
          SerialParseFunctions(ParseFunctionInputs<TypeSpecifier::kTag>(false),
                               TypenameSpecifier(false)),
          SerialParseFunctions(ParseFunctionInputs<TypeSpecifier::kTag>(false),
                               CvQualifier(false)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// type-specifier-seq:
// 	type_specifier, attribute_specifier_seq[opt]
// 	type_specifier, type_specifier_seq
inline ParseFunctionOutputs<TypeSpecifierSeq::kTag>
TypeSpecifierSeq::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      TypeSpecifierSeq::kTag, 2,
      SerialParseFunctions<TypeSpecifierSeq::kTag, TypeSpecifier,
                           AttributeSpecifierSeq>,
      SerialParseFunctions<TypeSpecifierSeq::kTag, TypeSpecifier,
                           TypeSpecifierSeq>>
      parallel_funcs_0(ParseFunctionInputs<TypeSpecifierSeq::kTag>(
                           false, output.last_token_, output.cur_token_),
                       SerialParseFunctions(
                           ParseFunctionInputs<TypeSpecifierSeq::kTag>(false),
                           TypeSpecifier(false), AttributeSpecifierSeq(true)),
                       SerialParseFunctions(
                           ParseFunctionInputs<TypeSpecifierSeq::kTag>(false),
                           TypeSpecifier(false), TypeSpecifierSeq(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// defining-type-specifier:
// 	type_specifier
// 	class_specifier
// 	enum_specifier
inline ParseFunctionOutputs<DefiningTypeSpecifier::kTag>
DefiningTypeSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      DefiningTypeSpecifier::kTag, 3,
      SerialParseFunctions<DefiningTypeSpecifier::kTag, TypeSpecifier>,
      SerialParseFunctions<DefiningTypeSpecifier::kTag, ClassSpecifier>,
      SerialParseFunctions<DefiningTypeSpecifier::kTag, EnumSpecifier>>
      parallel_funcs_0(
          ParseFunctionInputs<DefiningTypeSpecifier::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<DefiningTypeSpecifier::kTag>(false),
              TypeSpecifier(false)),
          SerialParseFunctions(
              ParseFunctionInputs<DefiningTypeSpecifier::kTag>(false),
              ClassSpecifier(false)),
          SerialParseFunctions(
              ParseFunctionInputs<DefiningTypeSpecifier::kTag>(false),
              EnumSpecifier(false)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// defining-type-specifier-seq:
// 	defining_type_specifier, attribute_specifier_seq[opt]
// 	defining_type_specifier, defining_type_specifier_seq
inline ParseFunctionOutputs<DefiningTypeSpecifierSeq::kTag>
DefiningTypeSpecifierSeq::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      DefiningTypeSpecifierSeq::kTag, 2,
      SerialParseFunctions<DefiningTypeSpecifierSeq::kTag,
                           DefiningTypeSpecifier, AttributeSpecifierSeq>,
      SerialParseFunctions<DefiningTypeSpecifierSeq::kTag,
                           DefiningTypeSpecifier, DefiningTypeSpecifierSeq>>
      parallel_funcs_0(
          ParseFunctionInputs<DefiningTypeSpecifierSeq::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<DefiningTypeSpecifierSeq::kTag>(false),
              DefiningTypeSpecifier(false), AttributeSpecifierSeq(true)),
          SerialParseFunctions(
              ParseFunctionInputs<DefiningTypeSpecifierSeq::kTag>(false),
              DefiningTypeSpecifier(false), DefiningTypeSpecifierSeq(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// simple-type-specifier:
// 	nested_name_specifier[opt], type_name
// 	nested_name_specifier, `template`, simple_template_id
// 	decltype_specifier
// 	placeholder_type_specifier
// 	nested_name_specifier[opt], template_name
// 	`char`
// 	`char8_t`
// 	`char16_t`
// 	`char32_t`
// 	`wchar_t`
// 	`bool`
// 	`short`
// 	`int`
// 	`long`
// 	`signed`
// 	`unsigned`
// 	`float`
// 	`double`
// 	`void`
inline ParseFunctionOutputs<SimpleTypeSpecifier::kTag>
SimpleTypeSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      SimpleTypeSpecifier::kTag, 19,
      SerialParseFunctions<SimpleTypeSpecifier::kTag, NestedNameSpecifier,
                           TypeName>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag, NestedNameSpecifier,
                           ParseFunction<SimpleTypeSpecifier::kTag>,
                           SimpleTemplateId>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag, DecltypeSpecifier>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag, PlaceholderTypeSpecifier>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag, NestedNameSpecifier,
                           TemplateName>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag,
                           ParseFunction<SimpleTypeSpecifier::kTag>>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag,
                           ParseFunction<SimpleTypeSpecifier::kTag>>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag,
                           ParseFunction<SimpleTypeSpecifier::kTag>>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag,
                           ParseFunction<SimpleTypeSpecifier::kTag>>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag,
                           ParseFunction<SimpleTypeSpecifier::kTag>>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag,
                           ParseFunction<SimpleTypeSpecifier::kTag>>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag,
                           ParseFunction<SimpleTypeSpecifier::kTag>>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag,
                           ParseFunction<SimpleTypeSpecifier::kTag>>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag,
                           ParseFunction<SimpleTypeSpecifier::kTag>>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag,
                           ParseFunction<SimpleTypeSpecifier::kTag>>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag,
                           ParseFunction<SimpleTypeSpecifier::kTag>>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag,
                           ParseFunction<SimpleTypeSpecifier::kTag>>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag,
                           ParseFunction<SimpleTypeSpecifier::kTag>>,
      SerialParseFunctions<SimpleTypeSpecifier::kTag,
                           ParseFunction<SimpleTypeSpecifier::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<SimpleTypeSpecifier::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              NestedNameSpecifier(true), TypeName(false)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              NestedNameSpecifier(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_template,
                      diag::DiagKind::simple_type_specifier_expect_kw_template),
              SimpleTemplateId(false)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              DecltypeSpecifier(false)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              PlaceholderTypeSpecifier(false)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              NestedNameSpecifier(true), TemplateName(false)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_char,
                      diag::DiagKind::simple_type_specifier_expect_kw_char)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_char8_t,
                      diag::DiagKind::simple_type_specifier_expect_kw_char8_t)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_char16_t,
                      diag::DiagKind::
                          simple_type_specifier_expect_kw_char16_t)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_char32_t,
                      diag::DiagKind::
                          simple_type_specifier_expect_kw_char32_t)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_wchar_t,
                      diag::DiagKind::simple_type_specifier_expect_kw_wchar_t)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_bool,
                      diag::DiagKind::simple_type_specifier_expect_kw_bool)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_short,
                      diag::DiagKind::simple_type_specifier_expect_kw_short)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_int,
                      diag::DiagKind::simple_type_specifier_expect_kw_int)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_long,
                      diag::DiagKind::simple_type_specifier_expect_kw_long)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_signed,
                      diag::DiagKind::simple_type_specifier_expect_kw_signed)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_unsigned,
                      diag::DiagKind::
                          simple_type_specifier_expect_kw_unsigned)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_float,
                      diag::DiagKind::simple_type_specifier_expect_kw_float)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_double,
                      diag::DiagKind::simple_type_specifier_expect_kw_double)),
          SerialParseFunctions(
              ParseFunctionInputs<SimpleTypeSpecifier::kTag>(false),
              ParseFunction<SimpleTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_void,
                      diag::DiagKind::simple_type_specifier_expect_kw_void)));

  static_assert(base::kNumberOfElements >= 19);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// type-name:
// 	class_name
// 	enum_name
// 	typedef_name
inline ParseFunctionOutputs<TypeName::kTag> TypeName::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<TypeName::kTag, 3,
                         SerialParseFunctions<TypeName::kTag, ClassName>,
                         SerialParseFunctions<TypeName::kTag, EnumName>,
                         SerialParseFunctions<TypeName::kTag, TypedefName>>
      parallel_funcs_0(
          ParseFunctionInputs<TypeName::kTag>(false, output.last_token_,
                                              output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<TypeName::kTag>(false),
                               ClassName(false)),
          SerialParseFunctions(ParseFunctionInputs<TypeName::kTag>(false),
                               EnumName(false)),
          SerialParseFunctions(ParseFunctionInputs<TypeName::kTag>(false),
                               TypedefName(false)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// elaborated-type-specifier:
// 	class_key, attribute_specifier_seq[opt], nested_name_specifier[opt], `identifier`
// 	class_key, simple_template_id
// 	class_key, nested_name_specifier, `template`[opt], simple_template_id
// 	elaborated_enum_specifier
inline ParseFunctionOutputs<ElaboratedTypeSpecifier::kTag>
ElaboratedTypeSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ElaboratedTypeSpecifier::kTag, 4,
      SerialParseFunctions<ElaboratedTypeSpecifier::kTag, ClassKey,
                           AttributeSpecifierSeq, NestedNameSpecifier,
                           ParseFunction<ElaboratedTypeSpecifier::kTag>>,
      SerialParseFunctions<ElaboratedTypeSpecifier::kTag, ClassKey,
                           SimpleTemplateId>,
      SerialParseFunctions<
          ElaboratedTypeSpecifier::kTag, ClassKey, NestedNameSpecifier,
          ParseFunction<ElaboratedTypeSpecifier::kTag>, SimpleTemplateId>,
      SerialParseFunctions<ElaboratedTypeSpecifier::kTag,
                           ElaboratedEnumSpecifier>>
      parallel_funcs_0(
          ParseFunctionInputs<ElaboratedTypeSpecifier::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ElaboratedTypeSpecifier::kTag>(false),
              ClassKey(false), AttributeSpecifierSeq(true),
              NestedNameSpecifier(true),
              ParseFunction<ElaboratedTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::identifier,
                      diag::DiagKind::
                          elaborated_type_specifier_expect_identifier)),
          SerialParseFunctions(
              ParseFunctionInputs<ElaboratedTypeSpecifier::kTag>(false),
              ClassKey(false), SimpleTemplateId(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ElaboratedTypeSpecifier::kTag>(false),
              ClassKey(false), NestedNameSpecifier(false),
              ParseFunction<ElaboratedTypeSpecifier::kTag>::
                  create_single_token_check(
                      true, token::tok::TokenKind::kw_template,
                      diag::DiagKind::
                          elaborated_type_specifier_expect_kw_template),
              SimpleTemplateId(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ElaboratedTypeSpecifier::kTag>(false),
              ElaboratedEnumSpecifier(false)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// elaborated-enum-specifier:
// 	`enum`, nested_name_specifier[opt], `identifier`
inline ParseFunctionOutputs<ElaboratedEnumSpecifier::kTag>
ElaboratedEnumSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ElaboratedEnumSpecifier::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<ElaboratedEnumSpecifier::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_enum,
          diag::DiagKind::elaborated_enum_specifier_expect_kw_enum),
      NestedNameSpecifier(true),
      ParseFunction<ElaboratedEnumSpecifier::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::elaborated_enum_specifier_expect_identifier));
  return serial_funcs();
}

// decltype-specifier:
// 	`decltype`, `(`, expression, `)`
inline ParseFunctionOutputs<DecltypeSpecifier::kTag>
DecltypeSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<DecltypeSpecifier::kTag>(false, output.last_token_,
                                                   output.cur_token_),
      ParseFunction<DecltypeSpecifier::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_decltype,
          diag::DiagKind::decltype_specifier_expect_kw_decltype),
      ParseFunction<DecltypeSpecifier::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_paren,
          diag::DiagKind::decltype_specifier_expect_l_paren),
      Expression(false),
      ParseFunction<DecltypeSpecifier::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_paren,
          diag::DiagKind::decltype_specifier_expect_r_paren));
  return serial_funcs();
}

// placeholder-type-specifier:
// 	type_constraint[opt], `auto`
// 	type_constraint[opt], `decltype`, `(`, `auto`, `)`
inline ParseFunctionOutputs<PlaceholderTypeSpecifier::kTag>
PlaceholderTypeSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      PlaceholderTypeSpecifier::kTag, 2,
      SerialParseFunctions<PlaceholderTypeSpecifier::kTag, TypeConstraint,
                           ParseFunction<PlaceholderTypeSpecifier::kTag>>,
      SerialParseFunctions<PlaceholderTypeSpecifier::kTag, TypeConstraint,
                           ParseFunction<PlaceholderTypeSpecifier::kTag>,
                           ParseFunction<PlaceholderTypeSpecifier::kTag>,
                           ParseFunction<PlaceholderTypeSpecifier::kTag>,
                           ParseFunction<PlaceholderTypeSpecifier::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<PlaceholderTypeSpecifier::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<PlaceholderTypeSpecifier::kTag>(false),
              TypeConstraint(true),
              ParseFunction<PlaceholderTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_auto,
                      diag::DiagKind::
                          placeholder_type_specifier_expect_kw_auto)),
          SerialParseFunctions(
              ParseFunctionInputs<PlaceholderTypeSpecifier::kTag>(false),
              TypeConstraint(true),
              ParseFunction<PlaceholderTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_decltype,
                      diag::DiagKind::
                          placeholder_type_specifier_expect_kw_decltype),
              ParseFunction<PlaceholderTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::
                          placeholder_type_specifier_expect_l_paren),
              ParseFunction<PlaceholderTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_auto,
                      diag::DiagKind::
                          placeholder_type_specifier_expect_kw_auto),
              ParseFunction<PlaceholderTypeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::
                          placeholder_type_specifier_expect_r_paren)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// init-declarator-list:
// 	init_declarator
// 	init_declarator_list, `,`, init_declarator
inline ParseFunctionOutputs<InitDeclaratorList::kTag>
InitDeclaratorList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        InitDeclaratorList::kTag, 1,
        SerialParseFunctions<InitDeclaratorList::kTag, InitDeclarator>>
        parallel_funcs_0(
            ParseFunctionInputs<InitDeclaratorList::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<InitDeclaratorList::kTag>(false),
                InitDeclarator(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      InitDeclaratorList::kTag, 1,
      SerialParseFunctions<InitDeclaratorList::kTag, InitDeclaratorList,
                           ParseFunction<InitDeclaratorList::kTag>,
                           InitDeclarator>>
      parallel_funcs_1(
          ParseFunctionInputs<InitDeclaratorList::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<InitDeclaratorList::kTag>(false),
              InitDeclaratorList(false),
              ParseFunction<InitDeclaratorList::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::comma,
                      diag::DiagKind::init_declarator_list_expect_comma),
              InitDeclarator(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// init-declarator:
// 	declarator, initializer[opt]
// 	declarator, requires_clause
inline ParseFunctionOutputs<InitDeclarator::kTag> InitDeclarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      InitDeclarator::kTag, 2,
      SerialParseFunctions<InitDeclarator::kTag, Declarator, Initializer>,
      SerialParseFunctions<InitDeclarator::kTag, Declarator, RequiresClause>>
      parallel_funcs_0(
          ParseFunctionInputs<InitDeclarator::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<InitDeclarator::kTag>(false),
                               Declarator(false), Initializer(true)),
          SerialParseFunctions(ParseFunctionInputs<InitDeclarator::kTag>(false),
                               Declarator(false), RequiresClause(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// declarator:
// 	ptr_declarator
// 	noptr_declarator, parameters_and_qualifiers, trailing_return_type
inline ParseFunctionOutputs<Declarator::kTag> Declarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      Declarator::kTag, 2,
      SerialParseFunctions<Declarator::kTag, PtrDeclarator>,
      SerialParseFunctions<Declarator::kTag, NoptrDeclarator,
                           ParametersAndQualifiers, TrailingReturnType>>
      parallel_funcs_0(
          ParseFunctionInputs<Declarator::kTag>(false, output.last_token_,
                                                output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<Declarator::kTag>(false),
                               PtrDeclarator(false)),
          SerialParseFunctions(ParseFunctionInputs<Declarator::kTag>(false),
                               NoptrDeclarator(false),
                               ParametersAndQualifiers(false),
                               TrailingReturnType(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// ptr-declarator:
// 	noptr_declarator
// 	ptr_operator, ptr_declarator
inline ParseFunctionOutputs<PtrDeclarator::kTag> PtrDeclarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      PtrDeclarator::kTag, 2,
      SerialParseFunctions<PtrDeclarator::kTag, NoptrDeclarator>,
      SerialParseFunctions<PtrDeclarator::kTag, PtrOperator, PtrDeclarator>>
      parallel_funcs_0(
          ParseFunctionInputs<PtrDeclarator::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<PtrDeclarator::kTag>(false),
                               NoptrDeclarator(false)),
          SerialParseFunctions(ParseFunctionInputs<PtrDeclarator::kTag>(false),
                               PtrOperator(false), PtrDeclarator(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// noptr-declarator:
// 	declarator_id, attribute_specifier_seq
// 	noptr_declarator, parameters_and_qualifiers
// 	noptr_declarator, `[`, constant_expression[opt], `]`, attribute_specifier_seq[opt]
// 	`(`, ptr_declarator, `)`
inline ParseFunctionOutputs<NoptrDeclarator::kTag>
NoptrDeclarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        NoptrDeclarator::kTag, 2,
        SerialParseFunctions<NoptrDeclarator::kTag, DeclaratorId,
                             AttributeSpecifierSeq>,
        SerialParseFunctions<
            NoptrDeclarator::kTag, ParseFunction<NoptrDeclarator::kTag>,
            PtrDeclarator, ParseFunction<NoptrDeclarator::kTag>>>
        parallel_funcs_0(
            ParseFunctionInputs<NoptrDeclarator::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<NoptrDeclarator::kTag>(false),
                DeclaratorId(false), AttributeSpecifierSeq(false)),
            SerialParseFunctions(
                ParseFunctionInputs<NoptrDeclarator::kTag>(false),
                ParseFunction<NoptrDeclarator::kTag>::create_single_token_check(
                    false, token::tok::TokenKind::l_paren,
                    diag::DiagKind::noptr_declarator_expect_l_paren),
                PtrDeclarator(false),
                ParseFunction<NoptrDeclarator::kTag>::create_single_token_check(
                    false, token::tok::TokenKind::r_paren,
                    diag::DiagKind::noptr_declarator_expect_r_paren)));

    static_assert(base::kNumberOfElements >= 2);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      NoptrDeclarator::kTag, 2,
      SerialParseFunctions<NoptrDeclarator::kTag, NoptrDeclarator,
                           ParametersAndQualifiers>,
      SerialParseFunctions<
          NoptrDeclarator::kTag, NoptrDeclarator,
          ParseFunction<NoptrDeclarator::kTag>, ConstantExpression,
          ParseFunction<NoptrDeclarator::kTag>, AttributeSpecifierSeq>>
      parallel_funcs_1(
          ParseFunctionInputs<NoptrDeclarator::kTag>(false, output.last_token_,
                                                     output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<NoptrDeclarator::kTag>(false),
              NoptrDeclarator(false), ParametersAndQualifiers(false)),
          SerialParseFunctions(
              ParseFunctionInputs<NoptrDeclarator::kTag>(false),
              NoptrDeclarator(false),
              ParseFunction<NoptrDeclarator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_square,
                  diag::DiagKind::noptr_declarator_expect_l_square),
              ConstantExpression(true),
              ParseFunction<NoptrDeclarator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_square,
                  diag::DiagKind::noptr_declarator_expect_r_square),
              AttributeSpecifierSeq(true)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// parameters-and-qualifiers:
// 	`(`, parameter_declaration_clause, `)`, cv_qualifier_seq[opt]
// 	ref_qualifier[opt], noexcept_specifier[opt], attribute_specifier_seq[opt]
inline ParseFunctionOutputs<ParametersAndQualifiers::kTag>
ParametersAndQualifiers::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ParametersAndQualifiers::kTag, 2,
      SerialParseFunctions<ParametersAndQualifiers::kTag,
                           ParseFunction<ParametersAndQualifiers::kTag>,
                           ParameterDeclarationClause,
                           ParseFunction<ParametersAndQualifiers::kTag>,
                           CvQualifierSeq>,
      SerialParseFunctions<ParametersAndQualifiers::kTag, RefQualifier,
                           NoexceptSpecifier, AttributeSpecifierSeq>>
      parallel_funcs_0(
          ParseFunctionInputs<ParametersAndQualifiers::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ParametersAndQualifiers::kTag>(false),
              ParseFunction<ParametersAndQualifiers::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::parameters_and_qualifiers_expect_l_paren),
              ParameterDeclarationClause(false),
              ParseFunction<ParametersAndQualifiers::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::parameters_and_qualifiers_expect_r_paren),
              CvQualifierSeq(true)),
          SerialParseFunctions(
              ParseFunctionInputs<ParametersAndQualifiers::kTag>(false),
              RefQualifier(true), NoexceptSpecifier(true),
              AttributeSpecifierSeq(true)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// trailing-return-type:
// 	`_>`, type_id
inline ParseFunctionOutputs<TrailingReturnType::kTag>
TrailingReturnType::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<TrailingReturnType::kTag>(false, output.last_token_,
                                                    output.cur_token_),
      ParseFunction<TrailingReturnType::kTag>::create_single_token_check(
          false, token::tok::TokenKind::arrow,
          diag::DiagKind::trailing_return_type_expect_arrow),
      TypeId(false));
  return serial_funcs();
}

// ptr-operator:
// 	`*`, attribute_specifier_seq[opt], cv_qualifier_seq[opt]
// 	`&`, attribute_specifier_seq[opt]
// 	`&&`, attribute_specifier_seq[opt]
// 	nested_name_specifier, `*`, attribute_specifier_seq[opt], cv_qualifier_seq[opt]
inline ParseFunctionOutputs<PtrOperator::kTag> PtrOperator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      PtrOperator::kTag, 4,
      SerialParseFunctions<PtrOperator::kTag, ParseFunction<PtrOperator::kTag>,
                           AttributeSpecifierSeq, CvQualifierSeq>,
      SerialParseFunctions<PtrOperator::kTag, ParseFunction<PtrOperator::kTag>,
                           AttributeSpecifierSeq>,
      SerialParseFunctions<PtrOperator::kTag, ParseFunction<PtrOperator::kTag>,
                           AttributeSpecifierSeq>,
      SerialParseFunctions<PtrOperator::kTag, NestedNameSpecifier,
                           ParseFunction<PtrOperator::kTag>,
                           AttributeSpecifierSeq, CvQualifierSeq>>
      parallel_funcs_0(
          ParseFunctionInputs<PtrOperator::kTag>(false, output.last_token_,
                                                 output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<PtrOperator::kTag>(false),
              ParseFunction<PtrOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::star,
                  diag::DiagKind::ptr_operator_expect_star),
              AttributeSpecifierSeq(true), CvQualifierSeq(true)),
          SerialParseFunctions(
              ParseFunctionInputs<PtrOperator::kTag>(false),
              ParseFunction<PtrOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::amp,
                  diag::DiagKind::ptr_operator_expect_amp),
              AttributeSpecifierSeq(true)),
          SerialParseFunctions(
              ParseFunctionInputs<PtrOperator::kTag>(false),
              ParseFunction<PtrOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::ampamp,
                  diag::DiagKind::ptr_operator_expect_ampamp),
              AttributeSpecifierSeq(true)),
          SerialParseFunctions(
              ParseFunctionInputs<PtrOperator::kTag>(false),
              NestedNameSpecifier(false),
              ParseFunction<PtrOperator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::star,
                  diag::DiagKind::ptr_operator_expect_star),
              AttributeSpecifierSeq(true), CvQualifierSeq(true)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// cv-qualifier-seq:
// 	cv_qualifier, cv_qualifier_seq[opt]
inline ParseFunctionOutputs<CvQualifierSeq::kTag> CvQualifierSeq::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<CvQualifierSeq::kTag>(false, output.last_token_,
                                                output.cur_token_),
      CvQualifier(false), CvQualifierSeq(true));
  return serial_funcs();
}

// cv-qualifier:
// 	`const`
// 	`volatile`
inline ParseFunctionOutputs<CvQualifier::kTag> CvQualifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      CvQualifier::kTag, 2,
      SerialParseFunctions<CvQualifier::kTag, ParseFunction<CvQualifier::kTag>>,
      SerialParseFunctions<CvQualifier::kTag, ParseFunction<CvQualifier::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<CvQualifier::kTag>(false, output.last_token_,
                                                 output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<CvQualifier::kTag>(false),
              ParseFunction<CvQualifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_const,
                  diag::DiagKind::cv_qualifier_expect_kw_const)),
          SerialParseFunctions(
              ParseFunctionInputs<CvQualifier::kTag>(false),
              ParseFunction<CvQualifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_volatile,
                  diag::DiagKind::cv_qualifier_expect_kw_volatile)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// ref-qualifier:
// 	`&`
// 	`&&`
inline ParseFunctionOutputs<RefQualifier::kTag> RefQualifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      RefQualifier::kTag, 2,
      SerialParseFunctions<RefQualifier::kTag,
                           ParseFunction<RefQualifier::kTag>>,
      SerialParseFunctions<RefQualifier::kTag,
                           ParseFunction<RefQualifier::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<RefQualifier::kTag>(false, output.last_token_,
                                                  output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<RefQualifier::kTag>(false),
              ParseFunction<RefQualifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::amp,
                  diag::DiagKind::ref_qualifier_expect_amp)),
          SerialParseFunctions(
              ParseFunctionInputs<RefQualifier::kTag>(false),
              ParseFunction<RefQualifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::ampamp,
                  diag::DiagKind::ref_qualifier_expect_ampamp)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// declarator-id:
// 	`...`[opt], id_expression
inline ParseFunctionOutputs<DeclaratorId::kTag> DeclaratorId::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<DeclaratorId::kTag>(false, output.last_token_,
                                              output.cur_token_),
      ParseFunction<DeclaratorId::kTag>::create_single_token_check(
          true, token::tok::TokenKind::ellipsis,
          diag::DiagKind::declarator_id_expect_ellipsis),
      IdExpression(false));
  return serial_funcs();
}

// type-id:
// 	type_specifier_seq, abstract_declarator[opt]
inline ParseFunctionOutputs<TypeId::kTag> TypeId::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<TypeId::kTag>(false, output.last_token_,
                                        output.cur_token_),
      TypeSpecifierSeq(false), AbstractDeclarator(true));
  return serial_funcs();
}

// defining-type-id:
// 	defining_type_specifier_seq, abstract_declarator[opt]
inline ParseFunctionOutputs<DefiningTypeId::kTag> DefiningTypeId::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<DefiningTypeId::kTag>(false, output.last_token_,
                                                output.cur_token_),
      DefiningTypeSpecifierSeq(false), AbstractDeclarator(true));
  return serial_funcs();
}

// abstract-declarator:
// 	ptr_abstract_declarator
// 	noptr_abstract_declarator, parameters_and_qualifiers, trailing_return_type
// 	abstract_pack_declarator
inline ParseFunctionOutputs<AbstractDeclarator::kTag>
AbstractDeclarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      AbstractDeclarator::kTag, 3,
      SerialParseFunctions<AbstractDeclarator::kTag, PtrAbstractDeclarator>,
      SerialParseFunctions<AbstractDeclarator::kTag, NoptrAbstractDeclarator,
                           ParametersAndQualifiers, TrailingReturnType>,
      SerialParseFunctions<AbstractDeclarator::kTag, AbstractPackDeclarator>>
      parallel_funcs_0(
          ParseFunctionInputs<AbstractDeclarator::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<AbstractDeclarator::kTag>(false),
              PtrAbstractDeclarator(false)),
          SerialParseFunctions(
              ParseFunctionInputs<AbstractDeclarator::kTag>(false),
              NoptrAbstractDeclarator(false), ParametersAndQualifiers(false),
              TrailingReturnType(false)),
          SerialParseFunctions(
              ParseFunctionInputs<AbstractDeclarator::kTag>(false),
              AbstractPackDeclarator(false)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// ptr-abstract-declarator:
// 	noptr_abstract_declarator
// 	ptr_operator, ptr_abstract_declarator
inline ParseFunctionOutputs<PtrAbstractDeclarator::kTag>
PtrAbstractDeclarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      PtrAbstractDeclarator::kTag, 2,
      SerialParseFunctions<PtrAbstractDeclarator::kTag,
                           NoptrAbstractDeclarator>,
      SerialParseFunctions<PtrAbstractDeclarator::kTag, PtrOperator,
                           PtrAbstractDeclarator>>
      parallel_funcs_0(
          ParseFunctionInputs<PtrAbstractDeclarator::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<PtrAbstractDeclarator::kTag>(false),
              NoptrAbstractDeclarator(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PtrAbstractDeclarator::kTag>(false),
              PtrOperator(false), PtrAbstractDeclarator(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// noptr-abstract-declarator:
// 	noptr_abstract_declarator[opt], parameters_and_qualifiers
// 	noptr_abstract_declarator[opt], `[`, constant_expression[opt], `]`, attribute_specifier_seq[opt]
// 	`(`, ptr_abstract_declarator, `)`
inline ParseFunctionOutputs<NoptrAbstractDeclarator::kTag>
NoptrAbstractDeclarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        NoptrAbstractDeclarator::kTag, 1,
        SerialParseFunctions<NoptrAbstractDeclarator::kTag,
                             ParseFunction<NoptrAbstractDeclarator::kTag>,
                             PtrAbstractDeclarator,
                             ParseFunction<NoptrAbstractDeclarator::kTag>>>
        parallel_funcs_0(
            ParseFunctionInputs<NoptrAbstractDeclarator::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<NoptrAbstractDeclarator::kTag>(false),
                ParseFunction<NoptrAbstractDeclarator::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::l_paren,
                        diag::DiagKind::
                            noptr_abstract_declarator_expect_l_paren),
                PtrAbstractDeclarator(false),
                ParseFunction<NoptrAbstractDeclarator::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::r_paren,
                        diag::DiagKind::
                            noptr_abstract_declarator_expect_r_paren)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      NoptrAbstractDeclarator::kTag, 2,
      SerialParseFunctions<NoptrAbstractDeclarator::kTag,
                           NoptrAbstractDeclarator, ParametersAndQualifiers>,
      SerialParseFunctions<
          NoptrAbstractDeclarator::kTag, NoptrAbstractDeclarator,
          ParseFunction<NoptrAbstractDeclarator::kTag>, ConstantExpression,
          ParseFunction<NoptrAbstractDeclarator::kTag>, AttributeSpecifierSeq>>
      parallel_funcs_1(
          ParseFunctionInputs<NoptrAbstractDeclarator::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<NoptrAbstractDeclarator::kTag>(false),
              NoptrAbstractDeclarator(true), ParametersAndQualifiers(false)),
          SerialParseFunctions(
              ParseFunctionInputs<NoptrAbstractDeclarator::kTag>(false),
              NoptrAbstractDeclarator(true),
              ParseFunction<NoptrAbstractDeclarator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_square,
                      diag::DiagKind::
                          noptr_abstract_declarator_expect_l_square),
              ConstantExpression(true),
              ParseFunction<NoptrAbstractDeclarator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_square,
                      diag::DiagKind::
                          noptr_abstract_declarator_expect_r_square),
              AttributeSpecifierSeq(true)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// abstract-pack-declarator:
// 	noptr_abstract_pack_declarator
// 	ptr_operator, abstract_pack_declarator
inline ParseFunctionOutputs<AbstractPackDeclarator::kTag>
AbstractPackDeclarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      AbstractPackDeclarator::kTag, 2,
      SerialParseFunctions<AbstractPackDeclarator::kTag,
                           NoptrAbstractPackDeclarator>,
      SerialParseFunctions<AbstractPackDeclarator::kTag, PtrOperator,
                           AbstractPackDeclarator>>
      parallel_funcs_0(
          ParseFunctionInputs<AbstractPackDeclarator::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<AbstractPackDeclarator::kTag>(false),
              NoptrAbstractPackDeclarator(false)),
          SerialParseFunctions(
              ParseFunctionInputs<AbstractPackDeclarator::kTag>(false),
              PtrOperator(false), AbstractPackDeclarator(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// noptr-abstract-pack-declarator:
// 	noptr_abstract_pack_declarator, parameters_and_qualifiers
// 	noptr_abstract_pack_declarator, `[`, constant_expression[opt], `]`, attribute_specifier_seq[opt]
// 	`...`
inline ParseFunctionOutputs<NoptrAbstractPackDeclarator::kTag>
NoptrAbstractPackDeclarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        NoptrAbstractPackDeclarator::kTag, 1,
        SerialParseFunctions<NoptrAbstractPackDeclarator::kTag,
                             ParseFunction<NoptrAbstractPackDeclarator::kTag>>>
        parallel_funcs_0(
            ParseFunctionInputs<NoptrAbstractPackDeclarator::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<NoptrAbstractPackDeclarator::kTag>(false),
                ParseFunction<NoptrAbstractPackDeclarator::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::ellipsis,
                        diag::DiagKind::
                            noptr_abstract_pack_declarator_expect_ellipsis)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      NoptrAbstractPackDeclarator::kTag, 2,
      SerialParseFunctions<NoptrAbstractPackDeclarator::kTag,
                           NoptrAbstractPackDeclarator,
                           ParametersAndQualifiers>,
      SerialParseFunctions<
          NoptrAbstractPackDeclarator::kTag, NoptrAbstractPackDeclarator,
          ParseFunction<NoptrAbstractPackDeclarator::kTag>, ConstantExpression,
          ParseFunction<NoptrAbstractPackDeclarator::kTag>,
          AttributeSpecifierSeq>>
      parallel_funcs_1(
          ParseFunctionInputs<NoptrAbstractPackDeclarator::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<NoptrAbstractPackDeclarator::kTag>(false),
              NoptrAbstractPackDeclarator(false),
              ParametersAndQualifiers(false)),
          SerialParseFunctions(
              ParseFunctionInputs<NoptrAbstractPackDeclarator::kTag>(false),
              NoptrAbstractPackDeclarator(false),
              ParseFunction<NoptrAbstractPackDeclarator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_square,
                      diag::DiagKind::
                          noptr_abstract_pack_declarator_expect_l_square),
              ConstantExpression(true),
              ParseFunction<NoptrAbstractPackDeclarator::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_square,
                      diag::DiagKind::
                          noptr_abstract_pack_declarator_expect_r_square),
              AttributeSpecifierSeq(true)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// parameter-declaration-clause:
// 	parameter_declaration_list[opt], `...`[opt]
// 	parameter_declaration_list, `,`, `...`
inline ParseFunctionOutputs<ParameterDeclarationClause::kTag>
ParameterDeclarationClause::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ParameterDeclarationClause::kTag, 2,
      SerialParseFunctions<ParameterDeclarationClause::kTag,
                           ParameterDeclarationList,
                           ParseFunction<ParameterDeclarationClause::kTag>>,
      SerialParseFunctions<ParameterDeclarationClause::kTag,
                           ParameterDeclarationList,
                           ParseFunction<ParameterDeclarationClause::kTag>,
                           ParseFunction<ParameterDeclarationClause::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<ParameterDeclarationClause::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ParameterDeclarationClause::kTag>(false),
              ParameterDeclarationList(true),
              ParseFunction<ParameterDeclarationClause::kTag>::
                  create_single_token_check(
                      true, token::tok::TokenKind::ellipsis,
                      diag::DiagKind::
                          parameter_declaration_clause_expect_ellipsis)),
          SerialParseFunctions(
              ParseFunctionInputs<ParameterDeclarationClause::kTag>(false),
              ParameterDeclarationList(false),
              ParseFunction<ParameterDeclarationClause::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::comma,
                      diag::DiagKind::
                          parameter_declaration_clause_expect_comma),
              ParseFunction<ParameterDeclarationClause::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::ellipsis,
                      diag::DiagKind::
                          parameter_declaration_clause_expect_ellipsis)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// parameter-declaration-list:
// 	parameter_declaration
// 	parameter_declaration_list, `,`, parameter_declaration
inline ParseFunctionOutputs<ParameterDeclarationList::kTag>
ParameterDeclarationList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<ParameterDeclarationList::kTag, 1,
                           SerialParseFunctions<ParameterDeclarationList::kTag,
                                                ParameterDeclaration>>
        parallel_funcs_0(
            ParseFunctionInputs<ParameterDeclarationList::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<ParameterDeclarationList::kTag>(false),
                ParameterDeclaration(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      ParameterDeclarationList::kTag, 1,
      SerialParseFunctions<
          ParameterDeclarationList::kTag, ParameterDeclarationList,
          ParseFunction<ParameterDeclarationList::kTag>, ParameterDeclaration>>
      parallel_funcs_1(
          ParseFunctionInputs<ParameterDeclarationList::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ParameterDeclarationList::kTag>(false),
              ParameterDeclarationList(false),
              ParseFunction<ParameterDeclarationList::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::comma,
                      diag::DiagKind::parameter_declaration_list_expect_comma),
              ParameterDeclaration(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// parameter-declaration:
// 	attribute_specifier_seq[opt], decl_specifier_seq, declarator
// 	attribute_specifier_seq[opt], decl_specifier_seq, declarator, `=`, initializer_clause
// 	attribute_specifier_seq[opt], decl_specifier_seq, abstract_declarator[opt]
// 	attribute_specifier_seq[opt], decl_specifier_seq, abstract_declarator[opt], `=`, initializer_clause
inline ParseFunctionOutputs<ParameterDeclaration::kTag>
ParameterDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ParameterDeclaration::kTag, 4,
      SerialParseFunctions<ParameterDeclaration::kTag, AttributeSpecifierSeq,
                           DeclSpecifierSeq, Declarator>,
      SerialParseFunctions<ParameterDeclaration::kTag, AttributeSpecifierSeq,
                           DeclSpecifierSeq, Declarator,
                           ParseFunction<ParameterDeclaration::kTag>,
                           InitializerClause>,
      SerialParseFunctions<ParameterDeclaration::kTag, AttributeSpecifierSeq,
                           DeclSpecifierSeq, AbstractDeclarator>,
      SerialParseFunctions<ParameterDeclaration::kTag, AttributeSpecifierSeq,
                           DeclSpecifierSeq, AbstractDeclarator,
                           ParseFunction<ParameterDeclaration::kTag>,
                           InitializerClause>>
      parallel_funcs_0(
          ParseFunctionInputs<ParameterDeclaration::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ParameterDeclaration::kTag>(false),
              AttributeSpecifierSeq(true), DeclSpecifierSeq(false),
              Declarator(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ParameterDeclaration::kTag>(false),
              AttributeSpecifierSeq(true), DeclSpecifierSeq(false),
              Declarator(false),
              ParseFunction<ParameterDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::equal,
                      diag::DiagKind::parameter_declaration_expect_equal),
              InitializerClause(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ParameterDeclaration::kTag>(false),
              AttributeSpecifierSeq(true), DeclSpecifierSeq(false),
              AbstractDeclarator(true)),
          SerialParseFunctions(
              ParseFunctionInputs<ParameterDeclaration::kTag>(false),
              AttributeSpecifierSeq(true), DeclSpecifierSeq(false),
              AbstractDeclarator(true),
              ParseFunction<ParameterDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::equal,
                      diag::DiagKind::parameter_declaration_expect_equal),
              InitializerClause(false)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// initializer:
// 	brace_or_equal_initializer
// 	`(`, expression_list, `)`
inline ParseFunctionOutputs<Initializer::kTag> Initializer::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      Initializer::kTag, 2,
      SerialParseFunctions<Initializer::kTag, BraceOrEqualInitializer>,
      SerialParseFunctions<Initializer::kTag, ParseFunction<Initializer::kTag>,
                           ExpressionList, ParseFunction<Initializer::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<Initializer::kTag>(false, output.last_token_,
                                                 output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<Initializer::kTag>(false),
                               BraceOrEqualInitializer(false)),
          SerialParseFunctions(
              ParseFunctionInputs<Initializer::kTag>(false),
              ParseFunction<Initializer::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::initializer_expect_l_paren),
              ExpressionList(false),
              ParseFunction<Initializer::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::initializer_expect_r_paren)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// brace-or-equal-initializer:
// 	`=`, initializer_clause
// 	braced_init_list
inline ParseFunctionOutputs<BraceOrEqualInitializer::kTag>
BraceOrEqualInitializer::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      BraceOrEqualInitializer::kTag, 2,
      SerialParseFunctions<BraceOrEqualInitializer::kTag,
                           ParseFunction<BraceOrEqualInitializer::kTag>,
                           InitializerClause>,
      SerialParseFunctions<BraceOrEqualInitializer::kTag, BracedInitList>>
      parallel_funcs_0(
          ParseFunctionInputs<BraceOrEqualInitializer::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<BraceOrEqualInitializer::kTag>(false),
              ParseFunction<BraceOrEqualInitializer::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::equal,
                      diag::DiagKind::brace_or_equal_initializer_expect_equal),
              InitializerClause(false)),
          SerialParseFunctions(
              ParseFunctionInputs<BraceOrEqualInitializer::kTag>(false),
              BracedInitList(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// initializer-clause:
// 	assignment_expression
// 	braced_init_list
inline ParseFunctionOutputs<InitializerClause::kTag>
InitializerClause::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      InitializerClause::kTag, 2,
      SerialParseFunctions<InitializerClause::kTag, AssignmentExpression>,
      SerialParseFunctions<InitializerClause::kTag, BracedInitList>>
      parallel_funcs_0(ParseFunctionInputs<InitializerClause::kTag>(
                           false, output.last_token_, output.cur_token_),
                       SerialParseFunctions(
                           ParseFunctionInputs<InitializerClause::kTag>(false),
                           AssignmentExpression(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<InitializerClause::kTag>(false),
                           BracedInitList(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// braced-init-list:
// 	`{`, initializer_list, `,`[opt], `}`
// 	`{`, designated_initializer_list, `,`[opt], `}`
// 	`{`, `}`
inline ParseFunctionOutputs<BracedInitList::kTag> BracedInitList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      BracedInitList::kTag, 3,
      SerialParseFunctions<BracedInitList::kTag,
                           ParseFunction<BracedInitList::kTag>, InitializerList,
                           ParseFunction<BracedInitList::kTag>,
                           ParseFunction<BracedInitList::kTag>>,
      SerialParseFunctions<
          BracedInitList::kTag, ParseFunction<BracedInitList::kTag>,
          DesignatedInitializerList, ParseFunction<BracedInitList::kTag>,
          ParseFunction<BracedInitList::kTag>>,
      SerialParseFunctions<BracedInitList::kTag,
                           ParseFunction<BracedInitList::kTag>,
                           ParseFunction<BracedInitList::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<BracedInitList::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<BracedInitList::kTag>(false),
              ParseFunction<BracedInitList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_brace,
                  diag::DiagKind::braced_init_list_expect_l_brace),
              InitializerList(false),
              ParseFunction<BracedInitList::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::comma,
                  diag::DiagKind::braced_init_list_expect_comma),
              ParseFunction<BracedInitList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_brace,
                  diag::DiagKind::braced_init_list_expect_r_brace)),
          SerialParseFunctions(
              ParseFunctionInputs<BracedInitList::kTag>(false),
              ParseFunction<BracedInitList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_brace,
                  diag::DiagKind::braced_init_list_expect_l_brace),
              DesignatedInitializerList(false),
              ParseFunction<BracedInitList::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::comma,
                  diag::DiagKind::braced_init_list_expect_comma),
              ParseFunction<BracedInitList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_brace,
                  diag::DiagKind::braced_init_list_expect_r_brace)),
          SerialParseFunctions(
              ParseFunctionInputs<BracedInitList::kTag>(false),
              ParseFunction<BracedInitList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_brace,
                  diag::DiagKind::braced_init_list_expect_l_brace),
              ParseFunction<BracedInitList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_brace,
                  diag::DiagKind::braced_init_list_expect_r_brace)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// initializer-list:
// 	initializer_clause, `...`[opt]
// 	initializer_list, `,`, initializer_clause, `...`[opt]
inline ParseFunctionOutputs<InitializerList::kTag>
InitializerList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        InitializerList::kTag, 1,
        SerialParseFunctions<InitializerList::kTag, InitializerClause,
                             ParseFunction<InitializerList::kTag>>>
        parallel_funcs_0(
            ParseFunctionInputs<InitializerList::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<InitializerList::kTag>(false),
                InitializerClause(false),
                ParseFunction<InitializerList::kTag>::create_single_token_check(
                    true, token::tok::TokenKind::ellipsis,
                    diag::DiagKind::initializer_list_expect_ellipsis)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      InitializerList::kTag, 1,
      SerialParseFunctions<InitializerList::kTag, InitializerList,
                           ParseFunction<InitializerList::kTag>,
                           InitializerClause,
                           ParseFunction<InitializerList::kTag>>>
      parallel_funcs_1(
          ParseFunctionInputs<InitializerList::kTag>(false, output.last_token_,
                                                     output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<InitializerList::kTag>(false),
              InitializerList(false),
              ParseFunction<InitializerList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::comma,
                  diag::DiagKind::initializer_list_expect_comma),
              InitializerClause(false),
              ParseFunction<InitializerList::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::initializer_list_expect_ellipsis)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// designated-initializer-list:
// 	designated_initializer_clause
// 	designated_initializer_list, `,`, designated_initializer_clause
inline ParseFunctionOutputs<DesignatedInitializerList::kTag>
DesignatedInitializerList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<DesignatedInitializerList::kTag, 1,
                           SerialParseFunctions<DesignatedInitializerList::kTag,
                                                DesignatedInitializerClause>>
        parallel_funcs_0(
            ParseFunctionInputs<DesignatedInitializerList::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<DesignatedInitializerList::kTag>(false),
                DesignatedInitializerClause(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      DesignatedInitializerList::kTag, 1,
      SerialParseFunctions<DesignatedInitializerList::kTag,
                           DesignatedInitializerList,
                           ParseFunction<DesignatedInitializerList::kTag>,
                           DesignatedInitializerClause>>
      parallel_funcs_1(
          ParseFunctionInputs<DesignatedInitializerList::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<DesignatedInitializerList::kTag>(false),
              DesignatedInitializerList(false),
              ParseFunction<DesignatedInitializerList::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::comma,
                      diag::DiagKind::designated_initializer_list_expect_comma),
              DesignatedInitializerClause(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// designated-initializer-clause:
// 	designator, brace_or_equal_initializer
inline ParseFunctionOutputs<DesignatedInitializerClause::kTag>
DesignatedInitializerClause::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<DesignatedInitializerClause::kTag>(
          false, output.last_token_, output.cur_token_),
      Designator(false), BraceOrEqualInitializer(false));
  return serial_funcs();
}

// designator:
// 	`.`, `identifier`
inline ParseFunctionOutputs<Designator::kTag> Designator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<Designator::kTag>(false, output.last_token_,
                                            output.cur_token_),
      ParseFunction<Designator::kTag>::create_single_token_check(
          false, token::tok::TokenKind::period,
          diag::DiagKind::designator_expect_period),
      ParseFunction<Designator::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::designator_expect_identifier));
  return serial_funcs();
}

// expr-or-braced-init-list:
// 	expression
// 	braced_init_list
inline ParseFunctionOutputs<ExprOrBracedInitList::kTag>
ExprOrBracedInitList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ExprOrBracedInitList::kTag, 2,
      SerialParseFunctions<ExprOrBracedInitList::kTag, Expression>,
      SerialParseFunctions<ExprOrBracedInitList::kTag, BracedInitList>>
      parallel_funcs_0(
          ParseFunctionInputs<ExprOrBracedInitList::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ExprOrBracedInitList::kTag>(false),
              Expression(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ExprOrBracedInitList::kTag>(false),
              BracedInitList(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// function-definition:
// 	attribute_specifier_seq[opt], decl_specifier_seq[opt], declarator, virt_specifier_seq[opt], function_body
// 	attribute_specifier_seq[opt], decl_specifier_seq[opt], declarator, requires_clause, function_body
inline ParseFunctionOutputs<FunctionDefinition::kTag>
FunctionDefinition::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      FunctionDefinition::kTag, 2,
      SerialParseFunctions<FunctionDefinition::kTag, AttributeSpecifierSeq,
                           DeclSpecifierSeq, Declarator, VirtSpecifierSeq,
                           FunctionBody>,
      SerialParseFunctions<FunctionDefinition::kTag, AttributeSpecifierSeq,
                           DeclSpecifierSeq, Declarator, RequiresClause,
                           FunctionBody>>
      parallel_funcs_0(
          ParseFunctionInputs<FunctionDefinition::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<FunctionDefinition::kTag>(false),
              AttributeSpecifierSeq(true), DeclSpecifierSeq(true),
              Declarator(false), VirtSpecifierSeq(true), FunctionBody(false)),
          SerialParseFunctions(
              ParseFunctionInputs<FunctionDefinition::kTag>(false),
              AttributeSpecifierSeq(true), DeclSpecifierSeq(true),
              Declarator(false), RequiresClause(false), FunctionBody(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// function-body:
// 	ctor_initializer[opt], compound_statement
// 	function_try_block
// 	`=`, `default`, `;`
// 	`=`, `delete`, `;`
inline ParseFunctionOutputs<FunctionBody::kTag> FunctionBody::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      FunctionBody::kTag, 4,
      SerialParseFunctions<FunctionBody::kTag, CtorInitializer,
                           CompoundStatement>,
      SerialParseFunctions<FunctionBody::kTag, FunctionTryBlock>,
      SerialParseFunctions<
          FunctionBody::kTag, ParseFunction<FunctionBody::kTag>,
          ParseFunction<FunctionBody::kTag>, ParseFunction<FunctionBody::kTag>>,
      SerialParseFunctions<
          FunctionBody::kTag, ParseFunction<FunctionBody::kTag>,
          ParseFunction<FunctionBody::kTag>, ParseFunction<FunctionBody::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<FunctionBody::kTag>(false, output.last_token_,
                                                  output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<FunctionBody::kTag>(false),
                               CtorInitializer(true), CompoundStatement(false)),
          SerialParseFunctions(ParseFunctionInputs<FunctionBody::kTag>(false),
                               FunctionTryBlock(false)),
          SerialParseFunctions(
              ParseFunctionInputs<FunctionBody::kTag>(false),
              ParseFunction<FunctionBody::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::equal,
                  diag::DiagKind::function_body_expect_equal),
              ParseFunction<FunctionBody::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_default,
                  diag::DiagKind::function_body_expect_kw_default),
              ParseFunction<FunctionBody::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::semi,
                  diag::DiagKind::function_body_expect_semi)),
          SerialParseFunctions(
              ParseFunctionInputs<FunctionBody::kTag>(false),
              ParseFunction<FunctionBody::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::equal,
                  diag::DiagKind::function_body_expect_equal),
              ParseFunction<FunctionBody::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_delete,
                  diag::DiagKind::function_body_expect_kw_delete),
              ParseFunction<FunctionBody::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::semi,
                  diag::DiagKind::function_body_expect_semi)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// enum-name:
// 	`identifier`
inline ParseFunctionOutputs<EnumName::kTag> EnumName::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<EnumName::kTag>(false, output.last_token_,
                                          output.cur_token_),
      ParseFunction<EnumName::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::enum_name_expect_identifier));
  return serial_funcs();
}

// enum-specifier:
// 	enum_head, `{`, enumerator_list[opt], `}`
// 	enum_head, `{`, enumerator_list, `,`, `}`
inline ParseFunctionOutputs<EnumSpecifier::kTag> EnumSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      EnumSpecifier::kTag, 2,
      SerialParseFunctions<EnumSpecifier::kTag, EnumHead,
                           ParseFunction<EnumSpecifier::kTag>, EnumeratorList,
                           ParseFunction<EnumSpecifier::kTag>>,
      SerialParseFunctions<EnumSpecifier::kTag, EnumHead,
                           ParseFunction<EnumSpecifier::kTag>, EnumeratorList,
                           ParseFunction<EnumSpecifier::kTag>,
                           ParseFunction<EnumSpecifier::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<EnumSpecifier::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<EnumSpecifier::kTag>(false), EnumHead(false),
              ParseFunction<EnumSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_brace,
                  diag::DiagKind::enum_specifier_expect_l_brace),
              EnumeratorList(true),
              ParseFunction<EnumSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_brace,
                  diag::DiagKind::enum_specifier_expect_r_brace)),
          SerialParseFunctions(
              ParseFunctionInputs<EnumSpecifier::kTag>(false), EnumHead(false),
              ParseFunction<EnumSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_brace,
                  diag::DiagKind::enum_specifier_expect_l_brace),
              EnumeratorList(false),
              ParseFunction<EnumSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::comma,
                  diag::DiagKind::enum_specifier_expect_comma),
              ParseFunction<EnumSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_brace,
                  diag::DiagKind::enum_specifier_expect_r_brace)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// enum-head:
// 	enum_key, attribute_specifier_seq[opt], enum_head_name[opt], enum_base[opt]
inline ParseFunctionOutputs<EnumHead::kTag> EnumHead::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<EnumHead::kTag>(false, output.last_token_,
                                          output.cur_token_),
      EnumKey(false), AttributeSpecifierSeq(true), EnumHeadName(true),
      EnumBase(true));
  return serial_funcs();
}

// enum-head-name:
// 	nested_name_specifier[opt], `identifier`
inline ParseFunctionOutputs<EnumHeadName::kTag> EnumHeadName::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<EnumHeadName::kTag>(false, output.last_token_,
                                              output.cur_token_),
      NestedNameSpecifier(true),
      ParseFunction<EnumHeadName::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::enum_head_name_expect_identifier));
  return serial_funcs();
}

// opaque-enum-declaration:
// 	enum_key, attribute_specifier_seq[opt], enum_head_name, enum_base[opt], `;`
inline ParseFunctionOutputs<OpaqueEnumDeclaration::kTag>
OpaqueEnumDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<OpaqueEnumDeclaration::kTag>(
          false, output.last_token_, output.cur_token_),
      EnumKey(false), AttributeSpecifierSeq(true), EnumHeadName(false),
      EnumBase(true),
      ParseFunction<OpaqueEnumDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::opaque_enum_declaration_expect_semi));
  return serial_funcs();
}

// enum-key:
// 	`enum`
// 	`enum`, `class`
// 	`enum`, `struct`
inline ParseFunctionOutputs<EnumKey::kTag> EnumKey::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      EnumKey::kTag, 3,
      SerialParseFunctions<EnumKey::kTag, ParseFunction<EnumKey::kTag>>,
      SerialParseFunctions<EnumKey::kTag, ParseFunction<EnumKey::kTag>,
                           ParseFunction<EnumKey::kTag>>,
      SerialParseFunctions<EnumKey::kTag, ParseFunction<EnumKey::kTag>,
                           ParseFunction<EnumKey::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<EnumKey::kTag>(false, output.last_token_,
                                             output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<EnumKey::kTag>(false),
              ParseFunction<EnumKey::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_enum,
                  diag::DiagKind::enum_key_expect_kw_enum)),
          SerialParseFunctions(
              ParseFunctionInputs<EnumKey::kTag>(false),
              ParseFunction<EnumKey::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_enum,
                  diag::DiagKind::enum_key_expect_kw_enum),
              ParseFunction<EnumKey::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_class,
                  diag::DiagKind::enum_key_expect_kw_class)),
          SerialParseFunctions(
              ParseFunctionInputs<EnumKey::kTag>(false),
              ParseFunction<EnumKey::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_enum,
                  diag::DiagKind::enum_key_expect_kw_enum),
              ParseFunction<EnumKey::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_struct,
                  diag::DiagKind::enum_key_expect_kw_struct)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// enum-base:
// 	`:`, type_specifier_seq
inline ParseFunctionOutputs<EnumBase::kTag> EnumBase::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      EnumBase::kTag, 1,
      SerialParseFunctions<EnumBase::kTag, ParseFunction<EnumBase::kTag>,
                           TypeSpecifierSeq>>
      parallel_funcs_0(
          ParseFunctionInputs<EnumBase::kTag>(false, output.last_token_,
                                              output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<EnumBase::kTag>(false),
              ParseFunction<EnumBase::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::colon,
                  diag::DiagKind::enum_base_expect_colon),
              TypeSpecifierSeq(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// enumerator-list:
// 	enumerator_definition
// 	enumerator_list, `,`, enumerator_definition
inline ParseFunctionOutputs<EnumeratorList::kTag> EnumeratorList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        EnumeratorList::kTag, 1,
        SerialParseFunctions<EnumeratorList::kTag, EnumeratorDefinition>>
        parallel_funcs_0(ParseFunctionInputs<EnumeratorList::kTag>(
                             false, output.last_token_, output.cur_token_),
                         SerialParseFunctions(
                             ParseFunctionInputs<EnumeratorList::kTag>(false),
                             EnumeratorDefinition(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      EnumeratorList::kTag, 1,
      SerialParseFunctions<EnumeratorList::kTag, EnumeratorList,
                           ParseFunction<EnumeratorList::kTag>,
                           EnumeratorDefinition>>
      parallel_funcs_1(
          ParseFunctionInputs<EnumeratorList::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<EnumeratorList::kTag>(false),
              EnumeratorList(false),
              ParseFunction<EnumeratorList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::comma,
                  diag::DiagKind::enumerator_list_expect_comma),
              EnumeratorDefinition(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// enumerator-definition:
// 	enumerator
// 	enumerator, `=`, constant_expression
inline ParseFunctionOutputs<EnumeratorDefinition::kTag>
EnumeratorDefinition::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      EnumeratorDefinition::kTag, 2,
      SerialParseFunctions<EnumeratorDefinition::kTag, Enumerator>,
      SerialParseFunctions<EnumeratorDefinition::kTag, Enumerator,
                           ParseFunction<EnumeratorDefinition::kTag>,
                           ConstantExpression>>
      parallel_funcs_0(
          ParseFunctionInputs<EnumeratorDefinition::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<EnumeratorDefinition::kTag>(false),
              Enumerator(false)),
          SerialParseFunctions(
              ParseFunctionInputs<EnumeratorDefinition::kTag>(false),
              Enumerator(false),
              ParseFunction<EnumeratorDefinition::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::equal,
                      diag::DiagKind::enumerator_definition_expect_equal),
              ConstantExpression(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// enumerator:
// 	`identifier`, attribute_specifier_seq[opt]
inline ParseFunctionOutputs<Enumerator::kTag> Enumerator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<Enumerator::kTag>(false, output.last_token_,
                                            output.cur_token_),
      ParseFunction<Enumerator::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::enumerator_expect_identifier),
      AttributeSpecifierSeq(true));
  return serial_funcs();
}

// using-enum-declaration:
// 	`using`, elaborated_enum_specifier, `;`
inline ParseFunctionOutputs<UsingEnumDeclaration::kTag>
UsingEnumDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<UsingEnumDeclaration::kTag>(false, output.last_token_,
                                                      output.cur_token_),
      ParseFunction<UsingEnumDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_using,
          diag::DiagKind::using_enum_declaration_expect_kw_using),
      ElaboratedEnumSpecifier(false),
      ParseFunction<UsingEnumDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::using_enum_declaration_expect_semi));
  return serial_funcs();
}

// namespace-name:
// 	`identifier`
// 	namespace_alias
inline ParseFunctionOutputs<NamespaceName::kTag> NamespaceName::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      NamespaceName::kTag, 2,
      SerialParseFunctions<NamespaceName::kTag,
                           ParseFunction<NamespaceName::kTag>>,
      SerialParseFunctions<NamespaceName::kTag, NamespaceAlias>>
      parallel_funcs_0(
          ParseFunctionInputs<NamespaceName::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<NamespaceName::kTag>(false),
              ParseFunction<NamespaceName::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::namespace_name_expect_identifier)),
          SerialParseFunctions(ParseFunctionInputs<NamespaceName::kTag>(false),
                               NamespaceAlias(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// namespace-definition:
// 	named_namespace_definition
// 	unnamed_namespace_definition
// 	nested_namespace_definition
inline ParseFunctionOutputs<NamespaceDefinition::kTag>
NamespaceDefinition::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      NamespaceDefinition::kTag, 3,
      SerialParseFunctions<NamespaceDefinition::kTag, NamedNamespaceDefinition>,
      SerialParseFunctions<NamespaceDefinition::kTag,
                           UnnamedNamespaceDefinition>,
      SerialParseFunctions<NamespaceDefinition::kTag,
                           NestedNamespaceDefinition>>
      parallel_funcs_0(
          ParseFunctionInputs<NamespaceDefinition::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<NamespaceDefinition::kTag>(false),
              NamedNamespaceDefinition(false)),
          SerialParseFunctions(
              ParseFunctionInputs<NamespaceDefinition::kTag>(false),
              UnnamedNamespaceDefinition(false)),
          SerialParseFunctions(
              ParseFunctionInputs<NamespaceDefinition::kTag>(false),
              NestedNamespaceDefinition(false)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// named-namespace-definition:
// 	`inline`[opt], `namespace`, attribute_specifier_seq[opt], `identifier`, `{`, namespace_body, `}`
inline ParseFunctionOutputs<NamedNamespaceDefinition::kTag>
NamedNamespaceDefinition::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<NamedNamespaceDefinition::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<NamedNamespaceDefinition::kTag>::create_single_token_check(
          true, token::tok::TokenKind::kw_inline,
          diag::DiagKind::named_namespace_definition_expect_kw_inline),
      ParseFunction<NamedNamespaceDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_namespace,
          diag::DiagKind::named_namespace_definition_expect_kw_namespace),
      AttributeSpecifierSeq(true),
      ParseFunction<NamedNamespaceDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::named_namespace_definition_expect_identifier),
      ParseFunction<NamedNamespaceDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_brace,
          diag::DiagKind::named_namespace_definition_expect_l_brace),
      NamespaceBody(false),
      ParseFunction<NamedNamespaceDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_brace,
          diag::DiagKind::named_namespace_definition_expect_r_brace));
  return serial_funcs();
}

// unnamed-namespace-definition:
// 	`inline`[opt], `namespace`, attribute_specifier_seq[opt], `{`, namespace_body, `}`
inline ParseFunctionOutputs<UnnamedNamespaceDefinition::kTag>
UnnamedNamespaceDefinition::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<UnnamedNamespaceDefinition::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<UnnamedNamespaceDefinition::kTag>::
          create_single_token_check(
              true, token::tok::TokenKind::kw_inline,
              diag::DiagKind::unnamed_namespace_definition_expect_kw_inline),
      ParseFunction<UnnamedNamespaceDefinition::kTag>::
          create_single_token_check(
              false, token::tok::TokenKind::kw_namespace,
              diag::DiagKind::unnamed_namespace_definition_expect_kw_namespace),
      AttributeSpecifierSeq(true),
      ParseFunction<UnnamedNamespaceDefinition::kTag>::
          create_single_token_check(
              false, token::tok::TokenKind::l_brace,
              diag::DiagKind::unnamed_namespace_definition_expect_l_brace),
      NamespaceBody(false),
      ParseFunction<UnnamedNamespaceDefinition::kTag>::
          create_single_token_check(
              false, token::tok::TokenKind::r_brace,
              diag::DiagKind::unnamed_namespace_definition_expect_r_brace));
  return serial_funcs();
}

// nested-namespace-definition:
// 	`namespace`, enclosing_namespace_specifier, `::`, `inline`[opt], `identifier`, `{`, namespace_body, `}`
inline ParseFunctionOutputs<NestedNamespaceDefinition::kTag>
NestedNamespaceDefinition::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<NestedNamespaceDefinition::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<NestedNamespaceDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_namespace,
          diag::DiagKind::nested_namespace_definition_expect_kw_namespace),
      EnclosingNamespaceSpecifier(false),
      ParseFunction<NestedNamespaceDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::coloncolon,
          diag::DiagKind::nested_namespace_definition_expect_coloncolon),
      ParseFunction<NestedNamespaceDefinition::kTag>::create_single_token_check(
          true, token::tok::TokenKind::kw_inline,
          diag::DiagKind::nested_namespace_definition_expect_kw_inline),
      ParseFunction<NestedNamespaceDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::nested_namespace_definition_expect_identifier),
      ParseFunction<NestedNamespaceDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_brace,
          diag::DiagKind::nested_namespace_definition_expect_l_brace),
      NamespaceBody(false),
      ParseFunction<NestedNamespaceDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_brace,
          diag::DiagKind::nested_namespace_definition_expect_r_brace));
  return serial_funcs();
}

// enclosing-namespace-specifier:
// 	`identifier`
// 	enclosing_namespace_specifier, `::`, `inline`[opt], `identifier`
inline ParseFunctionOutputs<EnclosingNamespaceSpecifier::kTag>
EnclosingNamespaceSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        EnclosingNamespaceSpecifier::kTag, 1,
        SerialParseFunctions<EnclosingNamespaceSpecifier::kTag,
                             ParseFunction<EnclosingNamespaceSpecifier::kTag>>>
        parallel_funcs_0(
            ParseFunctionInputs<EnclosingNamespaceSpecifier::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<EnclosingNamespaceSpecifier::kTag>(false),
                ParseFunction<EnclosingNamespaceSpecifier::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::identifier,
                        diag::DiagKind::
                            enclosing_namespace_specifier_expect_identifier)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      EnclosingNamespaceSpecifier::kTag, 1,
      SerialParseFunctions<EnclosingNamespaceSpecifier::kTag,
                           EnclosingNamespaceSpecifier,
                           ParseFunction<EnclosingNamespaceSpecifier::kTag>,
                           ParseFunction<EnclosingNamespaceSpecifier::kTag>,
                           ParseFunction<EnclosingNamespaceSpecifier::kTag>>>
      parallel_funcs_1(
          ParseFunctionInputs<EnclosingNamespaceSpecifier::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<EnclosingNamespaceSpecifier::kTag>(false),
              EnclosingNamespaceSpecifier(false),
              ParseFunction<EnclosingNamespaceSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::coloncolon,
                      diag::DiagKind::
                          enclosing_namespace_specifier_expect_coloncolon),
              ParseFunction<EnclosingNamespaceSpecifier::kTag>::
                  create_single_token_check(
                      true, token::tok::TokenKind::kw_inline,
                      diag::DiagKind::
                          enclosing_namespace_specifier_expect_kw_inline),
              ParseFunction<EnclosingNamespaceSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::identifier,
                      diag::DiagKind::
                          enclosing_namespace_specifier_expect_identifier)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// namespace-body:
// 	declaration_seq[opt]
inline ParseFunctionOutputs<NamespaceBody::kTag> NamespaceBody::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<NamespaceBody::kTag>(false, output.last_token_,
                                               output.cur_token_),
      DeclarationSeq(true));
  return serial_funcs();
}

// namespace-alias:
// 	`identifier`
inline ParseFunctionOutputs<NamespaceAlias::kTag> NamespaceAlias::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<NamespaceAlias::kTag>(false, output.last_token_,
                                                output.cur_token_),
      ParseFunction<NamespaceAlias::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::namespace_alias_expect_identifier));
  return serial_funcs();
}

// namespace-alias-definition:
// 	`namespace`, `identifier`, `=`, qualified_namespace_specifier, `;`
inline ParseFunctionOutputs<NamespaceAliasDefinition::kTag>
NamespaceAliasDefinition::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<NamespaceAliasDefinition::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<NamespaceAliasDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_namespace,
          diag::DiagKind::namespace_alias_definition_expect_kw_namespace),
      ParseFunction<NamespaceAliasDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::namespace_alias_definition_expect_identifier),
      ParseFunction<NamespaceAliasDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::equal,
          diag::DiagKind::namespace_alias_definition_expect_equal),
      QualifiedNamespaceSpecifier(false),
      ParseFunction<NamespaceAliasDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::namespace_alias_definition_expect_semi));
  return serial_funcs();
}

// qualified-namespace-specifier:
// 	nested_name_specifier[opt], namespace_name
inline ParseFunctionOutputs<QualifiedNamespaceSpecifier::kTag>
QualifiedNamespaceSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<QualifiedNamespaceSpecifier::kTag>(
          false, output.last_token_, output.cur_token_),
      NestedNameSpecifier(true), NamespaceName(false));
  return serial_funcs();
}

// using-directive:
// 	attribute_specifier_seq[opt], `using`, `namespace`, nested_name_specifier[opt], namespace_name, `;`
inline ParseFunctionOutputs<UsingDirective::kTag> UsingDirective::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<UsingDirective::kTag>(false, output.last_token_,
                                                output.cur_token_),
      AttributeSpecifierSeq(true),
      ParseFunction<UsingDirective::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_using,
          diag::DiagKind::using_directive_expect_kw_using),
      ParseFunction<UsingDirective::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_namespace,
          diag::DiagKind::using_directive_expect_kw_namespace),
      NestedNameSpecifier(true), NamespaceName(false),
      ParseFunction<UsingDirective::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::using_directive_expect_semi));
  return serial_funcs();
}

// using-declaration:
// 	`using`, using_declarator_list, `;`
inline ParseFunctionOutputs<UsingDeclaration::kTag>
UsingDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<UsingDeclaration::kTag>(false, output.last_token_,
                                                  output.cur_token_),
      ParseFunction<UsingDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_using,
          diag::DiagKind::using_declaration_expect_kw_using),
      UsingDeclaratorList(false),
      ParseFunction<UsingDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::using_declaration_expect_semi));
  return serial_funcs();
}

// using-declarator-list:
// 	using_declarator, `...`[opt]
// 	using_declarator_list, `,`, using_declarator, `...`[opt]
inline ParseFunctionOutputs<UsingDeclaratorList::kTag>
UsingDeclaratorList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        UsingDeclaratorList::kTag, 1,
        SerialParseFunctions<UsingDeclaratorList::kTag, UsingDeclarator,
                             ParseFunction<UsingDeclaratorList::kTag>>>
        parallel_funcs_0(
            ParseFunctionInputs<UsingDeclaratorList::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<UsingDeclaratorList::kTag>(false),
                UsingDeclarator(false),
                ParseFunction<UsingDeclaratorList::kTag>::
                    create_single_token_check(
                        true, token::tok::TokenKind::ellipsis,
                        diag::DiagKind::
                            using_declarator_list_expect_ellipsis)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      UsingDeclaratorList::kTag, 1,
      SerialParseFunctions<UsingDeclaratorList::kTag, UsingDeclaratorList,
                           ParseFunction<UsingDeclaratorList::kTag>,
                           UsingDeclarator,
                           ParseFunction<UsingDeclaratorList::kTag>>>
      parallel_funcs_1(
          ParseFunctionInputs<UsingDeclaratorList::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<UsingDeclaratorList::kTag>(false),
              UsingDeclaratorList(false),
              ParseFunction<UsingDeclaratorList::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::comma,
                      diag::DiagKind::using_declarator_list_expect_comma),
              UsingDeclarator(false),
              ParseFunction<UsingDeclaratorList::kTag>::
                  create_single_token_check(
                      true, token::tok::TokenKind::ellipsis,
                      diag::DiagKind::using_declarator_list_expect_ellipsis)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// using-declarator:
// 	`typename`[opt], nested_name_specifier, unqualified_id
inline ParseFunctionOutputs<UsingDeclarator::kTag>
UsingDeclarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<UsingDeclarator::kTag>(false, output.last_token_,
                                                 output.cur_token_),
      ParseFunction<UsingDeclarator::kTag>::create_single_token_check(
          true, token::tok::TokenKind::kw_typename,
          diag::DiagKind::using_declarator_expect_kw_typename),
      NestedNameSpecifier(false), UnqualifiedId(false));
  return serial_funcs();
}

// asm-declaration:
// 	attribute_specifier_seq[opt], `asm`, `(`, string_literal, `)`, `;`
inline ParseFunctionOutputs<AsmDeclaration::kTag> AsmDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<AsmDeclaration::kTag>(false, output.last_token_,
                                                output.cur_token_),
      AttributeSpecifierSeq(true),
      ParseFunction<AsmDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_asm,
          diag::DiagKind::asm_declaration_expect_kw_asm),
      ParseFunction<AsmDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_paren,
          diag::DiagKind::asm_declaration_expect_l_paren),
      StringLiteral(false),
      ParseFunction<AsmDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_paren,
          diag::DiagKind::asm_declaration_expect_r_paren),
      ParseFunction<AsmDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::asm_declaration_expect_semi));
  return serial_funcs();
}

// linkage-specification:
// 	`extern`, string_literal, `{`, declaration_seq[opt], `}`
// 	`extern`, string_literal, declaration
inline ParseFunctionOutputs<LinkageSpecification::kTag>
LinkageSpecification::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      LinkageSpecification::kTag, 2,
      SerialParseFunctions<
          LinkageSpecification::kTag, ParseFunction<LinkageSpecification::kTag>,
          StringLiteral, ParseFunction<LinkageSpecification::kTag>,
          DeclarationSeq, ParseFunction<LinkageSpecification::kTag>>,
      SerialParseFunctions<LinkageSpecification::kTag,
                           ParseFunction<LinkageSpecification::kTag>,
                           StringLiteral, Declaration>>
      parallel_funcs_0(
          ParseFunctionInputs<LinkageSpecification::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<LinkageSpecification::kTag>(false),
              ParseFunction<LinkageSpecification::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_extern,
                      diag::DiagKind::linkage_specification_expect_kw_extern),
              StringLiteral(false),
              ParseFunction<LinkageSpecification::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_brace,
                      diag::DiagKind::linkage_specification_expect_l_brace),
              DeclarationSeq(true),
              ParseFunction<LinkageSpecification::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_brace,
                      diag::DiagKind::linkage_specification_expect_r_brace)),
          SerialParseFunctions(
              ParseFunctionInputs<LinkageSpecification::kTag>(false),
              ParseFunction<LinkageSpecification::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_extern,
                      diag::DiagKind::linkage_specification_expect_kw_extern),
              StringLiteral(false), Declaration(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// attribute-specifier-seq:
// 	attribute_specifier, attribute_specifier_seq[opt]
inline ParseFunctionOutputs<AttributeSpecifierSeq::kTag>
AttributeSpecifierSeq::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<AttributeSpecifierSeq::kTag>(
          false, output.last_token_, output.cur_token_),
      AttributeSpecifier(false), AttributeSpecifierSeq(true));
  return serial_funcs();
}

// attribute-specifier:
// 	`[`, `[`, attribute_using_prefix[opt], attribute_list, `]`, `]`
// 	alignment_specifier
inline ParseFunctionOutputs<AttributeSpecifier::kTag>
AttributeSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      AttributeSpecifier::kTag, 2,
      SerialParseFunctions<
          AttributeSpecifier::kTag, ParseFunction<AttributeSpecifier::kTag>,
          ParseFunction<AttributeSpecifier::kTag>, AttributeUsingPrefix,
          AttributeList, ParseFunction<AttributeSpecifier::kTag>,
          ParseFunction<AttributeSpecifier::kTag>>,
      SerialParseFunctions<AttributeSpecifier::kTag, AlignmentSpecifier>>
      parallel_funcs_0(
          ParseFunctionInputs<AttributeSpecifier::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<AttributeSpecifier::kTag>(false),
              ParseFunction<AttributeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_square,
                      diag::DiagKind::attribute_specifier_expect_l_square),
              ParseFunction<AttributeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_square,
                      diag::DiagKind::attribute_specifier_expect_l_square),
              AttributeUsingPrefix(true), AttributeList(false),
              ParseFunction<AttributeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_square,
                      diag::DiagKind::attribute_specifier_expect_r_square),
              ParseFunction<AttributeSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_square,
                      diag::DiagKind::attribute_specifier_expect_r_square)),
          SerialParseFunctions(
              ParseFunctionInputs<AttributeSpecifier::kTag>(false),
              AlignmentSpecifier(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// alignment-specifier:
// 	`alignas`, `(`, type_id, `...`[opt], `)`
// 	`alignas`, `(`, constant_expression, `...`[opt], `)`
inline ParseFunctionOutputs<AlignmentSpecifier::kTag>
AlignmentSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      AlignmentSpecifier::kTag, 2,
      SerialParseFunctions<AlignmentSpecifier::kTag,
                           ParseFunction<AlignmentSpecifier::kTag>,
                           ParseFunction<AlignmentSpecifier::kTag>, TypeId,
                           ParseFunction<AlignmentSpecifier::kTag>,
                           ParseFunction<AlignmentSpecifier::kTag>>,
      SerialParseFunctions<
          AlignmentSpecifier::kTag, ParseFunction<AlignmentSpecifier::kTag>,
          ParseFunction<AlignmentSpecifier::kTag>, ConstantExpression,
          ParseFunction<AlignmentSpecifier::kTag>,
          ParseFunction<AlignmentSpecifier::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<AlignmentSpecifier::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<AlignmentSpecifier::kTag>(false),
              ParseFunction<AlignmentSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_alignas,
                      diag::DiagKind::alignment_specifier_expect_kw_alignas),
              ParseFunction<AlignmentSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::alignment_specifier_expect_l_paren),
              TypeId(false),
              ParseFunction<AlignmentSpecifier::kTag>::
                  create_single_token_check(
                      true, token::tok::TokenKind::ellipsis,
                      diag::DiagKind::alignment_specifier_expect_ellipsis),
              ParseFunction<AlignmentSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::alignment_specifier_expect_r_paren)),
          SerialParseFunctions(
              ParseFunctionInputs<AlignmentSpecifier::kTag>(false),
              ParseFunction<AlignmentSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_alignas,
                      diag::DiagKind::alignment_specifier_expect_kw_alignas),
              ParseFunction<AlignmentSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::alignment_specifier_expect_l_paren),
              ConstantExpression(false),
              ParseFunction<AlignmentSpecifier::kTag>::
                  create_single_token_check(
                      true, token::tok::TokenKind::ellipsis,
                      diag::DiagKind::alignment_specifier_expect_ellipsis),
              ParseFunction<AlignmentSpecifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::alignment_specifier_expect_r_paren)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// attribute-using-prefix:
// 	`using`, attribute_namespace, `:`
inline ParseFunctionOutputs<AttributeUsingPrefix::kTag>
AttributeUsingPrefix::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<AttributeUsingPrefix::kTag>(false, output.last_token_,
                                                      output.cur_token_),
      ParseFunction<AttributeUsingPrefix::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_using,
          diag::DiagKind::attribute_using_prefix_expect_kw_using),
      AttributeNamespace(false),
      ParseFunction<AttributeUsingPrefix::kTag>::create_single_token_check(
          false, token::tok::TokenKind::colon,
          diag::DiagKind::attribute_using_prefix_expect_colon));
  return serial_funcs();
}

// attribute-list:
// 	attribute[opt]
// 	attribute_list, `,`, attribute[opt]
// 	attribute, `...`
// 	attribute_list, `,`, attribute, `...`
inline ParseFunctionOutputs<AttributeList::kTag> AttributeList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        AttributeList::kTag, 2,
        SerialParseFunctions<AttributeList::kTag, Attribute>,
        SerialParseFunctions<AttributeList::kTag, Attribute,
                             ParseFunction<AttributeList::kTag>>>
        parallel_funcs_0(
            ParseFunctionInputs<AttributeList::kTag>(false, output.last_token_,
                                                     output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<AttributeList::kTag>(false),
                Attribute(true)),
            SerialParseFunctions(
                ParseFunctionInputs<AttributeList::kTag>(false),
                Attribute(false),
                ParseFunction<AttributeList::kTag>::create_single_token_check(
                    false, token::tok::TokenKind::ellipsis,
                    diag::DiagKind::attribute_list_expect_ellipsis)));

    static_assert(base::kNumberOfElements >= 2);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      AttributeList::kTag, 2,
      SerialParseFunctions<AttributeList::kTag, AttributeList,
                           ParseFunction<AttributeList::kTag>, Attribute>,
      SerialParseFunctions<AttributeList::kTag, AttributeList,
                           ParseFunction<AttributeList::kTag>, Attribute,
                           ParseFunction<AttributeList::kTag>>>
      parallel_funcs_1(
          ParseFunctionInputs<AttributeList::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<AttributeList::kTag>(false),
              AttributeList(false),
              ParseFunction<AttributeList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::comma,
                  diag::DiagKind::attribute_list_expect_comma),
              Attribute(true)),
          SerialParseFunctions(
              ParseFunctionInputs<AttributeList::kTag>(false),
              AttributeList(false),
              ParseFunction<AttributeList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::comma,
                  diag::DiagKind::attribute_list_expect_comma),
              Attribute(false),
              ParseFunction<AttributeList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::attribute_list_expect_ellipsis)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// attribute:
// 	attribute_token, attribute_argument_clause[opt]
inline ParseFunctionOutputs<Attribute::kTag> Attribute::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<Attribute::kTag>(false, output.last_token_,
                                           output.cur_token_),
      AttributeToken(false), AttributeArgumentClause(true));
  return serial_funcs();
}

// attribute-token:
// 	`identifier`
// 	attribute_scoped_token
inline ParseFunctionOutputs<AttributeToken::kTag> AttributeToken::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      AttributeToken::kTag, 2,
      SerialParseFunctions<AttributeToken::kTag,
                           ParseFunction<AttributeToken::kTag>>,
      SerialParseFunctions<AttributeToken::kTag, AttributeScopedToken>>
      parallel_funcs_0(
          ParseFunctionInputs<AttributeToken::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<AttributeToken::kTag>(false),
              ParseFunction<AttributeToken::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::attribute_token_expect_identifier)),
          SerialParseFunctions(ParseFunctionInputs<AttributeToken::kTag>(false),
                               AttributeScopedToken(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// attribute-scoped-token:
// 	attribute_namespace, `::`, `identifier`
inline ParseFunctionOutputs<AttributeScopedToken::kTag>
AttributeScopedToken::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<AttributeScopedToken::kTag>(false, output.last_token_,
                                                      output.cur_token_),
      AttributeNamespace(false),
      ParseFunction<AttributeScopedToken::kTag>::create_single_token_check(
          false, token::tok::TokenKind::coloncolon,
          diag::DiagKind::attribute_scoped_token_expect_coloncolon),
      ParseFunction<AttributeScopedToken::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::attribute_scoped_token_expect_identifier));
  return serial_funcs();
}

// attribute-namespace:
// 	`identifier`
inline ParseFunctionOutputs<AttributeNamespace::kTag>
AttributeNamespace::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<AttributeNamespace::kTag>(false, output.last_token_,
                                                    output.cur_token_),
      ParseFunction<AttributeNamespace::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::attribute_namespace_expect_identifier));
  return serial_funcs();
}

// attribute-argument-clause:
// 	`(`, balanced_token_seq[opt], `)`
inline ParseFunctionOutputs<AttributeArgumentClause::kTag>
AttributeArgumentClause::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<AttributeArgumentClause::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<AttributeArgumentClause::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_paren,
          diag::DiagKind::attribute_argument_clause_expect_l_paren),
      BalancedTokenSeq(true),
      ParseFunction<AttributeArgumentClause::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_paren,
          diag::DiagKind::attribute_argument_clause_expect_r_paren));
  return serial_funcs();
}

// balanced-token-seq:
// 	balanced_token
// 	balanced_token_seq, balanced_token
inline ParseFunctionOutputs<BalancedTokenSeq::kTag>
BalancedTokenSeq::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        BalancedTokenSeq::kTag, 1,
        SerialParseFunctions<BalancedTokenSeq::kTag, BalancedToken>>
        parallel_funcs_0(ParseFunctionInputs<BalancedTokenSeq::kTag>(
                             false, output.last_token_, output.cur_token_),
                         SerialParseFunctions(
                             ParseFunctionInputs<BalancedTokenSeq::kTag>(false),
                             BalancedToken(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<BalancedTokenSeq::kTag, 1,
                         SerialParseFunctions<BalancedTokenSeq::kTag,
                                              BalancedTokenSeq, BalancedToken>>
      parallel_funcs_1(ParseFunctionInputs<BalancedTokenSeq::kTag>(
                           false, output.last_token_, output.cur_token_),
                       SerialParseFunctions(
                           ParseFunctionInputs<BalancedTokenSeq::kTag>(false),
                           BalancedTokenSeq(false), BalancedToken(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// balanced-token:
// 	`(`, balanced_token_seq[opt], `)`
// 	`[`, balanced_token_seq[opt], `]`
// 	`{`, balanced_token_seq[opt], `}`
// 	`identifier`
// 	keyword
// 	literal
// 	operator_or_punctuator
inline ParseFunctionOutputs<BalancedToken::kTag> BalancedToken::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      BalancedToken::kTag, 7,
      SerialParseFunctions<BalancedToken::kTag,
                           ParseFunction<BalancedToken::kTag>, BalancedTokenSeq,
                           ParseFunction<BalancedToken::kTag>>,
      SerialParseFunctions<BalancedToken::kTag,
                           ParseFunction<BalancedToken::kTag>, BalancedTokenSeq,
                           ParseFunction<BalancedToken::kTag>>,
      SerialParseFunctions<BalancedToken::kTag,
                           ParseFunction<BalancedToken::kTag>, BalancedTokenSeq,
                           ParseFunction<BalancedToken::kTag>>,
      SerialParseFunctions<BalancedToken::kTag,
                           ParseFunction<BalancedToken::kTag>>,
      SerialParseFunctions<BalancedToken::kTag, Keyword>,
      SerialParseFunctions<BalancedToken::kTag, Literal>,
      SerialParseFunctions<BalancedToken::kTag, OperatorOrPunctuator>>
      parallel_funcs_0(
          ParseFunctionInputs<BalancedToken::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<BalancedToken::kTag>(false),
              ParseFunction<BalancedToken::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::balanced_token_expect_l_paren),
              BalancedTokenSeq(true),
              ParseFunction<BalancedToken::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::balanced_token_expect_r_paren)),
          SerialParseFunctions(
              ParseFunctionInputs<BalancedToken::kTag>(false),
              ParseFunction<BalancedToken::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_square,
                  diag::DiagKind::balanced_token_expect_l_square),
              BalancedTokenSeq(true),
              ParseFunction<BalancedToken::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_square,
                  diag::DiagKind::balanced_token_expect_r_square)),
          SerialParseFunctions(
              ParseFunctionInputs<BalancedToken::kTag>(false),
              ParseFunction<BalancedToken::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_brace,
                  diag::DiagKind::balanced_token_expect_l_brace),
              BalancedTokenSeq(true),
              ParseFunction<BalancedToken::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_brace,
                  diag::DiagKind::balanced_token_expect_r_brace)),
          SerialParseFunctions(
              ParseFunctionInputs<BalancedToken::kTag>(false),
              ParseFunction<BalancedToken::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::balanced_token_expect_identifier)),
          SerialParseFunctions(ParseFunctionInputs<BalancedToken::kTag>(false),
                               Keyword(false)),
          SerialParseFunctions(ParseFunctionInputs<BalancedToken::kTag>(false),
                               Literal(false)),
          SerialParseFunctions(ParseFunctionInputs<BalancedToken::kTag>(false),
                               OperatorOrPunctuator(false)));

  static_assert(base::kNumberOfElements >= 7);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// module-declaration:
// 	`export`[opt], `module`, module_name, module_partition[opt], attribute_specifier_seq[opt], `;`
inline ParseFunctionOutputs<ModuleDeclaration::kTag>
ModuleDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ModuleDeclaration::kTag>(false, output.last_token_,
                                                   output.cur_token_),
      ParseFunction<ModuleDeclaration::kTag>::create_single_token_check(
          true, token::tok::TokenKind::kw_export,
          diag::DiagKind::module_declaration_expect_kw_export),
      ParseFunction<ModuleDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_module,
          diag::DiagKind::module_declaration_expect_kw_module),
      ModuleName(false), ModulePartition(true), AttributeSpecifierSeq(true),
      ParseFunction<ModuleDeclaration::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::module_declaration_expect_semi));
  return serial_funcs();
}

// module-name:
// 	module_name_qualifier[opt], `identifier`
inline ParseFunctionOutputs<ModuleName::kTag> ModuleName::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ModuleName::kTag>(false, output.last_token_,
                                            output.cur_token_),
      ModuleNameQualifier(true),
      ParseFunction<ModuleName::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::module_name_expect_identifier));
  return serial_funcs();
}

// module-partition:
// 	`:`, module_name_qualifier[opt], `identifier`
inline ParseFunctionOutputs<ModulePartition::kTag>
ModulePartition::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ModulePartition::kTag>(false, output.last_token_,
                                                 output.cur_token_),
      ParseFunction<ModulePartition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::colon,
          diag::DiagKind::module_partition_expect_colon),
      ModuleNameQualifier(true),
      ParseFunction<ModulePartition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::module_partition_expect_identifier));
  return serial_funcs();
}

// module-name-qualifier:
// 	`identifier`, `.`
// 	module_name_qualifier, `identifier`, `.`
inline ParseFunctionOutputs<ModuleNameQualifier::kTag>
ModuleNameQualifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        ModuleNameQualifier::kTag, 1,
        SerialParseFunctions<ModuleNameQualifier::kTag,
                             ParseFunction<ModuleNameQualifier::kTag>,
                             ParseFunction<ModuleNameQualifier::kTag>>>
        parallel_funcs_0(
            ParseFunctionInputs<ModuleNameQualifier::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<ModuleNameQualifier::kTag>(false),
                ParseFunction<ModuleNameQualifier::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::identifier,
                        diag::DiagKind::
                            module_name_qualifier_expect_identifier),
                ParseFunction<ModuleNameQualifier::kTag>::
                    create_single_token_check(
                        false, token::tok::TokenKind::period,
                        diag::DiagKind::module_name_qualifier_expect_period)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      ModuleNameQualifier::kTag, 1,
      SerialParseFunctions<ModuleNameQualifier::kTag, ModuleNameQualifier,
                           ParseFunction<ModuleNameQualifier::kTag>,
                           ParseFunction<ModuleNameQualifier::kTag>>>
      parallel_funcs_1(
          ParseFunctionInputs<ModuleNameQualifier::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ModuleNameQualifier::kTag>(false),
              ModuleNameQualifier(false),
              ParseFunction<ModuleNameQualifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::identifier,
                      diag::DiagKind::module_name_qualifier_expect_identifier),
              ParseFunction<ModuleNameQualifier::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::period,
                      diag::DiagKind::module_name_qualifier_expect_period)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// export-declaration:
// 	`export`, declaration
// 	`export`, `{`, declaration_seq[opt], `}`
// 	`export`, module_import_declaration
inline ParseFunctionOutputs<ExportDeclaration::kTag>
ExportDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ExportDeclaration::kTag, 3,
      SerialParseFunctions<ExportDeclaration::kTag,
                           ParseFunction<ExportDeclaration::kTag>, Declaration>,
      SerialParseFunctions<
          ExportDeclaration::kTag, ParseFunction<ExportDeclaration::kTag>,
          ParseFunction<ExportDeclaration::kTag>, DeclarationSeq,
          ParseFunction<ExportDeclaration::kTag>>,
      SerialParseFunctions<ExportDeclaration::kTag,
                           ParseFunction<ExportDeclaration::kTag>,
                           ModuleImportDeclaration>>
      parallel_funcs_0(
          ParseFunctionInputs<ExportDeclaration::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ExportDeclaration::kTag>(false),
              ParseFunction<ExportDeclaration::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_export,
                  diag::DiagKind::export_declaration_expect_kw_export),
              Declaration(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ExportDeclaration::kTag>(false),
              ParseFunction<ExportDeclaration::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_export,
                  diag::DiagKind::export_declaration_expect_kw_export),
              ParseFunction<ExportDeclaration::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_brace,
                  diag::DiagKind::export_declaration_expect_l_brace),
              DeclarationSeq(true),
              ParseFunction<ExportDeclaration::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_brace,
                  diag::DiagKind::export_declaration_expect_r_brace)),
          SerialParseFunctions(
              ParseFunctionInputs<ExportDeclaration::kTag>(false),
              ParseFunction<ExportDeclaration::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_export,
                  diag::DiagKind::export_declaration_expect_kw_export),
              ModuleImportDeclaration(false)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// module-import-declaration:
// 	`import`, module_name, attribute_specifier_seq[opt]
// 	`import`, module_partition, attribute_specifier_seq[opt]
// 	`import`, header_name, attribute_specifier_seq[opt]
inline ParseFunctionOutputs<ModuleImportDeclaration::kTag>
ModuleImportDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ModuleImportDeclaration::kTag, 3,
      SerialParseFunctions<ModuleImportDeclaration::kTag,
                           ParseFunction<ModuleImportDeclaration::kTag>,
                           ModuleName, AttributeSpecifierSeq>,
      SerialParseFunctions<ModuleImportDeclaration::kTag,
                           ParseFunction<ModuleImportDeclaration::kTag>,
                           ModulePartition, AttributeSpecifierSeq>,
      SerialParseFunctions<ModuleImportDeclaration::kTag,
                           ParseFunction<ModuleImportDeclaration::kTag>,
                           HeaderName, AttributeSpecifierSeq>>
      parallel_funcs_0(
          ParseFunctionInputs<ModuleImportDeclaration::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ModuleImportDeclaration::kTag>(false),
              ParseFunction<ModuleImportDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_import,
                      diag::DiagKind::
                          module_import_declaration_expect_kw_import),
              ModuleName(false), AttributeSpecifierSeq(true)),
          SerialParseFunctions(
              ParseFunctionInputs<ModuleImportDeclaration::kTag>(false),
              ParseFunction<ModuleImportDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_import,
                      diag::DiagKind::
                          module_import_declaration_expect_kw_import),
              ModulePartition(false), AttributeSpecifierSeq(true)),
          SerialParseFunctions(
              ParseFunctionInputs<ModuleImportDeclaration::kTag>(false),
              ParseFunction<ModuleImportDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_import,
                      diag::DiagKind::
                          module_import_declaration_expect_kw_import),
              HeaderName(false), AttributeSpecifierSeq(true)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// global-module-fragment:
// 	`module`, `;`, declaration_seq[opt]
inline ParseFunctionOutputs<GlobalModuleFragment::kTag>
GlobalModuleFragment::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<GlobalModuleFragment::kTag>(false, output.last_token_,
                                                      output.cur_token_),
      ParseFunction<GlobalModuleFragment::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_module,
          diag::DiagKind::global_module_fragment_expect_kw_module),
      ParseFunction<GlobalModuleFragment::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::global_module_fragment_expect_semi),
      DeclarationSeq(true));
  return serial_funcs();
}

// private-module-fragment:
// 	`module`, `:`, `private`, `;`, declaration_seq[opt]
inline ParseFunctionOutputs<PrivateModuleFragment::kTag>
PrivateModuleFragment::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<PrivateModuleFragment::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<PrivateModuleFragment::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_module,
          diag::DiagKind::private_module_fragment_expect_kw_module),
      ParseFunction<PrivateModuleFragment::kTag>::create_single_token_check(
          false, token::tok::TokenKind::colon,
          diag::DiagKind::private_module_fragment_expect_colon),
      ParseFunction<PrivateModuleFragment::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_private,
          diag::DiagKind::private_module_fragment_expect_kw_private),
      ParseFunction<PrivateModuleFragment::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::private_module_fragment_expect_semi),
      DeclarationSeq(true));
  return serial_funcs();
}

// class-name:
// 	`identifier`
// 	simple_template_id
inline ParseFunctionOutputs<ClassName::kTag> ClassName::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ClassName::kTag, 2,
      SerialParseFunctions<ClassName::kTag, ParseFunction<ClassName::kTag>>,
      SerialParseFunctions<ClassName::kTag, SimpleTemplateId>>
      parallel_funcs_0(
          ParseFunctionInputs<ClassName::kTag>(false, output.last_token_,
                                               output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ClassName::kTag>(false),
              ParseFunction<ClassName::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::class_name_expect_identifier)),
          SerialParseFunctions(ParseFunctionInputs<ClassName::kTag>(false),
                               SimpleTemplateId(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// class-specifier:
// 	class_head, `{`, member_specification[opt], `}`
inline ParseFunctionOutputs<ClassSpecifier::kTag> ClassSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ClassSpecifier::kTag>(false, output.last_token_,
                                                output.cur_token_),
      ClassHead(false),
      ParseFunction<ClassSpecifier::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_brace,
          diag::DiagKind::class_specifier_expect_l_brace),
      MemberSpecification(true),
      ParseFunction<ClassSpecifier::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_brace,
          diag::DiagKind::class_specifier_expect_r_brace));
  return serial_funcs();
}

// class-head:
// 	class_key, attribute_specifier_seq[opt], class_head_name, class_virt_specifier[opt], base_clause[opt]
// 	class_key, attribute_specifier_seq[opt], base_clause[opt]
inline ParseFunctionOutputs<ClassHead::kTag> ClassHead::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ClassHead::kTag, 2,
      SerialParseFunctions<ClassHead::kTag, ClassKey, AttributeSpecifierSeq,
                           ClassHeadName, ClassVirtSpecifier, BaseClause>,
      SerialParseFunctions<ClassHead::kTag, ClassKey, AttributeSpecifierSeq,
                           BaseClause>>
      parallel_funcs_0(
          ParseFunctionInputs<ClassHead::kTag>(false, output.last_token_,
                                               output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<ClassHead::kTag>(false),
                               ClassKey(false), AttributeSpecifierSeq(true),
                               ClassHeadName(false), ClassVirtSpecifier(true),
                               BaseClause(true)),
          SerialParseFunctions(ParseFunctionInputs<ClassHead::kTag>(false),
                               ClassKey(false), AttributeSpecifierSeq(true),
                               BaseClause(true)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// class-head-name:
// 	nested_name_specifier[opt], class_name
inline ParseFunctionOutputs<ClassHeadName::kTag> ClassHeadName::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ClassHeadName::kTag>(false, output.last_token_,
                                               output.cur_token_),
      NestedNameSpecifier(true), ClassName(false));
  return serial_funcs();
}

// class-virt-specifier:
// 	`final`
inline ParseFunctionOutputs<ClassVirtSpecifier::kTag>
ClassVirtSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ClassVirtSpecifier::kTag>(false, output.last_token_,
                                                    output.cur_token_),
      ParseFunction<ClassVirtSpecifier::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_final,
          diag::DiagKind::class_virt_specifier_expect_kw_final));
  return serial_funcs();
}

// class-key:
// 	`class`
// 	`struct`
// 	`union`
inline ParseFunctionOutputs<ClassKey::kTag> ClassKey::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ClassKey::kTag, 3,
      SerialParseFunctions<ClassKey::kTag, ParseFunction<ClassKey::kTag>>,
      SerialParseFunctions<ClassKey::kTag, ParseFunction<ClassKey::kTag>>,
      SerialParseFunctions<ClassKey::kTag, ParseFunction<ClassKey::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<ClassKey::kTag>(false, output.last_token_,
                                              output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ClassKey::kTag>(false),
              ParseFunction<ClassKey::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_class,
                  diag::DiagKind::class_key_expect_kw_class)),
          SerialParseFunctions(
              ParseFunctionInputs<ClassKey::kTag>(false),
              ParseFunction<ClassKey::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_struct,
                  diag::DiagKind::class_key_expect_kw_struct)),
          SerialParseFunctions(
              ParseFunctionInputs<ClassKey::kTag>(false),
              ParseFunction<ClassKey::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_union,
                  diag::DiagKind::class_key_expect_kw_union)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// member-specification:
// 	member_declaration, member_specification[opt]
// 	access_specifier, `:`, member_specification[opt]
inline ParseFunctionOutputs<MemberSpecification::kTag>
MemberSpecification::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      MemberSpecification::kTag, 2,
      SerialParseFunctions<MemberSpecification::kTag, MemberDeclaration,
                           MemberSpecification>,
      SerialParseFunctions<MemberSpecification::kTag, AccessSpecifier,
                           ParseFunction<MemberSpecification::kTag>,
                           MemberSpecification>>
      parallel_funcs_0(
          ParseFunctionInputs<MemberSpecification::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<MemberSpecification::kTag>(false),
              MemberDeclaration(false), MemberSpecification(true)),
          SerialParseFunctions(
              ParseFunctionInputs<MemberSpecification::kTag>(false),
              AccessSpecifier(false),
              ParseFunction<MemberSpecification::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::colon,
                      diag::DiagKind::member_specification_expect_colon),
              MemberSpecification(true)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// member-declaration:
// 	attribute_specifier_seq[opt], decl_specifier_seq[opt], member_declarator_list[opt], `;`
// 	function_definition
// 	using_declaration
// 	using_enum_declaration
// 	static_assert_declaration
// 	template_declaration
// 	explicit_specialization
// 	deduction_guide
// 	alias_declaration
// 	opaque_enum_declaration
// 	empty_declaration
inline ParseFunctionOutputs<MemberDeclaration::kTag>
MemberDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      MemberDeclaration::kTag, 11,
      SerialParseFunctions<MemberDeclaration::kTag, AttributeSpecifierSeq,
                           DeclSpecifierSeq, MemberDeclaratorList,
                           ParseFunction<MemberDeclaration::kTag>>,
      SerialParseFunctions<MemberDeclaration::kTag, FunctionDefinition>,
      SerialParseFunctions<MemberDeclaration::kTag, UsingDeclaration>,
      SerialParseFunctions<MemberDeclaration::kTag, UsingEnumDeclaration>,
      SerialParseFunctions<MemberDeclaration::kTag, StaticAssertDeclaration>,
      SerialParseFunctions<MemberDeclaration::kTag, TemplateDeclaration>,
      SerialParseFunctions<MemberDeclaration::kTag, ExplicitSpecialization>,
      SerialParseFunctions<MemberDeclaration::kTag, DeductionGuide>,
      SerialParseFunctions<MemberDeclaration::kTag, AliasDeclaration>,
      SerialParseFunctions<MemberDeclaration::kTag, OpaqueEnumDeclaration>,
      SerialParseFunctions<MemberDeclaration::kTag, EmptyDeclaration>>
      parallel_funcs_0(
          ParseFunctionInputs<MemberDeclaration::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclaration::kTag>(false),
              AttributeSpecifierSeq(true), DeclSpecifierSeq(true),
              MemberDeclaratorList(true),
              ParseFunction<MemberDeclaration::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::semi,
                  diag::DiagKind::member_declaration_expect_semi)),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclaration::kTag>(false),
              FunctionDefinition(false)),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclaration::kTag>(false),
              UsingDeclaration(false)),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclaration::kTag>(false),
              UsingEnumDeclaration(false)),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclaration::kTag>(false),
              StaticAssertDeclaration(false)),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclaration::kTag>(false),
              TemplateDeclaration(false)),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclaration::kTag>(false),
              ExplicitSpecialization(false)),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclaration::kTag>(false),
              DeductionGuide(false)),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclaration::kTag>(false),
              AliasDeclaration(false)),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclaration::kTag>(false),
              OpaqueEnumDeclaration(false)),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclaration::kTag>(false),
              EmptyDeclaration(false)));

  static_assert(base::kNumberOfElements >= 11);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// member-declarator-list:
// 	member_declarator
// 	member_declarator_list, `,`, member_declarator
inline ParseFunctionOutputs<MemberDeclaratorList::kTag>
MemberDeclaratorList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        MemberDeclaratorList::kTag, 1,
        SerialParseFunctions<MemberDeclaratorList::kTag, MemberDeclarator>>
        parallel_funcs_0(
            ParseFunctionInputs<MemberDeclaratorList::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<MemberDeclaratorList::kTag>(false),
                MemberDeclarator(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      MemberDeclaratorList::kTag, 1,
      SerialParseFunctions<MemberDeclaratorList::kTag, MemberDeclaratorList,
                           ParseFunction<MemberDeclaratorList::kTag>,
                           MemberDeclarator>>
      parallel_funcs_1(
          ParseFunctionInputs<MemberDeclaratorList::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclaratorList::kTag>(false),
              MemberDeclaratorList(false),
              ParseFunction<MemberDeclaratorList::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::comma,
                      diag::DiagKind::member_declarator_list_expect_comma),
              MemberDeclarator(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// member-declarator:
// 	declarator, virt_specifier_seq[opt], pure_specifier[opt]
// 	declarator, requires_clause
// 	declarator, brace_or_equal_initializer[opt]
// 	`identifier`[opt], attribute_specifier_seq[opt], `:`, constant_expression, brace_or_equal_initializer[opt]
inline ParseFunctionOutputs<MemberDeclarator::kTag>
MemberDeclarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      MemberDeclarator::kTag, 4,
      SerialParseFunctions<MemberDeclarator::kTag, Declarator, VirtSpecifierSeq,
                           PureSpecifier>,
      SerialParseFunctions<MemberDeclarator::kTag, Declarator, RequiresClause>,
      SerialParseFunctions<MemberDeclarator::kTag, Declarator,
                           BraceOrEqualInitializer>,
      SerialParseFunctions<
          MemberDeclarator::kTag, ParseFunction<MemberDeclarator::kTag>,
          AttributeSpecifierSeq, ParseFunction<MemberDeclarator::kTag>,
          ConstantExpression, BraceOrEqualInitializer>>
      parallel_funcs_0(
          ParseFunctionInputs<MemberDeclarator::kTag>(false, output.last_token_,
                                                      output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclarator::kTag>(false),
              Declarator(false), VirtSpecifierSeq(true), PureSpecifier(true)),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclarator::kTag>(false),
              Declarator(false), RequiresClause(false)),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclarator::kTag>(false),
              Declarator(false), BraceOrEqualInitializer(true)),
          SerialParseFunctions(
              ParseFunctionInputs<MemberDeclarator::kTag>(false),
              ParseFunction<MemberDeclarator::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::identifier,
                  diag::DiagKind::member_declarator_expect_identifier),
              AttributeSpecifierSeq(true),
              ParseFunction<MemberDeclarator::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::colon,
                  diag::DiagKind::member_declarator_expect_colon),
              ConstantExpression(false), BraceOrEqualInitializer(true)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// virt-specifier-seq:
// 	virt_specifier
// 	virt_specifier_seq, virt_specifier
inline ParseFunctionOutputs<VirtSpecifierSeq::kTag>
VirtSpecifierSeq::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        VirtSpecifierSeq::kTag, 1,
        SerialParseFunctions<VirtSpecifierSeq::kTag, VirtSpecifier>>
        parallel_funcs_0(ParseFunctionInputs<VirtSpecifierSeq::kTag>(
                             false, output.last_token_, output.cur_token_),
                         SerialParseFunctions(
                             ParseFunctionInputs<VirtSpecifierSeq::kTag>(false),
                             VirtSpecifier(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<VirtSpecifierSeq::kTag, 1,
                         SerialParseFunctions<VirtSpecifierSeq::kTag,
                                              VirtSpecifierSeq, VirtSpecifier>>
      parallel_funcs_1(ParseFunctionInputs<VirtSpecifierSeq::kTag>(
                           false, output.last_token_, output.cur_token_),
                       SerialParseFunctions(
                           ParseFunctionInputs<VirtSpecifierSeq::kTag>(false),
                           VirtSpecifierSeq(false), VirtSpecifier(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// virt-specifier:
// 	`override`
// 	`final`
inline ParseFunctionOutputs<VirtSpecifier::kTag> VirtSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      VirtSpecifier::kTag, 2,
      SerialParseFunctions<VirtSpecifier::kTag,
                           ParseFunction<VirtSpecifier::kTag>>,
      SerialParseFunctions<VirtSpecifier::kTag,
                           ParseFunction<VirtSpecifier::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<VirtSpecifier::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<VirtSpecifier::kTag>(false),
              ParseFunction<VirtSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_override,
                  diag::DiagKind::virt_specifier_expect_kw_override)),
          SerialParseFunctions(
              ParseFunctionInputs<VirtSpecifier::kTag>(false),
              ParseFunction<VirtSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_final,
                  diag::DiagKind::virt_specifier_expect_kw_final)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// pure-specifier:
// 	`=`, `0`
inline ParseFunctionOutputs<PureSpecifier::kTag> PureSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(PureSpecifier::kTag);
  return output;
}

// conversion-function-id:
// 	`operator`, conversion_type_id
inline ParseFunctionOutputs<ConversionFunctionId::kTag>
ConversionFunctionId::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ConversionFunctionId::kTag>(false, output.last_token_,
                                                      output.cur_token_),
      ParseFunction<ConversionFunctionId::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_operator,
          diag::DiagKind::conversion_function_id_expect_kw_operator),
      ConversionTypeId(false));
  return serial_funcs();
}

// conversion-type-id:
// 	type_specifier_seq, conversion_declarator[opt]
inline ParseFunctionOutputs<ConversionTypeId::kTag>
ConversionTypeId::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ConversionTypeId::kTag>(false, output.last_token_,
                                                  output.cur_token_),
      TypeSpecifierSeq(false), ConversionDeclarator(true));
  return serial_funcs();
}

// conversion-declarator:
// 	ptr_operator, conversion_declarator[opt]
inline ParseFunctionOutputs<ConversionDeclarator::kTag>
ConversionDeclarator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ConversionDeclarator::kTag>(false, output.last_token_,
                                                      output.cur_token_),
      PtrOperator(false), ConversionDeclarator(true));
  return serial_funcs();
}

// base-clause:
// 	`:`, base_specifier_list
inline ParseFunctionOutputs<BaseClause::kTag> BaseClause::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<BaseClause::kTag>(false, output.last_token_,
                                            output.cur_token_),
      ParseFunction<BaseClause::kTag>::create_single_token_check(
          false, token::tok::TokenKind::colon,
          diag::DiagKind::base_clause_expect_colon),
      BaseSpecifierList(false));
  return serial_funcs();
}

// base-specifier-list:
// 	base_specifier, `...`[opt]
// 	base_specifier_list, `,`, base_specifier, `...`[opt]
inline ParseFunctionOutputs<BaseSpecifierList::kTag>
BaseSpecifierList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        BaseSpecifierList::kTag, 1,
        SerialParseFunctions<BaseSpecifierList::kTag, BaseSpecifier,
                             ParseFunction<BaseSpecifierList::kTag>>>
        parallel_funcs_0(
            ParseFunctionInputs<BaseSpecifierList::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<BaseSpecifierList::kTag>(false),
                BaseSpecifier(false),
                ParseFunction<BaseSpecifierList::kTag>::
                    create_single_token_check(
                        true, token::tok::TokenKind::ellipsis,
                        diag::DiagKind::base_specifier_list_expect_ellipsis)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      BaseSpecifierList::kTag, 1,
      SerialParseFunctions<BaseSpecifierList::kTag, BaseSpecifierList,
                           ParseFunction<BaseSpecifierList::kTag>,
                           BaseSpecifier,
                           ParseFunction<BaseSpecifierList::kTag>>>
      parallel_funcs_1(
          ParseFunctionInputs<BaseSpecifierList::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<BaseSpecifierList::kTag>(false),
              BaseSpecifierList(false),
              ParseFunction<BaseSpecifierList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::comma,
                  diag::DiagKind::base_specifier_list_expect_comma),
              BaseSpecifier(false),
              ParseFunction<BaseSpecifierList::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::base_specifier_list_expect_ellipsis)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// base-specifier:
// 	attribute_specifier_seq[opt], class_or_decltype
// 	attribute_specifier_seq[opt], `virtual`, access_specifier[opt], class_or_decltype
// 	attribute_specifier_seq[opt], access_specifier, `virtual`[opt], class_or_decltype
inline ParseFunctionOutputs<BaseSpecifier::kTag> BaseSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      BaseSpecifier::kTag, 3,
      SerialParseFunctions<BaseSpecifier::kTag, AttributeSpecifierSeq,
                           ClassOrDecltype>,
      SerialParseFunctions<BaseSpecifier::kTag, AttributeSpecifierSeq,
                           ParseFunction<BaseSpecifier::kTag>, AccessSpecifier,
                           ClassOrDecltype>,
      SerialParseFunctions<BaseSpecifier::kTag, AttributeSpecifierSeq,
                           AccessSpecifier, ParseFunction<BaseSpecifier::kTag>,
                           ClassOrDecltype>>
      parallel_funcs_0(
          ParseFunctionInputs<BaseSpecifier::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<BaseSpecifier::kTag>(false),
                               AttributeSpecifierSeq(true),
                               ClassOrDecltype(false)),
          SerialParseFunctions(
              ParseFunctionInputs<BaseSpecifier::kTag>(false),
              AttributeSpecifierSeq(true),
              ParseFunction<BaseSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_virtual,
                  diag::DiagKind::base_specifier_expect_kw_virtual),
              AccessSpecifier(true), ClassOrDecltype(false)),
          SerialParseFunctions(
              ParseFunctionInputs<BaseSpecifier::kTag>(false),
              AttributeSpecifierSeq(true), AccessSpecifier(false),
              ParseFunction<BaseSpecifier::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::kw_virtual,
                  diag::DiagKind::base_specifier_expect_kw_virtual),
              ClassOrDecltype(false)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// class-or-decltype:
// 	nested_name_specifier[opt], type_name
// 	nested_name_specifier, `template`, simple_template_id
// 	decltype_specifier
inline ParseFunctionOutputs<ClassOrDecltype::kTag>
ClassOrDecltype::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ClassOrDecltype::kTag, 3,
      SerialParseFunctions<ClassOrDecltype::kTag, NestedNameSpecifier,
                           TypeName>,
      SerialParseFunctions<ClassOrDecltype::kTag, NestedNameSpecifier,
                           ParseFunction<ClassOrDecltype::kTag>,
                           SimpleTemplateId>,
      SerialParseFunctions<ClassOrDecltype::kTag, DecltypeSpecifier>>
      parallel_funcs_0(
          ParseFunctionInputs<ClassOrDecltype::kTag>(false, output.last_token_,
                                                     output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ClassOrDecltype::kTag>(false),
              NestedNameSpecifier(true), TypeName(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ClassOrDecltype::kTag>(false),
              NestedNameSpecifier(false),
              ParseFunction<ClassOrDecltype::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_template,
                  diag::DiagKind::class_or_decltype_expect_kw_template),
              SimpleTemplateId(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ClassOrDecltype::kTag>(false),
              DecltypeSpecifier(false)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// access-specifier:
// 	`private`
// 	`protected`
// 	`public`
inline ParseFunctionOutputs<AccessSpecifier::kTag>
AccessSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      AccessSpecifier::kTag, 3,
      SerialParseFunctions<AccessSpecifier::kTag,
                           ParseFunction<AccessSpecifier::kTag>>,
      SerialParseFunctions<AccessSpecifier::kTag,
                           ParseFunction<AccessSpecifier::kTag>>,
      SerialParseFunctions<AccessSpecifier::kTag,
                           ParseFunction<AccessSpecifier::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<AccessSpecifier::kTag>(false, output.last_token_,
                                                     output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<AccessSpecifier::kTag>(false),
              ParseFunction<AccessSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_private,
                  diag::DiagKind::access_specifier_expect_kw_private)),
          SerialParseFunctions(
              ParseFunctionInputs<AccessSpecifier::kTag>(false),
              ParseFunction<AccessSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_protected,
                  diag::DiagKind::access_specifier_expect_kw_protected)),
          SerialParseFunctions(
              ParseFunctionInputs<AccessSpecifier::kTag>(false),
              ParseFunction<AccessSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_public,
                  diag::DiagKind::access_specifier_expect_kw_public)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// ctor-initializer:
// 	`:`, mem_initializer_list
inline ParseFunctionOutputs<CtorInitializer::kTag>
CtorInitializer::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<CtorInitializer::kTag>(false, output.last_token_,
                                                 output.cur_token_),
      ParseFunction<CtorInitializer::kTag>::create_single_token_check(
          false, token::tok::TokenKind::colon,
          diag::DiagKind::ctor_initializer_expect_colon),
      MemInitializerList(false));
  return serial_funcs();
}

// mem-initializer-list:
// 	mem_initializer, `...`[opt]
// 	mem_initializer_list, mem_initializer, `...`[opt]
inline ParseFunctionOutputs<MemInitializerList::kTag>
MemInitializerList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        MemInitializerList::kTag, 1,
        SerialParseFunctions<MemInitializerList::kTag, MemInitializer,
                             ParseFunction<MemInitializerList::kTag>>>
        parallel_funcs_0(
            ParseFunctionInputs<MemInitializerList::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<MemInitializerList::kTag>(false),
                MemInitializer(false),
                ParseFunction<MemInitializerList::kTag>::
                    create_single_token_check(
                        true, token::tok::TokenKind::ellipsis,
                        diag::DiagKind::mem_initializer_list_expect_ellipsis)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      MemInitializerList::kTag, 1,
      SerialParseFunctions<MemInitializerList::kTag, MemInitializerList,
                           MemInitializer,
                           ParseFunction<MemInitializerList::kTag>>>
      parallel_funcs_1(
          ParseFunctionInputs<MemInitializerList::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<MemInitializerList::kTag>(false),
              MemInitializerList(false), MemInitializer(false),
              ParseFunction<MemInitializerList::kTag>::
                  create_single_token_check(
                      true, token::tok::TokenKind::ellipsis,
                      diag::DiagKind::mem_initializer_list_expect_ellipsis)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// mem-initializer:
// 	mem_initializer_id, `(`, expression_list[opt], `)`
// 	mem_initializer_id, braced_init_list
inline ParseFunctionOutputs<MemInitializer::kTag> MemInitializer::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      MemInitializer::kTag, 2,
      SerialParseFunctions<MemInitializer::kTag, MemInitializerId,
                           ParseFunction<MemInitializer::kTag>, ExpressionList,
                           ParseFunction<MemInitializer::kTag>>,
      SerialParseFunctions<MemInitializer::kTag, MemInitializerId,
                           BracedInitList>>
      parallel_funcs_0(
          ParseFunctionInputs<MemInitializer::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<MemInitializer::kTag>(false),
              MemInitializerId(false),
              ParseFunction<MemInitializer::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::mem_initializer_expect_l_paren),
              ExpressionList(true),
              ParseFunction<MemInitializer::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::mem_initializer_expect_r_paren)),
          SerialParseFunctions(ParseFunctionInputs<MemInitializer::kTag>(false),
                               MemInitializerId(false), BracedInitList(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// mem-initializer-id:
// 	class_or_decltype
// 	`identifier`
inline ParseFunctionOutputs<MemInitializerId::kTag>
MemInitializerId::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      MemInitializerId::kTag, 2,
      SerialParseFunctions<MemInitializerId::kTag, ClassOrDecltype>,
      SerialParseFunctions<MemInitializerId::kTag,
                           ParseFunction<MemInitializerId::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<MemInitializerId::kTag>(false, output.last_token_,
                                                      output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<MemInitializerId::kTag>(false),
              ClassOrDecltype(false)),
          SerialParseFunctions(
              ParseFunctionInputs<MemInitializerId::kTag>(false),
              ParseFunction<MemInitializerId::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::mem_initializer_id_expect_identifier)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// operator-function-id:
// 	`operator`, the_operator
inline ParseFunctionOutputs<OperatorFunctionId::kTag>
OperatorFunctionId::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<OperatorFunctionId::kTag>(false, output.last_token_,
                                                    output.cur_token_),
      ParseFunction<OperatorFunctionId::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_operator,
          diag::DiagKind::operator_function_id_expect_kw_operator),
      TheOperator(false));
  return serial_funcs();
}

// the_operator:
// 	`new`
// 	`delete`
// 	`new[]`
// 	`delete[]`, `co_await`
// 	`()`
// 	`[]`
// 	`_>`
// 	`_>*`
// 	`~`
// 	`!`
// 	`+`
// 	`_`
// 	`*`
// 	`/`
// 	`%`
// 	`^`
// 	`&`
// 	`|`
// 	`=`
// 	`+=`
// 	`_=`
// 	`*=`
// 	`/=`
// 	`%=`
// 	`^=`
// 	`&=`
// 	`|=`
// 	`==`
// 	`!=`
// 	`<`
// 	`>`
// 	`<=`
// 	`>=`
// 	`<=>`
// 	`&&`
// 	`||`
// 	`<<`
// 	`>>`
// 	`<<=`
// 	`>>=`
// 	`++`
// 	`__`
// 	`,`
inline ParseFunctionOutputs<TheOperator::kTag> TheOperator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(TheOperator::kTag);
  return output;
}

// literal-operator-id:
// 	`operator`, string_literal, `identifier`
// 	`operator`, user_defined_string_literal
inline ParseFunctionOutputs<LiteralOperatorId::kTag>
LiteralOperatorId::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      LiteralOperatorId::kTag, 2,
      SerialParseFunctions<
          LiteralOperatorId::kTag, ParseFunction<LiteralOperatorId::kTag>,
          StringLiteral, ParseFunction<LiteralOperatorId::kTag>>,
      SerialParseFunctions<LiteralOperatorId::kTag,
                           ParseFunction<LiteralOperatorId::kTag>,
                           UserDefinedStringLiteral>>
      parallel_funcs_0(
          ParseFunctionInputs<LiteralOperatorId::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<LiteralOperatorId::kTag>(false),
              ParseFunction<LiteralOperatorId::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_operator,
                  diag::DiagKind::literal_operator_id_expect_kw_operator),
              StringLiteral(false),
              ParseFunction<LiteralOperatorId::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::literal_operator_id_expect_identifier)),
          SerialParseFunctions(
              ParseFunctionInputs<LiteralOperatorId::kTag>(false),
              ParseFunction<LiteralOperatorId::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_operator,
                  diag::DiagKind::literal_operator_id_expect_kw_operator),
              UserDefinedStringLiteral(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// template-declaration:
// 	template_head, declaration
// 	template_head, concept_definition
inline ParseFunctionOutputs<TemplateDeclaration::kTag>
TemplateDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<TemplateDeclaration::kTag, 2,
                         SerialParseFunctions<TemplateDeclaration::kTag,
                                              TemplateHead, Declaration>,
                         SerialParseFunctions<TemplateDeclaration::kTag,
                                              TemplateHead, ConceptDefinition>>
      parallel_funcs_0(
          ParseFunctionInputs<TemplateDeclaration::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<TemplateDeclaration::kTag>(false),
              TemplateHead(false), Declaration(false)),
          SerialParseFunctions(
              ParseFunctionInputs<TemplateDeclaration::kTag>(false),
              TemplateHead(false), ConceptDefinition(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// template-head:
// 	`template`, `<`, template_parameter_list, `>`, requires_clause
inline ParseFunctionOutputs<TemplateHead::kTag> TemplateHead::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<TemplateHead::kTag>(false, output.last_token_,
                                              output.cur_token_),
      ParseFunction<TemplateHead::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_template,
          diag::DiagKind::template_head_expect_kw_template),
      ParseFunction<TemplateHead::kTag>::create_single_token_check(
          false, token::tok::TokenKind::less,
          diag::DiagKind::template_head_expect_less),
      TemplateParameterList(false),
      ParseFunction<TemplateHead::kTag>::create_single_token_check(
          false, token::tok::TokenKind::greater,
          diag::DiagKind::template_head_expect_greater),
      RequiresClause(false));
  return serial_funcs();
}

// template-parameter-list:
// 	template_parameter
// 	template_parameter_list, `,`, template_parameter
inline ParseFunctionOutputs<TemplateParameterList::kTag>
TemplateParameterList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        TemplateParameterList::kTag, 1,
        SerialParseFunctions<TemplateParameterList::kTag, TemplateParameter>>
        parallel_funcs_0(
            ParseFunctionInputs<TemplateParameterList::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<TemplateParameterList::kTag>(false),
                TemplateParameter(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      TemplateParameterList::kTag, 1,
      SerialParseFunctions<TemplateParameterList::kTag, TemplateParameterList,
                           ParseFunction<TemplateParameterList::kTag>,
                           TemplateParameter>>
      parallel_funcs_1(
          ParseFunctionInputs<TemplateParameterList::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<TemplateParameterList::kTag>(false),
              TemplateParameterList(false),
              ParseFunction<TemplateParameterList::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::comma,
                      diag::DiagKind::template_parameter_list_expect_comma),
              TemplateParameter(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// requires-clause:
// 	`requires`, constraint_logical_or_expression
inline ParseFunctionOutputs<RequiresClause::kTag> RequiresClause::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<RequiresClause::kTag>(false, output.last_token_,
                                                output.cur_token_),
      ParseFunction<RequiresClause::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_requires,
          diag::DiagKind::requires_clause_expect_kw_requires),
      ConstraintLogicalOrExpression(false));
  return serial_funcs();
}

// constraint-logical-or-expression:
// 	constraint_logical_and_expression
// 	constraint_logical_or_expression, `||`, constraint_logical_and_expression
inline ParseFunctionOutputs<ConstraintLogicalOrExpression::kTag>
ConstraintLogicalOrExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        ConstraintLogicalOrExpression::kTag, 1,
        SerialParseFunctions<ConstraintLogicalOrExpression::kTag,
                             ConstraintLogicalAndExpression>>
        parallel_funcs_0(
            ParseFunctionInputs<ConstraintLogicalOrExpression::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<ConstraintLogicalOrExpression::kTag>(false),
                ConstraintLogicalAndExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      ConstraintLogicalOrExpression::kTag, 1,
      SerialParseFunctions<ConstraintLogicalOrExpression::kTag,
                           ConstraintLogicalOrExpression,
                           ParseFunction<ConstraintLogicalOrExpression::kTag>,
                           ConstraintLogicalAndExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<ConstraintLogicalOrExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ConstraintLogicalOrExpression::kTag>(false),
              ConstraintLogicalOrExpression(false),
              ParseFunction<ConstraintLogicalOrExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::pipepipe,
                      diag::DiagKind::
                          constraint_logical_or_expression_expect_pipepipe),
              ConstraintLogicalAndExpression(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// constraint-logical-and-expression:
// 	primary_expression
// 	constraint_logical_and_expression, `&&`, primary_expression
inline ParseFunctionOutputs<ConstraintLogicalAndExpression::kTag>
ConstraintLogicalAndExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        ConstraintLogicalAndExpression::kTag, 1,
        SerialParseFunctions<ConstraintLogicalAndExpression::kTag,
                             PrimaryExpression>>
        parallel_funcs_0(
            ParseFunctionInputs<ConstraintLogicalAndExpression::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<ConstraintLogicalAndExpression::kTag>(
                    false),
                PrimaryExpression(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      ConstraintLogicalAndExpression::kTag, 1,
      SerialParseFunctions<ConstraintLogicalAndExpression::kTag,
                           ConstraintLogicalAndExpression,
                           ParseFunction<ConstraintLogicalAndExpression::kTag>,
                           PrimaryExpression>>
      parallel_funcs_1(
          ParseFunctionInputs<ConstraintLogicalAndExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ConstraintLogicalAndExpression::kTag>(false),
              ConstraintLogicalAndExpression(false),
              ParseFunction<ConstraintLogicalAndExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::ampamp,
                      diag::DiagKind::
                          constraint_logical_and_expression_expect_ampamp),
              PrimaryExpression(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// template-parameter:
// 	type_parameter
// 	parameter_declaration
inline ParseFunctionOutputs<TemplateParameter::kTag>
TemplateParameter::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      TemplateParameter::kTag, 2,
      SerialParseFunctions<TemplateParameter::kTag, TypeParameter>,
      SerialParseFunctions<TemplateParameter::kTag, ParameterDeclaration>>
      parallel_funcs_0(ParseFunctionInputs<TemplateParameter::kTag>(
                           false, output.last_token_, output.cur_token_),
                       SerialParseFunctions(
                           ParseFunctionInputs<TemplateParameter::kTag>(false),
                           TypeParameter(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<TemplateParameter::kTag>(false),
                           ParameterDeclaration(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// type-parameter:
// 	type_parameter_key, `...`[opt], `identifier`[opt]
// 	type_parameter_key, `identifier`[opt], `=`, type_id
// 	type_constraint, `...`[opt], `identifier`[opt]
// 	type_constraint, `identifier`[opt], `=`, type_id
// 	template_head, type_parameter_key, `...`[opt], `identifier`[opt]
// 	template_head, type_parameter_key, `identifier`[opt], `=`, id_expression
inline ParseFunctionOutputs<TypeParameter::kTag> TypeParameter::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      TypeParameter::kTag, 6,
      SerialParseFunctions<TypeParameter::kTag, TypeParameterKey,
                           ParseFunction<TypeParameter::kTag>,
                           ParseFunction<TypeParameter::kTag>>,
      SerialParseFunctions<TypeParameter::kTag, TypeParameterKey,
                           ParseFunction<TypeParameter::kTag>,
                           ParseFunction<TypeParameter::kTag>, TypeId>,
      SerialParseFunctions<TypeParameter::kTag, TypeConstraint,
                           ParseFunction<TypeParameter::kTag>,
                           ParseFunction<TypeParameter::kTag>>,
      SerialParseFunctions<TypeParameter::kTag, TypeConstraint,
                           ParseFunction<TypeParameter::kTag>,
                           ParseFunction<TypeParameter::kTag>, TypeId>,
      SerialParseFunctions<TypeParameter::kTag, TemplateHead, TypeParameterKey,
                           ParseFunction<TypeParameter::kTag>,
                           ParseFunction<TypeParameter::kTag>>,
      SerialParseFunctions<TypeParameter::kTag, TemplateHead, TypeParameterKey,
                           ParseFunction<TypeParameter::kTag>,
                           ParseFunction<TypeParameter::kTag>, IdExpression>>
      parallel_funcs_0(
          ParseFunctionInputs<TypeParameter::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<TypeParameter::kTag>(false),
              TypeParameterKey(false),
              ParseFunction<TypeParameter::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::type_parameter_expect_ellipsis),
              ParseFunction<TypeParameter::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::identifier,
                  diag::DiagKind::type_parameter_expect_identifier)),
          SerialParseFunctions(
              ParseFunctionInputs<TypeParameter::kTag>(false),
              TypeParameterKey(false),
              ParseFunction<TypeParameter::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::identifier,
                  diag::DiagKind::type_parameter_expect_identifier),
              ParseFunction<TypeParameter::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::equal,
                  diag::DiagKind::type_parameter_expect_equal),
              TypeId(false)),
          SerialParseFunctions(
              ParseFunctionInputs<TypeParameter::kTag>(false),
              TypeConstraint(false),
              ParseFunction<TypeParameter::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::type_parameter_expect_ellipsis),
              ParseFunction<TypeParameter::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::identifier,
                  diag::DiagKind::type_parameter_expect_identifier)),
          SerialParseFunctions(
              ParseFunctionInputs<TypeParameter::kTag>(false),
              TypeConstraint(false),
              ParseFunction<TypeParameter::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::identifier,
                  diag::DiagKind::type_parameter_expect_identifier),
              ParseFunction<TypeParameter::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::equal,
                  diag::DiagKind::type_parameter_expect_equal),
              TypeId(false)),
          SerialParseFunctions(
              ParseFunctionInputs<TypeParameter::kTag>(false),
              TemplateHead(false), TypeParameterKey(false),
              ParseFunction<TypeParameter::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::type_parameter_expect_ellipsis),
              ParseFunction<TypeParameter::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::identifier,
                  diag::DiagKind::type_parameter_expect_identifier)),
          SerialParseFunctions(
              ParseFunctionInputs<TypeParameter::kTag>(false),
              TemplateHead(false), TypeParameterKey(false),
              ParseFunction<TypeParameter::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::identifier,
                  diag::DiagKind::type_parameter_expect_identifier),
              ParseFunction<TypeParameter::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::equal,
                  diag::DiagKind::type_parameter_expect_equal),
              IdExpression(false)));

  static_assert(base::kNumberOfElements >= 6);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// type-parameter-key:
// 	`class`
// 	`typename`
inline ParseFunctionOutputs<TypeParameterKey::kTag>
TypeParameterKey::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      TypeParameterKey::kTag, 2,
      SerialParseFunctions<TypeParameterKey::kTag,
                           ParseFunction<TypeParameterKey::kTag>>,
      SerialParseFunctions<TypeParameterKey::kTag,
                           ParseFunction<TypeParameterKey::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<TypeParameterKey::kTag>(false, output.last_token_,
                                                      output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<TypeParameterKey::kTag>(false),
              ParseFunction<TypeParameterKey::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_class,
                  diag::DiagKind::type_parameter_key_expect_kw_class)),
          SerialParseFunctions(
              ParseFunctionInputs<TypeParameterKey::kTag>(false),
              ParseFunction<TypeParameterKey::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_typename,
                  diag::DiagKind::type_parameter_key_expect_kw_typename)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// type-constraint:
// 	nested_name_specifier[opt], concept_name
// 	nested_name_specifier[opt], concept_name, `<`, template_argument_list[opt], `>`
inline ParseFunctionOutputs<TypeConstraint::kTag> TypeConstraint::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      TypeConstraint::kTag, 2,
      SerialParseFunctions<TypeConstraint::kTag, NestedNameSpecifier,
                           ConceptName>,
      SerialParseFunctions<TypeConstraint::kTag, NestedNameSpecifier,
                           ConceptName, ParseFunction<TypeConstraint::kTag>,
                           TemplateArgumentList,
                           ParseFunction<TypeConstraint::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<TypeConstraint::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<TypeConstraint::kTag>(false),
                               NestedNameSpecifier(true), ConceptName(false)),
          SerialParseFunctions(
              ParseFunctionInputs<TypeConstraint::kTag>(false),
              NestedNameSpecifier(true), ConceptName(false),
              ParseFunction<TypeConstraint::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::less,
                  diag::DiagKind::type_constraint_expect_less),
              TemplateArgumentList(true),
              ParseFunction<TypeConstraint::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::greater,
                  diag::DiagKind::type_constraint_expect_greater)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// simple-template-id:
// 	template_name, `<`, template_argument_list[opt], `>`
inline ParseFunctionOutputs<SimpleTemplateId::kTag>
SimpleTemplateId::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<SimpleTemplateId::kTag>(false, output.last_token_,
                                                  output.cur_token_),
      TemplateName(false),
      ParseFunction<SimpleTemplateId::kTag>::create_single_token_check(
          false, token::tok::TokenKind::less,
          diag::DiagKind::simple_template_id_expect_less),
      TemplateArgumentList(true),
      ParseFunction<SimpleTemplateId::kTag>::create_single_token_check(
          false, token::tok::TokenKind::greater,
          diag::DiagKind::simple_template_id_expect_greater));
  return serial_funcs();
}

// template-id:
// 	simple_template_id
// 	operator_function_id, `<`, template_argument_list[opt], `>`
// 	literal_operator_id, `<`, template_argument_list[opt], `>`
inline ParseFunctionOutputs<TemplateId::kTag> TemplateId::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      TemplateId::kTag, 3,
      SerialParseFunctions<TemplateId::kTag, SimpleTemplateId>,
      SerialParseFunctions<
          TemplateId::kTag, OperatorFunctionId, ParseFunction<TemplateId::kTag>,
          TemplateArgumentList, ParseFunction<TemplateId::kTag>>,
      SerialParseFunctions<
          TemplateId::kTag, LiteralOperatorId, ParseFunction<TemplateId::kTag>,
          TemplateArgumentList, ParseFunction<TemplateId::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<TemplateId::kTag>(false, output.last_token_,
                                                output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<TemplateId::kTag>(false),
                               SimpleTemplateId(false)),
          SerialParseFunctions(
              ParseFunctionInputs<TemplateId::kTag>(false),
              OperatorFunctionId(false),
              ParseFunction<TemplateId::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::less,
                  diag::DiagKind::template_id_expect_less),
              TemplateArgumentList(true),
              ParseFunction<TemplateId::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::greater,
                  diag::DiagKind::template_id_expect_greater)),
          SerialParseFunctions(
              ParseFunctionInputs<TemplateId::kTag>(false),
              LiteralOperatorId(false),
              ParseFunction<TemplateId::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::less,
                  diag::DiagKind::template_id_expect_less),
              TemplateArgumentList(true),
              ParseFunction<TemplateId::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::greater,
                  diag::DiagKind::template_id_expect_greater)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// template-name:
// 	`identifier`
inline ParseFunctionOutputs<TemplateName::kTag> TemplateName::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<TemplateName::kTag>(false, output.last_token_,
                                              output.cur_token_),
      ParseFunction<TemplateName::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::template_name_expect_identifier));
  return serial_funcs();
}

// template-argument-list:
// 	template_argument, `...`[opt]
// 	template_argument_list, `,`, template_argument, `...`[opt]
inline ParseFunctionOutputs<TemplateArgumentList::kTag>
TemplateArgumentList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        TemplateArgumentList::kTag, 1,
        SerialParseFunctions<TemplateArgumentList::kTag, TemplateArgument,
                             ParseFunction<TemplateArgumentList::kTag>>>
        parallel_funcs_0(
            ParseFunctionInputs<TemplateArgumentList::kTag>(
                false, output.last_token_, output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<TemplateArgumentList::kTag>(false),
                TemplateArgument(false),
                ParseFunction<TemplateArgumentList::kTag>::
                    create_single_token_check(
                        true, token::tok::TokenKind::ellipsis,
                        diag::DiagKind::
                            template_argument_list_expect_ellipsis)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      TemplateArgumentList::kTag, 1,
      SerialParseFunctions<TemplateArgumentList::kTag, TemplateArgumentList,
                           ParseFunction<TemplateArgumentList::kTag>,
                           TemplateArgument,
                           ParseFunction<TemplateArgumentList::kTag>>>
      parallel_funcs_1(
          ParseFunctionInputs<TemplateArgumentList::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<TemplateArgumentList::kTag>(false),
              TemplateArgumentList(false),
              ParseFunction<TemplateArgumentList::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::comma,
                      diag::DiagKind::template_argument_list_expect_comma),
              TemplateArgument(false),
              ParseFunction<TemplateArgumentList::kTag>::
                  create_single_token_check(
                      true, token::tok::TokenKind::ellipsis,
                      diag::DiagKind::template_argument_list_expect_ellipsis)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// template-argument:
// 	constant_expression
// 	type_id
// 	id_expression
inline ParseFunctionOutputs<TemplateArgument::kTag>
TemplateArgument::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      TemplateArgument::kTag, 3,
      SerialParseFunctions<TemplateArgument::kTag, ConstantExpression>,
      SerialParseFunctions<TemplateArgument::kTag, TypeId>,
      SerialParseFunctions<TemplateArgument::kTag, IdExpression>>
      parallel_funcs_0(ParseFunctionInputs<TemplateArgument::kTag>(
                           false, output.last_token_, output.cur_token_),
                       SerialParseFunctions(
                           ParseFunctionInputs<TemplateArgument::kTag>(false),
                           ConstantExpression(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<TemplateArgument::kTag>(false),
                           TypeId(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<TemplateArgument::kTag>(false),
                           IdExpression(false)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// constraint-expression:
// 	logical_or_expression
inline ParseFunctionOutputs<ConstraintExpression::kTag>
ConstraintExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ConstraintExpression::kTag>(false, output.last_token_,
                                                      output.cur_token_),
      LogicalOrExpression(false));
  return serial_funcs();
}

// deduction-guide:
// 	explicit_specifier[opt], template_name, `(`, parameter_declaration_clause, `)`, `_>`, simple_template_id, `;`
inline ParseFunctionOutputs<DeductionGuide::kTag> DeductionGuide::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<DeductionGuide::kTag>(false, output.last_token_,
                                                output.cur_token_),
      ExplicitSpecifier(true), TemplateName(false),
      ParseFunction<DeductionGuide::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_paren,
          diag::DiagKind::deduction_guide_expect_l_paren),
      ParameterDeclarationClause(false),
      ParseFunction<DeductionGuide::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_paren,
          diag::DiagKind::deduction_guide_expect_r_paren),
      ParseFunction<DeductionGuide::kTag>::create_single_token_check(
          false, token::tok::TokenKind::arrow,
          diag::DiagKind::deduction_guide_expect_arrow),
      SimpleTemplateId(false),
      ParseFunction<DeductionGuide::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::deduction_guide_expect_semi));
  return serial_funcs();
}

// concept-definition:
// 	`concept`, concept_name, `=`, constraint_expression, `;`
inline ParseFunctionOutputs<ConceptDefinition::kTag>
ConceptDefinition::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ConceptDefinition::kTag>(false, output.last_token_,
                                                   output.cur_token_),
      ParseFunction<ConceptDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_concept,
          diag::DiagKind::concept_definition_expect_kw_concept),
      ConceptName(false),
      ParseFunction<ConceptDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::equal,
          diag::DiagKind::concept_definition_expect_equal),
      ConstraintExpression(false),
      ParseFunction<ConceptDefinition::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::concept_definition_expect_semi));
  return serial_funcs();
}

// concept-name:
// 	`identifier`
inline ParseFunctionOutputs<ConceptName::kTag> ConceptName::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ConceptName::kTag>(false, output.last_token_,
                                             output.cur_token_),
      ParseFunction<ConceptName::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::concept_name_expect_identifier));
  return serial_funcs();
}

// typename-specifier:
// 	`typename`, nested_name_specifier, `identifier`
// 	`typename`, nested_name_specifier, `template`[opt], simple_template_id
inline ParseFunctionOutputs<TypenameSpecifier::kTag>
TypenameSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      TypenameSpecifier::kTag, 2,
      SerialParseFunctions<
          TypenameSpecifier::kTag, ParseFunction<TypenameSpecifier::kTag>,
          NestedNameSpecifier, ParseFunction<TypenameSpecifier::kTag>>,
      SerialParseFunctions<
          TypenameSpecifier::kTag, ParseFunction<TypenameSpecifier::kTag>,
          NestedNameSpecifier, ParseFunction<TypenameSpecifier::kTag>,
          SimpleTemplateId>>
      parallel_funcs_0(
          ParseFunctionInputs<TypenameSpecifier::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<TypenameSpecifier::kTag>(false),
              ParseFunction<TypenameSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_typename,
                  diag::DiagKind::typename_specifier_expect_kw_typename),
              NestedNameSpecifier(false),
              ParseFunction<TypenameSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::typename_specifier_expect_identifier)),
          SerialParseFunctions(
              ParseFunctionInputs<TypenameSpecifier::kTag>(false),
              ParseFunction<TypenameSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_typename,
                  diag::DiagKind::typename_specifier_expect_kw_typename),
              NestedNameSpecifier(false),
              ParseFunction<TypenameSpecifier::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::kw_template,
                  diag::DiagKind::typename_specifier_expect_kw_template),
              SimpleTemplateId(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// explicit-instantiation:
// 	`extern`[opt], `template`, declaration
inline ParseFunctionOutputs<ExplicitInstantiation::kTag>
ExplicitInstantiation::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ExplicitInstantiation::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<ExplicitInstantiation::kTag>::create_single_token_check(
          true, token::tok::TokenKind::kw_extern,
          diag::DiagKind::explicit_instantiation_expect_kw_extern),
      ParseFunction<ExplicitInstantiation::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_template,
          diag::DiagKind::explicit_instantiation_expect_kw_template),
      Declaration(false));
  return serial_funcs();
}

// explicit-specialization:
// 	`template`, `<`, `>`, declaration
inline ParseFunctionOutputs<ExplicitSpecialization::kTag>
ExplicitSpecialization::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ExplicitSpecialization::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<ExplicitSpecialization::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_template,
          diag::DiagKind::explicit_specialization_expect_kw_template),
      ParseFunction<ExplicitSpecialization::kTag>::create_single_token_check(
          false, token::tok::TokenKind::less,
          diag::DiagKind::explicit_specialization_expect_less),
      ParseFunction<ExplicitSpecialization::kTag>::create_single_token_check(
          false, token::tok::TokenKind::greater,
          diag::DiagKind::explicit_specialization_expect_greater),
      Declaration(false));
  return serial_funcs();
}

// try-block:
// 	`try`, compound_statement, handler_seq
inline ParseFunctionOutputs<TryBlock::kTag> TryBlock::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<TryBlock::kTag>(false, output.last_token_,
                                          output.cur_token_),
      ParseFunction<TryBlock::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_try,
          diag::DiagKind::try_block_expect_kw_try),
      CompoundStatement(false), HandlerSeq(false));
  return serial_funcs();
}

// function-try-block:
// 	`try`, ctor_initializer[opt], compound_statement, handler_seq
inline ParseFunctionOutputs<FunctionTryBlock::kTag>
FunctionTryBlock::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<FunctionTryBlock::kTag>(false, output.last_token_,
                                                  output.cur_token_),
      ParseFunction<FunctionTryBlock::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_try,
          diag::DiagKind::function_try_block_expect_kw_try),
      CtorInitializer(true), CompoundStatement(false), HandlerSeq(false));
  return serial_funcs();
}

// handler-seq:
// 	handler, handler_seq[opt]
inline ParseFunctionOutputs<HandlerSeq::kTag> HandlerSeq::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<HandlerSeq::kTag>(false, output.last_token_,
                                            output.cur_token_),
      Handler(false), HandlerSeq(true));
  return serial_funcs();
}

// handler:
// 	`catch`, `(`, exception_declaration, `)`, compound_statement
inline ParseFunctionOutputs<Handler::kTag> Handler::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<Handler::kTag>(false, output.last_token_,
                                         output.cur_token_),
      ParseFunction<Handler::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_catch,
          diag::DiagKind::handler_expect_kw_catch),
      ParseFunction<Handler::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_paren,
          diag::DiagKind::handler_expect_l_paren),
      ExceptionDeclaration(false),
      ParseFunction<Handler::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_paren,
          diag::DiagKind::handler_expect_r_paren),
      CompoundStatement(false));
  return serial_funcs();
}

// exception-declaration:
// 	attribute_specifier_seq[opt], type_specifier_seq, declarator
// 	attribute_specifier_seq[opt], type_specifier_seq, abstract_declarator[opt]
// 	`...`
inline ParseFunctionOutputs<ExceptionDeclaration::kTag>
ExceptionDeclaration::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ExceptionDeclaration::kTag, 3,
      SerialParseFunctions<ExceptionDeclaration::kTag, AttributeSpecifierSeq,
                           TypeSpecifierSeq, Declarator>,
      SerialParseFunctions<ExceptionDeclaration::kTag, AttributeSpecifierSeq,
                           TypeSpecifierSeq, AbstractDeclarator>,
      SerialParseFunctions<ExceptionDeclaration::kTag,
                           ParseFunction<ExceptionDeclaration::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<ExceptionDeclaration::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ExceptionDeclaration::kTag>(false),
              AttributeSpecifierSeq(true), TypeSpecifierSeq(false),
              Declarator(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ExceptionDeclaration::kTag>(false),
              AttributeSpecifierSeq(true), TypeSpecifierSeq(false),
              AbstractDeclarator(true)),
          SerialParseFunctions(
              ParseFunctionInputs<ExceptionDeclaration::kTag>(false),
              ParseFunction<ExceptionDeclaration::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::ellipsis,
                      diag::DiagKind::exception_declaration_expect_ellipsis)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// noexcept-specifier:
// 	`noexcept`, `(`, constant_expression, `)`
// 	`noexcept`
inline ParseFunctionOutputs<NoexceptSpecifier::kTag>
NoexceptSpecifier::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      NoexceptSpecifier::kTag, 2,
      SerialParseFunctions<
          NoexceptSpecifier::kTag, ParseFunction<NoexceptSpecifier::kTag>,
          ParseFunction<NoexceptSpecifier::kTag>, ConstantExpression,
          ParseFunction<NoexceptSpecifier::kTag>>,
      SerialParseFunctions<NoexceptSpecifier::kTag,
                           ParseFunction<NoexceptSpecifier::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<NoexceptSpecifier::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<NoexceptSpecifier::kTag>(false),
              ParseFunction<NoexceptSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_noexcept,
                  diag::DiagKind::noexcept_specifier_expect_kw_noexcept),
              ParseFunction<NoexceptSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::l_paren,
                  diag::DiagKind::noexcept_specifier_expect_l_paren),
              ConstantExpression(false),
              ParseFunction<NoexceptSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::noexcept_specifier_expect_r_paren)),
          SerialParseFunctions(
              ParseFunctionInputs<NoexceptSpecifier::kTag>(false),
              ParseFunction<NoexceptSpecifier::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_noexcept,
                  diag::DiagKind::noexcept_specifier_expect_kw_noexcept)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// preprocessing-file:
// 	group[opt]
// 	module_file
inline ParseFunctionOutputs<PreprocessingFile::kTag>
PreprocessingFile::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      PreprocessingFile::kTag, 2,
      SerialParseFunctions<PreprocessingFile::kTag, Group>,
      SerialParseFunctions<PreprocessingFile::kTag, ModuleFile>>
      parallel_funcs_0(
          ParseFunctionInputs<PreprocessingFile::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingFile::kTag>(false), Group(true)),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingFile::kTag>(false),
              ModuleFile(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// module-file:
// 	pp_global_module_fragment[opt], pp_module, group[opt], pp_private_module_fragment[opt]
inline ParseFunctionOutputs<ModuleFile::kTag> ModuleFile::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ModuleFile::kTag>(false, output.last_token_,
                                            output.cur_token_),
      PpGlobalModuleFragment(true), PpModule(false), Group(true),
      PpPrivateModuleFragment(true));
  return serial_funcs();
}

// pp-global-module-fragment:
// 	`module`, `;`, new_line, group[opt]
inline ParseFunctionOutputs<PpGlobalModuleFragment::kTag>
PpGlobalModuleFragment::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<PpGlobalModuleFragment::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<PpGlobalModuleFragment::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_module,
          diag::DiagKind::pp_global_module_fragment_expect_kw_module),
      ParseFunction<PpGlobalModuleFragment::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::pp_global_module_fragment_expect_semi),
      NewLine(false), Group(true));
  return serial_funcs();
}

// pp-private-module-fragment:
// 	`module`, `:`, `private`, `;`, new_line, group[opt]
inline ParseFunctionOutputs<PpPrivateModuleFragment::kTag>
PpPrivateModuleFragment::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<PpPrivateModuleFragment::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<PpPrivateModuleFragment::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_module,
          diag::DiagKind::pp_private_module_fragment_expect_kw_module),
      ParseFunction<PpPrivateModuleFragment::kTag>::create_single_token_check(
          false, token::tok::TokenKind::colon,
          diag::DiagKind::pp_private_module_fragment_expect_colon),
      ParseFunction<PpPrivateModuleFragment::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_private,
          diag::DiagKind::pp_private_module_fragment_expect_kw_private),
      ParseFunction<PpPrivateModuleFragment::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::pp_private_module_fragment_expect_semi),
      NewLine(false), Group(true));
  return serial_funcs();
}

// group:
// 	group_part
// 	group, group_part
inline ParseFunctionOutputs<Group::kTag> Group::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<Group::kTag, 1,
                           SerialParseFunctions<Group::kTag, GroupPart>>
        parallel_funcs_0(
            ParseFunctionInputs<Group::kTag>(false, output.last_token_,
                                             output.cur_token_),
            SerialParseFunctions(ParseFunctionInputs<Group::kTag>(false),
                                 GroupPart(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<Group::kTag, 1,
                         SerialParseFunctions<Group::kTag, Group, GroupPart>>
      parallel_funcs_1(
          ParseFunctionInputs<Group::kTag>(false, output.last_token_,
                                           output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<Group::kTag>(false),
                               Group(false), GroupPart(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// group-part:
// 	control_line
// 	if_section
// 	text_line
// 	`#`, conditionally_supported_directive
inline ParseFunctionOutputs<GroupPart::kTag> GroupPart::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      GroupPart::kTag, 4, SerialParseFunctions<GroupPart::kTag, ControlLine>,
      SerialParseFunctions<GroupPart::kTag, IfSection>,
      SerialParseFunctions<GroupPart::kTag, TextLine>,
      SerialParseFunctions<GroupPart::kTag, ParseFunction<GroupPart::kTag>,
                           ConditionallySupportedDirective>>
      parallel_funcs_0(
          ParseFunctionInputs<GroupPart::kTag>(false, output.last_token_,
                                               output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<GroupPart::kTag>(false),
                               ControlLine(false)),
          SerialParseFunctions(ParseFunctionInputs<GroupPart::kTag>(false),
                               IfSection(false)),
          SerialParseFunctions(ParseFunctionInputs<GroupPart::kTag>(false),
                               TextLine(false)),
          SerialParseFunctions(
              ParseFunctionInputs<GroupPart::kTag>(false),
              ParseFunction<GroupPart::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::hash,
                  diag::DiagKind::group_part_expect_hash),
              ConditionallySupportedDirective(false)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// control-line:
// 	`#`, `include`, pp_tokens, new_line
// 	pp_import
// 	`#`, `define`, `identifier`, replacement_list, new_line
// 	`#`, `define`, `identifier`, lparen, identifier_list[opt], `)`, replacement_list, new_line
// 	`#`, `define`, `identifier`, lparen, `...`, `)`, replacement_list, new_line
// 	`#`, `define`, `identifier`, lparen, identifier_list, `...`, `)`, replacement_list, new_line
// 	`#`, `undef`, `identifier`, new_line
// 	`#`, `line`, pp_tokens, new_line
// 	`#`, `error`, pp_tokens[opt], new_line
// 	`#`, `pragma`, pp_tokens[opt], new_line
// 	`#`, new_line
inline ParseFunctionOutputs<ControlLine::kTag> ControlLine::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      ControlLine::kTag, 11,
      SerialParseFunctions<ControlLine::kTag, ParseFunction<ControlLine::kTag>,
                           ParseFunction<ControlLine::kTag>, PpTokens, NewLine>,
      SerialParseFunctions<ControlLine::kTag, PpImport>,
      SerialParseFunctions<ControlLine::kTag, ParseFunction<ControlLine::kTag>,
                           ParseFunction<ControlLine::kTag>,
                           ParseFunction<ControlLine::kTag>, ReplacementList,
                           NewLine>,
      SerialParseFunctions<ControlLine::kTag, ParseFunction<ControlLine::kTag>,
                           ParseFunction<ControlLine::kTag>,
                           ParseFunction<ControlLine::kTag>, Lparen,
                           IdentifierList, ParseFunction<ControlLine::kTag>,
                           ReplacementList, NewLine>,
      SerialParseFunctions<
          ControlLine::kTag, ParseFunction<ControlLine::kTag>,
          ParseFunction<ControlLine::kTag>, ParseFunction<ControlLine::kTag>,
          Lparen, ParseFunction<ControlLine::kTag>,
          ParseFunction<ControlLine::kTag>, ReplacementList, NewLine>,
      SerialParseFunctions<
          ControlLine::kTag, ParseFunction<ControlLine::kTag>,
          ParseFunction<ControlLine::kTag>, ParseFunction<ControlLine::kTag>,
          Lparen, IdentifierList, ParseFunction<ControlLine::kTag>,
          ParseFunction<ControlLine::kTag>, ReplacementList, NewLine>,
      SerialParseFunctions<ControlLine::kTag, ParseFunction<ControlLine::kTag>,
                           ParseFunction<ControlLine::kTag>,
                           ParseFunction<ControlLine::kTag>, NewLine>,
      SerialParseFunctions<ControlLine::kTag, ParseFunction<ControlLine::kTag>,
                           ParseFunction<ControlLine::kTag>, PpTokens, NewLine>,
      SerialParseFunctions<ControlLine::kTag, ParseFunction<ControlLine::kTag>,
                           ParseFunction<ControlLine::kTag>, PpTokens, NewLine>,
      SerialParseFunctions<ControlLine::kTag, ParseFunction<ControlLine::kTag>,
                           ParseFunction<ControlLine::kTag>, PpTokens, NewLine>,
      SerialParseFunctions<ControlLine::kTag, ParseFunction<ControlLine::kTag>,
                           NewLine>>
      parallel_funcs_0(
          ParseFunctionInputs<ControlLine::kTag>(false, output.last_token_,
                                                 output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<ControlLine::kTag>(false),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::hash,
                  diag::DiagKind::control_line_expect_hash),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_include,
                  diag::DiagKind::control_line_expect_kw_include),
              PpTokens(false), NewLine(false)),
          SerialParseFunctions(ParseFunctionInputs<ControlLine::kTag>(false),
                               PpImport(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ControlLine::kTag>(false),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::hash,
                  diag::DiagKind::control_line_expect_hash),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_define,
                  diag::DiagKind::control_line_expect_kw_define),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::control_line_expect_identifier),
              ReplacementList(false), NewLine(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ControlLine::kTag>(false),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::hash,
                  diag::DiagKind::control_line_expect_hash),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_define,
                  diag::DiagKind::control_line_expect_kw_define),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::control_line_expect_identifier),
              Lparen(false), IdentifierList(true),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::control_line_expect_r_paren),
              ReplacementList(false), NewLine(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ControlLine::kTag>(false),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::hash,
                  diag::DiagKind::control_line_expect_hash),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_define,
                  diag::DiagKind::control_line_expect_kw_define),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::control_line_expect_identifier),
              Lparen(false),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::control_line_expect_ellipsis),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::control_line_expect_r_paren),
              ReplacementList(false), NewLine(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ControlLine::kTag>(false),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::hash,
                  diag::DiagKind::control_line_expect_hash),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_define,
                  diag::DiagKind::control_line_expect_kw_define),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::control_line_expect_identifier),
              Lparen(false), IdentifierList(false),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::ellipsis,
                  diag::DiagKind::control_line_expect_ellipsis),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::r_paren,
                  diag::DiagKind::control_line_expect_r_paren),
              ReplacementList(false), NewLine(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ControlLine::kTag>(false),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::hash,
                  diag::DiagKind::control_line_expect_hash),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_undef,
                  diag::DiagKind::control_line_expect_kw_undef),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::control_line_expect_identifier),
              NewLine(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ControlLine::kTag>(false),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::hash,
                  diag::DiagKind::control_line_expect_hash),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_line,
                  diag::DiagKind::control_line_expect_kw_line),
              PpTokens(false), NewLine(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ControlLine::kTag>(false),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::hash,
                  diag::DiagKind::control_line_expect_hash),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_error,
                  diag::DiagKind::control_line_expect_kw_error),
              PpTokens(true), NewLine(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ControlLine::kTag>(false),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::hash,
                  diag::DiagKind::control_line_expect_hash),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_pragma,
                  diag::DiagKind::control_line_expect_kw_pragma),
              PpTokens(true), NewLine(false)),
          SerialParseFunctions(
              ParseFunctionInputs<ControlLine::kTag>(false),
              ParseFunction<ControlLine::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::hash,
                  diag::DiagKind::control_line_expect_hash),
              NewLine(false)));

  static_assert(base::kNumberOfElements >= 11);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// if-section:
// 	if_group, elif_groups[opt], else_group[opt], endif_line
inline ParseFunctionOutputs<IfSection::kTag> IfSection::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<IfSection::kTag>(false, output.last_token_,
                                           output.cur_token_),
      IfGroup(false), ElifGroups(true), ElseGroup(true), EndifLine(false));
  return serial_funcs();
}

// if-group:
// 	`#`, `if`, constant_expression, new_line, group[opt]
// 	`#`, `ifdef`, `identifier`, new_line, group[opt]
// 	`#`, `ifndef`, `identifier`, new_line, group[opt]
inline ParseFunctionOutputs<IfGroup::kTag> IfGroup::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      IfGroup::kTag, 3,
      SerialParseFunctions<IfGroup::kTag, ParseFunction<IfGroup::kTag>,
                           ParseFunction<IfGroup::kTag>, ConstantExpression,
                           NewLine, Group>,
      SerialParseFunctions<IfGroup::kTag, ParseFunction<IfGroup::kTag>,
                           ParseFunction<IfGroup::kTag>,
                           ParseFunction<IfGroup::kTag>, NewLine, Group>,
      SerialParseFunctions<IfGroup::kTag, ParseFunction<IfGroup::kTag>,
                           ParseFunction<IfGroup::kTag>,
                           ParseFunction<IfGroup::kTag>, NewLine, Group>>
      parallel_funcs_0(
          ParseFunctionInputs<IfGroup::kTag>(false, output.last_token_,
                                             output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<IfGroup::kTag>(false),
              ParseFunction<IfGroup::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::hash,
                  diag::DiagKind::if_group_expect_hash),
              ParseFunction<IfGroup::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_if,
                  diag::DiagKind::if_group_expect_kw_if),
              ConstantExpression(false), NewLine(false), Group(true)),
          SerialParseFunctions(
              ParseFunctionInputs<IfGroup::kTag>(false),
              ParseFunction<IfGroup::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::hash,
                  diag::DiagKind::if_group_expect_hash),
              ParseFunction<IfGroup::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_ifdef,
                  diag::DiagKind::if_group_expect_kw_ifdef),
              ParseFunction<IfGroup::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::if_group_expect_identifier),
              NewLine(false), Group(true)),
          SerialParseFunctions(
              ParseFunctionInputs<IfGroup::kTag>(false),
              ParseFunction<IfGroup::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::hash,
                  diag::DiagKind::if_group_expect_hash),
              ParseFunction<IfGroup::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_ifndef,
                  diag::DiagKind::if_group_expect_kw_ifndef),
              ParseFunction<IfGroup::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::if_group_expect_identifier),
              NewLine(false), Group(true)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// elif-groups:
// 	elif_group
// 	elif_groups, elif_group
inline ParseFunctionOutputs<ElifGroups::kTag> ElifGroups::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<ElifGroups::kTag, 1,
                           SerialParseFunctions<ElifGroups::kTag, ElifGroup>>
        parallel_funcs_0(
            ParseFunctionInputs<ElifGroups::kTag>(false, output.last_token_,
                                                  output.cur_token_),
            SerialParseFunctions(ParseFunctionInputs<ElifGroups::kTag>(false),
                                 ElifGroup(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      ElifGroups::kTag, 1,
      SerialParseFunctions<ElifGroups::kTag, ElifGroups, ElifGroup>>
      parallel_funcs_1(
          ParseFunctionInputs<ElifGroups::kTag>(false, output.last_token_,
                                                output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<ElifGroups::kTag>(false),
                               ElifGroups(false), ElifGroup(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// elif-group:
// 	`#`, `elif`, constant_expression, new_line, group[opt]
inline ParseFunctionOutputs<ElifGroup::kTag> ElifGroup::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ElifGroup::kTag>(false, output.last_token_,
                                           output.cur_token_),
      ParseFunction<ElifGroup::kTag>::create_single_token_check(
          false, token::tok::TokenKind::hash,
          diag::DiagKind::elif_group_expect_hash),
      ParseFunction<ElifGroup::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_elif,
          diag::DiagKind::elif_group_expect_kw_elif),
      ConstantExpression(false), NewLine(false), Group(true));
  return serial_funcs();
}

// else-group:
// 	`#`, `else`, new_line, group[opt]
inline ParseFunctionOutputs<ElseGroup::kTag> ElseGroup::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ElseGroup::kTag>(false, output.last_token_,
                                           output.cur_token_),
      ParseFunction<ElseGroup::kTag>::create_single_token_check(
          false, token::tok::TokenKind::hash,
          diag::DiagKind::else_group_expect_hash),
      ParseFunction<ElseGroup::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_else,
          diag::DiagKind::else_group_expect_kw_else),
      NewLine(false), Group(true));
  return serial_funcs();
}

// endif-line:
// 	`#`, `endif`, new_line
inline ParseFunctionOutputs<EndifLine::kTag> EndifLine::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<EndifLine::kTag>(false, output.last_token_,
                                           output.cur_token_),
      ParseFunction<EndifLine::kTag>::create_single_token_check(
          false, token::tok::TokenKind::hash,
          diag::DiagKind::endif_line_expect_hash),
      ParseFunction<EndifLine::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_endif,
          diag::DiagKind::endif_line_expect_kw_endif),
      NewLine(false));
  return serial_funcs();
}

// text-line:
// 	pp_tokens[opt], new_line
inline ParseFunctionOutputs<TextLine::kTag> TextLine::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<TextLine::kTag>(false, output.last_token_,
                                          output.cur_token_),
      PpTokens(true), NewLine(false));
  return serial_funcs();
}

// conditionally-supported-directive:
// 	pp_tokens, new_line
inline ParseFunctionOutputs<ConditionallySupportedDirective::kTag>
ConditionallySupportedDirective::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ConditionallySupportedDirective::kTag>(
          false, output.last_token_, output.cur_token_),
      PpTokens(false), NewLine(false));
  return serial_funcs();
}

// lparen:

inline ParseFunctionOutputs<Lparen::kTag> Lparen::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {
    unreachable(Lparen::kTag);
    return base::operator()();
  }
}

// identifier-list:
// 	`identifier`
// 	identifier_list, `,`, `identifier`
inline ParseFunctionOutputs<IdentifierList::kTag> IdentifierList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        IdentifierList::kTag, 1,
        SerialParseFunctions<IdentifierList::kTag,
                             ParseFunction<IdentifierList::kTag>>>
        parallel_funcs_0(
            ParseFunctionInputs<IdentifierList::kTag>(false, output.last_token_,
                                                      output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<IdentifierList::kTag>(false),
                ParseFunction<IdentifierList::kTag>::create_single_token_check(
                    false, token::tok::TokenKind::identifier,
                    diag::DiagKind::identifier_list_expect_identifier)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      IdentifierList::kTag, 1,
      SerialParseFunctions<IdentifierList::kTag, IdentifierList,
                           ParseFunction<IdentifierList::kTag>,
                           ParseFunction<IdentifierList::kTag>>>
      parallel_funcs_1(
          ParseFunctionInputs<IdentifierList::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<IdentifierList::kTag>(false),
              IdentifierList(false),
              ParseFunction<IdentifierList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::comma,
                  diag::DiagKind::identifier_list_expect_comma),
              ParseFunction<IdentifierList::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::identifier_list_expect_identifier)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// replacement-list:
// 	pp_tokens[opt]
inline ParseFunctionOutputs<ReplacementList::kTag>
ReplacementList::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<ReplacementList::kTag>(false, output.last_token_,
                                                 output.cur_token_),
      PpTokens(true));
  return serial_funcs();
}

// pp-tokens:
// 	preprocessing_token
// 	pp_tokens, preprocessing_token
inline ParseFunctionOutputs<PpTokens::kTag> PpTokens::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        PpTokens::kTag, 1,
        SerialParseFunctions<PpTokens::kTag, PreprocessingToken>>
        parallel_funcs_0(
            ParseFunctionInputs<PpTokens::kTag>(false, output.last_token_,
                                                output.cur_token_),
            SerialParseFunctions(ParseFunctionInputs<PpTokens::kTag>(false),
                                 PreprocessingToken(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      PpTokens::kTag, 1,
      SerialParseFunctions<PpTokens::kTag, PpTokens, PreprocessingToken>>
      parallel_funcs_1(
          ParseFunctionInputs<PpTokens::kTag>(false, output.last_token_,
                                              output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<PpTokens::kTag>(false),
                               PpTokens(false), PreprocessingToken(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// new-line:

inline ParseFunctionOutputs<NewLine::kTag> NewLine::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {
    unreachable(NewLine::kTag);
    return base::operator()();
  }
}

// defined-macro-expression:
// 	`defined`, `identifier`
// 	`defined`, `(`, `identifier`, `)`
inline ParseFunctionOutputs<DefinedMacroExpression::kTag>
DefinedMacroExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      DefinedMacroExpression::kTag, 2,
      SerialParseFunctions<DefinedMacroExpression::kTag,
                           ParseFunction<DefinedMacroExpression::kTag>,
                           ParseFunction<DefinedMacroExpression::kTag>>,
      SerialParseFunctions<DefinedMacroExpression::kTag,
                           ParseFunction<DefinedMacroExpression::kTag>,
                           ParseFunction<DefinedMacroExpression::kTag>,
                           ParseFunction<DefinedMacroExpression::kTag>,
                           ParseFunction<DefinedMacroExpression::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<DefinedMacroExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<DefinedMacroExpression::kTag>(false),
              ParseFunction<DefinedMacroExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_defined,
                      diag::DiagKind::
                          defined_macro_expression_expect_kw_defined),
              ParseFunction<DefinedMacroExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::identifier,
                      diag::DiagKind::
                          defined_macro_expression_expect_identifier)),
          SerialParseFunctions(
              ParseFunctionInputs<DefinedMacroExpression::kTag>(false),
              ParseFunction<DefinedMacroExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_defined,
                      diag::DiagKind::
                          defined_macro_expression_expect_kw_defined),
              ParseFunction<DefinedMacroExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::defined_macro_expression_expect_l_paren),
              ParseFunction<DefinedMacroExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::identifier,
                      diag::DiagKind::
                          defined_macro_expression_expect_identifier),
              ParseFunction<DefinedMacroExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::
                          defined_macro_expression_expect_r_paren)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// h-preprocessing-token:

inline ParseFunctionOutputs<HPreprocessingToken::kTag>
HPreprocessingToken::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {
    unreachable(HPreprocessingToken::kTag);
    return base::operator()();
  }
}

// h-pp-tokens:
// 	h_preprocessing_token
// 	h_pp_tokens, h_preprocessing_token
inline ParseFunctionOutputs<HPpTokens::kTag> HPpTokens::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<
        HPpTokens::kTag, 1,
        SerialParseFunctions<HPpTokens::kTag, HPreprocessingToken>>
        parallel_funcs_0(
            ParseFunctionInputs<HPpTokens::kTag>(false, output.last_token_,
                                                 output.cur_token_),
            SerialParseFunctions(ParseFunctionInputs<HPpTokens::kTag>(false),
                                 HPreprocessingToken(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      HPpTokens::kTag, 1,
      SerialParseFunctions<HPpTokens::kTag, HPpTokens, HPreprocessingToken>>
      parallel_funcs_1(
          ParseFunctionInputs<HPpTokens::kTag>(false, output.last_token_,
                                               output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<HPpTokens::kTag>(false),
                               HPpTokens(false), HPreprocessingToken(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// header-name-tokens:
// 	string_literal
// 	`<`, h_pp_tokens, `>`
inline ParseFunctionOutputs<HeaderNameTokens::kTag>
HeaderNameTokens::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      HeaderNameTokens::kTag, 2,
      SerialParseFunctions<HeaderNameTokens::kTag, StringLiteral>,
      SerialParseFunctions<HeaderNameTokens::kTag,
                           ParseFunction<HeaderNameTokens::kTag>, HPpTokens,
                           ParseFunction<HeaderNameTokens::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<HeaderNameTokens::kTag>(false, output.last_token_,
                                                      output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<HeaderNameTokens::kTag>(false),
              StringLiteral(false)),
          SerialParseFunctions(
              ParseFunctionInputs<HeaderNameTokens::kTag>(false),
              ParseFunction<HeaderNameTokens::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::less,
                  diag::DiagKind::header_name_tokens_expect_less),
              HPpTokens(false),
              ParseFunction<HeaderNameTokens::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::greater,
                  diag::DiagKind::header_name_tokens_expect_greater)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// has-include-expression:
// 	`__has_include`, `(`, header_name, `)`
// 	`__has_include`, `(`, header_name_tokens, `)`
inline ParseFunctionOutputs<HasIncludeExpression::kTag>
HasIncludeExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      HasIncludeExpression::kTag, 2,
      SerialParseFunctions<
          HasIncludeExpression::kTag, ParseFunction<HasIncludeExpression::kTag>,
          ParseFunction<HasIncludeExpression::kTag>, HeaderName,
          ParseFunction<HasIncludeExpression::kTag>>,
      SerialParseFunctions<
          HasIncludeExpression::kTag, ParseFunction<HasIncludeExpression::kTag>,
          ParseFunction<HasIncludeExpression::kTag>, HeaderNameTokens,
          ParseFunction<HasIncludeExpression::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<HasIncludeExpression::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<HasIncludeExpression::kTag>(false),
              ParseFunction<HasIncludeExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw___has_include,
                      diag::DiagKind::
                          has_include_expression_expect_kw___has_include),
              ParseFunction<HasIncludeExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::has_include_expression_expect_l_paren),
              HeaderName(false),
              ParseFunction<HasIncludeExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::has_include_expression_expect_r_paren)),
          SerialParseFunctions(
              ParseFunctionInputs<HasIncludeExpression::kTag>(false),
              ParseFunction<HasIncludeExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw___has_include,
                      diag::DiagKind::
                          has_include_expression_expect_kw___has_include),
              ParseFunction<HasIncludeExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::l_paren,
                      diag::DiagKind::has_include_expression_expect_l_paren),
              HeaderNameTokens(false),
              ParseFunction<HasIncludeExpression::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::r_paren,
                      diag::DiagKind::has_include_expression_expect_r_paren)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// has-attribute-expression:
// 	`__has_cpp_attribute`, `(`, pp_tokens, `)`
inline ParseFunctionOutputs<HasAttributeExpression::kTag>
HasAttributeExpression::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<HasAttributeExpression::kTag>(
          false, output.last_token_, output.cur_token_),
      ParseFunction<HasAttributeExpression::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw___has_cpp_attribute,
          diag::DiagKind::
              has_attribute_expression_expect_kw___has_cpp_attribute),
      ParseFunction<HasAttributeExpression::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_paren,
          diag::DiagKind::has_attribute_expression_expect_l_paren),
      PpTokens(false),
      ParseFunction<HasAttributeExpression::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_paren,
          diag::DiagKind::has_attribute_expression_expect_r_paren));
  return serial_funcs();
}

// pp-module:
// 	`export`[opt], `module`, pp_tokens[opt], `;`, new_line
inline ParseFunctionOutputs<PpModule::kTag> PpModule::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<PpModule::kTag>(false, output.last_token_,
                                          output.cur_token_),
      ParseFunction<PpModule::kTag>::create_single_token_check(
          true, token::tok::TokenKind::kw_export,
          diag::DiagKind::pp_module_expect_kw_export),
      ParseFunction<PpModule::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_module,
          diag::DiagKind::pp_module_expect_kw_module),
      PpTokens(true),
      ParseFunction<PpModule::kTag>::create_single_token_check(
          false, token::tok::TokenKind::semi,
          diag::DiagKind::pp_module_expect_semi),
      NewLine(false));
  return serial_funcs();
}

// pp-import:
// 	`export`[opt], `import`, header_name, pp_tokens[opt], `;`, new_line
// 	`export`[opt], `import`, header_name_tokens, pp_tokens[opt], `;`, new_line
// 	`export`[opt], `import`, pp_tokens, `;`, new_line
inline ParseFunctionOutputs<PpImport::kTag> PpImport::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      PpImport::kTag, 3,
      SerialParseFunctions<PpImport::kTag, ParseFunction<PpImport::kTag>,
                           ParseFunction<PpImport::kTag>, HeaderName, PpTokens,
                           ParseFunction<PpImport::kTag>, NewLine>,
      SerialParseFunctions<PpImport::kTag, ParseFunction<PpImport::kTag>,
                           ParseFunction<PpImport::kTag>, HeaderNameTokens,
                           PpTokens, ParseFunction<PpImport::kTag>, NewLine>,
      SerialParseFunctions<PpImport::kTag, ParseFunction<PpImport::kTag>,
                           ParseFunction<PpImport::kTag>, PpTokens,
                           ParseFunction<PpImport::kTag>, NewLine>>
      parallel_funcs_0(
          ParseFunctionInputs<PpImport::kTag>(false, output.last_token_,
                                              output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<PpImport::kTag>(false),
              ParseFunction<PpImport::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::kw_export,
                  diag::DiagKind::pp_import_expect_kw_export),
              ParseFunction<PpImport::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_import,
                  diag::DiagKind::pp_import_expect_kw_import),
              HeaderName(false), PpTokens(true),
              ParseFunction<PpImport::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::semi,
                  diag::DiagKind::pp_import_expect_semi),
              NewLine(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PpImport::kTag>(false),
              ParseFunction<PpImport::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::kw_export,
                  diag::DiagKind::pp_import_expect_kw_export),
              ParseFunction<PpImport::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_import,
                  diag::DiagKind::pp_import_expect_kw_import),
              HeaderNameTokens(false), PpTokens(true),
              ParseFunction<PpImport::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::semi,
                  diag::DiagKind::pp_import_expect_semi),
              NewLine(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PpImport::kTag>(false),
              ParseFunction<PpImport::kTag>::create_single_token_check(
                  true, token::tok::TokenKind::kw_export,
                  diag::DiagKind::pp_import_expect_kw_export),
              ParseFunction<PpImport::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_import,
                  diag::DiagKind::pp_import_expect_kw_import),
              PpTokens(false),
              ParseFunction<PpImport::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::semi,
                  diag::DiagKind::pp_import_expect_semi),
              NewLine(false)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// va-opt-replacement:
// 	`__va_opt__`, `(`, pp_tokens[opt], `)`
inline ParseFunctionOutputs<VaOptReplacement::kTag>
VaOptReplacement::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<VaOptReplacement::kTag>(false, output.last_token_,
                                                  output.cur_token_),
      ParseFunction<VaOptReplacement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw___va_opt__,
          diag::DiagKind::va_opt_replacement_expect_kw___va_opt__),
      ParseFunction<VaOptReplacement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::l_paren,
          diag::DiagKind::va_opt_replacement_expect_l_paren),
      PpTokens(true),
      ParseFunction<VaOptReplacement::kTag>::create_single_token_check(
          false, token::tok::TokenKind::r_paren,
          diag::DiagKind::va_opt_replacement_expect_r_paren));
  return serial_funcs();
}

// hex-quad:
// 	hexadecimal_digit, hexadecimal_digit, hexadecimal_digit, hexadecimal_digit
inline ParseFunctionOutputs<HexQuad::kTag> HexQuad::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<HexQuad::kTag>(false, output.last_token_,
                                         output.cur_token_),
      HexadecimalDigit(false), HexadecimalDigit(false), HexadecimalDigit(false),
      HexadecimalDigit(false));
  return serial_funcs();
}

// universal-character-name:
// 	`\u`, hex_quad
// 	`\U`, hex_quad, hex_quad
inline ParseFunctionOutputs<UniversalCharacterName::kTag>
UniversalCharacterName::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(UniversalCharacterName::kTag);
  return output;
}

// preprocessing-token:
// 	header_name
// 	`import`
// 	`module`
// 	`export`
// 	`identifier`
// 	pp_number
// 	character_literal
// 	user_defined_character_literal
// 	string_literal
// 	user_defined_string_literal
// 	preprocessing_op_or_punc
inline ParseFunctionOutputs<PreprocessingToken::kTag>
PreprocessingToken::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      PreprocessingToken::kTag, 11,
      SerialParseFunctions<PreprocessingToken::kTag, HeaderName>,
      SerialParseFunctions<PreprocessingToken::kTag,
                           ParseFunction<PreprocessingToken::kTag>>,
      SerialParseFunctions<PreprocessingToken::kTag,
                           ParseFunction<PreprocessingToken::kTag>>,
      SerialParseFunctions<PreprocessingToken::kTag,
                           ParseFunction<PreprocessingToken::kTag>>,
      SerialParseFunctions<PreprocessingToken::kTag,
                           ParseFunction<PreprocessingToken::kTag>>,
      SerialParseFunctions<PreprocessingToken::kTag, PpNumber>,
      SerialParseFunctions<PreprocessingToken::kTag, CharacterLiteral>,
      SerialParseFunctions<PreprocessingToken::kTag,
                           UserDefinedCharacterLiteral>,
      SerialParseFunctions<PreprocessingToken::kTag, StringLiteral>,
      SerialParseFunctions<PreprocessingToken::kTag, UserDefinedStringLiteral>,
      SerialParseFunctions<PreprocessingToken::kTag, PreprocessingOpOrPunc>>
      parallel_funcs_0(
          ParseFunctionInputs<PreprocessingToken::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingToken::kTag>(false),
              HeaderName(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingToken::kTag>(false),
              ParseFunction<PreprocessingToken::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_import,
                      diag::DiagKind::preprocessing_token_expect_kw_import)),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingToken::kTag>(false),
              ParseFunction<PreprocessingToken::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_module,
                      diag::DiagKind::preprocessing_token_expect_kw_module)),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingToken::kTag>(false),
              ParseFunction<PreprocessingToken::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::kw_export,
                      diag::DiagKind::preprocessing_token_expect_kw_export)),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingToken::kTag>(false),
              ParseFunction<PreprocessingToken::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::identifier,
                      diag::DiagKind::preprocessing_token_expect_identifier)),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingToken::kTag>(false),
              PpNumber(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingToken::kTag>(false),
              CharacterLiteral(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingToken::kTag>(false),
              UserDefinedCharacterLiteral(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingToken::kTag>(false),
              StringLiteral(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingToken::kTag>(false),
              UserDefinedStringLiteral(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingToken::kTag>(false),
              PreprocessingOpOrPunc(false)));

  static_assert(base::kNumberOfElements >= 11);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// token:
// 	`identifier`
// 	keyword
// 	literal
// 	operator_or_punctuator
inline ParseFunctionOutputs<Token::kTag> Token::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      Token::kTag, 4,
      SerialParseFunctions<Token::kTag, ParseFunction<Token::kTag>>,
      SerialParseFunctions<Token::kTag, Keyword>,
      SerialParseFunctions<Token::kTag, Literal>,
      SerialParseFunctions<Token::kTag, OperatorOrPunctuator>>
      parallel_funcs_0(
          ParseFunctionInputs<Token::kTag>(false, output.last_token_,
                                           output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<Token::kTag>(false),
              ParseFunction<Token::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::identifier,
                  diag::DiagKind::token_expect_identifier)),
          SerialParseFunctions(ParseFunctionInputs<Token::kTag>(false),
                               Keyword(false)),
          SerialParseFunctions(ParseFunctionInputs<Token::kTag>(false),
                               Literal(false)),
          SerialParseFunctions(ParseFunctionInputs<Token::kTag>(false),
                               OperatorOrPunctuator(false)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// header-name:
// 	`<`, h_char_sequence, `>`
// 	`"`, q_char_sequence, `"`
inline ParseFunctionOutputs<HeaderName::kTag> HeaderName::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(HeaderName::kTag);
  return output;
}

// h-char-sequence:
// 	h_char
// 	h_char_sequence, h_char
inline ParseFunctionOutputs<HCharSequence::kTag> HCharSequence::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<HCharSequence::kTag, 1,
                           SerialParseFunctions<HCharSequence::kTag, HChar>>
        parallel_funcs_0(
            ParseFunctionInputs<HCharSequence::kTag>(false, output.last_token_,
                                                     output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<HCharSequence::kTag>(false), HChar(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      HCharSequence::kTag, 1,
      SerialParseFunctions<HCharSequence::kTag, HCharSequence, HChar>>
      parallel_funcs_1(
          ParseFunctionInputs<HCharSequence::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<HCharSequence::kTag>(false),
                               HCharSequence(false), HChar(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// h-char:

inline ParseFunctionOutputs<HChar::kTag> HChar::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {
    unreachable(HChar::kTag);
    return base::operator()();
  }
}

// q-char-sequence:
// 	q_char
// 	q_char_sequence, q_char
inline ParseFunctionOutputs<QCharSequence::kTag> QCharSequence::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<QCharSequence::kTag, 1,
                           SerialParseFunctions<QCharSequence::kTag, QChar>>
        parallel_funcs_0(
            ParseFunctionInputs<QCharSequence::kTag>(false, output.last_token_,
                                                     output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<QCharSequence::kTag>(false), QChar(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      QCharSequence::kTag, 1,
      SerialParseFunctions<QCharSequence::kTag, QCharSequence, QChar>>
      parallel_funcs_1(
          ParseFunctionInputs<QCharSequence::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<QCharSequence::kTag>(false),
                               QCharSequence(false), QChar(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// q-char:

inline ParseFunctionOutputs<QChar::kTag> QChar::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {
    unreachable(QChar::kTag);
    return base::operator()();
  }
}

// pp-number:
// 	digit
// 	`.`, digit
// 	pp_number, digit
// 	pp_number, identifier_nondigit
// 	pp_number, `'`, digit
// 	pp_number, `'`, nondigit
// 	pp_number, `e`, sign
// 	pp_number, `E`, sign
// 	pp_number, `p`, sign
// 	pp_number, `P`, sign
// 	pp_number, `.`
inline ParseFunctionOutputs<PpNumber::kTag> PpNumber::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(PpNumber::kTag);
  return output;
}

// identifier-nondigit:
// 	nondigit
// 	universal_character_name
inline ParseFunctionOutputs<IdentifierNondigit::kTag>
IdentifierNondigit::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      IdentifierNondigit::kTag, 2,
      SerialParseFunctions<IdentifierNondigit::kTag, Nondigit>,
      SerialParseFunctions<IdentifierNondigit::kTag, UniversalCharacterName>>
      parallel_funcs_0(ParseFunctionInputs<IdentifierNondigit::kTag>(
                           false, output.last_token_, output.cur_token_),
                       SerialParseFunctions(
                           ParseFunctionInputs<IdentifierNondigit::kTag>(false),
                           Nondigit(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<IdentifierNondigit::kTag>(false),
                           UniversalCharacterName(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// nondigit:

inline ParseFunctionOutputs<Nondigit::kTag> Nondigit::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {
    unreachable(Nondigit::kTag);
    return base::operator()();
  }
}

// digit:

inline ParseFunctionOutputs<Digit::kTag> Digit::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {
    unreachable(Digit::kTag);
    return base::operator()();
  }
}

// keyword:

inline ParseFunctionOutputs<Keyword::kTag> Keyword::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {
    unreachable(Keyword::kTag);
    return base::operator()();
  }
}

// preprocessing-op-or-punc:
// 	preprocessing_operator
// 	operator_or_punctuator
inline ParseFunctionOutputs<PreprocessingOpOrPunc::kTag>
PreprocessingOpOrPunc::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      PreprocessingOpOrPunc::kTag, 2,
      SerialParseFunctions<PreprocessingOpOrPunc::kTag, PreprocessingOperator>,
      SerialParseFunctions<PreprocessingOpOrPunc::kTag, OperatorOrPunctuator>>
      parallel_funcs_0(
          ParseFunctionInputs<PreprocessingOpOrPunc::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingOpOrPunc::kTag>(false),
              PreprocessingOperator(false)),
          SerialParseFunctions(
              ParseFunctionInputs<PreprocessingOpOrPunc::kTag>(false),
              OperatorOrPunctuator(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// preprocessing-operator:
// 	`#`
// 	`##`
// 	`%:`
// 	`%:%:`
inline ParseFunctionOutputs<PreprocessingOperator::kTag>
PreprocessingOperator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(PreprocessingOperator::kTag);
  return output;
}

// operator-or-punctuator:
// 	`{`
// 	`}`
// 	`[`
// 	`]`
// 	`(`
// 	`)`
// 	`<:`
// 	`:>`
// 	`<%`
// 	`%>`
// 	`;`
// 	`:`
// 	`...`
// 	`?`
// 	`::`
// 	`.`
// 	`.*`
// 	`_>`
// 	`_>*`
// 	`~`
// 	`!`
// 	`+`
// 	`_`
// 	`*`
// 	`/`
// 	`%`
// 	`^`
// 	`&`
// 	`|`
// 	`=`
// 	`+=`
// 	`_=`
// 	`*=`
// 	`/=`
// 	`%=`
// 	`^=`
// 	`&=`
// 	`|=`
// 	`==`
// 	`!=`
// 	`<`
// 	`>`
// 	`<=`
// 	`>=`
// 	`<=>`
// 	`&&`
// 	`||`
// 	`<<`
// 	`>>`
// 	`<<=`
// 	`>>=`
// 	`++`
// 	`__`
// 	`,`
// 	`and`
// 	`or`
// 	`xor`
// 	`not`
// 	`bitand`
// 	`bitor`
// 	`compl`
// 	`and_eq`
// 	`or_eq`
// 	`xor_eq`
// 	`not_eq`
inline ParseFunctionOutputs<OperatorOrPunctuator::kTag>
OperatorOrPunctuator::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(OperatorOrPunctuator::kTag);
  return output;
}

// literal:
// 	integer_literal
// 	character_literal
// 	floating_point_literal
// 	string_literal
// 	boolean_literal
// 	pointer_literal
// 	user_defined_literal
inline ParseFunctionOutputs<Literal::kTag> Literal::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      Literal::kTag, 7, SerialParseFunctions<Literal::kTag, IntegerLiteral>,
      SerialParseFunctions<Literal::kTag, CharacterLiteral>,
      SerialParseFunctions<Literal::kTag, FloatingPointLiteral>,
      SerialParseFunctions<Literal::kTag, StringLiteral>,
      SerialParseFunctions<Literal::kTag, BooleanLiteral>,
      SerialParseFunctions<Literal::kTag, PointerLiteral>,
      SerialParseFunctions<Literal::kTag, UserDefinedLiteral>>
      parallel_funcs_0(
          ParseFunctionInputs<Literal::kTag>(false, output.last_token_,
                                             output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<Literal::kTag>(false),
                               IntegerLiteral(false)),
          SerialParseFunctions(ParseFunctionInputs<Literal::kTag>(false),
                               CharacterLiteral(false)),
          SerialParseFunctions(ParseFunctionInputs<Literal::kTag>(false),
                               FloatingPointLiteral(false)),
          SerialParseFunctions(ParseFunctionInputs<Literal::kTag>(false),
                               StringLiteral(false)),
          SerialParseFunctions(ParseFunctionInputs<Literal::kTag>(false),
                               BooleanLiteral(false)),
          SerialParseFunctions(ParseFunctionInputs<Literal::kTag>(false),
                               PointerLiteral(false)),
          SerialParseFunctions(ParseFunctionInputs<Literal::kTag>(false),
                               UserDefinedLiteral(false)));

  static_assert(base::kNumberOfElements >= 7);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// integer-literal:
// 	binary_literal, integer_suffix[opt]
// 	octal_literal, integer_suffix[opt]
// 	decimal_literal, integer_suffix[opt]
// 	hexadecimal_literal, integer_suffix[opt]
inline ParseFunctionOutputs<IntegerLiteral::kTag> IntegerLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      IntegerLiteral::kTag, 4,
      SerialParseFunctions<IntegerLiteral::kTag, BinaryLiteral, IntegerSuffix>,
      SerialParseFunctions<IntegerLiteral::kTag, OctalLiteral, IntegerSuffix>,
      SerialParseFunctions<IntegerLiteral::kTag, DecimalLiteral, IntegerSuffix>,
      SerialParseFunctions<IntegerLiteral::kTag, HexadecimalLiteral,
                           IntegerSuffix>>
      parallel_funcs_0(
          ParseFunctionInputs<IntegerLiteral::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<IntegerLiteral::kTag>(false),
                               BinaryLiteral(false), IntegerSuffix(true)),
          SerialParseFunctions(ParseFunctionInputs<IntegerLiteral::kTag>(false),
                               OctalLiteral(false), IntegerSuffix(true)),
          SerialParseFunctions(ParseFunctionInputs<IntegerLiteral::kTag>(false),
                               DecimalLiteral(false), IntegerSuffix(true)),
          SerialParseFunctions(ParseFunctionInputs<IntegerLiteral::kTag>(false),
                               HexadecimalLiteral(false), IntegerSuffix(true)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// binary-literal:
// 	`0b`, binary_digit
// 	`0B`, `binary_digit`
// 	binary_literal, `'`
// 	binary_literal, `'`[opt], binary_digit
inline ParseFunctionOutputs<BinaryLiteral::kTag> BinaryLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(BinaryLiteral::kTag);
  return output;
}

// octal-literal:
// 	`0`
// 	octal_literal, `'`[opt], octal_digit
inline ParseFunctionOutputs<OctalLiteral::kTag> OctalLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(OctalLiteral::kTag);
  return output;
}

// decimal-literal:
// 	nonzero_digit
// 	decimal_literal, `'`[opt], digit
inline ParseFunctionOutputs<DecimalLiteral::kTag> DecimalLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(DecimalLiteral::kTag);
  return output;
}

// hexadecimal-literal:
// 	hexadecimal_prefix, hexadecimal_digit_sequence
inline ParseFunctionOutputs<HexadecimalLiteral::kTag>
HexadecimalLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<HexadecimalLiteral::kTag>(false, output.last_token_,
                                                    output.cur_token_),
      HexadecimalPrefix(false), HexadecimalDigitSequence(false));
  return serial_funcs();
}

// binary-digit:
// 	`0`
// 	`1`
inline ParseFunctionOutputs<BinaryDigit::kTag> BinaryDigit::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(BinaryDigit::kTag);
  return output;
}

// octal-digit:
// 	`0`
// 	`1`
// 	`2`
// 	`3`
// 	`4`
// 	`5`
// 	`6`
// 	`7`
inline ParseFunctionOutputs<OctalDigit::kTag> OctalDigit::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(OctalDigit::kTag);
  return output;
}

// nonzero-digit:
// 	`1`
// 	`2`
// 	`3`
// 	`4`
// 	`5`
// 	`6`
// 	`7`
// 	`8`
// 	`9`
inline ParseFunctionOutputs<NonzeroDigit::kTag> NonzeroDigit::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(NonzeroDigit::kTag);
  return output;
}

// hexadecimal-prefix:
// 	`0x`
// 	`0X`
inline ParseFunctionOutputs<HexadecimalPrefix::kTag>
HexadecimalPrefix::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(HexadecimalPrefix::kTag);
  return output;
}

// hexadecimal-digit-sequence:
// 	hexadecimal_digit
// 	hexadecimal_digit_sequence, `'`[opt], hexadecimal_digit
inline ParseFunctionOutputs<HexadecimalDigitSequence::kTag>
HexadecimalDigitSequence::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(HexadecimalDigitSequence::kTag);
  return output;
}

// hexadecimal-digit:
// 	`0`
// 	`1`
// 	`2`
// 	`3`
// 	`4`
// 	`5`
// 	`6`
// 	`7`
// 	`8`
// 	`9`
// 	`A`
// 	`B`
// 	`C`
// 	`D`
// 	`E`
// 	`F`
inline ParseFunctionOutputs<HexadecimalDigit::kTag>
HexadecimalDigit::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(HexadecimalDigit::kTag);
  return output;
}

// integer-suffix:
// 	unsigned_suffix, long_suffix[opt]
// 	unsigned_suffix, long_long_suffix[opt]
// 	long_suffix, unsigned_suffix[opt]
// 	long_long_suffix, unsigned_suffix[opt]
inline ParseFunctionOutputs<IntegerSuffix::kTag> IntegerSuffix::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      IntegerSuffix::kTag, 4,
      SerialParseFunctions<IntegerSuffix::kTag, UnsignedSuffix, LongSuffix>,
      SerialParseFunctions<IntegerSuffix::kTag, UnsignedSuffix, LongLongSuffix>,
      SerialParseFunctions<IntegerSuffix::kTag, LongSuffix, UnsignedSuffix>,
      SerialParseFunctions<IntegerSuffix::kTag, LongLongSuffix, UnsignedSuffix>>
      parallel_funcs_0(
          ParseFunctionInputs<IntegerSuffix::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<IntegerSuffix::kTag>(false),
                               UnsignedSuffix(false), LongSuffix(true)),
          SerialParseFunctions(ParseFunctionInputs<IntegerSuffix::kTag>(false),
                               UnsignedSuffix(false), LongLongSuffix(true)),
          SerialParseFunctions(ParseFunctionInputs<IntegerSuffix::kTag>(false),
                               LongSuffix(false), UnsignedSuffix(true)),
          SerialParseFunctions(ParseFunctionInputs<IntegerSuffix::kTag>(false),
                               LongLongSuffix(false), UnsignedSuffix(true)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// unsigned-suffix:
// 	`u`
// 	`U`
inline ParseFunctionOutputs<UnsignedSuffix::kTag> UnsignedSuffix::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(UnsignedSuffix::kTag);
  return output;
}

// long-suffix:
// 	`l`
// 	`L`
inline ParseFunctionOutputs<LongSuffix::kTag> LongSuffix::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(LongSuffix::kTag);
  return output;
}

// long-long-suffix:
// 	`ll`
// 	`LL`
inline ParseFunctionOutputs<LongLongSuffix::kTag> LongLongSuffix::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(LongLongSuffix::kTag);
  return output;
}

// character-literal:
// 	encoding_prefix[opt], `'`, c_char_sequence, `'`
inline ParseFunctionOutputs<CharacterLiteral::kTag>
CharacterLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(CharacterLiteral::kTag);
  return output;
}

// encoding-prefix:
// 	`u8`
// 	`u`
// 	`U`
// 	`L`
inline ParseFunctionOutputs<EncodingPrefix::kTag> EncodingPrefix::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(EncodingPrefix::kTag);
  return output;
}

// c-char-sequence:
// 	c_char
// 	c_char_sequence, c_char
inline ParseFunctionOutputs<CCharSequence::kTag> CCharSequence::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<CCharSequence::kTag, 1,
                           SerialParseFunctions<CCharSequence::kTag, CChar>>
        parallel_funcs_0(
            ParseFunctionInputs<CCharSequence::kTag>(false, output.last_token_,
                                                     output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<CCharSequence::kTag>(false), CChar(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      CCharSequence::kTag, 1,
      SerialParseFunctions<CCharSequence::kTag, CCharSequence, CChar>>
      parallel_funcs_1(
          ParseFunctionInputs<CCharSequence::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<CCharSequence::kTag>(false),
                               CCharSequence(false), CChar(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// c-char:
// 	escape_sequence
// 	universal_character_name
inline ParseFunctionOutputs<CChar::kTag> CChar::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      CChar::kTag, 2, SerialParseFunctions<CChar::kTag, EscapeSequence>,
      SerialParseFunctions<CChar::kTag, UniversalCharacterName>>
      parallel_funcs_0(
          ParseFunctionInputs<CChar::kTag>(false, output.last_token_,
                                           output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<CChar::kTag>(false),
                               EscapeSequence(false)),
          SerialParseFunctions(ParseFunctionInputs<CChar::kTag>(false),
                               UniversalCharacterName(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// escape-sequence:
// 	simple_escape_sequence
// 	octal_escape_sequence
// 	hexadecimal_escape_sequence
inline ParseFunctionOutputs<EscapeSequence::kTag> EscapeSequence::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      EscapeSequence::kTag, 3,
      SerialParseFunctions<EscapeSequence::kTag, SimpleEscapeSequence>,
      SerialParseFunctions<EscapeSequence::kTag, OctalEscapeSequence>,
      SerialParseFunctions<EscapeSequence::kTag, HexadecimalEscapeSequence>>
      parallel_funcs_0(
          ParseFunctionInputs<EscapeSequence::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<EscapeSequence::kTag>(false),
                               SimpleEscapeSequence(false)),
          SerialParseFunctions(ParseFunctionInputs<EscapeSequence::kTag>(false),
                               OctalEscapeSequence(false)),
          SerialParseFunctions(ParseFunctionInputs<EscapeSequence::kTag>(false),
                               HexadecimalEscapeSequence(false)));

  static_assert(base::kNumberOfElements >= 3);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// simple-escape-sequence:
// 	`\'`
// 	`\"`
// 	`\?`
// 	`\\
// 	`\a`
// 	`\b`
// 	`\f`
// 	`\n`
// 	`\r`
// 	`\t`
// 	`\v
inline ParseFunctionOutputs<SimpleEscapeSequence::kTag>
SimpleEscapeSequence::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(SimpleEscapeSequence::kTag);
  return output;
}

// octal-escape-sequence:
// 	`\`, octal_digit
// 	`\`, octal_digit, octal_digit
// 	`\`, octal_digit, octal_digit, octal_digit
inline ParseFunctionOutputs<OctalEscapeSequence::kTag>
OctalEscapeSequence::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(OctalEscapeSequence::kTag);
  return output;
}

// hexadecimal-escape-sequence:
// 	`\x`, hexadecimal_digit
// 	hexadecimal_escape_sequence, hexadecimal_digit
inline ParseFunctionOutputs<HexadecimalEscapeSequence::kTag>
HexadecimalEscapeSequence::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(HexadecimalEscapeSequence::kTag);
  return output;
}

// floating-point-literal:
// 	decimal_floating_point_literal
// 	hexadecimal_floating_point_literal
inline ParseFunctionOutputs<FloatingPointLiteral::kTag>
FloatingPointLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<FloatingPointLiteral::kTag, 2,
                         SerialParseFunctions<FloatingPointLiteral::kTag,
                                              DecimalFloatingPointLiteral>,
                         SerialParseFunctions<FloatingPointLiteral::kTag,
                                              HexadecimalFloatingPointLiteral>>
      parallel_funcs_0(
          ParseFunctionInputs<FloatingPointLiteral::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<FloatingPointLiteral::kTag>(false),
              DecimalFloatingPointLiteral(false)),
          SerialParseFunctions(
              ParseFunctionInputs<FloatingPointLiteral::kTag>(false),
              HexadecimalFloatingPointLiteral(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// decimal-floating-point-literal:
// 	fractional_constant, exponent_part[opt], floating_point_suffix[opt]
// 	digit_sequence, exponent_part[opt], floating_point_suffix[opt]
inline ParseFunctionOutputs<DecimalFloatingPointLiteral::kTag>
DecimalFloatingPointLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      DecimalFloatingPointLiteral::kTag, 2,
      SerialParseFunctions<DecimalFloatingPointLiteral::kTag,
                           FractionalConstant, ExponentPart,
                           FloatingPointSuffix>,
      SerialParseFunctions<DecimalFloatingPointLiteral::kTag, DigitSequence,
                           ExponentPart, FloatingPointSuffix>>
      parallel_funcs_0(
          ParseFunctionInputs<DecimalFloatingPointLiteral::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<DecimalFloatingPointLiteral::kTag>(false),
              FractionalConstant(false), ExponentPart(true),
              FloatingPointSuffix(true)),
          SerialParseFunctions(
              ParseFunctionInputs<DecimalFloatingPointLiteral::kTag>(false),
              DigitSequence(false), ExponentPart(true),
              FloatingPointSuffix(true)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// hexadecimal-floating-point-literal:
// 	hexadecimal_prefix, hexadecimal_fractional_constant, binary_exponent_part, floating_point_suffix[opt]
// 	hexadecimal_prefix, hexadecimal_digit_sequence, binary_exponent_part, floating_point_suffix[opt]
inline ParseFunctionOutputs<HexadecimalFloatingPointLiteral::kTag>
HexadecimalFloatingPointLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      HexadecimalFloatingPointLiteral::kTag, 2,
      SerialParseFunctions<HexadecimalFloatingPointLiteral::kTag,
                           HexadecimalPrefix, HexadecimalFractionalConstant,
                           BinaryExponentPart, FloatingPointSuffix>,
      SerialParseFunctions<HexadecimalFloatingPointLiteral::kTag,
                           HexadecimalPrefix, HexadecimalDigitSequence,
                           BinaryExponentPart, FloatingPointSuffix>>
      parallel_funcs_0(
          ParseFunctionInputs<HexadecimalFloatingPointLiteral::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<HexadecimalFloatingPointLiteral::kTag>(false),
              HexadecimalPrefix(false), HexadecimalFractionalConstant(false),
              BinaryExponentPart(false), FloatingPointSuffix(true)),
          SerialParseFunctions(
              ParseFunctionInputs<HexadecimalFloatingPointLiteral::kTag>(false),
              HexadecimalPrefix(false), HexadecimalDigitSequence(false),
              BinaryExponentPart(false), FloatingPointSuffix(true)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// fractional-constant:
// 	digit_sequence[opt], `.`, digit_sequence
// 	digit_sequence, `.`
inline ParseFunctionOutputs<FractionalConstant::kTag>
FractionalConstant::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      FractionalConstant::kTag, 2,
      SerialParseFunctions<FractionalConstant::kTag, DigitSequence,
                           ParseFunction<FractionalConstant::kTag>,
                           DigitSequence>,
      SerialParseFunctions<FractionalConstant::kTag, DigitSequence,
                           ParseFunction<FractionalConstant::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<FractionalConstant::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<FractionalConstant::kTag>(false),
              DigitSequence(true),
              ParseFunction<FractionalConstant::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::period,
                      diag::DiagKind::fractional_constant_expect_period),
              DigitSequence(false)),
          SerialParseFunctions(
              ParseFunctionInputs<FractionalConstant::kTag>(false),
              DigitSequence(false),
              ParseFunction<FractionalConstant::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::period,
                      diag::DiagKind::fractional_constant_expect_period)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// hexadecimal-fractional-constant:
// 	hexadecimal_digit_sequence[opt], `.`, hexadecimal_digit_sequence
// 	hexadecimal_digit_sequence, `.`
inline ParseFunctionOutputs<HexadecimalFractionalConstant::kTag>
HexadecimalFractionalConstant::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      HexadecimalFractionalConstant::kTag, 2,
      SerialParseFunctions<HexadecimalFractionalConstant::kTag,
                           HexadecimalDigitSequence,
                           ParseFunction<HexadecimalFractionalConstant::kTag>,
                           HexadecimalDigitSequence>,
      SerialParseFunctions<HexadecimalFractionalConstant::kTag,
                           HexadecimalDigitSequence,
                           ParseFunction<HexadecimalFractionalConstant::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<HexadecimalFractionalConstant::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<HexadecimalFractionalConstant::kTag>(false),
              HexadecimalDigitSequence(true),
              ParseFunction<HexadecimalFractionalConstant::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::period,
                      diag::DiagKind::
                          hexadecimal_fractional_constant_expect_period),
              HexadecimalDigitSequence(false)),
          SerialParseFunctions(
              ParseFunctionInputs<HexadecimalFractionalConstant::kTag>(false),
              HexadecimalDigitSequence(false),
              ParseFunction<HexadecimalFractionalConstant::kTag>::
                  create_single_token_check(
                      false, token::tok::TokenKind::period,
                      diag::DiagKind::
                          hexadecimal_fractional_constant_expect_period)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// exponent-part:
// 	`e`, sign[opt], digit_sequence
// 	`E`, sign[opt], digit_sequence
inline ParseFunctionOutputs<ExponentPart::kTag> ExponentPart::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(ExponentPart::kTag);
  return output;
}

// binary-exponent-part:
// 	`p`, sign[opt], digit_sequence
// 	`P`, sign[opt], digit_sequence
inline ParseFunctionOutputs<BinaryExponentPart::kTag>
BinaryExponentPart::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(BinaryExponentPart::kTag);
  return output;
}

// sign:
// 	`+`
// 	`_`
inline ParseFunctionOutputs<Sign::kTag> Sign::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      Sign::kTag, 2,
      SerialParseFunctions<Sign::kTag, ParseFunction<Sign::kTag>>,
      SerialParseFunctions<Sign::kTag, ParseFunction<Sign::kTag>>>
      parallel_funcs_0(ParseFunctionInputs<Sign::kTag>(
                           false, output.last_token_, output.cur_token_),
                       SerialParseFunctions(
                           ParseFunctionInputs<Sign::kTag>(false),
                           ParseFunction<Sign::kTag>::create_single_token_check(
                               false, token::tok::TokenKind::plus,
                               diag::DiagKind::sign_expect_plus)),
                       SerialParseFunctions(
                           ParseFunctionInputs<Sign::kTag>(false),
                           ParseFunction<Sign::kTag>::create_single_token_check(
                               false, token::tok::TokenKind::minus,
                               diag::DiagKind::sign_expect_minus)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// digit-sequence:
// 	digit
// 	digit_sequence, `'`[opt], digit
inline ParseFunctionOutputs<DigitSequence::kTag> DigitSequence::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(DigitSequence::kTag);
  return output;
}

// floating-point-suffix:
// 	`f`
// 	`l`
// 	`F`
// 	`L`
inline ParseFunctionOutputs<FloatingPointSuffix::kTag>
FloatingPointSuffix::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(FloatingPointSuffix::kTag);
  return output;
}

// string-literal:
// 	encoding_prefix[opt], `"`, s_char_sequence[opt], `"`
// 	encoding_prefix[opt], `R`, raw_string
inline ParseFunctionOutputs<StringLiteral::kTag> StringLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(StringLiteral::kTag);
  return output;
}

// s-char-sequence:
// 	s_char
// 	s_char_sequence, s_char
inline ParseFunctionOutputs<SCharSequence::kTag> SCharSequence::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<SCharSequence::kTag, 1,
                           SerialParseFunctions<SCharSequence::kTag, SChar>>
        parallel_funcs_0(
            ParseFunctionInputs<SCharSequence::kTag>(false, output.last_token_,
                                                     output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<SCharSequence::kTag>(false), SChar(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      SCharSequence::kTag, 1,
      SerialParseFunctions<SCharSequence::kTag, SCharSequence, SChar>>
      parallel_funcs_1(
          ParseFunctionInputs<SCharSequence::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<SCharSequence::kTag>(false),
                               SCharSequence(false), SChar(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// s-char:
// 	escape_sequence
// 	universal_character_name
inline ParseFunctionOutputs<SChar::kTag> SChar::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      SChar::kTag, 2, SerialParseFunctions<SChar::kTag, EscapeSequence>,
      SerialParseFunctions<SChar::kTag, UniversalCharacterName>>
      parallel_funcs_0(
          ParseFunctionInputs<SChar::kTag>(false, output.last_token_,
                                           output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<SChar::kTag>(false),
                               EscapeSequence(false)),
          SerialParseFunctions(ParseFunctionInputs<SChar::kTag>(false),
                               UniversalCharacterName(false)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// raw-string:
// 	`"`, d_char_sequence[opt], `(`, r_char_sequence[opt], `)`, d_char_sequence[opt], `"`
inline ParseFunctionOutputs<RawString::kTag> RawString::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  unreachable(RawString::kTag);
  return output;
}

// r-char-sequence:
// 	r_char
// 	r_char_sequence, r_char
inline ParseFunctionOutputs<RCharSequence::kTag> RCharSequence::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<RCharSequence::kTag, 1,
                           SerialParseFunctions<RCharSequence::kTag, RChar>>
        parallel_funcs_0(
            ParseFunctionInputs<RCharSequence::kTag>(false, output.last_token_,
                                                     output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<RCharSequence::kTag>(false), RChar(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      RCharSequence::kTag, 1,
      SerialParseFunctions<RCharSequence::kTag, RCharSequence, RChar>>
      parallel_funcs_1(
          ParseFunctionInputs<RCharSequence::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<RCharSequence::kTag>(false),
                               RCharSequence(false), RChar(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// r-char:

inline ParseFunctionOutputs<RChar::kTag> RChar::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {
    unreachable(RChar::kTag);
    return base::operator()();
  }
}

// d-char-sequence:
// 	d_char
// 	d_char_sequence, d_char
inline ParseFunctionOutputs<DCharSequence::kTag> DCharSequence::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {

    ParallelParseFunctions<DCharSequence::kTag, 1,
                           SerialParseFunctions<DCharSequence::kTag, DChar>>
        parallel_funcs_0(
            ParseFunctionInputs<DCharSequence::kTag>(false, output.last_token_,
                                                     output.cur_token_),
            SerialParseFunctions(
                ParseFunctionInputs<DCharSequence::kTag>(false), DChar(false)));

    static_assert(base::kNumberOfElements >= 1);
    parallel_funcs_0.executed_mask(
        decltype(parallel_funcs_0)::base::bitset_type(
            this->executed_mask_.value()));

    output = parallel_funcs_0();
    if (!output.work_) {
      return output;
    }
  }

  ParallelParseFunctions<
      DCharSequence::kTag, 1,
      SerialParseFunctions<DCharSequence::kTag, DCharSequence, DChar>>
      parallel_funcs_1(
          ParseFunctionInputs<DCharSequence::kTag>(false, output.last_token_,
                                                   output.cur_token_),
          SerialParseFunctions(ParseFunctionInputs<DCharSequence::kTag>(false),
                               DCharSequence(false), DChar(false)));

  static_assert(base::kNumberOfElements >= 1);
  parallel_funcs_1.executed_mask(decltype(parallel_funcs_1)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_1();
}

// d-char:

inline ParseFunctionOutputs<DChar::kTag> DChar::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }
  {
    unreachable(DChar::kTag);
    return base::operator()();
  }
}

// boolean-literal:
// 	`false`
// 	`true`
inline ParseFunctionOutputs<BooleanLiteral::kTag> BooleanLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      BooleanLiteral::kTag, 2,
      SerialParseFunctions<BooleanLiteral::kTag,
                           ParseFunction<BooleanLiteral::kTag>>,
      SerialParseFunctions<BooleanLiteral::kTag,
                           ParseFunction<BooleanLiteral::kTag>>>
      parallel_funcs_0(
          ParseFunctionInputs<BooleanLiteral::kTag>(false, output.last_token_,
                                                    output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<BooleanLiteral::kTag>(false),
              ParseFunction<BooleanLiteral::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_false,
                  diag::DiagKind::boolean_literal_expect_kw_false)),
          SerialParseFunctions(
              ParseFunctionInputs<BooleanLiteral::kTag>(false),
              ParseFunction<BooleanLiteral::kTag>::create_single_token_check(
                  false, token::tok::TokenKind::kw_true,
                  diag::DiagKind::boolean_literal_expect_kw_true)));

  static_assert(base::kNumberOfElements >= 2);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// pointer-literal:
// 	`nullptr`
inline ParseFunctionOutputs<PointerLiteral::kTag> PointerLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<PointerLiteral::kTag>(false, output.last_token_,
                                                output.cur_token_),
      ParseFunction<PointerLiteral::kTag>::create_single_token_check(
          false, token::tok::TokenKind::kw_nullptr,
          diag::DiagKind::pointer_literal_expect_kw_nullptr));
  return serial_funcs();
}

// user-defined-literal:
// 	user_defined_integer_literal
// 	user_defined_floating_point_literal
// 	user_defined_string_literal
// 	user_defined_character_literal
inline ParseFunctionOutputs<UserDefinedLiteral::kTag>
UserDefinedLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      UserDefinedLiteral::kTag, 4,
      SerialParseFunctions<UserDefinedLiteral::kTag, UserDefinedIntegerLiteral>,
      SerialParseFunctions<UserDefinedLiteral::kTag,
                           UserDefinedFloatingPointLiteral>,
      SerialParseFunctions<UserDefinedLiteral::kTag, UserDefinedStringLiteral>,
      SerialParseFunctions<UserDefinedLiteral::kTag,
                           UserDefinedCharacterLiteral>>
      parallel_funcs_0(ParseFunctionInputs<UserDefinedLiteral::kTag>(
                           false, output.last_token_, output.cur_token_),
                       SerialParseFunctions(
                           ParseFunctionInputs<UserDefinedLiteral::kTag>(false),
                           UserDefinedIntegerLiteral(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<UserDefinedLiteral::kTag>(false),
                           UserDefinedFloatingPointLiteral(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<UserDefinedLiteral::kTag>(false),
                           UserDefinedStringLiteral(false)),
                       SerialParseFunctions(
                           ParseFunctionInputs<UserDefinedLiteral::kTag>(false),
                           UserDefinedCharacterLiteral(false)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// user-defined-integer-literal:
// 	decimal_literal, ud_suffix
// 	octal_literal, ud_suffix
// 	hexadecimal_literal, ud_suffix
// 	binary_literal, ud_suffix
inline ParseFunctionOutputs<UserDefinedIntegerLiteral::kTag>
UserDefinedIntegerLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<UserDefinedIntegerLiteral::kTag, 4,
                         SerialParseFunctions<UserDefinedIntegerLiteral::kTag,
                                              DecimalLiteral, UdSuffix>,
                         SerialParseFunctions<UserDefinedIntegerLiteral::kTag,
                                              OctalLiteral, UdSuffix>,
                         SerialParseFunctions<UserDefinedIntegerLiteral::kTag,
                                              HexadecimalLiteral, UdSuffix>,
                         SerialParseFunctions<UserDefinedIntegerLiteral::kTag,
                                              BinaryLiteral, UdSuffix>>
      parallel_funcs_0(
          ParseFunctionInputs<UserDefinedIntegerLiteral::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<UserDefinedIntegerLiteral::kTag>(false),
              DecimalLiteral(false), UdSuffix(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UserDefinedIntegerLiteral::kTag>(false),
              OctalLiteral(false), UdSuffix(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UserDefinedIntegerLiteral::kTag>(false),
              HexadecimalLiteral(false), UdSuffix(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UserDefinedIntegerLiteral::kTag>(false),
              BinaryLiteral(false), UdSuffix(false)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// user-defined-floating-point-literal:
// 	fractional_constant, exponent_part[opt], ud_suffix
// 	digit_sequence, exponent_part, ud_suffix
// 	hexadecimal_prefix, hexadecimal_fractional_constant, binary_exponent_part, ud_suffix
// 	hexadecimal_prefix, hexadecimal_digit_sequence, binary_exponent_part, ud_suffix
inline ParseFunctionOutputs<UserDefinedFloatingPointLiteral::kTag>
UserDefinedFloatingPointLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  ParallelParseFunctions<
      UserDefinedFloatingPointLiteral::kTag, 4,
      SerialParseFunctions<UserDefinedFloatingPointLiteral::kTag,
                           FractionalConstant, ExponentPart, UdSuffix>,
      SerialParseFunctions<UserDefinedFloatingPointLiteral::kTag, DigitSequence,
                           ExponentPart, UdSuffix>,
      SerialParseFunctions<UserDefinedFloatingPointLiteral::kTag,
                           HexadecimalPrefix, HexadecimalFractionalConstant,
                           BinaryExponentPart, UdSuffix>,
      SerialParseFunctions<UserDefinedFloatingPointLiteral::kTag,
                           HexadecimalPrefix, HexadecimalDigitSequence,
                           BinaryExponentPart, UdSuffix>>
      parallel_funcs_0(
          ParseFunctionInputs<UserDefinedFloatingPointLiteral::kTag>(
              false, output.last_token_, output.cur_token_),
          SerialParseFunctions(
              ParseFunctionInputs<UserDefinedFloatingPointLiteral::kTag>(false),
              FractionalConstant(false), ExponentPart(true), UdSuffix(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UserDefinedFloatingPointLiteral::kTag>(false),
              DigitSequence(false), ExponentPart(false), UdSuffix(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UserDefinedFloatingPointLiteral::kTag>(false),
              HexadecimalPrefix(false), HexadecimalFractionalConstant(false),
              BinaryExponentPart(false), UdSuffix(false)),
          SerialParseFunctions(
              ParseFunctionInputs<UserDefinedFloatingPointLiteral::kTag>(false),
              HexadecimalPrefix(false), HexadecimalDigitSequence(false),
              BinaryExponentPart(false), UdSuffix(false)));

  static_assert(base::kNumberOfElements >= 4);
  parallel_funcs_0.executed_mask(decltype(parallel_funcs_0)::base::bitset_type(
      this->executed_mask_.value()));

  return parallel_funcs_0();
}

// user-defined-string-literal:
// 	string_literal, ud_suffix
inline ParseFunctionOutputs<UserDefinedStringLiteral::kTag>
UserDefinedStringLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<UserDefinedStringLiteral::kTag>(
          false, output.last_token_, output.cur_token_),
      StringLiteral(false), UdSuffix(false));
  return serial_funcs();
}

// user-defined-character-literal:
// 	character_literal, ud_suffix
inline ParseFunctionOutputs<UserDefinedCharacterLiteral::kTag>
UserDefinedCharacterLiteral::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<UserDefinedCharacterLiteral::kTag>(
          false, output.last_token_, output.cur_token_),
      CharacterLiteral(false), UdSuffix(false));
  return serial_funcs();
}

// ud-suffix:
// 	`identifier`
inline ParseFunctionOutputs<UdSuffix::kTag> UdSuffix::operator()() {
  auto output = base::operator()();
  if (!this->valid()) {
    return output;
  }

  SerialParseFunctions serial_funcs(
      ParseFunctionInputs<UdSuffix::kTag>(false, output.last_token_,
                                          output.cur_token_),
      ParseFunction<UdSuffix::kTag>::create_single_token_check(
          false, token::tok::TokenKind::identifier,
          diag::DiagKind::ud_suffix_expect_identifier));
  return serial_funcs();
}
}  // namespace lps::parser::details
