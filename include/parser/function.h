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

#include "parser.h"

namespace lps::parser::details {

template <meta::Str TagName, typename... ParseFuncs>
class ParallelParseFunctions : public ParseFunction<TagName> {

 public:
  using base = ParseFunction<TagName>;
  ParallelParseFunctions(const ParallelParseFunctions& other) = default;
  explicit ParallelParseFunctions(const ParseFunctionInputs<TagName>& param,
                                  ParseFuncs&&... funcs)
      : base(param), parse_functions_(funcs...) {}
  ParseFunctionOutputs<TagName> operator()() override {
    ParseFunctionOutputs<TagName> output;

    bool flg_continue = true;
    std::apply(
        [this, &flg_continue, &output](ParseFuncs&... funcs) {
          (
              [this, &flg_continue, &output](auto& func) {
                if (flg_continue) {
                  func.last_token(this->last_token());
                  func.file_id(this->file_id());
                  func.start(this->start());
                  auto local_output = func();
                  if (output.work_) {  // only allow one of them work
                    flg_continue = false;
                    output = local_output;
                  } else {
                    output.concat(std::move(local_output), false);
                  }
                }
              }(funcs),
              ...);
        },
        parse_functions_);
    return output;
  }

 protected:
  std::tuple<ParseFuncs...> parse_functions_;
};

template <meta::Str TagName, typename... ParseFuncs>
class SerialParseFunctions : public ParseFunction<TagName> {

 public:
  using base = ParseFunction<TagName>;
  SerialParseFunctions(const SerialParseFunctions& other) = default;
  explicit SerialParseFunctions(const ParseFunctionInputs<TagName>& param,
                                ParseFuncs&&... funcs)
      : base(param), parse_functions_(funcs...) {}
  ParseFunctionOutputs<TagName> operator()() override {
    ParseFunctionOutputs<TagName> output;
    token::Token<TagName + "_first_token"> tok;
    output.last_token_ = this->last_token();
    output.file_id_ = this->file_id();
    output.start_ = this->start();

    bool flg_continue = true;
    std::apply(
        [this, &flg_continue, &output](ParseFuncs&... funcs) {
          (
              [this, &flg_continue, &output](auto& func) {
                if (flg_continue) {
                  func.last_token(output.last_token_);
                  func.file_id(output.file_id_);
                  func.start(output.start_);
                  auto local_output = func();
                  if (!local_output.work_ && !func.opt()) {
                    flg_continue = false;
                  }
                  output.concat(std::move(local_output), func.opt());
                }
              }(funcs),
              ...);
        },
        parse_functions_);
    return output;
  }

 protected:
  std::tuple<ParseFuncs...> parse_functions_;
};

#define ParseFunctionDef(NAME)                                                \
  template <meta::Str TagName = meta::S(#NAME)>                               \
  class NAME : public ParseFunction<TagName> {                                \
   public:                                                                    \
    using base = ParseFunction<TagName>;                                      \
    template <typename... Params>                                             \
    explicit NAME(bool opt, Params... params) : base(opt, params...) {}       \
    template <typename... Params>                                             \
    explicit NAME(Params... params) : base(params...) {}                      \
    explicit NAME(const ParseFunctionInputs<TagName>& param) : base(param) {} \
    template <meta::Str TagNameOther>                                         \
    explicit NAME(const ParseFunctionInputs<TagNameOther>& param)             \
        : base(param) {}                                                      \
    ParseFunctionOutputs<TagName> operator()() override;                      \
  };

ParseFunctionDef(TranslationUnit);
ParseFunctionDef(DeclarationSeq);
ParseFunctionDef(ModulePartition);
ParseFunctionDef(AttributeSpecifierSeq);
ParseFunctionDef(GlobalModuleFragment);
ParseFunctionDef(ModuleDeclaration);
ParseFunctionDef(PrivateModuleFragment);

// ----------A.5 Expressions----------
//expression:
ParseFunctionDef(Expression);
ParseFunctionDef(IdExpression);
ParseFunctionDef(LambdaExpression);
ParseFunctionDef(FoldExpression);
ParseFunctionDef(RequiresExpression);

// id-expression:
ParseFunctionDef(UnqualifiedId);
ParseFunctionDef(QualifiedId);

// unqualified_id:
ParseFunctionDef(Identifier);
ParseFunctionDef(OperatorFunctionId);
ParseFunctionDef(ConversionFunctionId);
ParseFunctionDef(LiteralOperatorId);
ParseFunctionDef(TypeName);
ParseFunctionDef(DecltypeSpecifier);
ParseFunctionDef(TemplateId);

// qualified_id:
ParseFunctionDef(NestedNameSpecifier);

// nested_name_specifier:
ParseFunctionDef(NamespaceName);
ParseFunctionDef(SimpleTemplateId);

// lambda_expression:
ParseFunctionDef(LambdaIntroducer);
ParseFunctionDef(LambdaDeclarator);
ParseFunctionDef(CompoundStatement);
ParseFunctionDef(TemplateParameterList);
ParseFunctionDef(RequiresClause);

// lambda_introducer:
ParseFunctionDef(LambdaCapture);

// lambda_declarator:
ParseFunctionDef(ParameterDeclarationClause);
ParseFunctionDef(DeclSpecifierSeq);
ParseFunctionDef(NoexceptSpecifier);

// lambda_capture:
ParseFunctionDef(CaptureDefault);
ParseFunctionDef(CaptureList);
ParseFunctionDef(Capture);
ParseFunctionDef(SimpleCapture);
ParseFunctionDef(InitCapture);

ParseFunctionDef(Initializer);

// fold_expression:
ParseFunctionDef(CastExpression);
ParseFunctionDef(FoldOperator);

// requires_expression:
ParseFunctionDef(RequirementParameterList);
ParseFunctionDef(RequirementBody);
ParseFunctionDef(RequirementSeq);
ParseFunctionDef(Requirement);

// requirement:
ParseFunctionDef(SimpleRequirement);
ParseFunctionDef(TypeRequirement);
ParseFunctionDef(CompoundRequirement);
ParseFunctionDef(NestedRequirement);
ParseFunctionDef(ReturnTypeRequirement);

ParseFunctionDef(TypeConstraint);
ParseFunctionDef(ConstraintExpression);

ParseFunctionDef(TypeId);

// postfix_expression:
ParseFunctionDef(PostfixExpression);
ParseFunctionDef(PrimaryExpression);
ParseFunctionDef(ExprOrBracedInitList);
ParseFunctionDef(ExpressionList);
ParseFunctionDef(SimpleTypeSpecifier);
ParseFunctionDef(TypenameSpecifier);
ParseFunctionDef(BracedInitList);

// expression-list:
ParseFunctionDef(InitializerList);

// unary_expression:
ParseFunctionDef(UnaryExpression);
ParseFunctionDef(UnaryOperator);
ParseFunctionDef(AwaitExpression);
ParseFunctionDef(NoexceptExpression);
ParseFunctionDef(NewExpression);
ParseFunctionDef(DeleteExpression);

// new_expression:
ParseFunctionDef(NewPlacement);
ParseFunctionDef(NewTypeId);
ParseFunctionDef(NewInitializer);
ParseFunctionDef(TypeSpecifierSeq);
ParseFunctionDef(PtrOperator);
ParseFunctionDef(NoptrNewDeclarator);
ParseFunctionDef(NewDeclarator);
ParseFunctionDef(ConstantExpression);

// pm_expression:
ParseFunctionDef(PmExpression);

// multiplicative_expression:
ParseFunctionDef(MultiplicativeExpression);

// additive_expression:
ParseFunctionDef(AdditiveExpression);

//shift_expression:
ParseFunctionDef(ShiftExpression);

// compare_expression:
ParseFunctionDef(CompareExpression);

// relational_expression:
ParseFunctionDef(RelationalExpression);

// equality-expression:
ParseFunctionDef(EqualityExpression);

// and-expression:
ParseFunctionDef(AndExpression);

// exclusive_or_expression:
ParseFunctionDef(ExclusiveOrExpression);

// inclusive-or-expression:
ParseFunctionDef(InclusiveOrExpression);

// logical_and_expression:
ParseFunctionDef(LogicalAndExpression);

// logical_and_expression:
ParseFunctionDef(LogicalOrExpression);

// conditional_expression:
ParseFunctionDef(ConditionalExpression);
ParseFunctionDef(AssignmentExpression);

// yield_expression:
ParseFunctionDef(YieldExpression);

// throw_expression:
ParseFunctionDef(ThrowExpression);

// assignment_expression::
ParseFunctionDef(AssignmentOperator);
ParseFunctionDef(InitializerClause);

// ----------A.6 Statements----------

// statement:
ParseFunctionDef(Statement);
ParseFunctionDef(LabeledStatement);
ParseFunctionDef(ExpressionStatement);
ParseFunctionDef(SelectionStatement);
ParseFunctionDef(IterationStatement);
ParseFunctionDef(JumpStatement);
ParseFunctionDef(DeclarationStatement);
ParseFunctionDef(TryBlock);

// init_statement:
ParseFunctionDef(InitStatement);

// condition:
ParseFunctionDef(Condition);
ParseFunctionDef(BraceOrEqualInitializer);

ParseFunctionDef(StatementSeq);
ParseFunctionDef(ForRangeDeclaration);
ParseFunctionDef(ForRangeInitializer);
ParseFunctionDef(RefQualifier);
ParseFunctionDef(IdentifierList);
ParseFunctionDef(CoroutineReturnStatement);

// ----------A.7 Declarations----------

// Declaration
ParseFunctionDef(Declaration);

ParseFunctionDef(BlockDeclaration);
ParseFunctionDef(NodeclspecFunctionDeclaration);
ParseFunctionDef(FunctionDefinition);
ParseFunctionDef(TemplateDeclaration);
ParseFunctionDef(DeductionGuide);
ParseFunctionDef(ExplicitInstantiation);
ParseFunctionDef(ExplicitSpecialization);
ParseFunctionDef(ExportDeclaration);
ParseFunctionDef(LinkageSpecification);
ParseFunctionDef(NamespaceDefinition);
ParseFunctionDef(EmptyDeclaration);
ParseFunctionDef(AttributeDeclaration);
ParseFunctionDef(ModuleImportDeclaration);

// block_declaration:
ParseFunctionDef(SimpleDeclaration);
ParseFunctionDef(AsmDeclaration);
ParseFunctionDef(NamespaceAliasDefinition);
ParseFunctionDef(UsingDeclaration);
ParseFunctionDef(UsingEnumDeclaration);
ParseFunctionDef(UsingDirective);
ParseFunctionDef(StaticAssertDeclaration);
ParseFunctionDef(AliasDeclaration);
ParseFunctionDef(OpaqueEnumDeclaration);

// declarator:
ParseFunctionDef(Declarator);
ParseFunctionDef(PtrDeclarator);
ParseFunctionDef(NoptrDeclarator);
ParseFunctionDef(ParametersAndQualifiers);
ParseFunctionDef(TrailingReturnType);

ParseFunctionDef(DefiningTypeId);
ParseFunctionDef(InitDeclaratorList);
ParseFunctionDef(StringLiteral);

// decl_specifier:
ParseFunctionDef(DeclSpecifier);
ParseFunctionDef(StorageClassSpecifier);
ParseFunctionDef(DefiningTypeSpecifier);
ParseFunctionDef(FunctionSpecifier);

ParseFunctionDef(ExplicitSpecifier);

// type-specifier:
ParseFunctionDef(TypeSpecifier);
ParseFunctionDef(ElaboratedTypeSpecifier);
ParseFunctionDef(CvQualifier);

// defining_type_specifier:
ParseFunctionDef(DefiningTypeSpecifierSeq);
ParseFunctionDef(ClassSpecifier);
ParseFunctionDef(EnumSpecifier);

#undef ParseFunctionDef

}  // namespace lps::parser::details
