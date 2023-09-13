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

#include "sema.h"
#include <memory>
#include "basic/exception.h"
#include "basic/vec.h"
#include "semantic_unit/unit.h"
#include "token.h"

namespace lps::sema {

std::unique_ptr<details::Unit> Factory::create(
    const parser::details::Tree::Node& node) {

  lps_assert(kTag, node.valid());

  if (node.kind_ == parser::details::ParseFunctionKind::kExpectedToken) {
    return create_by_token(node);
  }

  basic::Vector<2, std::unique_ptr<details::Unit>> elements;
  int32_t idx = 0;
  for (const auto& child : node.children_) {
    elements.append(std::move(create(child)));
  }

  switch (node.kind_) {

    case parser::details::ParseFunctionKind::kTranslationUnit: {
      return std::make_unique<details::unit::TranslationUnit>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPrimaryExpression: {
      return std::make_unique<details::unit::PrimaryExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kIdExpression: {
      return std::make_unique<details::unit::IdExpression>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kUnqualifiedId: {
      return std::make_unique<details::unit::UnqualifiedId>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kQualifiedId: {
      return std::make_unique<details::unit::QualifiedId>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNestedNameSpecifier: {
      return std::make_unique<details::unit::NestedNameSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kLambdaExpression: {
      return std::make_unique<details::unit::LambdaExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kLambdaIntroducer: {
      return std::make_unique<details::unit::LambdaIntroducer>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kLambdaDeclarator: {
      return std::make_unique<details::unit::LambdaDeclarator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kLambdaCapture: {
      return std::make_unique<details::unit::LambdaCapture>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kCaptureDefault: {
      return std::make_unique<details::unit::CaptureDefault>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kCaptureList: {
      return std::make_unique<details::unit::CaptureList>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kCapture: {
      return std::make_unique<details::unit::Capture>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kSimpleCapture: {
      return std::make_unique<details::unit::SimpleCapture>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kInitCapture: {
      return std::make_unique<details::unit::InitCapture>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kFoldExpression: {
      return std::make_unique<details::unit::FoldExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kFoldOperator: {
      return std::make_unique<details::unit::FoldOperator>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kRequiresExpression: {
      return std::make_unique<details::unit::RequiresExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kRequirementParameterList: {
      return std::make_unique<details::unit::RequirementParameterList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kRequirementBody: {
      return std::make_unique<details::unit::RequirementBody>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kRequirementSeq: {
      return std::make_unique<details::unit::RequirementSeq>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kRequirement: {
      return std::make_unique<details::unit::Requirement>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kSimpleRequirement: {
      return std::make_unique<details::unit::SimpleRequirement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTypeRequirement: {
      return std::make_unique<details::unit::TypeRequirement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kCompoundRequirement: {
      return std::make_unique<details::unit::CompoundRequirement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kReturnTypeRequirement: {
      return std::make_unique<details::unit::ReturnTypeRequirement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNestedRequirement: {
      return std::make_unique<details::unit::NestedRequirement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPostfixExpression: {
      return std::make_unique<details::unit::PostfixExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kExpressionList: {
      return std::make_unique<details::unit::ExpressionList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kUnaryExpression: {
      return std::make_unique<details::unit::UnaryExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kUnaryOperator: {
      return std::make_unique<details::unit::UnaryOperator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAwaitExpression: {
      return std::make_unique<details::unit::AwaitExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNoexceptExpression: {
      return std::make_unique<details::unit::NoexceptExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNewExpression: {
      return std::make_unique<details::unit::NewExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNewPlacement: {
      return std::make_unique<details::unit::NewPlacement>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNewTypeId: {
      return std::make_unique<details::unit::NewTypeId>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNewDeclarator: {
      return std::make_unique<details::unit::NewDeclarator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNoptrNewDeclarator: {
      return std::make_unique<details::unit::NoptrNewDeclarator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNewInitializer: {
      return std::make_unique<details::unit::NewInitializer>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDeleteExpression: {
      return std::make_unique<details::unit::DeleteExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kCastExpression: {
      return std::make_unique<details::unit::CastExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPmExpression: {
      return std::make_unique<details::unit::PmExpression>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kMultiplicativeExpression: {
      return std::make_unique<details::unit::MultiplicativeExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAdditiveExpression: {
      return std::make_unique<details::unit::AdditiveExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kShiftExpression: {
      return std::make_unique<details::unit::ShiftExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kCompareExpression: {
      return std::make_unique<details::unit::CompareExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kRelationalExpression: {
      return std::make_unique<details::unit::RelationalExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEqualityExpression: {
      return std::make_unique<details::unit::EqualityExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAndExpression: {
      return std::make_unique<details::unit::AndExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kExclusiveOrExpression: {
      return std::make_unique<details::unit::ExclusiveOrExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kInclusiveOrExpression: {
      return std::make_unique<details::unit::InclusiveOrExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kLogicalAndExpression: {
      return std::make_unique<details::unit::LogicalAndExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kLogicalOrExpression: {
      return std::make_unique<details::unit::LogicalOrExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kConditionalExpression: {
      return std::make_unique<details::unit::ConditionalExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kYieldExpression: {
      return std::make_unique<details::unit::YieldExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kThrowExpression: {
      return std::make_unique<details::unit::ThrowExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAssignmentExpression: {
      return std::make_unique<details::unit::AssignmentExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAssignmentOperator: {
      return std::make_unique<details::unit::AssignmentOperator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kExpression: {
      return std::make_unique<details::unit::Expression>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kConstantExpression: {
      return std::make_unique<details::unit::ConstantExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kStatement: {
      return std::make_unique<details::unit::Statement>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kInitStatement: {
      return std::make_unique<details::unit::InitStatement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kCondition: {
      return std::make_unique<details::unit::Condition>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kLabeledStatement: {
      return std::make_unique<details::unit::LabeledStatement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kExpressionStatement: {
      return std::make_unique<details::unit::ExpressionStatement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kCompoundStatement: {
      return std::make_unique<details::unit::CompoundStatement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kStatementSeq: {
      return std::make_unique<details::unit::StatementSeq>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kSelectionStatement: {
      return std::make_unique<details::unit::SelectionStatement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kIterationStatement: {
      return std::make_unique<details::unit::IterationStatement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kForRangeDeclaration: {
      return std::make_unique<details::unit::ForRangeDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kForRangeInitializer: {
      return std::make_unique<details::unit::ForRangeInitializer>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kJumpStatement: {
      return std::make_unique<details::unit::JumpStatement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kCoroutineReturnStatement: {
      return std::make_unique<details::unit::CoroutineReturnStatement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDeclarationStatement: {
      return std::make_unique<details::unit::DeclarationStatement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDeclarationSeq: {
      return std::make_unique<details::unit::DeclarationSeq>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDeclaration: {
      return std::make_unique<details::unit::Declaration>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kBlockDeclaration: {
      return std::make_unique<details::unit::BlockDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNodeclspecFunctionDeclaration: {
      return std::make_unique<details::unit::NodeclspecFunctionDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAliasDeclaration: {
      return std::make_unique<details::unit::AliasDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kSimpleDeclaration: {
      return std::make_unique<details::unit::SimpleDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kStaticAssertDeclaration: {
      return std::make_unique<details::unit::StaticAssertDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEmptyDeclaration: {
      return std::make_unique<details::unit::EmptyDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAttributeDeclaration: {
      return std::make_unique<details::unit::AttributeDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDeclSpecifier: {
      return std::make_unique<details::unit::DeclSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDeclSpecifierSeq: {
      return std::make_unique<details::unit::DeclSpecifierSeq>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kStorageClassSpecifier: {
      return std::make_unique<details::unit::StorageClassSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kFunctionSpecifier: {
      return std::make_unique<details::unit::FunctionSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kExplicitSpecifier: {
      return std::make_unique<details::unit::ExplicitSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTypedefName: {
      return std::make_unique<details::unit::TypedefName>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTypeSpecifier: {
      return std::make_unique<details::unit::TypeSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTypeSpecifierSeq: {
      return std::make_unique<details::unit::TypeSpecifierSeq>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDefiningTypeSpecifier: {
      return std::make_unique<details::unit::DefiningTypeSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDefiningTypeSpecifierSeq: {
      return std::make_unique<details::unit::DefiningTypeSpecifierSeq>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kSimpleTypeSpecifier: {
      return std::make_unique<details::unit::SimpleTypeSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTypeName: {
      return std::make_unique<details::unit::TypeName>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kElaboratedTypeSpecifier: {
      return std::make_unique<details::unit::ElaboratedTypeSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kElaboratedEnumSpecifier: {
      return std::make_unique<details::unit::ElaboratedEnumSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDecltypeSpecifier: {
      return std::make_unique<details::unit::DecltypeSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPlaceholderTypeSpecifier: {
      return std::make_unique<details::unit::PlaceholderTypeSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kInitDeclaratorList: {
      return std::make_unique<details::unit::InitDeclaratorList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kInitDeclarator: {
      return std::make_unique<details::unit::InitDeclarator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDeclarator: {
      return std::make_unique<details::unit::Declarator>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPtrDeclarator: {
      return std::make_unique<details::unit::PtrDeclarator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNoptrDeclarator: {
      return std::make_unique<details::unit::NoptrDeclarator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kParametersAndQualifiers: {
      return std::make_unique<details::unit::ParametersAndQualifiers>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTrailingReturnType: {
      return std::make_unique<details::unit::TrailingReturnType>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPtrOperator: {
      return std::make_unique<details::unit::PtrOperator>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kCvQualifierSeq: {
      return std::make_unique<details::unit::CvQualifierSeq>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kCvQualifier: {
      return std::make_unique<details::unit::CvQualifier>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kRefQualifier: {
      return std::make_unique<details::unit::RefQualifier>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDeclaratorId: {
      return std::make_unique<details::unit::DeclaratorId>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTypeId: {
      return std::make_unique<details::unit::TypeId>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDefiningTypeId: {
      return std::make_unique<details::unit::DefiningTypeId>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAbstractDeclarator: {
      return std::make_unique<details::unit::AbstractDeclarator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPtrAbstractDeclarator: {
      return std::make_unique<details::unit::PtrAbstractDeclarator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNoptrAbstractDeclarator: {
      return std::make_unique<details::unit::NoptrAbstractDeclarator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAbstractPackDeclarator: {
      return std::make_unique<details::unit::AbstractPackDeclarator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNoptrAbstractPackDeclarator: {
      return std::make_unique<details::unit::NoptrAbstractPackDeclarator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kParameterDeclarationClause: {
      return std::make_unique<details::unit::ParameterDeclarationClause>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kParameterDeclarationList: {
      return std::make_unique<details::unit::ParameterDeclarationList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kParameterDeclaration: {
      return std::make_unique<details::unit::ParameterDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kInitializer: {
      return std::make_unique<details::unit::Initializer>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kBraceOrEqualInitializer: {
      return std::make_unique<details::unit::BraceOrEqualInitializer>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kInitializerClause: {
      return std::make_unique<details::unit::InitializerClause>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kBracedInitList: {
      return std::make_unique<details::unit::BracedInitList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kInitializerList: {
      return std::make_unique<details::unit::InitializerList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDesignatedInitializerList: {
      return std::make_unique<details::unit::DesignatedInitializerList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDesignatedInitializerClause: {
      return std::make_unique<details::unit::DesignatedInitializerClause>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDesignator: {
      return std::make_unique<details::unit::Designator>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kExprOrBracedInitList: {
      return std::make_unique<details::unit::ExprOrBracedInitList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kFunctionDefinition: {
      return std::make_unique<details::unit::FunctionDefinition>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kFunctionBody: {
      return std::make_unique<details::unit::FunctionBody>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEnumName: {
      return std::make_unique<details::unit::EnumName>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEnumSpecifier: {
      return std::make_unique<details::unit::EnumSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEnumHead: {
      return std::make_unique<details::unit::EnumHead>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEnumHeadName: {
      return std::make_unique<details::unit::EnumHeadName>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kOpaqueEnumDeclaration: {
      return std::make_unique<details::unit::OpaqueEnumDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEnumKey: {
      return std::make_unique<details::unit::EnumKey>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEnumBase: {
      return std::make_unique<details::unit::EnumBase>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEnumeratorList: {
      return std::make_unique<details::unit::EnumeratorList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEnumeratorDefinition: {
      return std::make_unique<details::unit::EnumeratorDefinition>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEnumerator: {
      return std::make_unique<details::unit::Enumerator>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kUsingEnumDeclaration: {
      return std::make_unique<details::unit::UsingEnumDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNamespaceName: {
      return std::make_unique<details::unit::NamespaceName>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNamespaceDefinition: {
      return std::make_unique<details::unit::NamespaceDefinition>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNamedNamespaceDefinition: {
      return std::make_unique<details::unit::NamedNamespaceDefinition>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kUnnamedNamespaceDefinition: {
      return std::make_unique<details::unit::UnnamedNamespaceDefinition>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNestedNamespaceDefinition: {
      return std::make_unique<details::unit::NestedNamespaceDefinition>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEnclosingNamespaceSpecifier: {
      return std::make_unique<details::unit::EnclosingNamespaceSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNamespaceBody: {
      return std::make_unique<details::unit::NamespaceBody>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNamespaceAlias: {
      return std::make_unique<details::unit::NamespaceAlias>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNamespaceAliasDefinition: {
      return std::make_unique<details::unit::NamespaceAliasDefinition>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kQualifiedNamespaceSpecifier: {
      return std::make_unique<details::unit::QualifiedNamespaceSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kUsingDirective: {
      return std::make_unique<details::unit::UsingDirective>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kUsingDeclaration: {
      return std::make_unique<details::unit::UsingDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kUsingDeclaratorList: {
      return std::make_unique<details::unit::UsingDeclaratorList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kUsingDeclarator: {
      return std::make_unique<details::unit::UsingDeclarator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAsmDeclaration: {
      return std::make_unique<details::unit::AsmDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kLinkageSpecification: {
      return std::make_unique<details::unit::LinkageSpecification>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAttributeSpecifierSeq: {
      return std::make_unique<details::unit::AttributeSpecifierSeq>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAttributeSpecifier: {
      return std::make_unique<details::unit::AttributeSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAlignmentSpecifier: {
      return std::make_unique<details::unit::AlignmentSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAttributeUsingPrefix: {
      return std::make_unique<details::unit::AttributeUsingPrefix>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAttributeList: {
      return std::make_unique<details::unit::AttributeList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAttribute: {
      return std::make_unique<details::unit::Attribute>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAttributeToken: {
      return std::make_unique<details::unit::AttributeToken>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAttributeScopedToken: {
      return std::make_unique<details::unit::AttributeScopedToken>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAttributeNamespace: {
      return std::make_unique<details::unit::AttributeNamespace>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAttributeArgumentClause: {
      return std::make_unique<details::unit::AttributeArgumentClause>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kBalancedTokenSeq: {
      return std::make_unique<details::unit::BalancedTokenSeq>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kBalancedToken: {
      return std::make_unique<details::unit::BalancedToken>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kModuleDeclaration: {
      return std::make_unique<details::unit::ModuleDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kModuleName: {
      return std::make_unique<details::unit::ModuleName>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kModulePartition: {
      return std::make_unique<details::unit::ModulePartition>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kModuleNameQualifier: {
      return std::make_unique<details::unit::ModuleNameQualifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kExportDeclaration: {
      return std::make_unique<details::unit::ExportDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kModuleImportDeclaration: {
      return std::make_unique<details::unit::ModuleImportDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kGlobalModuleFragment: {
      return std::make_unique<details::unit::GlobalModuleFragment>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPrivateModuleFragment: {
      return std::make_unique<details::unit::PrivateModuleFragment>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kClassName: {
      return std::make_unique<details::unit::ClassName>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kClassSpecifier: {
      return std::make_unique<details::unit::ClassSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kClassHead: {
      return std::make_unique<details::unit::ClassHead>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kClassHeadName: {
      return std::make_unique<details::unit::ClassHeadName>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kClassVirtSpecifier: {
      return std::make_unique<details::unit::ClassVirtSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kClassKey: {
      return std::make_unique<details::unit::ClassKey>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kMemberSpecification: {
      return std::make_unique<details::unit::MemberSpecification>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kMemberDeclaration: {
      return std::make_unique<details::unit::MemberDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kMemberDeclaratorList: {
      return std::make_unique<details::unit::MemberDeclaratorList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kMemberDeclarator: {
      return std::make_unique<details::unit::MemberDeclarator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kVirtSpecifierSeq: {
      return std::make_unique<details::unit::VirtSpecifierSeq>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kVirtSpecifier: {
      return std::make_unique<details::unit::VirtSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPureSpecifier: {
      return std::make_unique<details::unit::PureSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kConversionFunctionId: {
      return std::make_unique<details::unit::ConversionFunctionId>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kConversionTypeId: {
      return std::make_unique<details::unit::ConversionTypeId>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kConversionDeclarator: {
      return std::make_unique<details::unit::ConversionDeclarator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kBaseClause: {
      return std::make_unique<details::unit::BaseClause>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kBaseSpecifierList: {
      return std::make_unique<details::unit::BaseSpecifierList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kBaseSpecifier: {
      return std::make_unique<details::unit::BaseSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kClassOrDecltype: {
      return std::make_unique<details::unit::ClassOrDecltype>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kAccessSpecifier: {
      return std::make_unique<details::unit::AccessSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kCtorInitializer: {
      return std::make_unique<details::unit::CtorInitializer>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kMemInitializerList: {
      return std::make_unique<details::unit::MemInitializerList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kMemInitializer: {
      return std::make_unique<details::unit::MemInitializer>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kMemInitializerId: {
      return std::make_unique<details::unit::MemInitializerId>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kOperatorFunctionId: {
      return std::make_unique<details::unit::OperatorFunctionId>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTheOperator: {
      return std::make_unique<details::unit::TheOperator>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kLiteralOperatorId: {
      return std::make_unique<details::unit::LiteralOperatorId>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTemplateDeclaration: {
      return std::make_unique<details::unit::TemplateDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTemplateHead: {
      return std::make_unique<details::unit::TemplateHead>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTemplateParameterList: {
      return std::make_unique<details::unit::TemplateParameterList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kRequiresClause: {
      return std::make_unique<details::unit::RequiresClause>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kConstraintLogicalOrExpression: {
      return std::make_unique<details::unit::ConstraintLogicalOrExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kConstraintLogicalAndExpression: {
      return std::make_unique<details::unit::ConstraintLogicalAndExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTemplateParameter: {
      return std::make_unique<details::unit::TemplateParameter>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTypeParameter: {
      return std::make_unique<details::unit::TypeParameter>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTypeParameterKey: {
      return std::make_unique<details::unit::TypeParameterKey>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTypeConstraint: {
      return std::make_unique<details::unit::TypeConstraint>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kSimpleTemplateId: {
      return std::make_unique<details::unit::SimpleTemplateId>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTemplateId: {
      return std::make_unique<details::unit::TemplateId>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTemplateName: {
      return std::make_unique<details::unit::TemplateName>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTemplateArgumentList: {
      return std::make_unique<details::unit::TemplateArgumentList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTemplateArgument: {
      return std::make_unique<details::unit::TemplateArgument>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kConstraintExpression: {
      return std::make_unique<details::unit::ConstraintExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDeductionGuide: {
      return std::make_unique<details::unit::DeductionGuide>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kConceptDefinition: {
      return std::make_unique<details::unit::ConceptDefinition>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kConceptName: {
      return std::make_unique<details::unit::ConceptName>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTypenameSpecifier: {
      return std::make_unique<details::unit::TypenameSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kExplicitInstantiation: {
      return std::make_unique<details::unit::ExplicitInstantiation>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kExplicitSpecialization: {
      return std::make_unique<details::unit::ExplicitSpecialization>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTryBlock: {
      return std::make_unique<details::unit::TryBlock>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kFunctionTryBlock: {
      return std::make_unique<details::unit::FunctionTryBlock>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHandlerSeq: {
      return std::make_unique<details::unit::HandlerSeq>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHandler: {
      return std::make_unique<details::unit::Handler>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kExceptionDeclaration: {
      return std::make_unique<details::unit::ExceptionDeclaration>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNoexceptSpecifier: {
      return std::make_unique<details::unit::NoexceptSpecifier>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPreprocessingFile: {
      return std::make_unique<details::unit::PreprocessingFile>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kModuleFile: {
      return std::make_unique<details::unit::ModuleFile>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPpGlobalModuleFragment: {
      return std::make_unique<details::unit::PpGlobalModuleFragment>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPpPrivateModuleFragment: {
      return std::make_unique<details::unit::PpPrivateModuleFragment>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kGroup: {
      return std::make_unique<details::unit::Group>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kGroupPart: {
      return std::make_unique<details::unit::GroupPart>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kControlLine: {
      return std::make_unique<details::unit::ControlLine>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kIfSection: {
      return std::make_unique<details::unit::IfSection>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kIfGroup: {
      return std::make_unique<details::unit::IfGroup>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kElifGroups: {
      return std::make_unique<details::unit::ElifGroups>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kElifGroup: {
      return std::make_unique<details::unit::ElifGroup>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kElseGroup: {
      return std::make_unique<details::unit::ElseGroup>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEndifLine: {
      return std::make_unique<details::unit::EndifLine>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kTextLine: {
      return std::make_unique<details::unit::TextLine>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kConditionallySupportedDirective: {
      return std::make_unique<details::unit::ConditionallySupportedDirective>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kLparen: {
      return std::make_unique<details::unit::Lparen>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kIdentifierList: {
      return std::make_unique<details::unit::IdentifierList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kReplacementList: {
      return std::make_unique<details::unit::ReplacementList>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPpTokens: {
      return std::make_unique<details::unit::PpTokens>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNewLine: {
      return std::make_unique<details::unit::NewLine>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDefinedMacroExpression: {
      return std::make_unique<details::unit::DefinedMacroExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHPreprocessingToken: {
      return std::make_unique<details::unit::HPreprocessingToken>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHPpTokens: {
      return std::make_unique<details::unit::HPpTokens>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHeaderNameTokens: {
      return std::make_unique<details::unit::HeaderNameTokens>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHasIncludeExpression: {
      return std::make_unique<details::unit::HasIncludeExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHasAttributeExpression: {
      return std::make_unique<details::unit::HasAttributeExpression>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPpModule: {
      return std::make_unique<details::unit::PpModule>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPpImport: {
      return std::make_unique<details::unit::PpImport>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kVaOptReplacement: {
      return std::make_unique<details::unit::VaOptReplacement>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHexQuad: {
      return std::make_unique<details::unit::HexQuad>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kUniversalCharacterName: {
      return std::make_unique<details::unit::UniversalCharacterName>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPreprocessingToken: {
      return std::make_unique<details::unit::PreprocessingToken>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kToken: {
      return std::make_unique<details::unit::Token>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHeaderName: {
      return std::make_unique<details::unit::HeaderName>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHCharSequence: {
      return std::make_unique<details::unit::HCharSequence>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHChar: {
      return std::make_unique<details::unit::HChar>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kQCharSequence: {
      return std::make_unique<details::unit::QCharSequence>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kQChar: {
      return std::make_unique<details::unit::QChar>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPpNumber: {
      return std::make_unique<details::unit::PpNumber>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kIdentifierNondigit: {
      return std::make_unique<details::unit::IdentifierNondigit>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNondigit: {
      return std::make_unique<details::unit::Nondigit>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDigit: {
      return std::make_unique<details::unit::Digit>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kKeyword: {
      return std::make_unique<details::unit::Keyword>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPreprocessingOpOrPunc: {
      return std::make_unique<details::unit::PreprocessingOpOrPunc>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPreprocessingOperator: {
      return std::make_unique<details::unit::PreprocessingOperator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kOperatorOrPunctuator: {
      return std::make_unique<details::unit::OperatorOrPunctuator>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kLiteral: {
      return std::make_unique<details::unit::Literal>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kBinaryDigit: {
      return std::make_unique<details::unit::BinaryDigit>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kOctalDigit: {
      return std::make_unique<details::unit::OctalDigit>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kNonzeroDigit: {
      return std::make_unique<details::unit::NonzeroDigit>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHexadecimalPrefix: {
      return std::make_unique<details::unit::HexadecimalPrefix>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHexadecimalDigitSequence: {
      return std::make_unique<details::unit::HexadecimalDigitSequence>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHexadecimalDigit: {
      return std::make_unique<details::unit::HexadecimalDigit>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kIntegerSuffix: {
      return std::make_unique<details::unit::IntegerSuffix>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kUnsignedSuffix: {
      return std::make_unique<details::unit::UnsignedSuffix>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kLongSuffix: {
      return std::make_unique<details::unit::LongSuffix>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kLongLongSuffix: {
      return std::make_unique<details::unit::LongLongSuffix>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEncodingPrefix: {
      return std::make_unique<details::unit::EncodingPrefix>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kCCharSequence: {
      return std::make_unique<details::unit::CCharSequence>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kCChar: {
      return std::make_unique<details::unit::CChar>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kEscapeSequence: {
      return std::make_unique<details::unit::EscapeSequence>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kSimpleEscapeSequence: {
      return std::make_unique<details::unit::SimpleEscapeSequence>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kOctalEscapeSequence: {
      return std::make_unique<details::unit::OctalEscapeSequence>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHexadecimalEscapeSequence: {
      return std::make_unique<details::unit::HexadecimalEscapeSequence>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDecimalFloatingPointLiteral: {
      return std::make_unique<details::unit::DecimalFloatingPointLiteral>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHexadecimalFloatingPointLiteral: {
      return std::make_unique<details::unit::HexadecimalFloatingPointLiteral>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kFractionalConstant: {
      return std::make_unique<details::unit::FractionalConstant>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kHexadecimalFractionalConstant: {
      return std::make_unique<details::unit::HexadecimalFractionalConstant>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kExponentPart: {
      return std::make_unique<details::unit::ExponentPart>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kBinaryExponentPart: {
      return std::make_unique<details::unit::BinaryExponentPart>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kSign: {
      return std::make_unique<details::unit::Sign>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDigitSequence: {
      return std::make_unique<details::unit::DigitSequence>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kFloatingPointSuffix: {
      return std::make_unique<details::unit::FloatingPointSuffix>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kSCharSequence: {
      return std::make_unique<details::unit::SCharSequence>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kSChar: {
      return std::make_unique<details::unit::SChar>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kRCharSequence: {
      return std::make_unique<details::unit::RCharSequence>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kRChar: {
      return std::make_unique<details::unit::RChar>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDCharSequence: {
      return std::make_unique<details::unit::DCharSequence>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kDChar: {
      return std::make_unique<details::unit::DChar>(std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kBooleanLiteral: {
      return std::make_unique<details::unit::BooleanLiteral>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kPointerLiteral: {
      return std::make_unique<details::unit::PointerLiteral>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kUserDefinedLiteral: {
      return std::make_unique<details::unit::UserDefinedLiteral>(
          std::move(elements));
      break;
    }

    case parser::details::ParseFunctionKind::kUdSuffix: {
      return std::make_unique<details::unit::UdSuffix>(std::move(elements));
      break;
    }

    default:
      LPS_ERROR(kTag, "not support yet: ", node.kind_);
      break;
  }
  unreachable(kTag);
  return nullptr;
}

std::unique_ptr<details::Unit> Factory::create_by_token(
    const parser::details::Tree::Node& node) {

  lps_assert(kTag, node.token_kind_ != token::details::TokenKind::unknown);

  switch (node.token_kind_) {
    case token::details::TokenKind::kw_this:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::l_paren:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::r_paren:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::tilde:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::kw_template:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::coloncolon:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::less:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::greater:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::l_square:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::r_square:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::comma:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::amp:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::equal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::ellipsis:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::star:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::plus:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::minus:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::slash:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::percent:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::caret:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::pipe:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::lessless:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::greatergreater:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::plusequal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::minusequal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::starequal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::slashequal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::percentequal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::caretequal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::ampequal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::pipeequal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::lesslessequal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::greatergreaterequal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::equalequal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::exclaimequal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::lessequal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::greaterequal:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::ampamp:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::pipepipe:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::periodstar:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::arrowstar:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::kw_requires:
      break;
    case token::details::TokenKind::l_brace:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::r_brace:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::semi:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::kw_typename:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::kw_noexcept:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::arrow:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::period:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::plusplus:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::minusminus:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::kw_dynamic_cast:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_static_cast:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_reinterpret_cast:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_const_cast:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_typeid:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_sizeof:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::identifier:
      return std::make_unique<details::Variable>(node.start_);
      break;
    case token::details::TokenKind::kw_alignof:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::exclaim:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::kw_co_await:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_new:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_delete:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::spaceship:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::question:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::colon:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::kw_co_yield:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_throw:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_case:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_default:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_if:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_constexpr:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_else:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_switch:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_while:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::hashhash:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::kw_do:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_for:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_break:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_continue:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_return:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_goto:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_co_return:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_using:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_static_assert:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_friend:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_typedef:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_consteval:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_constinit:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_inline:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_static:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_thread_local:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_extern:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_mutable:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_virtual:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_explicit:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_char:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_char8_t:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
      break;
    case token::details::TokenKind::kw_char16_t:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_char32_t:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_wchar_t:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_bool:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_short:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_int:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_long:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_signed:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_unsigned:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_float:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_double:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_void:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_enum:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_decltype:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_auto:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_const:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_volatile:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_class:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_struct:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_namespace:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_asm:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_alignas:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_export:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_module:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_import:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_private:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_final:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_union:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_override:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_operator:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_protected:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_public:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_concept:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_try:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_catch:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::hash:
      return std::make_unique<details::Symbol>(node.start_);
      break;
    case token::details::TokenKind::kw_include:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_define:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_undef:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_line:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_error:
      break;
    case token::details::TokenKind::kw_pragma:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_ifdef:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_ifndef:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_elif:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_endif:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_defined:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw___has_include:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw___has_cpp_attribute:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw___va_opt__:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_and:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_or:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_xor:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_not:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_bitand:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_bitor:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_compl:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_and_eq:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_or_eq:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_xor_eq:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_not_eq:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_false:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_true:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::kw_nullptr:
      return std::make_unique<details::KeyWord>(node.start_);
      break;
    case token::details::TokenKind::binary_literal:
      return std::make_unique<details::Literal>(node.start_);
      break;
    case token::details::TokenKind::floating_point_literal:
      return std::make_unique<details::Literal>(node.start_);
      break;
    case token::details::TokenKind::char_literal:
      return std::make_unique<details::Literal>(node.start_);
      break;
    case token::details::TokenKind::string_literal:
      return std::make_unique<details::Literal>(node.start_);
      break;
    case token::details::TokenKind::integer_literal:
      return std::make_unique<details::Literal>(node.start_);
      break;
    case token::details::TokenKind::decimal_literal:
      return std::make_unique<details::Literal>(node.start_);
      break;
    case token::details::TokenKind::octal_literal:
      return std::make_unique<details::Literal>(node.start_);
      break;
    case token::details::TokenKind::hexadecimal_literal:
      return std::make_unique<details::Literal>(node.start_);
      break;
    case token::details::TokenKind::raw_string:
      return std::make_unique<details::Literal>(node.start_);
      break;
    case token::details::TokenKind::user_defined_char_literal:
      return std::make_unique<details::Literal>(node.start_);
      break;
    case token::details::TokenKind::user_defined_string_literal:
      return std::make_unique<details::Literal>(node.start_);
      break;
    case token::details::TokenKind::user_defined_floating_point_literal:
      return std::make_unique<details::Literal>(node.start_);
      break;
    case token::details::TokenKind::user_defined_integer_literal:
      return std::make_unique<details::Literal>(node.start_);
      break;
    default:
      LPS_ERROR(kTag, "not support token: ", node.token_kind_);
      break;
  }
  unreachable(kTag);
  return nullptr;
}

}  // namespace lps::sema
