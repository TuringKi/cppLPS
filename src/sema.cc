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
#include "token.h"

namespace lps::sema {

std::unique_ptr<details::Unit> Factory::create(
    const parser::details::Tree::Node& node) {

  lps_assert(kTag, node.valid());

  if (node.kind_ == parser::details::ParseFunctionKind::kUnknown) {
    return create_by_token(node);
  }

  basic::Vector<2, std::unique_ptr<details::Unit>> elements(
      node.children_.size(), nullptr);
  int32_t idx = 0;
  for (const auto& child : node.children_) {
    elements[idx++] = std::move(create(node));
  }

  switch (node.kind_) {
    case parser::details::ParseFunctionKind::kLogicalAndExpression: {

      break;
    }

    case parser::details::ParseFunctionKind::kUnknown:
    case parser::details::ParseFunctionKind::kExpectedToken:
    case parser::details::ParseFunctionKind::kTranslationUnit:
    case parser::details::ParseFunctionKind::kPrimaryExpression:
    case parser::details::ParseFunctionKind::kIdExpression:
    case parser::details::ParseFunctionKind::kUnqualifiedId:
    case parser::details::ParseFunctionKind::kQualifiedId:
    case parser::details::ParseFunctionKind::kNestedNameSpecifier:
    case parser::details::ParseFunctionKind::kLambdaExpression:
    case parser::details::ParseFunctionKind::kLambdaIntroducer:
    case parser::details::ParseFunctionKind::kLambdaDeclarator:
    case parser::details::ParseFunctionKind::kLambdaCapture:
    case parser::details::ParseFunctionKind::kCaptureDefault:
    case parser::details::ParseFunctionKind::kCaptureList:
    case parser::details::ParseFunctionKind::kCapture:
    case parser::details::ParseFunctionKind::kSimpleCapture:
    case parser::details::ParseFunctionKind::kInitCapture:
    case parser::details::ParseFunctionKind::kFoldExpression:
    case parser::details::ParseFunctionKind::kFoldOperator:
    case parser::details::ParseFunctionKind::kRequiresExpression:
    case parser::details::ParseFunctionKind::kRequirementParameterList:
    case parser::details::ParseFunctionKind::kRequirementBody:
    case parser::details::ParseFunctionKind::kRequirementSeq:
    case parser::details::ParseFunctionKind::kRequirement:
    case parser::details::ParseFunctionKind::kSimpleRequirement:
    case parser::details::ParseFunctionKind::kTypeRequirement:
    case parser::details::ParseFunctionKind::kCompoundRequirement:
    case parser::details::ParseFunctionKind::kReturnTypeRequirement:
    case parser::details::ParseFunctionKind::kNestedRequirement:
    case parser::details::ParseFunctionKind::kPostfixExpression:
    case parser::details::ParseFunctionKind::kExpressionList:
    case parser::details::ParseFunctionKind::kUnaryExpression:
    case parser::details::ParseFunctionKind::kUnaryOperator:
    case parser::details::ParseFunctionKind::kAwaitExpression:
    case parser::details::ParseFunctionKind::kNoexceptExpression:
    case parser::details::ParseFunctionKind::kNewExpression:
    case parser::details::ParseFunctionKind::kNewPlacement:
    case parser::details::ParseFunctionKind::kNewTypeId:
    case parser::details::ParseFunctionKind::kNewDeclarator:
    case parser::details::ParseFunctionKind::kNoptrNewDeclarator:
    case parser::details::ParseFunctionKind::kNewInitializer:
    case parser::details::ParseFunctionKind::kDeleteExpression:
    case parser::details::ParseFunctionKind::kCastExpression:
    case parser::details::ParseFunctionKind::kPmExpression:
    case parser::details::ParseFunctionKind::kMultiplicativeExpression:
    case parser::details::ParseFunctionKind::kAdditiveExpression:
    case parser::details::ParseFunctionKind::kShiftExpression:
    case parser::details::ParseFunctionKind::kCompareExpression:
    case parser::details::ParseFunctionKind::kRelationalExpression:
    case parser::details::ParseFunctionKind::kEqualityExpression:
    case parser::details::ParseFunctionKind::kAndExpression:
    case parser::details::ParseFunctionKind::kExclusiveOrExpression:
    case parser::details::ParseFunctionKind::kInclusiveOrExpression:
    case parser::details::ParseFunctionKind::kLogicalOrExpression:
    case parser::details::ParseFunctionKind::kConditionalExpression:
    case parser::details::ParseFunctionKind::kYieldExpression:
    case parser::details::ParseFunctionKind::kThrowExpression:
    case parser::details::ParseFunctionKind::kAssignmentExpression:
    case parser::details::ParseFunctionKind::kAssignmentOperator:
    case parser::details::ParseFunctionKind::kExpression:
    case parser::details::ParseFunctionKind::kConstantExpression:
    case parser::details::ParseFunctionKind::kStatement:
    case parser::details::ParseFunctionKind::kInitStatement:
    case parser::details::ParseFunctionKind::kCondition:
    case parser::details::ParseFunctionKind::kLabeledStatement:
    case parser::details::ParseFunctionKind::kExpressionStatement:
    case parser::details::ParseFunctionKind::kCompoundStatement:
    case parser::details::ParseFunctionKind::kStatementSeq:
    case parser::details::ParseFunctionKind::kSelectionStatement:
    case parser::details::ParseFunctionKind::kIterationStatement:
    case parser::details::ParseFunctionKind::kForRangeDeclaration:
    case parser::details::ParseFunctionKind::kForRangeInitializer:
    case parser::details::ParseFunctionKind::kJumpStatement:
    case parser::details::ParseFunctionKind::kCoroutineReturnStatement:
    case parser::details::ParseFunctionKind::kDeclarationStatement:
    case parser::details::ParseFunctionKind::kDeclarationSeq:
    case parser::details::ParseFunctionKind::kDeclaration:
    case parser::details::ParseFunctionKind::kBlockDeclaration:
    case parser::details::ParseFunctionKind::kNodeclspecFunctionDeclaration:
    case parser::details::ParseFunctionKind::kAliasDeclaration:
    case parser::details::ParseFunctionKind::kSimpleDeclaration:
    case parser::details::ParseFunctionKind::kStaticAssertDeclaration:
    case parser::details::ParseFunctionKind::kEmptyDeclaration:
    case parser::details::ParseFunctionKind::kAttributeDeclaration:
    case parser::details::ParseFunctionKind::kDeclSpecifier:
    case parser::details::ParseFunctionKind::kDeclSpecifierSeq:
    case parser::details::ParseFunctionKind::kStorageClassSpecifier:
    case parser::details::ParseFunctionKind::kFunctionSpecifier:
    case parser::details::ParseFunctionKind::kExplicitSpecifier:
    case parser::details::ParseFunctionKind::kTypedefName:
    case parser::details::ParseFunctionKind::kTypeSpecifier:
    case parser::details::ParseFunctionKind::kTypeSpecifierSeq:
    case parser::details::ParseFunctionKind::kDefiningTypeSpecifier:
    case parser::details::ParseFunctionKind::kDefiningTypeSpecifierSeq:
    case parser::details::ParseFunctionKind::kSimpleTypeSpecifier:
    case parser::details::ParseFunctionKind::kTypeName:
    case parser::details::ParseFunctionKind::kElaboratedTypeSpecifier:
    case parser::details::ParseFunctionKind::kElaboratedEnumSpecifier:
    case parser::details::ParseFunctionKind::kDecltypeSpecifier:
    case parser::details::ParseFunctionKind::kPlaceholderTypeSpecifier:
    case parser::details::ParseFunctionKind::kInitDeclaratorList:
    case parser::details::ParseFunctionKind::kInitDeclarator:
    case parser::details::ParseFunctionKind::kDeclarator:
    case parser::details::ParseFunctionKind::kPtrDeclarator:
    case parser::details::ParseFunctionKind::kNoptrDeclarator:
    case parser::details::ParseFunctionKind::kParametersAndQualifiers:
    case parser::details::ParseFunctionKind::kTrailingReturnType:
    case parser::details::ParseFunctionKind::kPtrOperator:
    case parser::details::ParseFunctionKind::kCvQualifierSeq:
    case parser::details::ParseFunctionKind::kCvQualifier:
    case parser::details::ParseFunctionKind::kRefQualifier:
    case parser::details::ParseFunctionKind::kDeclaratorId:
    case parser::details::ParseFunctionKind::kTypeId:
    case parser::details::ParseFunctionKind::kDefiningTypeId:
    case parser::details::ParseFunctionKind::kAbstractDeclarator:
    case parser::details::ParseFunctionKind::kPtrAbstractDeclarator:
    case parser::details::ParseFunctionKind::kNoptrAbstractDeclarator:
    case parser::details::ParseFunctionKind::kAbstractPackDeclarator:
    case parser::details::ParseFunctionKind::kNoptrAbstractPackDeclarator:
    case parser::details::ParseFunctionKind::kParameterDeclarationClause:
    case parser::details::ParseFunctionKind::kParameterDeclarationList:
    case parser::details::ParseFunctionKind::kParameterDeclaration:
    case parser::details::ParseFunctionKind::kInitializer:
    case parser::details::ParseFunctionKind::kBraceOrEqualInitializer:
    case parser::details::ParseFunctionKind::kInitializerClause:
    case parser::details::ParseFunctionKind::kBracedInitList:
    case parser::details::ParseFunctionKind::kInitializerList:
    case parser::details::ParseFunctionKind::kDesignatedInitializerList:
    case parser::details::ParseFunctionKind::kDesignatedInitializerClause:
    case parser::details::ParseFunctionKind::kDesignator:
    case parser::details::ParseFunctionKind::kExprOrBracedInitList:
    case parser::details::ParseFunctionKind::kFunctionDefinition:
    case parser::details::ParseFunctionKind::kFunctionBody:
    case parser::details::ParseFunctionKind::kEnumName:
    case parser::details::ParseFunctionKind::kEnumSpecifier:
    case parser::details::ParseFunctionKind::kEnumHead:
    case parser::details::ParseFunctionKind::kEnumHeadName:
    case parser::details::ParseFunctionKind::kOpaqueEnumDeclaration:
    case parser::details::ParseFunctionKind::kEnumKey:
    case parser::details::ParseFunctionKind::kEnumBase:
    case parser::details::ParseFunctionKind::kEnumeratorList:
    case parser::details::ParseFunctionKind::kEnumeratorDefinition:
    case parser::details::ParseFunctionKind::kEnumerator:
    case parser::details::ParseFunctionKind::kUsingEnumDeclaration:
    case parser::details::ParseFunctionKind::kNamespaceName:
    case parser::details::ParseFunctionKind::kNamespaceDefinition:
    case parser::details::ParseFunctionKind::kNamedNamespaceDefinition:
    case parser::details::ParseFunctionKind::kUnnamedNamespaceDefinition:
    case parser::details::ParseFunctionKind::kNestedNamespaceDefinition:
    case parser::details::ParseFunctionKind::kEnclosingNamespaceSpecifier:
    case parser::details::ParseFunctionKind::kNamespaceBody:
    case parser::details::ParseFunctionKind::kNamespaceAlias:
    case parser::details::ParseFunctionKind::kNamespaceAliasDefinition:
    case parser::details::ParseFunctionKind::kQualifiedNamespaceSpecifier:
    case parser::details::ParseFunctionKind::kUsingDirective:
    case parser::details::ParseFunctionKind::kUsingDeclaration:
    case parser::details::ParseFunctionKind::kUsingDeclaratorList:
    case parser::details::ParseFunctionKind::kUsingDeclarator:
    case parser::details::ParseFunctionKind::kAsmDeclaration:
    case parser::details::ParseFunctionKind::kLinkageSpecification:
    case parser::details::ParseFunctionKind::kAttributeSpecifierSeq:
    case parser::details::ParseFunctionKind::kAttributeSpecifier:
    case parser::details::ParseFunctionKind::kAlignmentSpecifier:
    case parser::details::ParseFunctionKind::kAttributeUsingPrefix:
    case parser::details::ParseFunctionKind::kAttributeList:
    case parser::details::ParseFunctionKind::kAttribute:
    case parser::details::ParseFunctionKind::kAttributeToken:
    case parser::details::ParseFunctionKind::kAttributeScopedToken:
    case parser::details::ParseFunctionKind::kAttributeNamespace:
    case parser::details::ParseFunctionKind::kAttributeArgumentClause:
    case parser::details::ParseFunctionKind::kBalancedTokenSeq:
    case parser::details::ParseFunctionKind::kBalancedToken:
    case parser::details::ParseFunctionKind::kModuleDeclaration:
    case parser::details::ParseFunctionKind::kModuleName:
    case parser::details::ParseFunctionKind::kModulePartition:
    case parser::details::ParseFunctionKind::kModuleNameQualifier:
    case parser::details::ParseFunctionKind::kExportDeclaration:
    case parser::details::ParseFunctionKind::kModuleImportDeclaration:
    case parser::details::ParseFunctionKind::kGlobalModuleFragment:
    case parser::details::ParseFunctionKind::kPrivateModuleFragment:
    case parser::details::ParseFunctionKind::kClassName:
    case parser::details::ParseFunctionKind::kClassSpecifier:
    case parser::details::ParseFunctionKind::kClassHead:
    case parser::details::ParseFunctionKind::kClassHeadName:
    case parser::details::ParseFunctionKind::kClassVirtSpecifier:
    case parser::details::ParseFunctionKind::kClassKey:
    case parser::details::ParseFunctionKind::kMemberSpecification:
    case parser::details::ParseFunctionKind::kMemberDeclaration:
    case parser::details::ParseFunctionKind::kMemberDeclaratorList:
    case parser::details::ParseFunctionKind::kMemberDeclarator:
    case parser::details::ParseFunctionKind::kVirtSpecifierSeq:
    case parser::details::ParseFunctionKind::kVirtSpecifier:
    case parser::details::ParseFunctionKind::kPureSpecifier:
    case parser::details::ParseFunctionKind::kConversionFunctionId:
    case parser::details::ParseFunctionKind::kConversionTypeId:
    case parser::details::ParseFunctionKind::kConversionDeclarator:
    case parser::details::ParseFunctionKind::kBaseClause:
    case parser::details::ParseFunctionKind::kBaseSpecifierList:
    case parser::details::ParseFunctionKind::kBaseSpecifier:
    case parser::details::ParseFunctionKind::kClassOrDecltype:
    case parser::details::ParseFunctionKind::kAccessSpecifier:
    case parser::details::ParseFunctionKind::kCtorInitializer:
    case parser::details::ParseFunctionKind::kMemInitializerList:
    case parser::details::ParseFunctionKind::kMemInitializer:
    case parser::details::ParseFunctionKind::kMemInitializerId:
    case parser::details::ParseFunctionKind::kOperatorFunctionId:
    case parser::details::ParseFunctionKind::kTheOperator:
    case parser::details::ParseFunctionKind::kLiteralOperatorId:
    case parser::details::ParseFunctionKind::kTemplateDeclaration:
    case parser::details::ParseFunctionKind::kTemplateHead:
    case parser::details::ParseFunctionKind::kTemplateParameterList:
    case parser::details::ParseFunctionKind::kRequiresClause:
    case parser::details::ParseFunctionKind::kConstraintLogicalOrExpression:
    case parser::details::ParseFunctionKind::kConstraintLogicalAndExpression:
    case parser::details::ParseFunctionKind::kTemplateParameter:
    case parser::details::ParseFunctionKind::kTypeParameter:
    case parser::details::ParseFunctionKind::kTypeParameterKey:
    case parser::details::ParseFunctionKind::kTypeConstraint:
    case parser::details::ParseFunctionKind::kSimpleTemplateId:
    case parser::details::ParseFunctionKind::kTemplateId:
    case parser::details::ParseFunctionKind::kTemplateName:
    case parser::details::ParseFunctionKind::kTemplateArgumentList:
    case parser::details::ParseFunctionKind::kTemplateArgument:
    case parser::details::ParseFunctionKind::kConstraintExpression:
    case parser::details::ParseFunctionKind::kDeductionGuide:
    case parser::details::ParseFunctionKind::kConceptDefinition:
    case parser::details::ParseFunctionKind::kConceptName:
    case parser::details::ParseFunctionKind::kTypenameSpecifier:
    case parser::details::ParseFunctionKind::kExplicitInstantiation:
    case parser::details::ParseFunctionKind::kExplicitSpecialization:
    case parser::details::ParseFunctionKind::kTryBlock:
    case parser::details::ParseFunctionKind::kFunctionTryBlock:
    case parser::details::ParseFunctionKind::kHandlerSeq:
    case parser::details::ParseFunctionKind::kHandler:
    case parser::details::ParseFunctionKind::kExceptionDeclaration:
    case parser::details::ParseFunctionKind::kNoexceptSpecifier:
    case parser::details::ParseFunctionKind::kPreprocessingFile:
    case parser::details::ParseFunctionKind::kModuleFile:
    case parser::details::ParseFunctionKind::kPpGlobalModuleFragment:
    case parser::details::ParseFunctionKind::kPpPrivateModuleFragment:
    case parser::details::ParseFunctionKind::kGroup:
    case parser::details::ParseFunctionKind::kGroupPart:
    case parser::details::ParseFunctionKind::kControlLine:
    case parser::details::ParseFunctionKind::kIfSection:
    case parser::details::ParseFunctionKind::kIfGroup:
    case parser::details::ParseFunctionKind::kElifGroups:
    case parser::details::ParseFunctionKind::kElifGroup:
    case parser::details::ParseFunctionKind::kElseGroup:
    case parser::details::ParseFunctionKind::kEndifLine:
    case parser::details::ParseFunctionKind::kTextLine:
    case parser::details::ParseFunctionKind::kConditionallySupportedDirective:
    case parser::details::ParseFunctionKind::kLparen:
    case parser::details::ParseFunctionKind::kIdentifierList:
    case parser::details::ParseFunctionKind::kReplacementList:
    case parser::details::ParseFunctionKind::kPpTokens:
    case parser::details::ParseFunctionKind::kNewLine:
    case parser::details::ParseFunctionKind::kDefinedMacroExpression:
    case parser::details::ParseFunctionKind::kHPreprocessingToken:
    case parser::details::ParseFunctionKind::kHPpTokens:
    case parser::details::ParseFunctionKind::kHeaderNameTokens:
    case parser::details::ParseFunctionKind::kHasIncludeExpression:
    case parser::details::ParseFunctionKind::kHasAttributeExpression:
    case parser::details::ParseFunctionKind::kPpModule:
    case parser::details::ParseFunctionKind::kPpImport:
    case parser::details::ParseFunctionKind::kVaOptReplacement:
    case parser::details::ParseFunctionKind::kHexQuad:
    case parser::details::ParseFunctionKind::kUniversalCharacterName:
    case parser::details::ParseFunctionKind::kPreprocessingToken:
    case parser::details::ParseFunctionKind::kToken:
    case parser::details::ParseFunctionKind::kHeaderName:
    case parser::details::ParseFunctionKind::kHCharSequence:
    case parser::details::ParseFunctionKind::kHChar:
    case parser::details::ParseFunctionKind::kQCharSequence:
    case parser::details::ParseFunctionKind::kQChar:
    case parser::details::ParseFunctionKind::kPpNumber:
    case parser::details::ParseFunctionKind::kIdentifierNondigit:
    case parser::details::ParseFunctionKind::kNondigit:
    case parser::details::ParseFunctionKind::kDigit:
    case parser::details::ParseFunctionKind::kKeyword:
    case parser::details::ParseFunctionKind::kPreprocessingOpOrPunc:
    case parser::details::ParseFunctionKind::kPreprocessingOperator:
    case parser::details::ParseFunctionKind::kOperatorOrPunctuator:
    case parser::details::ParseFunctionKind::kLiteral:
    case parser::details::ParseFunctionKind::kBinaryDigit:
    case parser::details::ParseFunctionKind::kOctalDigit:
    case parser::details::ParseFunctionKind::kNonzeroDigit:
    case parser::details::ParseFunctionKind::kHexadecimalPrefix:
    case parser::details::ParseFunctionKind::kHexadecimalDigitSequence:
    case parser::details::ParseFunctionKind::kHexadecimalDigit:
    case parser::details::ParseFunctionKind::kIntegerSuffix:
    case parser::details::ParseFunctionKind::kUnsignedSuffix:
    case parser::details::ParseFunctionKind::kLongSuffix:
    case parser::details::ParseFunctionKind::kLongLongSuffix:
    case parser::details::ParseFunctionKind::kEncodingPrefix:
    case parser::details::ParseFunctionKind::kCCharSequence:
    case parser::details::ParseFunctionKind::kCChar:
    case parser::details::ParseFunctionKind::kEscapeSequence:
    case parser::details::ParseFunctionKind::kSimpleEscapeSequence:
    case parser::details::ParseFunctionKind::kOctalEscapeSequence:
    case parser::details::ParseFunctionKind::kHexadecimalEscapeSequence:
    case parser::details::ParseFunctionKind::kDecimalFloatingPointLiteral:
    case parser::details::ParseFunctionKind::kHexadecimalFloatingPointLiteral:
    case parser::details::ParseFunctionKind::kFractionalConstant:
    case parser::details::ParseFunctionKind::kHexadecimalFractionalConstant:
    case parser::details::ParseFunctionKind::kExponentPart:
    case parser::details::ParseFunctionKind::kBinaryExponentPart:
    case parser::details::ParseFunctionKind::kSign:
    case parser::details::ParseFunctionKind::kDigitSequence:
    case parser::details::ParseFunctionKind::kFloatingPointSuffix:
    case parser::details::ParseFunctionKind::kSCharSequence:
    case parser::details::ParseFunctionKind::kSChar:
    case parser::details::ParseFunctionKind::kRCharSequence:
    case parser::details::ParseFunctionKind::kRChar:
    case parser::details::ParseFunctionKind::kDCharSequence:
    case parser::details::ParseFunctionKind::kDChar:
    case parser::details::ParseFunctionKind::kBooleanLiteral:
    case parser::details::ParseFunctionKind::kPointerLiteral:
    case parser::details::ParseFunctionKind::kUserDefinedLiteral:
    case parser::details::ParseFunctionKind::kUdSuffix:
    case parser::details::ParseFunctionKind::kNum:
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
