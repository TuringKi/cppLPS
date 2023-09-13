
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

#include "sema.h"

namespace lps::sema::details::unit {

class TranslationUnit;
class PrimaryExpression;
class IdExpression;
class UnqualifiedId;
class QualifiedId;
class NestedNameSpecifier;
class LambdaExpression;
class LambdaIntroducer;
class LambdaDeclarator;
class LambdaCapture;
class CaptureDefault;
class CaptureList;
class Capture;
class SimpleCapture;
class InitCapture;
class FoldExpression;
class FoldOperator;
class RequiresExpression;
class RequirementParameterList;
class RequirementBody;
class RequirementSeq;
class Requirement;
class SimpleRequirement;
class TypeRequirement;
class CompoundRequirement;
class ReturnTypeRequirement;
class NestedRequirement;
class PostfixExpression;
class ExpressionList;
class UnaryExpression;
class UnaryOperator;
class AwaitExpression;
class NoexceptExpression;
class NewExpression;
class NewPlacement;
class NewTypeId;
class NewDeclarator;
class NoptrNewDeclarator;
class NewInitializer;
class DeleteExpression;
class CastExpression;
class PmExpression;
class MultiplicativeExpression;
class AdditiveExpression;
class ShiftExpression;
class CompareExpression;
class RelationalExpression;
class EqualityExpression;
class AndExpression;
class ExclusiveOrExpression;
class InclusiveOrExpression;
class LogicalAndExpression;
class LogicalOrExpression;
class ConditionalExpression;
class YieldExpression;
class ThrowExpression;
class AssignmentExpression;
class AssignmentOperator;
class Expression;
class ConstantExpression;
class Statement;
class InitStatement;
class Condition;
class LabeledStatement;
class ExpressionStatement;
class CompoundStatement;
class StatementSeq;
class SelectionStatement;
class IterationStatement;
class ForRangeDeclaration;
class ForRangeInitializer;
class JumpStatement;
class CoroutineReturnStatement;
class DeclarationStatement;
class DeclarationSeq;
class Declaration;
class BlockDeclaration;
class NodeclspecFunctionDeclaration;
class AliasDeclaration;
class SimpleDeclaration;
class StaticAssertDeclaration;
class EmptyDeclaration;
class AttributeDeclaration;
class DeclSpecifier;
class DeclSpecifierSeq;
class StorageClassSpecifier;
class FunctionSpecifier;
class ExplicitSpecifier;
class TypedefName;
class TypeSpecifier;
class TypeSpecifierSeq;
class DefiningTypeSpecifier;
class DefiningTypeSpecifierSeq;
class SimpleTypeSpecifier;
class TypeName;
class ElaboratedTypeSpecifier;
class ElaboratedEnumSpecifier;
class DecltypeSpecifier;
class PlaceholderTypeSpecifier;
class InitDeclaratorList;
class InitDeclarator;
class Declarator;
class PtrDeclarator;
class NoptrDeclarator;
class ParametersAndQualifiers;
class TrailingReturnType;
class PtrOperator;
class CvQualifierSeq;
class CvQualifier;
class RefQualifier;
class DeclaratorId;
class TypeId;
class DefiningTypeId;
class AbstractDeclarator;
class PtrAbstractDeclarator;
class NoptrAbstractDeclarator;
class AbstractPackDeclarator;
class NoptrAbstractPackDeclarator;
class ParameterDeclarationClause;
class ParameterDeclarationList;
class ParameterDeclaration;
class Initializer;
class BraceOrEqualInitializer;
class InitializerClause;
class BracedInitList;
class InitializerList;
class DesignatedInitializerList;
class DesignatedInitializerClause;
class Designator;
class ExprOrBracedInitList;
class FunctionDefinition;
class FunctionBody;
class EnumName;
class EnumSpecifier;
class EnumHead;
class EnumHeadName;
class OpaqueEnumDeclaration;
class EnumKey;
class EnumBase;
class EnumeratorList;
class EnumeratorDefinition;
class Enumerator;
class UsingEnumDeclaration;
class NamespaceName;
class NamespaceDefinition;
class NamedNamespaceDefinition;
class UnnamedNamespaceDefinition;
class NestedNamespaceDefinition;
class EnclosingNamespaceSpecifier;
class NamespaceBody;
class NamespaceAlias;
class NamespaceAliasDefinition;
class QualifiedNamespaceSpecifier;
class UsingDirective;
class UsingDeclaration;
class UsingDeclaratorList;
class UsingDeclarator;
class AsmDeclaration;
class LinkageSpecification;
class AttributeSpecifierSeq;
class AttributeSpecifier;
class AlignmentSpecifier;
class AttributeUsingPrefix;
class AttributeList;
class Attribute;
class AttributeToken;
class AttributeScopedToken;
class AttributeNamespace;
class AttributeArgumentClause;
class BalancedTokenSeq;
class BalancedToken;
class ModuleDeclaration;
class ModuleName;
class ModulePartition;
class ModuleNameQualifier;
class ExportDeclaration;
class ModuleImportDeclaration;
class GlobalModuleFragment;
class PrivateModuleFragment;
class ClassName;
class ClassSpecifier;
class ClassHead;
class ClassHeadName;
class ClassVirtSpecifier;
class ClassKey;
class MemberSpecification;
class MemberDeclaration;
class MemberDeclaratorList;
class MemberDeclarator;
class VirtSpecifierSeq;
class VirtSpecifier;
class PureSpecifier;
class ConversionFunctionId;
class ConversionTypeId;
class ConversionDeclarator;
class BaseClause;
class BaseSpecifierList;
class BaseSpecifier;
class ClassOrDecltype;
class AccessSpecifier;
class CtorInitializer;
class MemInitializerList;
class MemInitializer;
class MemInitializerId;
class OperatorFunctionId;
class TheOperator;
class LiteralOperatorId;
class TemplateDeclaration;
class TemplateHead;
class TemplateParameterList;
class RequiresClause;
class ConstraintLogicalOrExpression;
class ConstraintLogicalAndExpression;
class TemplateParameter;
class TypeParameter;
class TypeParameterKey;
class TypeConstraint;
class SimpleTemplateId;
class TemplateId;
class TemplateName;
class TemplateArgumentList;
class TemplateArgument;
class ConstraintExpression;
class DeductionGuide;
class ConceptDefinition;
class ConceptName;
class TypenameSpecifier;
class ExplicitInstantiation;
class ExplicitSpecialization;
class TryBlock;
class FunctionTryBlock;
class HandlerSeq;
class Handler;
class ExceptionDeclaration;
class NoexceptSpecifier;
class PreprocessingFile;
class ModuleFile;
class PpGlobalModuleFragment;
class PpPrivateModuleFragment;
class Group;
class GroupPart;
class ControlLine;
class IfSection;
class IfGroup;
class ElifGroups;
class ElifGroup;
class ElseGroup;
class EndifLine;
class TextLine;
class ConditionallySupportedDirective;
class Lparen;
class IdentifierList;
class ReplacementList;
class PpTokens;
class NewLine;
class DefinedMacroExpression;
class HPreprocessingToken;
class HPpTokens;
class HeaderNameTokens;
class HasIncludeExpression;
class HasAttributeExpression;
class PpModule;
class PpImport;
class VaOptReplacement;
class HexQuad;
class UniversalCharacterName;
class PreprocessingToken;
class Token;
class HeaderName;
class HCharSequence;
class HChar;
class QCharSequence;
class QChar;
class PpNumber;
class IdentifierNondigit;
class Nondigit;
class Digit;
class Keyword;
class PreprocessingOpOrPunc;
class PreprocessingOperator;
class OperatorOrPunctuator;
class Literal;
class BinaryDigit;
class OctalDigit;
class NonzeroDigit;
class HexadecimalPrefix;
class HexadecimalDigitSequence;
class HexadecimalDigit;
class IntegerSuffix;
class UnsignedSuffix;
class LongSuffix;
class LongLongSuffix;
class EncodingPrefix;
class CCharSequence;
class CChar;
class EscapeSequence;
class SimpleEscapeSequence;
class OctalEscapeSequence;
class HexadecimalEscapeSequence;
class DecimalFloatingPointLiteral;
class HexadecimalFloatingPointLiteral;
class FractionalConstant;
class HexadecimalFractionalConstant;
class ExponentPart;
class BinaryExponentPart;
class Sign;
class DigitSequence;
class FloatingPointSuffix;
class SCharSequence;
class SChar;
class RCharSequence;
class RChar;
class DCharSequence;
class DChar;
class BooleanLiteral;
class PointerLiteral;
class UserDefinedLiteral;
class UdSuffix;

class TranslationUnit : public Unit, public HasElements {

 public:
  explicit TranslationUnit(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTranslationUnit; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TranslationUnit() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TranslationUnit";
};

class PrimaryExpression : public Unit, public HasElements {

 public:
  explicit PrimaryExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kPrimaryExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PrimaryExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::PrimaryExpression";
};

class IdExpression : public Unit, public HasElements {

 public:
  explicit IdExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kIdExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~IdExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::IdExpression";
};

class UnqualifiedId : public Unit, public HasElements {

 public:
  explicit UnqualifiedId(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kUnqualifiedId; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~UnqualifiedId() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::UnqualifiedId";
};

class QualifiedId : public Unit, public HasElements {

 public:
  explicit QualifiedId(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kQualifiedId; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~QualifiedId() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::QualifiedId";
};

class NestedNameSpecifier : public Unit, public HasElements {

 public:
  explicit NestedNameSpecifier(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNestedNameSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NestedNameSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NestedNameSpecifier";
};

class LambdaExpression : public Unit, public HasElements {

 public:
  explicit LambdaExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kLambdaExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~LambdaExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::LambdaExpression";
};

class LambdaIntroducer : public Unit, public HasElements {

 public:
  explicit LambdaIntroducer(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kLambdaIntroducer; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~LambdaIntroducer() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::LambdaIntroducer";
};

class LambdaDeclarator : public Unit, public HasElements {

 public:
  explicit LambdaDeclarator(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kLambdaDeclarator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~LambdaDeclarator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::LambdaDeclarator";
};

class LambdaCapture : public Unit, public HasElements {

 public:
  explicit LambdaCapture(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kLambdaCapture; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~LambdaCapture() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::LambdaCapture";
};

class CaptureDefault : public Unit, public HasElements {

 public:
  explicit CaptureDefault(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kCaptureDefault; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~CaptureDefault() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::CaptureDefault";
};

class CaptureList : public Unit, public HasElements {

 public:
  explicit CaptureList(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kCaptureList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~CaptureList() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::CaptureList";
};

class Capture : public Unit, public HasElements {

 public:
  explicit Capture(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kCapture; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Capture() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Capture";
};

class SimpleCapture : public Unit, public HasElements {

 public:
  explicit SimpleCapture(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kSimpleCapture; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~SimpleCapture() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::SimpleCapture";
};

class InitCapture : public Unit, public HasElements {

 public:
  explicit InitCapture(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kInitCapture; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~InitCapture() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::InitCapture";
};

class FoldExpression : public Unit, public HasElements {

 public:
  explicit FoldExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kFoldExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~FoldExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::FoldExpression";
};

class FoldOperator : public Unit, public HasElements {

 public:
  explicit FoldOperator(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kFoldOperator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~FoldOperator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::FoldOperator";
};

class RequiresExpression : public Unit, public HasElements {

 public:
  explicit RequiresExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kRequiresExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~RequiresExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::RequiresExpression";
};

class RequirementParameterList : public Unit, public HasElements {

 public:
  explicit RequirementParameterList(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kRequirementParameterList;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~RequirementParameterList() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::RequirementParameterList";
};

class RequirementBody : public Unit, public HasElements {

 public:
  explicit RequirementBody(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kRequirementBody; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~RequirementBody() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::RequirementBody";
};

class RequirementSeq : public Unit, public HasElements {

 public:
  explicit RequirementSeq(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kRequirementSeq; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~RequirementSeq() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::RequirementSeq";
};

class Requirement : public Unit, public HasElements {

 public:
  explicit Requirement(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kRequirement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Requirement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Requirement";
};

class SimpleRequirement : public Unit, public HasElements {

 public:
  explicit SimpleRequirement(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kSimpleRequirement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~SimpleRequirement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::SimpleRequirement";
};

class TypeRequirement : public Unit, public HasElements {

 public:
  explicit TypeRequirement(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTypeRequirement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TypeRequirement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TypeRequirement";
};

class CompoundRequirement : public Unit, public HasElements {

 public:
  explicit CompoundRequirement(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kCompoundRequirement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~CompoundRequirement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::CompoundRequirement";
};

class ReturnTypeRequirement : public Unit, public HasElements {

 public:
  explicit ReturnTypeRequirement(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kReturnTypeRequirement;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ReturnTypeRequirement() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ReturnTypeRequirement";
};

class NestedRequirement : public Unit, public HasElements {

 public:
  explicit NestedRequirement(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNestedRequirement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NestedRequirement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NestedRequirement";
};

class PostfixExpression : public Unit, public HasElements {

 public:
  explicit PostfixExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kPostfixExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PostfixExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::PostfixExpression";
};

class ExpressionList : public Unit, public HasElements {

 public:
  explicit ExpressionList(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kExpressionList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ExpressionList() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ExpressionList";
};

class UnaryExpression : public Unit, public HasElements {

 public:
  explicit UnaryExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kUnaryExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~UnaryExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::UnaryExpression";
};

class UnaryOperator : public Unit, public HasElements {

 public:
  explicit UnaryOperator(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kUnaryOperator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~UnaryOperator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::UnaryOperator";
};

class AwaitExpression : public Unit, public HasElements {

 public:
  explicit AwaitExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAwaitExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AwaitExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::AwaitExpression";
};

class NoexceptExpression : public Unit, public HasElements {

 public:
  explicit NoexceptExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNoexceptExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NoexceptExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NoexceptExpression";
};

class NewExpression : public Unit, public HasElements {

 public:
  explicit NewExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNewExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NewExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NewExpression";
};

class NewPlacement : public Unit, public HasElements {

 public:
  explicit NewPlacement(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNewPlacement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NewPlacement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NewPlacement";
};

class NewTypeId : public Unit, public HasElements {

 public:
  explicit NewTypeId(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNewTypeId; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NewTypeId() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NewTypeId";
};

class NewDeclarator : public Unit, public HasElements {

 public:
  explicit NewDeclarator(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNewDeclarator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NewDeclarator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NewDeclarator";
};

class NoptrNewDeclarator : public Unit, public HasElements {

 public:
  explicit NoptrNewDeclarator(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNoptrNewDeclarator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NoptrNewDeclarator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NoptrNewDeclarator";
};

class NewInitializer : public Unit, public HasElements {

 public:
  explicit NewInitializer(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNewInitializer; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NewInitializer() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NewInitializer";
};

class DeleteExpression : public Unit, public HasElements {

 public:
  explicit DeleteExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDeleteExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DeleteExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::DeleteExpression";
};

class CastExpression : public Unit, public HasElements {

 public:
  explicit CastExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kCastExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~CastExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::CastExpression";
};

class PmExpression : public Unit, public HasElements {

 public:
  explicit PmExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kPmExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PmExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::PmExpression";
};

class MultiplicativeExpression : public Unit, public HasElements {

 public:
  explicit MultiplicativeExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kMultiplicativeExpression;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~MultiplicativeExpression() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::MultiplicativeExpression";
};

class AdditiveExpression : public Unit, public HasElements {

 public:
  explicit AdditiveExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAdditiveExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AdditiveExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::AdditiveExpression";
};

class ShiftExpression : public Unit, public HasElements {

 public:
  explicit ShiftExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kShiftExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ShiftExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ShiftExpression";
};

class CompareExpression : public Unit, public HasElements {

 public:
  explicit CompareExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kCompareExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~CompareExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::CompareExpression";
};

class RelationalExpression : public Unit, public HasElements {

 public:
  explicit RelationalExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kRelationalExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~RelationalExpression() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::RelationalExpression";
};

class EqualityExpression : public Unit, public HasElements {

 public:
  explicit EqualityExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kEqualityExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~EqualityExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::EqualityExpression";
};

class AndExpression : public Unit, public HasElements {

 public:
  explicit AndExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAndExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AndExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::AndExpression";
};

class ExclusiveOrExpression : public Unit, public HasElements {

 public:
  explicit ExclusiveOrExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kExclusiveOrExpression;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ExclusiveOrExpression() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ExclusiveOrExpression";
};

class InclusiveOrExpression : public Unit, public HasElements {

 public:
  explicit InclusiveOrExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kInclusiveOrExpression;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~InclusiveOrExpression() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::InclusiveOrExpression";
};

class LogicalAndExpression : public Unit, public HasElements {

 public:
  explicit LogicalAndExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kLogicalAndExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~LogicalAndExpression() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::LogicalAndExpression";
};

class LogicalOrExpression : public Unit, public HasElements {

 public:
  explicit LogicalOrExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kLogicalOrExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~LogicalOrExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::LogicalOrExpression";
};

class ConditionalExpression : public Unit, public HasElements {

 public:
  explicit ConditionalExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kConditionalExpression;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ConditionalExpression() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ConditionalExpression";
};

class YieldExpression : public Unit, public HasElements {

 public:
  explicit YieldExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kYieldExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~YieldExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::YieldExpression";
};

class ThrowExpression : public Unit, public HasElements {

 public:
  explicit ThrowExpression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kThrowExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ThrowExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ThrowExpression";
};

class AssignmentExpression : public Unit, public HasElements {

 public:
  explicit AssignmentExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAssignmentExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AssignmentExpression() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::AssignmentExpression";
};

class AssignmentOperator : public Unit, public HasElements {

 public:
  explicit AssignmentOperator(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAssignmentOperator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AssignmentOperator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::AssignmentOperator";
};

class Expression : public Unit, public HasElements {

 public:
  explicit Expression(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Expression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Expression";
};

class ConstantExpression : public Unit, public HasElements {

 public:
  explicit ConstantExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kConstantExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ConstantExpression() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ConstantExpression";
};

class Statement : public Unit, public HasElements {

 public:
  explicit Statement(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kStatement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Statement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Statement";
};

class InitStatement : public Unit, public HasElements {

 public:
  explicit InitStatement(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kInitStatement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~InitStatement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::InitStatement";
};

class Condition : public Unit, public HasElements {

 public:
  explicit Condition(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kCondition; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Condition() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Condition";
};

class LabeledStatement : public Unit, public HasElements {

 public:
  explicit LabeledStatement(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kLabeledStatement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~LabeledStatement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::LabeledStatement";
};

class ExpressionStatement : public Unit, public HasElements {

 public:
  explicit ExpressionStatement(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kExpressionStatement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ExpressionStatement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ExpressionStatement";
};

class CompoundStatement : public Unit, public HasElements {

 public:
  explicit CompoundStatement(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kCompoundStatement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~CompoundStatement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::CompoundStatement";
};

class StatementSeq : public Unit, public HasElements {

 public:
  explicit StatementSeq(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kStatementSeq; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~StatementSeq() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::StatementSeq";
};

class SelectionStatement : public Unit, public HasElements {

 public:
  explicit SelectionStatement(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kSelectionStatement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~SelectionStatement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::SelectionStatement";
};

class IterationStatement : public Unit, public HasElements {

 public:
  explicit IterationStatement(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kIterationStatement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~IterationStatement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::IterationStatement";
};

class ForRangeDeclaration : public Unit, public HasElements {

 public:
  explicit ForRangeDeclaration(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kForRangeDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ForRangeDeclaration() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ForRangeDeclaration";
};

class ForRangeInitializer : public Unit, public HasElements {

 public:
  explicit ForRangeInitializer(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kForRangeInitializer; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ForRangeInitializer() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ForRangeInitializer";
};

class JumpStatement : public Unit, public HasElements {

 public:
  explicit JumpStatement(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kJumpStatement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~JumpStatement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::JumpStatement";
};

class CoroutineReturnStatement : public Unit, public HasElements {

 public:
  explicit CoroutineReturnStatement(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kCoroutineReturnStatement;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~CoroutineReturnStatement() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::CoroutineReturnStatement";
};

class DeclarationStatement : public Unit, public HasElements {

 public:
  explicit DeclarationStatement(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDeclarationStatement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DeclarationStatement() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::DeclarationStatement";
};

class DeclarationSeq : public Unit, public HasElements {

 public:
  explicit DeclarationSeq(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDeclarationSeq; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DeclarationSeq() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::DeclarationSeq";
};

class Declaration : public Unit, public HasElements {

 public:
  explicit Declaration(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Declaration() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Declaration";
};

class BlockDeclaration : public Unit, public HasElements {

 public:
  explicit BlockDeclaration(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kBlockDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~BlockDeclaration() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::BlockDeclaration";
};

class NodeclspecFunctionDeclaration : public Unit, public HasElements {

 public:
  explicit NodeclspecFunctionDeclaration(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kNodeclspecFunctionDeclaration;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NodeclspecFunctionDeclaration() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::NodeclspecFunctionDeclaration";
};

class AliasDeclaration : public Unit, public HasElements {

 public:
  explicit AliasDeclaration(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAliasDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AliasDeclaration() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::AliasDeclaration";
};

class SimpleDeclaration : public Unit, public HasElements {

 public:
  explicit SimpleDeclaration(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kSimpleDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~SimpleDeclaration() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::SimpleDeclaration";
};

class StaticAssertDeclaration : public Unit, public HasElements {

 public:
  explicit StaticAssertDeclaration(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kStaticAssertDeclaration;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~StaticAssertDeclaration() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::StaticAssertDeclaration";
};

class EmptyDeclaration : public Unit, public HasElements {

 public:
  explicit EmptyDeclaration(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kEmptyDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~EmptyDeclaration() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::EmptyDeclaration";
};

class AttributeDeclaration : public Unit, public HasElements {

 public:
  explicit AttributeDeclaration(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAttributeDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AttributeDeclaration() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::AttributeDeclaration";
};

class DeclSpecifier : public Unit, public HasElements {

 public:
  explicit DeclSpecifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDeclSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DeclSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::DeclSpecifier";
};

class DeclSpecifierSeq : public Unit, public HasElements {

 public:
  explicit DeclSpecifierSeq(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDeclSpecifierSeq; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DeclSpecifierSeq() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::DeclSpecifierSeq";
};

class StorageClassSpecifier : public Unit, public HasElements {

 public:
  explicit StorageClassSpecifier(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kStorageClassSpecifier;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~StorageClassSpecifier() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::StorageClassSpecifier";
};

class FunctionSpecifier : public Unit, public HasElements {

 public:
  explicit FunctionSpecifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kFunctionSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~FunctionSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::FunctionSpecifier";
};

class ExplicitSpecifier : public Unit, public HasElements {

 public:
  explicit ExplicitSpecifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kExplicitSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ExplicitSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ExplicitSpecifier";
};

class TypedefName : public Unit, public HasElements {

 public:
  explicit TypedefName(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTypedefName; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TypedefName() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TypedefName";
};

class TypeSpecifier : public Unit, public HasElements {

 public:
  explicit TypeSpecifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTypeSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TypeSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TypeSpecifier";
};

class TypeSpecifierSeq : public Unit, public HasElements {

 public:
  explicit TypeSpecifierSeq(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTypeSpecifierSeq; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TypeSpecifierSeq() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TypeSpecifierSeq";
};

class DefiningTypeSpecifier : public Unit, public HasElements {

 public:
  explicit DefiningTypeSpecifier(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kDefiningTypeSpecifier;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DefiningTypeSpecifier() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::DefiningTypeSpecifier";
};

class DefiningTypeSpecifierSeq : public Unit, public HasElements {

 public:
  explicit DefiningTypeSpecifierSeq(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kDefiningTypeSpecifierSeq;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DefiningTypeSpecifierSeq() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::DefiningTypeSpecifierSeq";
};

class SimpleTypeSpecifier : public Unit, public HasElements {

 public:
  explicit SimpleTypeSpecifier(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kSimpleTypeSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~SimpleTypeSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::SimpleTypeSpecifier";
};

class TypeName : public Unit, public HasElements {

 public:
  explicit TypeName(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTypeName; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TypeName() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TypeName";
};

class ElaboratedTypeSpecifier : public Unit, public HasElements {

 public:
  explicit ElaboratedTypeSpecifier(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kElaboratedTypeSpecifier;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ElaboratedTypeSpecifier() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ElaboratedTypeSpecifier";
};

class ElaboratedEnumSpecifier : public Unit, public HasElements {

 public:
  explicit ElaboratedEnumSpecifier(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kElaboratedEnumSpecifier;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ElaboratedEnumSpecifier() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ElaboratedEnumSpecifier";
};

class DecltypeSpecifier : public Unit, public HasElements {

 public:
  explicit DecltypeSpecifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDecltypeSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DecltypeSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::DecltypeSpecifier";
};

class PlaceholderTypeSpecifier : public Unit, public HasElements {

 public:
  explicit PlaceholderTypeSpecifier(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kPlaceholderTypeSpecifier;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PlaceholderTypeSpecifier() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::PlaceholderTypeSpecifier";
};

class InitDeclaratorList : public Unit, public HasElements {

 public:
  explicit InitDeclaratorList(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kInitDeclaratorList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~InitDeclaratorList() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::InitDeclaratorList";
};

class InitDeclarator : public Unit, public HasElements {

 public:
  explicit InitDeclarator(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kInitDeclarator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~InitDeclarator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::InitDeclarator";
};

class Declarator : public Unit, public HasElements {

 public:
  explicit Declarator(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDeclarator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Declarator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Declarator";
};

class PtrDeclarator : public Unit, public HasElements {

 public:
  explicit PtrDeclarator(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kPtrDeclarator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PtrDeclarator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::PtrDeclarator";
};

class NoptrDeclarator : public Unit, public HasElements {

 public:
  explicit NoptrDeclarator(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNoptrDeclarator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NoptrDeclarator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NoptrDeclarator";
};

class ParametersAndQualifiers : public Unit, public HasElements {

 public:
  explicit ParametersAndQualifiers(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kParametersAndQualifiers;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ParametersAndQualifiers() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ParametersAndQualifiers";
};

class TrailingReturnType : public Unit, public HasElements {

 public:
  explicit TrailingReturnType(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTrailingReturnType; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TrailingReturnType() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TrailingReturnType";
};

class PtrOperator : public Unit, public HasElements {

 public:
  explicit PtrOperator(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kPtrOperator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PtrOperator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::PtrOperator";
};

class CvQualifierSeq : public Unit, public HasElements {

 public:
  explicit CvQualifierSeq(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kCvQualifierSeq; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~CvQualifierSeq() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::CvQualifierSeq";
};

class CvQualifier : public Unit, public HasElements {

 public:
  explicit CvQualifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kCvQualifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~CvQualifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::CvQualifier";
};

class RefQualifier : public Unit, public HasElements {

 public:
  explicit RefQualifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kRefQualifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~RefQualifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::RefQualifier";
};

class DeclaratorId : public Unit, public HasElements {

 public:
  explicit DeclaratorId(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDeclaratorId; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DeclaratorId() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::DeclaratorId";
};

class TypeId : public Unit, public HasElements {

 public:
  explicit TypeId(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTypeId; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TypeId() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TypeId";
};

class DefiningTypeId : public Unit, public HasElements {

 public:
  explicit DefiningTypeId(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDefiningTypeId; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DefiningTypeId() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::DefiningTypeId";
};

class AbstractDeclarator : public Unit, public HasElements {

 public:
  explicit AbstractDeclarator(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAbstractDeclarator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AbstractDeclarator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::AbstractDeclarator";
};

class PtrAbstractDeclarator : public Unit, public HasElements {

 public:
  explicit PtrAbstractDeclarator(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kPtrAbstractDeclarator;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PtrAbstractDeclarator() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::PtrAbstractDeclarator";
};

class NoptrAbstractDeclarator : public Unit, public HasElements {

 public:
  explicit NoptrAbstractDeclarator(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kNoptrAbstractDeclarator;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NoptrAbstractDeclarator() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::NoptrAbstractDeclarator";
};

class AbstractPackDeclarator : public Unit, public HasElements {

 public:
  explicit AbstractPackDeclarator(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kAbstractPackDeclarator;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AbstractPackDeclarator() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::AbstractPackDeclarator";
};

class NoptrAbstractPackDeclarator : public Unit, public HasElements {

 public:
  explicit NoptrAbstractPackDeclarator(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kNoptrAbstractPackDeclarator;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NoptrAbstractPackDeclarator() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::NoptrAbstractPackDeclarator";
};

class ParameterDeclarationClause : public Unit, public HasElements {

 public:
  explicit ParameterDeclarationClause(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kParameterDeclarationClause;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ParameterDeclarationClause() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ParameterDeclarationClause";
};

class ParameterDeclarationList : public Unit, public HasElements {

 public:
  explicit ParameterDeclarationList(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kParameterDeclarationList;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ParameterDeclarationList() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ParameterDeclarationList";
};

class ParameterDeclaration : public Unit, public HasElements {

 public:
  explicit ParameterDeclaration(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kParameterDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ParameterDeclaration() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ParameterDeclaration";
};

class Initializer : public Unit, public HasElements {

 public:
  explicit Initializer(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kInitializer; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Initializer() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Initializer";
};

class BraceOrEqualInitializer : public Unit, public HasElements {

 public:
  explicit BraceOrEqualInitializer(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kBraceOrEqualInitializer;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~BraceOrEqualInitializer() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::BraceOrEqualInitializer";
};

class InitializerClause : public Unit, public HasElements {

 public:
  explicit InitializerClause(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kInitializerClause; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~InitializerClause() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::InitializerClause";
};

class BracedInitList : public Unit, public HasElements {

 public:
  explicit BracedInitList(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kBracedInitList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~BracedInitList() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::BracedInitList";
};

class InitializerList : public Unit, public HasElements {

 public:
  explicit InitializerList(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kInitializerList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~InitializerList() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::InitializerList";
};

class DesignatedInitializerList : public Unit, public HasElements {

 public:
  explicit DesignatedInitializerList(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kDesignatedInitializerList;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DesignatedInitializerList() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::DesignatedInitializerList";
};

class DesignatedInitializerClause : public Unit, public HasElements {

 public:
  explicit DesignatedInitializerClause(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kDesignatedInitializerClause;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DesignatedInitializerClause() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::DesignatedInitializerClause";
};

class Designator : public Unit, public HasElements {

 public:
  explicit Designator(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDesignator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Designator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Designator";
};

class ExprOrBracedInitList : public Unit, public HasElements {

 public:
  explicit ExprOrBracedInitList(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kExprOrBracedInitList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ExprOrBracedInitList() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ExprOrBracedInitList";
};

class FunctionDefinition : public Unit, public HasElements {

 public:
  explicit FunctionDefinition(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kFunctionDefinition; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~FunctionDefinition() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::FunctionDefinition";
};

class FunctionBody : public Unit, public HasElements {

 public:
  explicit FunctionBody(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kFunctionBody; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~FunctionBody() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::FunctionBody";
};

class EnumName : public Unit, public HasElements {

 public:
  explicit EnumName(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kEnumName; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~EnumName() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::EnumName";
};

class EnumSpecifier : public Unit, public HasElements {

 public:
  explicit EnumSpecifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kEnumSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~EnumSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::EnumSpecifier";
};

class EnumHead : public Unit, public HasElements {

 public:
  explicit EnumHead(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kEnumHead; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~EnumHead() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::EnumHead";
};

class EnumHeadName : public Unit, public HasElements {

 public:
  explicit EnumHeadName(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kEnumHeadName; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~EnumHeadName() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::EnumHeadName";
};

class OpaqueEnumDeclaration : public Unit, public HasElements {

 public:
  explicit OpaqueEnumDeclaration(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kOpaqueEnumDeclaration;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~OpaqueEnumDeclaration() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::OpaqueEnumDeclaration";
};

class EnumKey : public Unit, public HasElements {

 public:
  explicit EnumKey(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kEnumKey; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~EnumKey() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::EnumKey";
};

class EnumBase : public Unit, public HasElements {

 public:
  explicit EnumBase(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kEnumBase; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~EnumBase() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::EnumBase";
};

class EnumeratorList : public Unit, public HasElements {

 public:
  explicit EnumeratorList(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kEnumeratorList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~EnumeratorList() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::EnumeratorList";
};

class EnumeratorDefinition : public Unit, public HasElements {

 public:
  explicit EnumeratorDefinition(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kEnumeratorDefinition; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~EnumeratorDefinition() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::EnumeratorDefinition";
};

class Enumerator : public Unit, public HasElements {

 public:
  explicit Enumerator(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kEnumerator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Enumerator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Enumerator";
};

class UsingEnumDeclaration : public Unit, public HasElements {

 public:
  explicit UsingEnumDeclaration(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kUsingEnumDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~UsingEnumDeclaration() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::UsingEnumDeclaration";
};

class NamespaceName : public Unit, public HasElements {

 public:
  explicit NamespaceName(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNamespaceName; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NamespaceName() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NamespaceName";
};

class NamespaceDefinition : public Unit, public HasElements {

 public:
  explicit NamespaceDefinition(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNamespaceDefinition; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NamespaceDefinition() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NamespaceDefinition";
};

class NamedNamespaceDefinition : public Unit, public HasElements {

 public:
  explicit NamedNamespaceDefinition(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kNamedNamespaceDefinition;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NamedNamespaceDefinition() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::NamedNamespaceDefinition";
};

class UnnamedNamespaceDefinition : public Unit, public HasElements {

 public:
  explicit UnnamedNamespaceDefinition(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kUnnamedNamespaceDefinition;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~UnnamedNamespaceDefinition() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::UnnamedNamespaceDefinition";
};

class NestedNamespaceDefinition : public Unit, public HasElements {

 public:
  explicit NestedNamespaceDefinition(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kNestedNamespaceDefinition;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NestedNamespaceDefinition() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::NestedNamespaceDefinition";
};

class EnclosingNamespaceSpecifier : public Unit, public HasElements {

 public:
  explicit EnclosingNamespaceSpecifier(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kEnclosingNamespaceSpecifier;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~EnclosingNamespaceSpecifier() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::EnclosingNamespaceSpecifier";
};

class NamespaceBody : public Unit, public HasElements {

 public:
  explicit NamespaceBody(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNamespaceBody; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NamespaceBody() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NamespaceBody";
};

class NamespaceAlias : public Unit, public HasElements {

 public:
  explicit NamespaceAlias(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNamespaceAlias; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NamespaceAlias() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NamespaceAlias";
};

class NamespaceAliasDefinition : public Unit, public HasElements {

 public:
  explicit NamespaceAliasDefinition(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kNamespaceAliasDefinition;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NamespaceAliasDefinition() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::NamespaceAliasDefinition";
};

class QualifiedNamespaceSpecifier : public Unit, public HasElements {

 public:
  explicit QualifiedNamespaceSpecifier(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kQualifiedNamespaceSpecifier;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~QualifiedNamespaceSpecifier() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::QualifiedNamespaceSpecifier";
};

class UsingDirective : public Unit, public HasElements {

 public:
  explicit UsingDirective(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kUsingDirective; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~UsingDirective() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::UsingDirective";
};

class UsingDeclaration : public Unit, public HasElements {

 public:
  explicit UsingDeclaration(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kUsingDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~UsingDeclaration() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::UsingDeclaration";
};

class UsingDeclaratorList : public Unit, public HasElements {

 public:
  explicit UsingDeclaratorList(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kUsingDeclaratorList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~UsingDeclaratorList() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::UsingDeclaratorList";
};

class UsingDeclarator : public Unit, public HasElements {

 public:
  explicit UsingDeclarator(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kUsingDeclarator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~UsingDeclarator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::UsingDeclarator";
};

class AsmDeclaration : public Unit, public HasElements {

 public:
  explicit AsmDeclaration(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAsmDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AsmDeclaration() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::AsmDeclaration";
};

class LinkageSpecification : public Unit, public HasElements {

 public:
  explicit LinkageSpecification(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kLinkageSpecification; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~LinkageSpecification() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::LinkageSpecification";
};

class AttributeSpecifierSeq : public Unit, public HasElements {

 public:
  explicit AttributeSpecifierSeq(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kAttributeSpecifierSeq;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AttributeSpecifierSeq() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::AttributeSpecifierSeq";
};

class AttributeSpecifier : public Unit, public HasElements {

 public:
  explicit AttributeSpecifier(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAttributeSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AttributeSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::AttributeSpecifier";
};

class AlignmentSpecifier : public Unit, public HasElements {

 public:
  explicit AlignmentSpecifier(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAlignmentSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AlignmentSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::AlignmentSpecifier";
};

class AttributeUsingPrefix : public Unit, public HasElements {

 public:
  explicit AttributeUsingPrefix(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAttributeUsingPrefix; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AttributeUsingPrefix() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::AttributeUsingPrefix";
};

class AttributeList : public Unit, public HasElements {

 public:
  explicit AttributeList(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAttributeList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AttributeList() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::AttributeList";
};

class Attribute : public Unit, public HasElements {

 public:
  explicit Attribute(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAttribute; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Attribute() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Attribute";
};

class AttributeToken : public Unit, public HasElements {

 public:
  explicit AttributeToken(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAttributeToken; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AttributeToken() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::AttributeToken";
};

class AttributeScopedToken : public Unit, public HasElements {

 public:
  explicit AttributeScopedToken(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAttributeScopedToken; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AttributeScopedToken() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::AttributeScopedToken";
};

class AttributeNamespace : public Unit, public HasElements {

 public:
  explicit AttributeNamespace(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAttributeNamespace; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AttributeNamespace() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::AttributeNamespace";
};

class AttributeArgumentClause : public Unit, public HasElements {

 public:
  explicit AttributeArgumentClause(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kAttributeArgumentClause;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AttributeArgumentClause() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::AttributeArgumentClause";
};

class BalancedTokenSeq : public Unit, public HasElements {

 public:
  explicit BalancedTokenSeq(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kBalancedTokenSeq; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~BalancedTokenSeq() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::BalancedTokenSeq";
};

class BalancedToken : public Unit, public HasElements {

 public:
  explicit BalancedToken(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kBalancedToken; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~BalancedToken() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::BalancedToken";
};

class ModuleDeclaration : public Unit, public HasElements {

 public:
  explicit ModuleDeclaration(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kModuleDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ModuleDeclaration() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ModuleDeclaration";
};

class ModuleName : public Unit, public HasElements {

 public:
  explicit ModuleName(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kModuleName; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ModuleName() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ModuleName";
};

class ModulePartition : public Unit, public HasElements {

 public:
  explicit ModulePartition(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kModulePartition; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ModulePartition() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ModulePartition";
};

class ModuleNameQualifier : public Unit, public HasElements {

 public:
  explicit ModuleNameQualifier(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kModuleNameQualifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ModuleNameQualifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ModuleNameQualifier";
};

class ExportDeclaration : public Unit, public HasElements {

 public:
  explicit ExportDeclaration(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kExportDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ExportDeclaration() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ExportDeclaration";
};

class ModuleImportDeclaration : public Unit, public HasElements {

 public:
  explicit ModuleImportDeclaration(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kModuleImportDeclaration;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ModuleImportDeclaration() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ModuleImportDeclaration";
};

class GlobalModuleFragment : public Unit, public HasElements {

 public:
  explicit GlobalModuleFragment(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kGlobalModuleFragment; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~GlobalModuleFragment() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::GlobalModuleFragment";
};

class PrivateModuleFragment : public Unit, public HasElements {

 public:
  explicit PrivateModuleFragment(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kPrivateModuleFragment;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PrivateModuleFragment() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::PrivateModuleFragment";
};

class ClassName : public Unit, public HasElements {

 public:
  explicit ClassName(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kClassName; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ClassName() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ClassName";
};

class ClassSpecifier : public Unit, public HasElements {

 public:
  explicit ClassSpecifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kClassSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ClassSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ClassSpecifier";
};

class ClassHead : public Unit, public HasElements {

 public:
  explicit ClassHead(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kClassHead; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ClassHead() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ClassHead";
};

class ClassHeadName : public Unit, public HasElements {

 public:
  explicit ClassHeadName(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kClassHeadName; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ClassHeadName() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ClassHeadName";
};

class ClassVirtSpecifier : public Unit, public HasElements {

 public:
  explicit ClassVirtSpecifier(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kClassVirtSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ClassVirtSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ClassVirtSpecifier";
};

class ClassKey : public Unit, public HasElements {

 public:
  explicit ClassKey(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kClassKey; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ClassKey() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ClassKey";
};

class MemberSpecification : public Unit, public HasElements {

 public:
  explicit MemberSpecification(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kMemberSpecification; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~MemberSpecification() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::MemberSpecification";
};

class MemberDeclaration : public Unit, public HasElements {

 public:
  explicit MemberDeclaration(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kMemberDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~MemberDeclaration() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::MemberDeclaration";
};

class MemberDeclaratorList : public Unit, public HasElements {

 public:
  explicit MemberDeclaratorList(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kMemberDeclaratorList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~MemberDeclaratorList() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::MemberDeclaratorList";
};

class MemberDeclarator : public Unit, public HasElements {

 public:
  explicit MemberDeclarator(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kMemberDeclarator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~MemberDeclarator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::MemberDeclarator";
};

class VirtSpecifierSeq : public Unit, public HasElements {

 public:
  explicit VirtSpecifierSeq(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kVirtSpecifierSeq; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~VirtSpecifierSeq() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::VirtSpecifierSeq";
};

class VirtSpecifier : public Unit, public HasElements {

 public:
  explicit VirtSpecifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kVirtSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~VirtSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::VirtSpecifier";
};

class PureSpecifier : public Unit, public HasElements {

 public:
  explicit PureSpecifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kPureSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PureSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::PureSpecifier";
};

class ConversionFunctionId : public Unit, public HasElements {

 public:
  explicit ConversionFunctionId(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kConversionFunctionId; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ConversionFunctionId() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ConversionFunctionId";
};

class ConversionTypeId : public Unit, public HasElements {

 public:
  explicit ConversionTypeId(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kConversionTypeId; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ConversionTypeId() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ConversionTypeId";
};

class ConversionDeclarator : public Unit, public HasElements {

 public:
  explicit ConversionDeclarator(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kConversionDeclarator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ConversionDeclarator() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ConversionDeclarator";
};

class BaseClause : public Unit, public HasElements {

 public:
  explicit BaseClause(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kBaseClause; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~BaseClause() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::BaseClause";
};

class BaseSpecifierList : public Unit, public HasElements {

 public:
  explicit BaseSpecifierList(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kBaseSpecifierList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~BaseSpecifierList() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::BaseSpecifierList";
};

class BaseSpecifier : public Unit, public HasElements {

 public:
  explicit BaseSpecifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kBaseSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~BaseSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::BaseSpecifier";
};

class ClassOrDecltype : public Unit, public HasElements {

 public:
  explicit ClassOrDecltype(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kClassOrDecltype; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ClassOrDecltype() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ClassOrDecltype";
};

class AccessSpecifier : public Unit, public HasElements {

 public:
  explicit AccessSpecifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kAccessSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~AccessSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::AccessSpecifier";
};

class CtorInitializer : public Unit, public HasElements {

 public:
  explicit CtorInitializer(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kCtorInitializer; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~CtorInitializer() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::CtorInitializer";
};

class MemInitializerList : public Unit, public HasElements {

 public:
  explicit MemInitializerList(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kMemInitializerList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~MemInitializerList() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::MemInitializerList";
};

class MemInitializer : public Unit, public HasElements {

 public:
  explicit MemInitializer(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kMemInitializer; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~MemInitializer() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::MemInitializer";
};

class MemInitializerId : public Unit, public HasElements {

 public:
  explicit MemInitializerId(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kMemInitializerId; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~MemInitializerId() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::MemInitializerId";
};

class OperatorFunctionId : public Unit, public HasElements {

 public:
  explicit OperatorFunctionId(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kOperatorFunctionId; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~OperatorFunctionId() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::OperatorFunctionId";
};

class TheOperator : public Unit, public HasElements {

 public:
  explicit TheOperator(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTheOperator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TheOperator() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TheOperator";
};

class LiteralOperatorId : public Unit, public HasElements {

 public:
  explicit LiteralOperatorId(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kLiteralOperatorId; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~LiteralOperatorId() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::LiteralOperatorId";
};

class TemplateDeclaration : public Unit, public HasElements {

 public:
  explicit TemplateDeclaration(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTemplateDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TemplateDeclaration() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TemplateDeclaration";
};

class TemplateHead : public Unit, public HasElements {

 public:
  explicit TemplateHead(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTemplateHead; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TemplateHead() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TemplateHead";
};

class TemplateParameterList : public Unit, public HasElements {

 public:
  explicit TemplateParameterList(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kTemplateParameterList;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TemplateParameterList() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::TemplateParameterList";
};

class RequiresClause : public Unit, public HasElements {

 public:
  explicit RequiresClause(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kRequiresClause; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~RequiresClause() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::RequiresClause";
};

class ConstraintLogicalOrExpression : public Unit, public HasElements {

 public:
  explicit ConstraintLogicalOrExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kConstraintLogicalOrExpression;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ConstraintLogicalOrExpression() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ConstraintLogicalOrExpression";
};

class ConstraintLogicalAndExpression : public Unit, public HasElements {

 public:
  explicit ConstraintLogicalAndExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kConstraintLogicalAndExpression;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ConstraintLogicalAndExpression() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ConstraintLogicalAndExpression";
};

class TemplateParameter : public Unit, public HasElements {

 public:
  explicit TemplateParameter(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTemplateParameter; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TemplateParameter() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TemplateParameter";
};

class TypeParameter : public Unit, public HasElements {

 public:
  explicit TypeParameter(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTypeParameter; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TypeParameter() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TypeParameter";
};

class TypeParameterKey : public Unit, public HasElements {

 public:
  explicit TypeParameterKey(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTypeParameterKey; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TypeParameterKey() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TypeParameterKey";
};

class TypeConstraint : public Unit, public HasElements {

 public:
  explicit TypeConstraint(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTypeConstraint; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TypeConstraint() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TypeConstraint";
};

class SimpleTemplateId : public Unit, public HasElements {

 public:
  explicit SimpleTemplateId(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kSimpleTemplateId; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~SimpleTemplateId() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::SimpleTemplateId";
};

class TemplateId : public Unit, public HasElements {

 public:
  explicit TemplateId(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTemplateId; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TemplateId() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TemplateId";
};

class TemplateName : public Unit, public HasElements {

 public:
  explicit TemplateName(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTemplateName; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TemplateName() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TemplateName";
};

class TemplateArgumentList : public Unit, public HasElements {

 public:
  explicit TemplateArgumentList(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTemplateArgumentList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TemplateArgumentList() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::TemplateArgumentList";
};

class TemplateArgument : public Unit, public HasElements {

 public:
  explicit TemplateArgument(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTemplateArgument; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TemplateArgument() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TemplateArgument";
};

class ConstraintExpression : public Unit, public HasElements {

 public:
  explicit ConstraintExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kConstraintExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ConstraintExpression() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ConstraintExpression";
};

class DeductionGuide : public Unit, public HasElements {

 public:
  explicit DeductionGuide(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDeductionGuide; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DeductionGuide() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::DeductionGuide";
};

class ConceptDefinition : public Unit, public HasElements {

 public:
  explicit ConceptDefinition(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kConceptDefinition; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ConceptDefinition() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ConceptDefinition";
};

class ConceptName : public Unit, public HasElements {

 public:
  explicit ConceptName(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kConceptName; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ConceptName() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ConceptName";
};

class TypenameSpecifier : public Unit, public HasElements {

 public:
  explicit TypenameSpecifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTypenameSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TypenameSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TypenameSpecifier";
};

class ExplicitInstantiation : public Unit, public HasElements {

 public:
  explicit ExplicitInstantiation(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kExplicitInstantiation;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ExplicitInstantiation() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ExplicitInstantiation";
};

class ExplicitSpecialization : public Unit, public HasElements {

 public:
  explicit ExplicitSpecialization(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kExplicitSpecialization;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ExplicitSpecialization() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ExplicitSpecialization";
};

class TryBlock : public Unit, public HasElements {

 public:
  explicit TryBlock(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTryBlock; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TryBlock() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TryBlock";
};

class FunctionTryBlock : public Unit, public HasElements {

 public:
  explicit FunctionTryBlock(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kFunctionTryBlock; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~FunctionTryBlock() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::FunctionTryBlock";
};

class HandlerSeq : public Unit, public HasElements {

 public:
  explicit HandlerSeq(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kHandlerSeq; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HandlerSeq() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::HandlerSeq";
};

class Handler : public Unit, public HasElements {

 public:
  explicit Handler(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kHandler; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Handler() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Handler";
};

class ExceptionDeclaration : public Unit, public HasElements {

 public:
  explicit ExceptionDeclaration(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kExceptionDeclaration; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ExceptionDeclaration() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ExceptionDeclaration";
};

class NoexceptSpecifier : public Unit, public HasElements {

 public:
  explicit NoexceptSpecifier(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNoexceptSpecifier; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NoexceptSpecifier() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NoexceptSpecifier";
};

class PreprocessingFile : public Unit, public HasElements {

 public:
  explicit PreprocessingFile(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kPreprocessingFile; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PreprocessingFile() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::PreprocessingFile";
};

class ModuleFile : public Unit, public HasElements {

 public:
  explicit ModuleFile(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kModuleFile; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ModuleFile() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ModuleFile";
};

class PpGlobalModuleFragment : public Unit, public HasElements {

 public:
  explicit PpGlobalModuleFragment(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kPpGlobalModuleFragment;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PpGlobalModuleFragment() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::PpGlobalModuleFragment";
};

class PpPrivateModuleFragment : public Unit, public HasElements {

 public:
  explicit PpPrivateModuleFragment(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kPpPrivateModuleFragment;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PpPrivateModuleFragment() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::PpPrivateModuleFragment";
};

class Group : public Unit, public HasElements {

 public:
  explicit Group(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kGroup; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Group() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Group";
};

class GroupPart : public Unit, public HasElements {

 public:
  explicit GroupPart(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kGroupPart; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~GroupPart() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::GroupPart";
};

class ControlLine : public Unit, public HasElements {

 public:
  explicit ControlLine(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kControlLine; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ControlLine() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ControlLine";
};

class IfSection : public Unit, public HasElements {

 public:
  explicit IfSection(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kIfSection; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~IfSection() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::IfSection";
};

class IfGroup : public Unit, public HasElements {

 public:
  explicit IfGroup(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kIfGroup; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~IfGroup() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::IfGroup";
};

class ElifGroups : public Unit, public HasElements {

 public:
  explicit ElifGroups(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kElifGroups; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ElifGroups() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ElifGroups";
};

class ElifGroup : public Unit, public HasElements {

 public:
  explicit ElifGroup(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kElifGroup; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ElifGroup() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ElifGroup";
};

class ElseGroup : public Unit, public HasElements {

 public:
  explicit ElseGroup(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kElseGroup; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ElseGroup() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ElseGroup";
};

class EndifLine : public Unit, public HasElements {

 public:
  explicit EndifLine(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kEndifLine; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~EndifLine() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::EndifLine";
};

class TextLine : public Unit, public HasElements {

 public:
  explicit TextLine(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kTextLine; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~TextLine() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::TextLine";
};

class ConditionallySupportedDirective : public Unit, public HasElements {

 public:
  explicit ConditionallySupportedDirective(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kConditionallySupportedDirective;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ConditionallySupportedDirective() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::ConditionallySupportedDirective";
};

class Lparen : public Unit, public HasElements {

 public:
  explicit Lparen(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kLparen; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Lparen() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Lparen";
};

class IdentifierList : public Unit, public HasElements {

 public:
  explicit IdentifierList(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kIdentifierList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~IdentifierList() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::IdentifierList";
};

class ReplacementList : public Unit, public HasElements {

 public:
  explicit ReplacementList(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kReplacementList; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ReplacementList() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ReplacementList";
};

class PpTokens : public Unit, public HasElements {

 public:
  explicit PpTokens(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kPpTokens; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PpTokens() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::PpTokens";
};

class NewLine : public Unit, public HasElements {

 public:
  explicit NewLine(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNewLine; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NewLine() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NewLine";
};

class DefinedMacroExpression : public Unit, public HasElements {

 public:
  explicit DefinedMacroExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kDefinedMacroExpression;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DefinedMacroExpression() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::DefinedMacroExpression";
};

class HPreprocessingToken : public Unit, public HasElements {

 public:
  explicit HPreprocessingToken(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kHPreprocessingToken; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HPreprocessingToken() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::HPreprocessingToken";
};

class HPpTokens : public Unit, public HasElements {

 public:
  explicit HPpTokens(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kHPpTokens; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HPpTokens() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::HPpTokens";
};

class HeaderNameTokens : public Unit, public HasElements {

 public:
  explicit HeaderNameTokens(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kHeaderNameTokens; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HeaderNameTokens() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::HeaderNameTokens";
};

class HasIncludeExpression : public Unit, public HasElements {

 public:
  explicit HasIncludeExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kHasIncludeExpression; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HasIncludeExpression() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::HasIncludeExpression";
};

class HasAttributeExpression : public Unit, public HasElements {

 public:
  explicit HasAttributeExpression(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kHasAttributeExpression;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HasAttributeExpression() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::HasAttributeExpression";
};

class PpModule : public Unit, public HasElements {

 public:
  explicit PpModule(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kPpModule; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PpModule() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::PpModule";
};

class PpImport : public Unit, public HasElements {

 public:
  explicit PpImport(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kPpImport; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PpImport() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::PpImport";
};

class VaOptReplacement : public Unit, public HasElements {

 public:
  explicit VaOptReplacement(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kVaOptReplacement; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~VaOptReplacement() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::VaOptReplacement";
};

class HexQuad : public Unit, public HasElements {

 public:
  explicit HexQuad(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kHexQuad; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HexQuad() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::HexQuad";
};

class UniversalCharacterName : public Unit, public HasElements {

 public:
  explicit UniversalCharacterName(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kUniversalCharacterName;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~UniversalCharacterName() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::UniversalCharacterName";
};

class PreprocessingToken : public Unit, public HasElements {

 public:
  explicit PreprocessingToken(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kPreprocessingToken; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PreprocessingToken() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::PreprocessingToken";
};

class Token : public Unit, public HasElements {

 public:
  explicit Token(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kToken; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Token() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Token";
};

class HeaderName : public Unit, public HasElements {

 public:
  explicit HeaderName(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kHeaderName; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HeaderName() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::HeaderName";
};

class HCharSequence : public Unit, public HasElements {

 public:
  explicit HCharSequence(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kHCharSequence; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HCharSequence() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::HCharSequence";
};

class HChar : public Unit, public HasElements {

 public:
  explicit HChar(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kHChar; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HChar() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::HChar";
};

class QCharSequence : public Unit, public HasElements {

 public:
  explicit QCharSequence(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kQCharSequence; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~QCharSequence() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::QCharSequence";
};

class QChar : public Unit, public HasElements {

 public:
  explicit QChar(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kQChar; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~QChar() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::QChar";
};

class PpNumber : public Unit, public HasElements {

 public:
  explicit PpNumber(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kPpNumber; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PpNumber() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::PpNumber";
};

class IdentifierNondigit : public Unit, public HasElements {

 public:
  explicit IdentifierNondigit(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kIdentifierNondigit; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~IdentifierNondigit() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::IdentifierNondigit";
};

class Nondigit : public Unit, public HasElements {

 public:
  explicit Nondigit(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNondigit; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Nondigit() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Nondigit";
};

class Digit : public Unit, public HasElements {

 public:
  explicit Digit(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDigit; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Digit() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Digit";
};

class Keyword : public Unit, public HasElements {

 public:
  explicit Keyword(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kKeyword; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Keyword() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Keyword";
};

class PreprocessingOpOrPunc : public Unit, public HasElements {

 public:
  explicit PreprocessingOpOrPunc(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kPreprocessingOpOrPunc;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PreprocessingOpOrPunc() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::PreprocessingOpOrPunc";
};

class PreprocessingOperator : public Unit, public HasElements {

 public:
  explicit PreprocessingOperator(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ = parser::details::ParseFunctionKind::kPreprocessingOperator;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PreprocessingOperator() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::PreprocessingOperator";
};

class OperatorOrPunctuator : public Unit, public HasElements {

 public:
  explicit OperatorOrPunctuator(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kOperatorOrPunctuator; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~OperatorOrPunctuator() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::OperatorOrPunctuator";
};

class Literal : public Unit, public HasElements {

 public:
  explicit Literal(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kLiteral; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Literal() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Literal";
};

class BinaryDigit : public Unit, public HasElements {

 public:
  explicit BinaryDigit(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kBinaryDigit; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~BinaryDigit() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::BinaryDigit";
};

class OctalDigit : public Unit, public HasElements {

 public:
  explicit OctalDigit(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kOctalDigit; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~OctalDigit() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::OctalDigit";
};

class NonzeroDigit : public Unit, public HasElements {

 public:
  explicit NonzeroDigit(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kNonzeroDigit; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~NonzeroDigit() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::NonzeroDigit";
};

class HexadecimalPrefix : public Unit, public HasElements {

 public:
  explicit HexadecimalPrefix(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kHexadecimalPrefix; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HexadecimalPrefix() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::HexadecimalPrefix";
};

class HexadecimalDigitSequence : public Unit, public HasElements {

 public:
  explicit HexadecimalDigitSequence(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kHexadecimalDigitSequence;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HexadecimalDigitSequence() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::HexadecimalDigitSequence";
};

class HexadecimalDigit : public Unit, public HasElements {

 public:
  explicit HexadecimalDigit(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kHexadecimalDigit; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HexadecimalDigit() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::HexadecimalDigit";
};

class IntegerSuffix : public Unit, public HasElements {

 public:
  explicit IntegerSuffix(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kIntegerSuffix; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~IntegerSuffix() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::IntegerSuffix";
};

class UnsignedSuffix : public Unit, public HasElements {

 public:
  explicit UnsignedSuffix(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kUnsignedSuffix; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~UnsignedSuffix() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::UnsignedSuffix";
};

class LongSuffix : public Unit, public HasElements {

 public:
  explicit LongSuffix(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kLongSuffix; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~LongSuffix() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::LongSuffix";
};

class LongLongSuffix : public Unit, public HasElements {

 public:
  explicit LongLongSuffix(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kLongLongSuffix; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~LongLongSuffix() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::LongLongSuffix";
};

class EncodingPrefix : public Unit, public HasElements {

 public:
  explicit EncodingPrefix(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kEncodingPrefix; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~EncodingPrefix() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::EncodingPrefix";
};

class CCharSequence : public Unit, public HasElements {

 public:
  explicit CCharSequence(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kCCharSequence; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~CCharSequence() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::CCharSequence";
};

class CChar : public Unit, public HasElements {

 public:
  explicit CChar(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kCChar; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~CChar() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::CChar";
};

class EscapeSequence : public Unit, public HasElements {

 public:
  explicit EscapeSequence(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kEscapeSequence; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~EscapeSequence() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::EscapeSequence";
};

class SimpleEscapeSequence : public Unit, public HasElements {

 public:
  explicit SimpleEscapeSequence(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kSimpleEscapeSequence; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~SimpleEscapeSequence() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::SimpleEscapeSequence";
};

class OctalEscapeSequence : public Unit, public HasElements {

 public:
  explicit OctalEscapeSequence(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kOctalEscapeSequence; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~OctalEscapeSequence() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::OctalEscapeSequence";
};

class HexadecimalEscapeSequence : public Unit, public HasElements {

 public:
  explicit HexadecimalEscapeSequence(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kHexadecimalEscapeSequence;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HexadecimalEscapeSequence() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::HexadecimalEscapeSequence";
};

class DecimalFloatingPointLiteral : public Unit, public HasElements {

 public:
  explicit DecimalFloatingPointLiteral(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kDecimalFloatingPointLiteral;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DecimalFloatingPointLiteral() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::DecimalFloatingPointLiteral";
};

class HexadecimalFloatingPointLiteral : public Unit, public HasElements {

 public:
  explicit HexadecimalFloatingPointLiteral(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kHexadecimalFloatingPointLiteral;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HexadecimalFloatingPointLiteral() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::HexadecimalFloatingPointLiteral";
};

class FractionalConstant : public Unit, public HasElements {

 public:
  explicit FractionalConstant(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kFractionalConstant; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~FractionalConstant() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::FractionalConstant";
};

class HexadecimalFractionalConstant : public Unit, public HasElements {

 public:
  explicit HexadecimalFractionalConstant(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    {
      this->kind_ =
          parser::details::ParseFunctionKind::kHexadecimalFractionalConstant;
    }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~HexadecimalFractionalConstant() override = default;

 private:
  constexpr static const char* kTag =
      "lps::sema::details::HexadecimalFractionalConstant";
};

class ExponentPart : public Unit, public HasElements {

 public:
  explicit ExponentPart(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kExponentPart; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~ExponentPart() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::ExponentPart";
};

class BinaryExponentPart : public Unit, public HasElements {

 public:
  explicit BinaryExponentPart(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kBinaryExponentPart; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~BinaryExponentPart() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::BinaryExponentPart";
};

class Sign : public Unit, public HasElements {

 public:
  explicit Sign(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kSign; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~Sign() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::Sign";
};

class DigitSequence : public Unit, public HasElements {

 public:
  explicit DigitSequence(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDigitSequence; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DigitSequence() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::DigitSequence";
};

class FloatingPointSuffix : public Unit, public HasElements {

 public:
  explicit FloatingPointSuffix(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kFloatingPointSuffix; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~FloatingPointSuffix() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::FloatingPointSuffix";
};

class SCharSequence : public Unit, public HasElements {

 public:
  explicit SCharSequence(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kSCharSequence; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~SCharSequence() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::SCharSequence";
};

class SChar : public Unit, public HasElements {

 public:
  explicit SChar(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kSChar; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~SChar() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::SChar";
};

class RCharSequence : public Unit, public HasElements {

 public:
  explicit RCharSequence(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kRCharSequence; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~RCharSequence() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::RCharSequence";
};

class RChar : public Unit, public HasElements {

 public:
  explicit RChar(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kRChar; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~RChar() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::RChar";
};

class DCharSequence : public Unit, public HasElements {

 public:
  explicit DCharSequence(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDCharSequence; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DCharSequence() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::DCharSequence";
};

class DChar : public Unit, public HasElements {

 public:
  explicit DChar(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kDChar; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~DChar() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::DChar";
};

class BooleanLiteral : public Unit, public HasElements {

 public:
  explicit BooleanLiteral(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kBooleanLiteral; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~BooleanLiteral() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::BooleanLiteral";
};

class PointerLiteral : public Unit, public HasElements {

 public:
  explicit PointerLiteral(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kPointerLiteral; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~PointerLiteral() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::PointerLiteral";
};

class UserDefinedLiteral : public Unit, public HasElements {

 public:
  explicit UserDefinedLiteral(
      basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kUserDefinedLiteral; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~UserDefinedLiteral() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::UserDefinedLiteral";
};

class UdSuffix : public Unit, public HasElements {

 public:
  explicit UdSuffix(basic::Vector<2, std::unique_ptr<Unit>>&& elements)
      : HasElements(std::move(elements)) {
    { this->kind_ = parser::details::ParseFunctionKind::kUdSuffix; }
  }
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
  ~UdSuffix() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::UdSuffix";
};

}  // namespace lps::sema::details::unit
