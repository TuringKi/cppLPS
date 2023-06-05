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
#include "parser.h"

namespace lps::parser::details {

#define ParseFunctionDef(NAME, N)                                       \
                                                                        \
  class NAME : public ParseFunction<meta::S(#NAME), N> {                \
   public:                                                              \
    using base = ParseFunction<meta::S(#NAME), N>;                      \
    ~NAME() = default;                                                  \
    template <typename... Params>                                       \
    explicit NAME(bool opt, Params... params) : base(opt, params...) {} \
    template <typename... Params>                                       \
    explicit NAME(Params... params) : base(params...) {}                \
    explicit NAME(const ParseFunctionInputs<base::kTag>& param)         \
        : base(param) {}                                                \
    template <meta::Str TagNameOther>                                   \
    explicit NAME(const ParseFunctionInputs<TagNameOther>& param)       \
        : base(param) {}                                                \
    ParseFunctionOutputs<base::kTag> operator()() override;             \
  };

ParseFunctionDef(TranslationUnit, 2);
ParseFunctionDef(PrimaryExpression, 7);
ParseFunctionDef(IdExpression, 2);
ParseFunctionDef(UnqualifiedId, 7);
ParseFunctionDef(QualifiedId, 1);
ParseFunctionDef(NestedNameSpecifier, 6);
ParseFunctionDef(LambdaExpression, 2);
ParseFunctionDef(LambdaIntroducer, 1);
ParseFunctionDef(LambdaDeclarator, 2);
ParseFunctionDef(LambdaCapture, 3);
ParseFunctionDef(CaptureDefault, 2);
ParseFunctionDef(CaptureList, 2);
ParseFunctionDef(Capture, 2);
ParseFunctionDef(SimpleCapture, 4);
ParseFunctionDef(InitCapture, 2);
ParseFunctionDef(FoldExpression, 3);
ParseFunctionDef(FoldOperator, 32);
ParseFunctionDef(RequiresExpression, 1);
ParseFunctionDef(RequirementParameterList, 1);
ParseFunctionDef(RequirementBody, 1);
ParseFunctionDef(RequirementSeq, 2);
ParseFunctionDef(Requirement, 4);
ParseFunctionDef(SimpleRequirement, 1);
ParseFunctionDef(TypeRequirement, 1);
ParseFunctionDef(CompoundRequirement, 1);
ParseFunctionDef(ReturnTypeRequirement, 1);
ParseFunctionDef(NestedRequirement, 1);
ParseFunctionDef(PostfixExpression, 17);
ParseFunctionDef(ExpressionList, 1);
ParseFunctionDef(UnaryExpression, 12);
ParseFunctionDef(UnaryOperator, 6);
ParseFunctionDef(AwaitExpression, 1);
ParseFunctionDef(NoexceptExpression, 1);
ParseFunctionDef(NewExpression, 2);
ParseFunctionDef(NewPlacement, 1);
ParseFunctionDef(NewTypeId, 1);
ParseFunctionDef(NewDeclarator, 2);
ParseFunctionDef(NoptrNewDeclarator, 2);
ParseFunctionDef(NewInitializer, 2);
ParseFunctionDef(DeleteExpression, 2);
ParseFunctionDef(CastExpression, 2);
ParseFunctionDef(PmExpression, 3);
ParseFunctionDef(MultiplicativeExpression, 4);
ParseFunctionDef(AdditiveExpression, 3);
ParseFunctionDef(ShiftExpression, 3);
ParseFunctionDef(CompareExpression, 2);
ParseFunctionDef(RelationalExpression, 5);
ParseFunctionDef(EqualityExpression, 3);
ParseFunctionDef(AndExpression, 2);
ParseFunctionDef(ExclusiveOrExpression, 2);
ParseFunctionDef(InclusiveOrExpression, 2);
ParseFunctionDef(LogicalAndExpression, 2);
ParseFunctionDef(LogicalOrExpression, 2);
ParseFunctionDef(ConditionalExpression, 2);
ParseFunctionDef(YieldExpression, 2);
ParseFunctionDef(ThrowExpression, 1);
ParseFunctionDef(AssignmentExpression, 4);
ParseFunctionDef(AssignmentOperator, 11);
ParseFunctionDef(Expression, 2);
ParseFunctionDef(ConstantExpression, 1);
ParseFunctionDef(Statement, 8);
ParseFunctionDef(InitStatement, 2);
ParseFunctionDef(Condition, 2);
ParseFunctionDef(LabeledStatement, 3);
ParseFunctionDef(ExpressionStatement, 1);
ParseFunctionDef(CompoundStatement, 1);
ParseFunctionDef(StatementSeq, 2);
ParseFunctionDef(SelectionStatement, 3);
ParseFunctionDef(IterationStatement, 4);
ParseFunctionDef(ForRangeDeclaration, 2);
ParseFunctionDef(ForRangeInitializer, 1);
ParseFunctionDef(JumpStatement, 5);
ParseFunctionDef(CoroutineReturnStatement, 1);
ParseFunctionDef(DeclarationStatement, 1);
ParseFunctionDef(DeclarationSeq, 2);
ParseFunctionDef(Declaration, 13);
ParseFunctionDef(BlockDeclaration, 9);
ParseFunctionDef(NodeclspecFunctionDeclaration, 1);
ParseFunctionDef(AliasDeclaration, 1);
ParseFunctionDef(SimpleDeclaration, 3);
ParseFunctionDef(StaticAssertDeclaration, 2);
ParseFunctionDef(EmptyDeclaration, 1);
ParseFunctionDef(AttributeDeclaration, 1);
ParseFunctionDef(DeclSpecifier, 9);
ParseFunctionDef(DeclSpecifierSeq, 2);
ParseFunctionDef(StorageClassSpecifier, 4);
ParseFunctionDef(FunctionSpecifier, 2);
ParseFunctionDef(ExplicitSpecifier, 2);
ParseFunctionDef(TypedefName, 2);
ParseFunctionDef(TypeSpecifier, 4);
ParseFunctionDef(TypeSpecifierSeq, 2);
ParseFunctionDef(DefiningTypeSpecifier, 3);
ParseFunctionDef(DefiningTypeSpecifierSeq, 2);
ParseFunctionDef(SimpleTypeSpecifier, 19);
ParseFunctionDef(TypeName, 3);
ParseFunctionDef(ElaboratedTypeSpecifier, 4);
ParseFunctionDef(ElaboratedEnumSpecifier, 1);
ParseFunctionDef(DecltypeSpecifier, 1);
ParseFunctionDef(PlaceholderTypeSpecifier, 2);
ParseFunctionDef(InitDeclaratorList, 2);
ParseFunctionDef(InitDeclarator, 2);
ParseFunctionDef(Declarator, 2);
ParseFunctionDef(PtrDeclarator, 2);
ParseFunctionDef(NoptrDeclarator, 4);
ParseFunctionDef(ParametersAndQualifiers, 2);
ParseFunctionDef(TrailingReturnType, 1);
ParseFunctionDef(PtrOperator, 4);
ParseFunctionDef(CvQualifierSeq, 1);
ParseFunctionDef(CvQualifier, 2);
ParseFunctionDef(RefQualifier, 2);
ParseFunctionDef(DeclaratorId, 1);
ParseFunctionDef(TypeId, 1);
ParseFunctionDef(DefiningTypeId, 1);
ParseFunctionDef(AbstractDeclarator, 3);
ParseFunctionDef(PtrAbstractDeclarator, 2);
ParseFunctionDef(NoptrAbstractDeclarator, 3);
ParseFunctionDef(AbstractPackDeclarator, 2);
ParseFunctionDef(NoptrAbstractPackDeclarator, 3);
ParseFunctionDef(ParameterDeclarationClause, 2);
ParseFunctionDef(ParameterDeclarationList, 2);
ParseFunctionDef(ParameterDeclaration, 4);
ParseFunctionDef(Initializer, 2);
ParseFunctionDef(BraceOrEqualInitializer, 2);
ParseFunctionDef(InitializerClause, 2);
ParseFunctionDef(BracedInitList, 3);
ParseFunctionDef(InitializerList, 2);
ParseFunctionDef(DesignatedInitializerList, 2);
ParseFunctionDef(DesignatedInitializerClause, 1);
ParseFunctionDef(Designator, 1);
ParseFunctionDef(ExprOrBracedInitList, 2);
ParseFunctionDef(FunctionDefinition, 2);
ParseFunctionDef(FunctionBody, 4);
ParseFunctionDef(EnumName, 1);
ParseFunctionDef(EnumSpecifier, 2);
ParseFunctionDef(EnumHead, 1);
ParseFunctionDef(EnumHeadName, 1);
ParseFunctionDef(OpaqueEnumDeclaration, 1);
ParseFunctionDef(EnumKey, 3);
ParseFunctionDef(EnumBase, 1);
ParseFunctionDef(EnumeratorList, 2);
ParseFunctionDef(EnumeratorDefinition, 2);
ParseFunctionDef(Enumerator, 1);
ParseFunctionDef(UsingEnumDeclaration, 1);
ParseFunctionDef(NamespaceName, 2);
ParseFunctionDef(NamespaceDefinition, 3);
ParseFunctionDef(NamedNamespaceDefinition, 1);
ParseFunctionDef(UnnamedNamespaceDefinition, 1);
ParseFunctionDef(NestedNamespaceDefinition, 1);
ParseFunctionDef(EnclosingNamespaceSpecifier, 2);
ParseFunctionDef(NamespaceBody, 1);
ParseFunctionDef(NamespaceAlias, 1);
ParseFunctionDef(NamespaceAliasDefinition, 1);
ParseFunctionDef(QualifiedNamespaceSpecifier, 1);
ParseFunctionDef(UsingDirective, 1);
ParseFunctionDef(UsingDeclaration, 1);
ParseFunctionDef(UsingDeclaratorList, 2);
ParseFunctionDef(UsingDeclarator, 1);
ParseFunctionDef(AsmDeclaration, 1);
ParseFunctionDef(LinkageSpecification, 2);
ParseFunctionDef(AttributeSpecifierSeq, 1);
ParseFunctionDef(AttributeSpecifier, 2);
ParseFunctionDef(AlignmentSpecifier, 2);
ParseFunctionDef(AttributeUsingPrefix, 1);
ParseFunctionDef(AttributeList, 4);
ParseFunctionDef(Attribute, 1);
ParseFunctionDef(AttributeToken, 2);
ParseFunctionDef(AttributeScopedToken, 1);
ParseFunctionDef(AttributeNamespace, 1);
ParseFunctionDef(AttributeArgumentClause, 1);
ParseFunctionDef(BalancedTokenSeq, 2);
ParseFunctionDef(BalancedToken, 7);
ParseFunctionDef(ModuleDeclaration, 1);
ParseFunctionDef(ModuleName, 1);
ParseFunctionDef(ModulePartition, 1);
ParseFunctionDef(ModuleNameQualifier, 2);
ParseFunctionDef(ExportDeclaration, 3);
ParseFunctionDef(ModuleImportDeclaration, 3);
ParseFunctionDef(GlobalModuleFragment, 1);
ParseFunctionDef(PrivateModuleFragment, 1);
ParseFunctionDef(ClassName, 2);
ParseFunctionDef(ClassSpecifier, 1);
ParseFunctionDef(ClassHead, 2);
ParseFunctionDef(ClassHeadName, 1);
ParseFunctionDef(ClassVirtSpecifier, 1);
ParseFunctionDef(ClassKey, 3);
ParseFunctionDef(MemberSpecification, 2);
ParseFunctionDef(MemberDeclaration, 11);
ParseFunctionDef(MemberDeclaratorList, 2);
ParseFunctionDef(MemberDeclarator, 4);
ParseFunctionDef(VirtSpecifierSeq, 2);
ParseFunctionDef(VirtSpecifier, 2);
ParseFunctionDef(PureSpecifier, 1);
ParseFunctionDef(ConversionFunctionId, 1);
ParseFunctionDef(ConversionTypeId, 1);
ParseFunctionDef(ConversionDeclarator, 1);
ParseFunctionDef(BaseClause, 1);
ParseFunctionDef(BaseSpecifierList, 2);
ParseFunctionDef(BaseSpecifier, 3);
ParseFunctionDef(ClassOrDecltype, 3);
ParseFunctionDef(AccessSpecifier, 3);
ParseFunctionDef(CtorInitializer, 1);
ParseFunctionDef(MemInitializerList, 2);
ParseFunctionDef(MemInitializer, 2);
ParseFunctionDef(MemInitializerId, 2);
ParseFunctionDef(OperatorFunctionId, 1);
ParseFunctionDef(TheOperator, 43);
ParseFunctionDef(LiteralOperatorId, 2);
ParseFunctionDef(TemplateDeclaration, 2);
ParseFunctionDef(TemplateHead, 1);
ParseFunctionDef(TemplateParameterList, 2);
ParseFunctionDef(RequiresClause, 1);
ParseFunctionDef(ConstraintLogicalOrExpression, 2);
ParseFunctionDef(ConstraintLogicalAndExpression, 2);
ParseFunctionDef(TemplateParameter, 2);
ParseFunctionDef(TypeParameter, 6);
ParseFunctionDef(TypeParameterKey, 2);
ParseFunctionDef(TypeConstraint, 2);
ParseFunctionDef(SimpleTemplateId, 1);
ParseFunctionDef(TemplateId, 3);
ParseFunctionDef(TemplateName, 1);
ParseFunctionDef(TemplateArgumentList, 2);
ParseFunctionDef(TemplateArgument, 3);
ParseFunctionDef(ConstraintExpression, 1);
ParseFunctionDef(DeductionGuide, 1);
ParseFunctionDef(ConceptDefinition, 1);
ParseFunctionDef(ConceptName, 1);
ParseFunctionDef(TypenameSpecifier, 2);
ParseFunctionDef(ExplicitInstantiation, 1);
ParseFunctionDef(ExplicitSpecialization, 1);
ParseFunctionDef(TryBlock, 1);
ParseFunctionDef(FunctionTryBlock, 1);
ParseFunctionDef(HandlerSeq, 1);
ParseFunctionDef(Handler, 1);
ParseFunctionDef(ExceptionDeclaration, 3);
ParseFunctionDef(NoexceptSpecifier, 2);
ParseFunctionDef(PreprocessingFile, 2);
ParseFunctionDef(ModuleFile, 1);
ParseFunctionDef(PpGlobalModuleFragment, 1);
ParseFunctionDef(PpPrivateModuleFragment, 1);
ParseFunctionDef(Group, 2);
ParseFunctionDef(GroupPart, 4);
ParseFunctionDef(ControlLine, 11);
ParseFunctionDef(IfSection, 1);
ParseFunctionDef(IfGroup, 3);
ParseFunctionDef(ElifGroups, 2);
ParseFunctionDef(ElifGroup, 1);
ParseFunctionDef(ElseGroup, 1);
ParseFunctionDef(EndifLine, 1);
ParseFunctionDef(TextLine, 1);
ParseFunctionDef(ConditionallySupportedDirective, 1);
ParseFunctionDef(Lparen, 1);
ParseFunctionDef(IdentifierList, 2);
ParseFunctionDef(ReplacementList, 1);
ParseFunctionDef(PpTokens, 2);
ParseFunctionDef(NewLine, 1);
ParseFunctionDef(DefinedMacroExpression, 2);
ParseFunctionDef(HPreprocessingToken, 1);
ParseFunctionDef(HPpTokens, 2);
ParseFunctionDef(HeaderNameTokens, 2);
ParseFunctionDef(HasIncludeExpression, 2);
ParseFunctionDef(HasAttributeExpression, 1);
ParseFunctionDef(PpModule, 1);
ParseFunctionDef(PpImport, 3);
ParseFunctionDef(VaOptReplacement, 1);
ParseFunctionDef(HexQuad, 1);
ParseFunctionDef(UniversalCharacterName, 2);
ParseFunctionDef(PreprocessingToken, 11);
ParseFunctionDef(Token, 4);
ParseFunctionDef(HeaderName, 2);
ParseFunctionDef(HCharSequence, 2);
ParseFunctionDef(HChar, 1);
ParseFunctionDef(QCharSequence, 2);
ParseFunctionDef(QChar, 1);
ParseFunctionDef(PpNumber, 11);
ParseFunctionDef(IdentifierNondigit, 2);
ParseFunctionDef(Nondigit, 1);
ParseFunctionDef(Digit, 1);
ParseFunctionDef(Keyword, 1);
ParseFunctionDef(PreprocessingOpOrPunc, 2);
ParseFunctionDef(PreprocessingOperator, 4);
ParseFunctionDef(OperatorOrPunctuator, 65);
ParseFunctionDef(Literal, 7);
ParseFunctionDef(IntegerLiteral, 4);
ParseFunctionDef(BinaryLiteral, 4);
ParseFunctionDef(OctalLiteral, 2);
ParseFunctionDef(DecimalLiteral, 2);
ParseFunctionDef(HexadecimalLiteral, 1);
ParseFunctionDef(BinaryDigit, 2);
ParseFunctionDef(OctalDigit, 8);
ParseFunctionDef(NonzeroDigit, 9);
ParseFunctionDef(HexadecimalPrefix, 2);
ParseFunctionDef(HexadecimalDigitSequence, 2);
ParseFunctionDef(HexadecimalDigit, 16);
ParseFunctionDef(IntegerSuffix, 4);
ParseFunctionDef(UnsignedSuffix, 2);
ParseFunctionDef(LongSuffix, 2);
ParseFunctionDef(LongLongSuffix, 2);
ParseFunctionDef(CharacterLiteral, 1);
ParseFunctionDef(EncodingPrefix, 4);
ParseFunctionDef(CCharSequence, 2);
ParseFunctionDef(CChar, 2);
ParseFunctionDef(EscapeSequence, 3);
ParseFunctionDef(SimpleEscapeSequence, 11);
ParseFunctionDef(OctalEscapeSequence, 3);
ParseFunctionDef(HexadecimalEscapeSequence, 2);
ParseFunctionDef(FloatingPointLiteral, 2);
ParseFunctionDef(DecimalFloatingPointLiteral, 2);
ParseFunctionDef(HexadecimalFloatingPointLiteral, 2);
ParseFunctionDef(FractionalConstant, 2);
ParseFunctionDef(HexadecimalFractionalConstant, 2);
ParseFunctionDef(ExponentPart, 2);
ParseFunctionDef(BinaryExponentPart, 2);
ParseFunctionDef(Sign, 2);
ParseFunctionDef(DigitSequence, 2);
ParseFunctionDef(FloatingPointSuffix, 4);
ParseFunctionDef(StringLiteral, 2);
ParseFunctionDef(SCharSequence, 2);
ParseFunctionDef(SChar, 2);
ParseFunctionDef(RawString, 1);
ParseFunctionDef(RCharSequence, 2);
ParseFunctionDef(RChar, 1);
ParseFunctionDef(DCharSequence, 2);
ParseFunctionDef(DChar, 1);
ParseFunctionDef(BooleanLiteral, 2);
ParseFunctionDef(PointerLiteral, 1);
ParseFunctionDef(UserDefinedLiteral, 4);
ParseFunctionDef(UserDefinedIntegerLiteral, 4);
ParseFunctionDef(UserDefinedFloatingPointLiteral, 4);
ParseFunctionDef(UserDefinedStringLiteral, 1);
ParseFunctionDef(UserDefinedCharacterLiteral, 1);
ParseFunctionDef(UdSuffix, 1);

#undef ParseFunctionDef

}  // namespace lps::parser::details
