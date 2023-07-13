#
# MIT License
# Copyright (c) 2023 mxlol233 (mxlol233@outlook.com)

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

sp_tokens = {
    "`this`":"kw_this",
    "`(`":"l_paren",
    "`)`":"r_paren",
    "`~`":"tilde",
    "`template`":"kw_template",
    "`::`":"coloncolon",
    "`<`":"less",
    "`>`":"greater",
    "`[`":"l_square",
    "`]`":"r_square",
    "`,`":"comma",
    "`&`":"amp",
    "`=`":"equal",
    "`...`":"ellipsis",
    "`*`":"star",
    "`+`":"plus",
    "`-`":"minus",
    "`/`":"slash",
    "`%`":"percent",
    "`^`":"caret",
    "`|`":"pipe",
    "`<<`":"lessless",
    "`>>`":"greatergreater",
    "`+=`":"plusequal",
    "`-=`":"minusequal",
    "`*=`":"starequal",
    "`/=`":"slashequal",
    "`%=`":"percentequal",
    "`^=`":"caretequal",
    "`&=`":"ampequal",
    "`|=`":"pipeequal",
    "`<<=`":"lesslessequal",
    "`>>=`":"greatergreaterequal",
    "`==`":"equalequal",
    "`!=`":"exclaimequal",
    "`<=`":"lessequal",
    "`>=`":"greaterequal",
    "`&&`":"ampamp",
    "`||`":"pipepipe",
    "`.*`":"periodstar",
    "`->*`":"arrowstar",
    "`requires`":"kw_requires",
    "`{`":"l_brace",
    "`}`":"r_brace",
    "`;`":"semi",
    "`typename`":"kw_typename",
    "`noexcept`":"kw_noexcept",
    "`->`":"arrow",
    "`.`":"period",
    "`++`":"plusplus",
    "`--`":"minusminus",
    "`dynamic_cast`":"kw_dynamic_cast",
    "`static_cast`":"kw_static_cast",
    "`reinterpret_cast`":"kw_reinterpret_cast",
    "`const_cast`":"kw_const_cast",
    "`typeid`":"kw_typeid",
    "`sizeof`":"kw_sizeof",
    "`identifier`":"identifier",
    "`alignof`":"kw_alignof",
    "`!`":"exclaim",
    "`co_await`":"kw_co_await",
    "`new`":"kw_new",
    "`delete`":"kw_delete",
    "`<=>`":"spaceship",
    "`?`":"question",
    "`:`":"colon",
    "`co_yield`":"kw_co_yield",
    "`throw`":"kw_throw",
    "`case`":"kw_case",
    "`default`":"kw_default",
    "`if`":"kw_if",
    "`constexpr`":"kw_constexpr",
    "`else`":"kw_else",
    "`switch`":"kw_switch",
    "`while`":"kw_while",
    "`##`":"hashhash",
    "`do`":"kw_do",
    "`for`":"kw_for",
    "`break`":"kw_break",
    "`continue`":"kw_continue",
    "`return`":"kw_return",
    "`goto`":"kw_goto",
    "`co_return`":"kw_co_return",
    "`using`":"kw_using",
    "`static_assert`":"kw_static_assert",
    "`friend`":"kw_friend",
    "`typedef`":"kw_typedef",
    "`consteval`":"kw_consteval",
    "`constinit`":"kw_constinit",
    "`inline`":"kw_inline",
    "`static`":"kw_static",
    "`thread_local`":"kw_thread_local",
    "`extern`":"kw_extern",
    "`mutable`":"kw_mutable",
    "`virtual`":"kw_virtual",
    "`explicit`":"kw_explicit",
    "`char`":"kw_char",
    "`char8_t`":"kw_char8_t",
    "`char16_t`":"kw_char16_t",
    "`char32_t`":"kw_char32_t",
    "`wchar_t`":"kw_wchar_t",
    "`bool`":"kw_bool",
    "`short`":"kw_short",
    "`int`":"kw_int",
    "`long`":"kw_long",
    "`signed`":"kw_signed",
    "`unsigned`":"kw_unsigned",
    "`float`":"kw_float",
    "`double`":"kw_double",
    "`void`":"kw_void",
    "`enum`":"kw_enum",
    "`decltype`":"kw_decltype",
    "`auto`":"kw_auto",
    "`const`":"kw_const",
    "`volatile`":"kw_volatile",
    "`class`":"kw_class",
    "`struct`":"kw_struct",
    "`namespace`":"kw_namespace",
    "`asm`":"kw_asm",
    "`alignas`":"kw_alignas",
    "`export`":"kw_export",
    "`module`":"kw_module",
    "`import`":"kw_import",
    "`private`":"kw_private",
    "`final`":"kw_final",
    "`union`":"kw_union",
    "`override`":"kw_override",
    "`operator`":"kw_operator",
    "`protected`":"kw_protected",
    "`public`":"kw_public",
    "`concept`":"kw_concept",
    "`try`":"kw_try",
    "`catch`":"kw_catch",
    "`#`":"hash",
    "`include`":"kw_include",
    "`define`":"kw_define",
    "`undef`":"kw_undef",
    "`line`":"kw_line",
    "`error`":"kw_error",
    "`pragma`":"kw_pragma",
    "`ifdef`":"kw_ifdef",
    "`ifndef`":"kw_ifndef",
    "`elif`":"kw_elif",
    "`endif`":"kw_endif",
    "`defined`":"kw_defined",
    "`__has_include`":"kw___has_include",
    "`__has_cpp_attribute`":"kw___has_cpp_attribute",
    "`__va_opt__`":"kw___va_opt__",
    "`and`":"kw_and",
    "`or`":"kw_or",
    "`xor`":"kw_xor",
    "`not`":"kw_not",
    "`bitand`":"kw_bitand",
    "`bitor`":"kw_bitor",
    "`compl`":"kw_compl",
    "`and_eq`":"kw_and_eq",
    "`or_eq`":"kw_or_eq",
    "`xor_eq`":"kw_xor_eq",
    "`not_eq`":"kw_not_eq",
    "`binary-digit`":"kw_binary-digit",
    "`false`":"kw_false",
    "`true`":"kw_true",
    "`nullptr`":"kw_nullptr",
    "`binary-literal`":"binary_literal",
    "`floating-point-literal`":"floating_point_literal",
    "`character-literal`":"char_literal",
    "`string-literal`":"string_literal",
    "`integer-literal`":"integer_literal",
    "`decimal-literal`":"decimal_literal",
    "`octal-literal`":"octal_literal",
    "`hexadecimal-literal`":"hexadecimal_literal",
    "`raw-string`":"raw_string",
    "`user-defined-character-literal`":"user_defined_char_literal",
    "`user-defined-string-literal`":"user_defined_string_literal",
    "`user-defined-floating-point-literal`":"user_defined_floating_point_literal",
    "`user-defined-integer-literal`":"user_defined_integer_literal",
}

gram_tree = {}

gram_tree["translation-unit"] = [
    ["declaration-seq[opt]"],
    ["global-module-fragment[opt]", "module-declaration", "declaration-seq[opt]", "private-module-fragment[opt]"],
]

gram_tree["primary-expression"] = [
    ["literal"],
    ["`this`"],
    ["`(`", "expression", "`)`"],
    ["id-expression"],
    ["lambda-expression"],
    ["fold-expression"],
    ["requires-expression"],
]

gram_tree["id-expression"] = [
    ["unqualified-id"],
    ["qualified-id"],
]

gram_tree["unqualified-id"] = [
    ["`identifier`"],
    ["operator-function-id"],
    ["conversion-function-id"],
    ["literal-operator-id"],
    ["`~`", "type-name"],
    ["`~`", "decltype-specifier"],
    ["`~`", "template-id"],
]

gram_tree["qualified-id"] = [
    "nested-name-specifier", "`template`[opt]", "unqualified-id"
]

gram_tree["nested-name-specifier"] = [
    ["`::`"],
    ["type-name", "`::`"],
    ["namespace-name", "`::`"],
    ["decltype-specifier", "`::`"],
    ["nested-name-specifier","`identifier`", "`::`"],
    ["nested-name-specifier","`template`[opt]", "simple-template-id", "`::`"],
]

gram_tree["lambda-expression"] = [
    ["lambda-introducer", "lambda-declarator[opt]", "compound-statement"],
    ["lambda-introducer", "`<`", "template-parameter-list", "`>`", "requires-clause[opt]", "lambda-declarator[opt]", "compound-statement"],
]

gram_tree["lambda-introducer"] = [
    "`[`", "lambda-capture[opt]", "`]`"
]

gram_tree["lambda-declarator"] = [
    ["`(`", "parameter-declaration-clause", "`)`", "decl-specifier-seq[opt]"],
    ["noexcept-specifier[opt]", "attribute-specifier-seq[opt]", "trailing-return-type[opt]", "requires-clause[opt]"]
]

gram_tree["lambda-capture"] = [
    ["capture-default"],
    ["capture-list"],
    ["capture-default", "`,`", "capture-list"],
]

gram_tree["capture-default"] = [
    ["`&`"],
    ["`=`"],
]

gram_tree["capture-list"] = [
    ["capture"],
    ["capture-list", "`,`", "capture"]
]

gram_tree["capture"] = [
    ["simple-capture"],
    ["init-capture"],
]

gram_tree["simple-capture"] = [
    ["`identifier`", "`...`[opt]"],
    ["`&`", "`identifier`", "`...`[opt]"],
    ["`this`"],
    ["`*`", "`this`"],
]

gram_tree["init-capture"] = [
    ["`...`[opt]", "`identifier`", "initializer"],
    ["`&`", "`...`[opt]", "`identifier`", "initializer"],
]

gram_tree["fold-expression"] = [
    ["`(`", "cast-expression", "fold-operator", "`...`", "`)`"],
    ["`(`", "`...`", "fold-operator", "cast-expression", "`)`"],
    ["`(`", "cast-expression", "fold-operator", "`...`", "fold-operator", "cast-expression", "`)`"]
]

gram_tree["fold-operator"] = [
    ["`+`"], ["`-`"], ["`*`"], ["`/`"], ["`%`"], ["`^`"], ["`&`"], ["`|`"], ["`<<`"], ["`>>`"],
    ["`+=`"], ["`-=`"], ["`*=`"], ["`/=`"], ["`%=`"], ["`^=`"], ["`&=`"], ["`|=`"], ["`<<=`"], ["`>>=`"], ["`=`"],
    ["`==`"], ["`!=`"], ["`<`"], ["`>`"], ["`<=`"], ["`>=`"], ["`&&`"], ["`||`"], ["`,`"], ["`.*`"], ["`->*`"]
]

gram_tree["requires-expression"] = [
    "`requires`", "requirement-parameter-list[opt]", "requirement-body",
]

gram_tree["requirement-parameter-list"] = [
    "`(`", "parameter-declaration-clause[opt]", "`)`",
]

gram_tree["requirement-body"] = [
    "`{`", "requirement-seq", "`}`",
]

gram_tree["requirement-seq"] = [
    ["requirement"],
    ["requirement-seq", "requirement"],
]

gram_tree["requirement"] = [
    ["simple-requirement"],
    ["type-requirement"],
    ["compound-requirement"],
    ["nested-requirement"],
]

gram_tree["simple-requirement"] = [
    "expression", "`;`"
]

gram_tree["type-requirement"] = [
    "`typename`", "nested-name-specifier[opt]", "type-name", "`;`"
]

gram_tree["compound-requirement"] = [
    "`{`", "expression", "`}`", "`noexcept`[opt]", "return-type-requirement[opt]", "`;`"
]

gram_tree["return-type-requirement"] = [
    "`->`", "type-constraint"
]

gram_tree["nested-requirement"] = [
    "`requires`", "constraint-expression", "`;`"
]

gram_tree["postfix-expression"] = [
    ["primary-expression"],
    ["postfix-expression", "`[`", "expr-or-braced-init-list", "`]`"],
    ["postfix-expression", "`(`", "expression-list[opt]", "`)`"],
    ["simple-type-specifier", "`(`", "expression-list[opt]", "`)`"],
    ["typename-specifier", "`(`", "expression-list[opt]", "`)`"],
    ["simple-type-specifier", "braced-init-list"],
    ["typename-specifier", "braced-init-list"],
    ["postfix-expression", "`.`", "`template`[opt]", "id-expression"],
    ["postfix-expression", "`->`", "`template`[opt]", "id-expression"],
    ["postfix-expression","`++`"],
    ["postfix-expression","`--`"],
    ["`dynamic_cast`", "`<`", "type-id", "`>`", "`(`", "expression", "`)`"],
    ["`static_cast`", "`<`", "type-id", "`>`", "`(`", "expression", "`)`"],
    ["`reinterpret_cast`", "`<`", "type-id", "`>`", "`(`", "expression", "`)`"],
    ["`const_cast`", "`<`", "type-id", "`>`", "`(`", "expression", "`)`"],
    ["`typeid`",  "`(`", "expression", "`)`"],
    ["`typeid`",  "`(`", "type-id", "`)`"],
]

gram_tree["expression-list"] = [
    "initializer-list"
]

gram_tree["unary-expression"] = [
    ["postfix-expression"],
    ["unary-operator", "cast-expression"],
    ["`++`", "cast-expression"],
    ["`--`", "cast-expression"],
    ["await-expression"],
    ["`sizeof`", "unary-expression"],
    ["`sizeof`", "`(`", "type-id", "`)`"],
    ["`sizeof`", "`...`", "`(`", "`identifier`", "`)`"],
    ["`alignof`", "`(`", "type-id", "`)`"],
    ["noexcept-expression"],
    ["new-expression"],
    ["delete-expression"],
]

gram_tree["unary-operator"] = [
    ["`*`"], ["`&`"], ["`+`"], ["`-`"], ["`!`"], ["`~`"]
]

gram_tree["await-expression"] = [
    "`co_await`", "cast-expression"
]

gram_tree["noexcept-expression"] = [
    "`noexcept`", "`(`", "expression", "`)`"
]

gram_tree["new-expression"] = [
    ["`::`[opt]", "`new`", "new-placement[opt]", "new-type-id", "new-initializer[opt]"],
    ["`::`[opt]", "`new`", "new-placement[opt]", "`(`", "type-id", "`)`", "new-initializer[opt]"],
]

gram_tree["new-placement"] = [
    "`(`", "expression-list", "`)`"
]

gram_tree["new-type-id"] = [
    "type-specifier-seq", "new-declarator[opt]"
]

gram_tree['new-declarator'] = [
    ["ptr-operator", "new-declarator[opt]"],
    ["noptr-new-declarator"]
]

gram_tree["noptr-new-declarator"] = [
    ["`[`", "expression[opt]", "`]`", "attribute-specifier-seq[opt]"],
    ["noptr-new-declarator", "`[`", "constant-expression", "`]`", "attribute-specifier-seq[opt]"]
]

gram_tree["new-initializer"] = [
    ["`(`", "expression-list[opt]", "`)`"],
    ["braced-init-list"],
]

gram_tree["delete-expression"] = [
    ["`::`[opt]", "`delete`", "cast-expression"],
    ["`::`[opt]", "`delete`", "`[`", "`]`",  "cast-expression"],
]

gram_tree["cast-expression"] = [
    ["unary-expression"],
    ["`(`", "type-id", "`)`", "cast-expression"]
]

gram_tree['pm-expression'] = [
    ["cast-expression"],
    ["pm-expression", "`.*`", "cast-expression"],
    ["pm-expression", "`->*`", "cast-expression"],
]

gram_tree["multiplicative-expression"] = [
    ["pm-expression"],
    ["multiplicative-expression", "`*`", "pm-expression"],
    ["multiplicative-expression", "`/`", "pm-expression"],
    ["multiplicative-expression", "`%`", "pm-expression"],
]

gram_tree["additive-expression"] = [
    ["multiplicative-expression"],
    ["additive-expression", "`+`", "multiplicative-expression"],
    ["additive-expression", "`-`", "multiplicative-expression"]
]

gram_tree["shift-expression"] = [
    ["additive-expression"],
    ["shift-expression", "`<<`", "additive-expression"],
    ["shift-expression", "`>>`", "additive-expression"],
]

gram_tree["compare-expression"] = [
    ["shift-expression"],
    ["compare-expression", "`<=>`", "shift-expression"],
]

gram_tree["relational-expression"] = [
    ["compare-expression"],
    ["relational-expression", "`<`", "compare-expression"],
    ["relational-expression", "`>`",  "compare-expression"],
    ["relational-expression", "`<=`",  "compare-expression"],
    ["relational-expression", "`>=`",  "compare-expression"],
]

gram_tree["equality-expression"] = [
    ["relational-expression"],
    ["equality-expression", "`==`", "relational-expression"],
    ["equality-expression", "`!=`", "relational-expression"]
]

gram_tree["and-expression"] = [
    ["equality-expression"],
    ["and-expression", "`&`", "equality-expression"]
]

gram_tree["exclusive-or-expression"] = [
    ["and-expression"],
    ["exclusive-or-expression", "`^`", "and-expression"],
]

gram_tree["inclusive-or-expression"] = [
    ["exclusive-or-expression"],
    ["inclusive-or-expression", "`|`", "exclusive-or-expression"],
]

gram_tree["logical-and-expression"] = [
    ["inclusive-or-expression"],
    ["logical-and-expression", "`&&`", "exclusive-or-expression"],
]

gram_tree["logical-or-expression"] = [
    ["logical-and-expression"],
    ["logical-or-expression", "`||`", "logical-and-expression"],
]

gram_tree["conditional-expression"] = [
    ["logical-or-expression"],
    ["logical-or-expression", "`?`", "expression", "`:`", "assignment-expression"]
]

gram_tree["yield-expression"] = [
    ["`co_yield`", "assignment-expression"],
    ["`co_yield`", "braced-init-list"],
]

gram_tree["throw-expression"] = [
    ["`throw`", "assignment-expression[opt]"],
]

gram_tree["assignment-expression"] = [
    ["conditional-expression"],
    ["yield-expression"],
    ["throw-expression"],
    ["logical-or-expression", "assignment-operator", "initializer-clause"]
]

gram_tree["assignment-operator"] = [
    ["`=`"], ["`*=`"], ["`/=`"], ["`%=`"], ["`+=`"], ["`-=`"], ["`>>=`"], ["`<<=`"], ["`&=`"], ["`^=`"], ["`|=`"]
]

gram_tree["expression"] = [
    ["assignment-expression"],
    ["expression", "`,`", "assignment-expression"],
]

gram_tree["constant-expression"] = [
    "conditional-expression"
]

gram_tree["statement"] = [
    ["labeled-statement"],
    ["attribute-specifier-seq[opt]", "expression-statement"],
    ["attribute-specifier-seq[opt]", "compound-statement"],
    ["attribute-specifier-seq[opt]", "selection-statement"],
    ["attribute-specifier-seq[opt]", "iteration-statement"],
    ["attribute-specifier-seq[opt]", "jump-statement"],
    ["declaration-statement"],
    ["attribute-specifier-seq[opt]", "try-block"],
]

gram_tree["init-statement"] = [
    ["expression-statement"],
    ["simple-declaration"]
]

gram_tree["condition"] = [
    ["expression"],
    ["attribute-specifier-seq[opt]", "decl-specifier-seq", "declarator", "brace-or-equal-initializer"],
]

gram_tree["labeled-statement"] = [
    ["attribute-specifier-seq[opt]", "`identifier`", "`:`", "statement"],
    ["attribute-specifier-seq[opt]", "`case`","constant-expression", "`:`", "statement"],
    ["attribute-specifier-seq[opt]", "`default`", "`:`", "statement"],
]

gram_tree["expression-statement"] = [
    "expression[opt]", "`;`"
]

gram_tree["compound-statement"] = [
    "`{`", "statement-seq[opt]", "`}`"
]

gram_tree["statement-seq"] = [
    ["statement"],
    ["statement-seq", "statement"],
]

gram_tree["selection-statement"] = [
    ["`if`", "`constexpr`[opt]", "`(`", "init-statement[opt]","condition",  "`)`", "statement"],
    ["`if`", "`constexpr`[opt]", "`(`", "init-statement[opt]","condition",  "`)`", "statement", "`else`", "statement"],
    ["`switch`", "`(`", "init-statement[opt]","condition",  "`)`", "statement"],
]

gram_tree["iteration-statement"] = [
    ["`while`", "`(`", "condition", "`)`", "statement"],
    ["`do`", "statement", "`while`", "`(`", "expression", "`)`", "`;`"],
    ["`for`", "`(`", "init-statement", "condition[opt]","`;`", "expression[opt]","`)`", "statement"],
    ["`for`", "`(`", "init-statement[opt]", "for-range-declaration", "`:`", "for-range-initializer", "`)`", "statement"],
]

gram_tree["for-range-declaration"] = [
    ["attribute-specifier-seq[opt]", "decl-specifier-seq", "declarator"],
    ["attribute-specifier-seq[opt]", "decl-specifier-seq", "ref-qualifier[opt]", "`[`", "identifier-list", "`]`" ],
]

gram_tree["for-range-initializer"] = [
    "expr-or-braced-init-list"
]

gram_tree["jump-statement"] = [
    ["`break`", "`;`"],
    ["`continue`", "`;`"],
    ["`return`", "expr-or-braced-init-list[opt]", "`;`"],
    ["coroutine-return-statement"],
    ["`goto`", "`identifier`", "`;`"]
]

gram_tree["coroutine-return-statement"] = [
    "`co_return`", "expr-or-braced-init-list[opt]", "`;`"
]

gram_tree["declaration-statement"] = [
    "block-declaration"
]

gram_tree["declaration-seq"] = [
    ["declaration"],
    ["declaration-seq", "declaration"],
]

gram_tree["declaration"] = [
    ["block-declaration"],
    ["nodeclspec-function-declaration"],
    ["function-definition"],
    ["template-declaration"],
    ["deduction-guide"],
    ["explicit-instantiation"],
    ["explicit-specialization"],
    ["export-declaration"],
    ["linkage-specification"],
    ["namespace-definition"],
    ["empty-declaration"],
    ["attribute-declaration"],
    ["module-import-declaration"],
]

gram_tree["block-declaration"] = [
    ["simple-declaration"],
    ["asm-declaration"],
    ["namespace-alias-definition"],
    ["using-declaration"],
    ["using-enum-declaration"],
    ["using-directive"],
    ["static_assert-declaration"],
    ["alias-declaration"],
    ["opaque-enum-declaration"],
]

gram_tree["nodeclspec-function-declaration"] = [
    "attribute-specifier-seq[opt]", "declarator", "`;`"
]

gram_tree["alias-declaration"] = [
"`using`", "`identifier`",  "attribute-specifier-seq[opt]", "`=`", "defining-type-id", "`;`"
]

gram_tree["simple-declaration"] = [
    ["decl-specifier-seq", "init-declarator-list[opt]", "`;`"],
    ["attribute-specifier-seq", "decl-specifier-seq", "init-declarator-list", "`;`"],
    ["attribute-specifier-seq[opt]", "decl-specifier-seq", "ref-qualifier[opt]", "`[`", "identifier-list", "`]`", "initializer", "`;`"]
]

gram_tree["static_assert-declaration"] = [
    ["`static_assert`", "`(`", "constant-expression", "`)`", "`;`"],
    ["`static_assert`", "`(`", "constant-expression", "`,`", "`string-literal`", "`)`", "`;`"]
]

gram_tree["empty-declaration"] = [
    "`;`"
]

gram_tree["attribute-declaration"] = [
    "attribute-specifier-seq", "`;`"
]

gram_tree["decl-specifier"] = [
    ["storage-class-specifier"],
    ["defining-type-specifier"],
    ["function-specifier"],
    ["`friend`"],
    ["`typedef`"],
    ["`constexpr`"],
    ["`consteval`"],
    ["`constinit`"],
    ["`inline`"],
]

gram_tree["decl-specifier-seq"] = [
    ["decl-specifier", "attribute-specifier-seq[opt]"],
    ["decl-specifier", "decl-specifier-seq"],
]

gram_tree["storage-class-specifier"] = [
    ["`static`"],
    ["`thread_local`"],
    ["`extern`"],
    ["`mutable`"],
]

gram_tree["function-specifier"] = [
    ["`virtual`"],
    ["explicit-specifier"]
]

gram_tree["explicit-specifier"] = [
    ["`explicit`", "`(`", "constant-expression", "`)`"],
    ["`explicit`"],
]

gram_tree["typedef-name"] = [
    ["`identifier`"],
    ["simple-template-id"],
]

gram_tree["type-specifier"] = [
    ["simple-type-specifier"],
    ["elaborated-type-specifier"],
    ["typename-specifier"],
    ["cv-qualifier"],
]

gram_tree["type-specifier-seq"] = [
    ["type-specifier", "attribute-specifier-seq[opt]"],
    ["type-specifier", "type-specifier-seq"],
]

gram_tree["defining-type-specifier"] = [
    ["type-specifier"],
    ["class-specifier"],
    ["enum-specifier"],
]

gram_tree["defining-type-specifier-seq"] = [
    ["defining-type-specifier", "attribute-specifier-seq[opt]"],
    ["defining-type-specifier", "defining-type-specifier-seq"],
]

gram_tree["simple-type-specifier"] = [
    ["nested-name-specifier[opt]", "type-name"],
    ["nested-name-specifier", "`template`", "simple-template-id"],
    ["decltype-specifier"],
    ["placeholder-type-specifier"],
    ["nested-name-specifier[opt]", "template-name"],
    ["`char`"],
    ["`char8_t`"],
    ["`char16_t`"],
    ["`char32_t`"],
    ["`wchar_t`"],
    ["`bool`"],
    ["`short`"],
    ["`int`"],
    ["`long`"],
    ["`signed`"],
    ["`unsigned`"],
    ["`float`"],
    ["`double`"],
    ["`void`"],
]

gram_tree["type-name"] = [
    ["class-name"],
    ["enum-name"],
    ["typedef-name"],
]

gram_tree["elaborated-type-specifier"] = [
    ["class-key", "attribute-specifier-seq[opt]", "nested-name-specifier[opt]", "`identifier`"],
    ["class-key", "simple-template-id"],
    ["class-key", "nested-name-specifier", "`template`[opt]", "simple-template-id"],
    ["elaborated-enum-specifier"],
]

gram_tree["elaborated-enum-specifier"] = [
    "`enum`", "nested-name-specifier[opt]", "`identifier`"
]

gram_tree["decltype-specifier"] = [
"`decltype`", "`(`", "expression", "`)`"
]

gram_tree["placeholder-type-specifier"] = [
    ["type-constraint[opt]", "`auto`"],
    ["type-constraint[opt]", "`decltype`", "`(`", "`auto`", "`)`"],
]

gram_tree["init-declarator-list"] = [
    ["init-declarator"],
    ["init-declarator-list", "`,`", "init-declarator"],
]

gram_tree["init-declarator"] = [
    ["declarator", "initializer[opt]"],
    ["declarator", "requires-clause"],
]

gram_tree["declarator"] = [
    ["ptr-declarator"],
    ["noptr-declarator", "parameters-and-qualifiers", "trailing-return-type"],
]

gram_tree["ptr-declarator"] = [
    ["noptr-declarator"],
    ["ptr-operator", "ptr-declarator"],
]

gram_tree["noptr-declarator"] = [
    ["declarator-id", "attribute-specifier-seq"],
    ["noptr-declarator", "parameters-and-qualifiers"],
    ["noptr-declarator", "`[`", "constant-expression[opt]", "`]`", "attribute-specifier-seq[opt]"],
    ["`(`", "ptr-declarator", "`)`"],
]

gram_tree["parameters-and-qualifiers"] = [
    ["`(`", "parameter-declaration-clause", "`)`", "cv-qualifier-seq[opt]"],
    ["ref-qualifier[opt]", "noexcept-specifier[opt]", "attribute-specifier-seq[opt]"],
]

gram_tree["trailing-return-type"] = [
    "`->`", "type-id",
]

gram_tree["ptr-operator"] = [
    ["`*`", "attribute-specifier-seq[opt]", "cv-qualifier-seq[opt]"],
    ["`&`", "attribute-specifier-seq[opt]"],
    ["`&&`", "attribute-specifier-seq[opt]"],
    ["nested-name-specifier", "`*`", "attribute-specifier-seq[opt]", "cv-qualifier-seq[opt]"],
]

gram_tree["cv-qualifier-seq"] = [
   "cv-qualifier", "cv-qualifier-seq[opt]",
]

gram_tree["cv-qualifier"] = [
    ["`const`"],
    ["`volatile`"],
]

gram_tree["ref-qualifier"] = [
    ["`&`"],
    ["`&&`"],
]

gram_tree["declarator-id"] = [
    "`...`[opt]", "id-expression",
]

gram_tree["type-id"] = [
    "type-specifier-seq", "abstract-declarator[opt]",
]

gram_tree["defining-type-id"] = [
    "defining-type-specifier-seq", "abstract-declarator[opt]"
]

gram_tree["abstract-declarator"] = [
    ["ptr-abstract-declarator"],
    ["noptr-abstract-declarator", "parameters-and-qualifiers", "trailing-return-type"],
    ["abstract-pack-declarator"],
]

gram_tree["ptr-abstract-declarator"] = [
    ["noptr-abstract-declarator"],
    ["ptr-operator", "ptr-abstract-declarator"]
]

gram_tree["noptr-abstract-declarator"] = [
    ["noptr-abstract-declarator[opt]", "parameters-and-qualifiers"],
    ["noptr-abstract-declarator[opt]", "`[`", "constant-expression[opt]"  , "`]`", "attribute-specifier-seq[opt]"],
    ["`(`", "ptr-abstract-declarator", "`)`"]
]

gram_tree["abstract-pack-declarator"] = [
    ["noptr-abstract-pack-declarator"],
    ["ptr-operator", "abstract-pack-declarator"],
]

gram_tree["noptr-abstract-pack-declarator"] = [
    ["noptr-abstract-pack-declarator", "parameters-and-qualifiers"],
    ["noptr-abstract-pack-declarator", "`[`", "constant-expression[opt]", "`]`", "attribute-specifier-seq[opt]"],
    ["`...`"],
]

gram_tree["parameter-declaration-clause"] = [
    ["parameter-declaration-list[opt]", "`...`[opt]"],
    ["parameter-declaration-list", "`,`", "`...`"],
]

gram_tree["parameter-declaration-list"] = [
    ["parameter-declaration"],
    ["parameter-declaration-list", "`,`", "parameter-declaration"],
]

gram_tree["parameter-declaration"] = [
    ["attribute-specifier-seq[opt]", "decl-specifier-seq", "declarator"],
    ["attribute-specifier-seq[opt]", "decl-specifier-seq", "declarator", "`=`", "initializer-clause"],
    ["attribute-specifier-seq[opt]", "decl-specifier-seq", "abstract-declarator[opt]"],
    ["attribute-specifier-seq[opt]", "decl-specifier-seq", "abstract-declarator[opt]", "`=`", "initializer-clause"],
]

gram_tree["initializer"] = [
    ["brace-or-equal-initializer"],
    ["`(`", "expression-list", "`)`"],
]

gram_tree["brace-or-equal-initializer"] = [
    ["`=`", "initializer-clause"],
    ["braced-init-list"],
]

gram_tree["initializer-clause"] = [
    ["assignment-expression"],
    ["braced-init-list"],
]

gram_tree["braced-init-list"] = [
    ["`{`", "initializer-list", "`,`[opt]", "`}`"],
    ["`{`", "designated-initializer-list", "`,`[opt]", "`}`"],
    ["`{`", "`}`"],
]

gram_tree["initializer-list"] = [
    ["initializer-clause", "`...`[opt]"],
    ["initializer-list", "`,`", "initializer-clause", "`...`[opt]"]
]

gram_tree["designated-initializer-list"] = [
    ["designated-initializer-clause"],
    ["designated-initializer-list", "`,`", "designated-initializer-clause"]
]

gram_tree["designated-initializer-clause"] = [
    "designator", "brace-or-equal-initializer"
]

gram_tree["designator"] = [
    "`.`", "`identifier`"
]

gram_tree["expr-or-braced-init-list"] = [
    ["expression"],
    ["braced-init-list"],
]

gram_tree["function-definition"] = [
    ["attribute-specifier-seq[opt]","decl-specifier-seq[opt]", "declarator", "virt-specifier-seq[opt]", "function-body"],
    ["attribute-specifier-seq[opt]","decl-specifier-seq[opt]", "declarator", "requires-clause", "function-body"],
]

gram_tree["function-body"] = [
    ["ctor-initializer[opt]", "compound-statement"],
    ["function-try-block"],
    ["`=`", "`default`", "`;`"],
    ["`=`", "`delete`", "`;`"],
]

gram_tree["enum-name"] = [
    "`identifier`"
]

gram_tree["enum-specifier"] = [
    ["enum-head", "`{`", "enumerator-list[opt]", "`}`"],
    ["enum-head", "`{`", "enumerator-list", "`,`", "`}`"],
]

gram_tree["enum-head"] = [
    "enum-key", "attribute-specifier-seq[opt]", "enum-head-name[opt]", "enum-base[opt]",
]

gram_tree["enum-head-name"] = [
    "nested-name-specifier[opt]", "`identifier`"
]

gram_tree["opaque-enum-declaration"] = [
    "enum-key", "attribute-specifier-seq[opt]", "enum-head-name", "enum-base[opt]", "`;`",
]

gram_tree["enum-key"] = [
    ["`enum`"],
    ["`enum`", "`class`"],
    ["`enum`", "`struct`"],
]

gram_tree["enum-base"] = [
    ["`:`", "type-specifier-seq"]
]

gram_tree["enumerator-list"] = [
    ["enumerator-definition"],
    ["enumerator-list", "`,`", "enumerator-definition"],
]

gram_tree["enumerator-definition"] = [
    ["enumerator"],
    ["enumerator", "`=`", "constant-expression"],
]

gram_tree["enumerator"] = [
    "`identifier`", "attribute-specifier-seq[opt]",
]

gram_tree["using-enum-declaration"] = [
    "`using`", "elaborated-enum-specifier", "`;`",
]

gram_tree["namespace-name"] = [
    ["`identifier`"],
    ["namespace-alias"],
]

gram_tree["namespace-definition"] = [
    ["named-namespace-definition"],
    ["unnamed-namespace-definition"],
    ["nested-namespace-definition"],
]

gram_tree["named-namespace-definition"] = [
    "`inline`[opt]", "`namespace`", "attribute-specifier-seq[opt]", "`identifier`", "`{`", "namespace-body", "`}`"
]

gram_tree["unnamed-namespace-definition"] = [
    "`inline`[opt]", "`namespace`", "attribute-specifier-seq[opt]",  "`{`", "namespace-body", "`}`"
]

gram_tree["nested-namespace-definition"] = [
    "`namespace`", "enclosing-namespace-specifier", "`::`", "`inline`[opt]", "`identifier`", "`{`", "namespace-body", "`}`",
]

gram_tree["enclosing-namespace-specifier"] = [
    ["`identifier`"],
    ["enclosing-namespace-specifier", "`::`", "`inline`[opt]", "`identifier`"],
]

gram_tree["namespace-body"] = [
    "declaration-seq[opt]",
]

gram_tree["namespace-alias"] = [
    "`identifier`"
]

gram_tree["namespace-alias-definition"] = [
    "`namespace`", "`identifier`", "`=`", "qualified-namespace-specifier", "`;`",
]

gram_tree["qualified-namespace-specifier"] = [
    "nested-name-specifier[opt]", "namespace-name",
]

gram_tree["using-directive"] = [
    "attribute-specifier-seq[opt]", "`using`",  "`namespace`",  "nested-name-specifier[opt]", "namespace-name", "`;`"
]

gram_tree["using-declaration"] = [
    "`using`", "using-declarator-list", "`;`",
]

gram_tree["using-declarator-list"] = [
    ["using-declarator", "`...`[opt]"],
    ["using-declarator-list", "`,`", "using-declarator", "`...`[opt]"],
]

gram_tree["using-declarator"] = [
    "`typename`[opt]", "nested-name-specifier", "unqualified-id",
]

gram_tree["asm-declaration"] = [
    "attribute-specifier-seq[opt]", "`asm`", "`(`", "`string-literal`", "`)`", "`;`",
]

gram_tree["linkage-specification"] = [
    ["`extern`", "`string-literal`", "`{`", "declaration-seq[opt]", "`}`"],
    ["`extern`", "`string-literal`", "declaration"],
]

gram_tree["attribute-specifier-seq"] = [
    "attribute-specifier", "attribute-specifier-seq[opt]",
]

gram_tree["attribute-specifier"] = [
    ["`[`", "`[`", "attribute-using-prefix[opt]", "attribute-list", "`]`", "`]`"],
    ["alignment-specifier"],
]

gram_tree["alignment-specifier"] = [
    ["`alignas`", "`(`", "type-id", "`...`[opt]", "`)`"],
    ["`alignas`", "`(`", "constant-expression", "`...`[opt]", "`)`"],
]

gram_tree["attribute-using-prefix"] = [
    "`using`", "attribute-namespace", "`:`",
]

gram_tree["attribute-list"] = [
    ["attribute[opt]"],
    ["attribute-list", "`,`", "attribute[opt]"],
    ["attribute", "`...`"],
    ["attribute-list", "`,`", "attribute", "`...`"],
]

gram_tree["attribute"] = [
    "attribute-token", "attribute-argument-clause[opt]",
]

gram_tree["attribute-token"] = [
    ["`identifier`"],
    ["attribute-scoped-token"],
]

gram_tree["attribute-scoped-token"] = [
    "attribute-namespace", "`::`", "`identifier`",
]

gram_tree["attribute-namespace"] = [
    "`identifier`",
]


gram_tree["attribute-argument-clause"] = [
    "`(`", "balanced-token-seq[opt]", "`)`",
]

gram_tree["balanced-token-seq"] = [
    ["balanced-token"],
    ["balanced-token-seq", "balanced-token"],
]

gram_tree["balanced-token"] = [
    ["`(`", "balanced-token-seq[opt]", "`)`"],
    ["`[`", "balanced-token-seq[opt]", "`]`"],
    ["`{`", "balanced-token-seq[opt]", "`}`"],
    ["`identifier`"],
    ["keyword"],
    ["literal"],
    ["operator-or-punctuator"], #but not `(`, `)`,  `<`, `>`, `[`, `]`.
]

gram_tree["module-declaration"] = [
    "`export`[opt]", "`module`", "module-name", "module-partition[opt]", "attribute-specifier-seq[opt]", "`;`",
]

gram_tree["module-name"] = [
    "module-name-qualifier[opt]", "`identifier`",
]

gram_tree["module-partition"] = [
   "`:`", "module-name-qualifier[opt]", "`identifier`",
]

gram_tree["module-name-qualifier"] = [
    ["`identifier`", "`.`"],
    ["module-name-qualifier", "`identifier`", "`.`"],
]

gram_tree["export-declaration"] = [
    ["`export`", "declaration"],
    ["`export`",  "`{`",  "declaration-seq[opt]", "`}`"],
    ["`export`",  "module-import-declaration"],
]

gram_tree["module-import-declaration"] = [
    ["`import`", "module-name", "attribute-specifier-seq[opt]"],
    ["`import`", "module-partition", "attribute-specifier-seq[opt]"],
    ["`import`", "header-name", "attribute-specifier-seq[opt]"],
]

gram_tree["global-module-fragment"] = [
    "`module`", "`;`", "declaration-seq[opt]",
]

gram_tree["private-module-fragment"] = [
    "`module`", "`:`", "`private`", "`;`", "declaration-seq[opt]",
]

gram_tree["class-name"] = [
    ["`identifier`"],
    ["simple-template-id"],
]

gram_tree["class-specifier"] = [
    "class-head", "`{`", "member-specification[opt]", "`}`",
]

gram_tree["class-head"] = [
    ["class-key", "attribute-specifier-seq[opt]", "class-head-name", "class-virt-specifier[opt]", "base-clause[opt]"],
    ["class-key", "attribute-specifier-seq[opt]", "base-clause[opt]"],
]

gram_tree["class-head-name"] = [
    "nested-name-specifier[opt]", "class-name",
]

gram_tree["class-virt-specifier"] = [
    "`final`",
]

gram_tree["class-key"] = [
    ["`class`"],
    ["`struct`"],
    ["`union`"],
]

gram_tree["member-specification"] = [
    ["member-declaration", "member-specification[opt]"],
    ["access-specifier", "`:`", "member-specification[opt]"],
]

gram_tree["member-declaration"] = [
    ["attribute-specifier-seq[opt]", "decl-specifier-seq[opt]", "member-declarator-list[opt]", "`;`"],
    ["function-definition"],
    ["using-declaration"],
    ["using-enum-declaration"],
    ["static_assert-declaration"],
    ["template-declaration"],
    ["explicit-specialization"],
    ["deduction-guide"],
    ["alias-declaration"],
    ["opaque-enum-declaration"],
    ["empty-declaration"],
]

gram_tree["member-declarator-list"] = [
    ["member-declarator"],
    ["member-declarator-list", "`,`", "member-declarator"],
]

gram_tree["member-declarator"] = [
    ["declarator", "virt-specifier-seq[opt]", "pure-specifier[opt]"],
    ["declarator", "requires-clause"],
    ["declarator", "brace-or-equal-initializer[opt]"],
    ["`identifier`[opt]","attribute-specifier-seq[opt]", "`:`", "constant-expression", "brace-or-equal-initializer[opt]"],
]

gram_tree["virt-specifier-seq"] = [
    ["virt-specifier"],
    ["virt-specifier-seq", "virt-specifier"],
]

gram_tree["virt-specifier"] = [
    ["`override`"],
    ["`final`"],
]

gram_tree["pure-specifier"] = [
    "`=`",  "`0`",
]

gram_tree["conversion-function-id"] = [
    "`operator`", "conversion-type-id",
]

gram_tree["conversion-type-id"] = [
    "type-specifier-seq", "conversion-declarator[opt]",
]

gram_tree["conversion-declarator"] = [
    "ptr-operator", "conversion-declarator[opt]",
]

gram_tree["base-clause"] = [
    "`:`", "base-specifier-list",
]

gram_tree["base-specifier-list"] = [
    ["base-specifier", "`...`[opt]"],
    ["base-specifier-list", "`,`", "base-specifier", "`...`[opt]"],
]

gram_tree["base-specifier"] = [
    ["attribute-specifier-seq[opt]", "class-or-decltype"],
    ["attribute-specifier-seq[opt]", "`virtual`", "access-specifier[opt]", "class-or-decltype"],
    ["attribute-specifier-seq[opt]",  "access-specifier", "`virtual`[opt]", "class-or-decltype"],
]

gram_tree["class-or-decltype"] = [
    ["nested-name-specifier[opt]", "type-name"],
    ["nested-name-specifier", "`template`", "simple-template-id"],
    ["decltype-specifier"],
]

gram_tree["access-specifier"] = [
    ["`private`"],
    ["`protected`"],
    ["`public`"],
]

gram_tree["ctor-initializer"] = [
    "`:`", "mem-initializer-list",
]

gram_tree["mem-initializer-list"] = [
    ["mem-initializer", "`...`[opt]"],
    ["mem-initializer-list", "mem-initializer", "`...`[opt]"],
]

gram_tree["mem-initializer"] = [
    ["mem-initializer-id", "`(`", "expression-list[opt]"  ,"`)`"],
    ["mem-initializer-id", "braced-init-list"],
]

gram_tree["mem-initializer-id"] = [
    ["class-or-decltype"],
    ["`identifier`"],
]

gram_tree["operator-function-id"] = [
    "`operator`", "the_operator",
]

gram_tree["the_operator"] = [
    ["`new`"], ["`delete`"], ["`new[]`"], ["`delete[]`", "`co_await`"],
    ["`()`"],
    ["`[]`"],
    ["`->`"],["`->*`"],
    ["`~`"],["`!`"],["`+`"],["`-`"],["`*`"],["`/`"],["`%`"],["`^`"],["`&`"],
    ["`|`"],["`=`"],["`+=`"],["`-=`"],["`*=`"],["`/=`"],["`%=`"],["`^=`"],["`&=`"],
    ["`|=`"],["`==`"],["`!=`"],["`<`"],["`>`"],["`<=`"],["`>=`"],["`<=>`"],["`&&`"],
    ["`||`"],["`<<`"],["`>>`"],["`<<=`"],["`>>=`"],["`++`"],["`--`"],["`,`"],
]

gram_tree["literal-operator-id"] = [
    ["`operator`", "`string-literal`", "`identifier`"],
    ["`operator`", "`user-defined-string-literal`"],
]

gram_tree["template-declaration"] = [
    ["template-head", "declaration"],
    ["template-head", "concept-definition"],
]

gram_tree["template-head"] = [
    "`template`", "`<`", "template-parameter-list", "`>`", "requires-clause",
]

gram_tree["template-parameter-list"] = [
    ["template-parameter"],
    ["template-parameter-list", "`,`", "template-parameter"],
]

gram_tree["requires-clause"] = [
    "`requires`", "constraint-logical-or-expression",
]

gram_tree["constraint-logical-or-expression"] = [
    ["constraint-logical-and-expression"],
    ["constraint-logical-or-expression", "`||`", "constraint-logical-and-expression"],
]

gram_tree["constraint-logical-and-expression"] = [
    ["primary-expression"],
    ["constraint-logical-and-expression", "`&&`", "primary-expression"],
]

gram_tree["template-parameter"] = [
    ["type-parameter"],
    ["parameter-declaration"],
]

gram_tree["type-parameter"] = [
    ["type-parameter-key", "`...`[opt]", "`identifier`[opt]"],
    ["type-parameter-key", "`identifier`[opt]", "`=`", "type-id"],
    ["type-constraint",  "`...`[opt]", "`identifier`[opt]"],
    ["type-constraint", "`identifier`[opt]", "`=`", "type-id"],
    ["template-head", "type-parameter-key", "`...`[opt]", "`identifier`[opt]"],
    ["template-head", "type-parameter-key", "`identifier`[opt]", "`=`", "id-expression"],
]

gram_tree["type-parameter-key"] = [
    ["`class`"],
    ["`typename`"],
]

gram_tree["type-constraint"] = [
    ["nested-name-specifier[opt]", "concept-name"],
    ["nested-name-specifier[opt]", "concept-name", "`<`", "template-argument-list[opt]", "`>`"],
]

gram_tree["simple-template-id"] = [
    "template-name", "`<`", "template-argument-list[opt]", "`>`"
]

gram_tree["template-id"] = [
    ["simple-template-id"],
    ["operator-function-id", "`<`", "template-argument-list[opt]", "`>`"],
    ["literal-operator-id", "`<`", "template-argument-list[opt]", "`>`"],
]

gram_tree["template-name"] = [
    "`identifier`",
]

gram_tree["template-argument-list"] = [
    ["template-argument", "`...`[opt]"],
    ["template-argument-list" , "`,`",  "template-argument", "`...`[opt]"],
]

gram_tree["template-argument"] = [
    ["constant-expression"],
    ["type-id"],
    ["id-expression"],
]

gram_tree["constraint-expression"] = [
    "logical-or-expression",
]

gram_tree["deduction-guide"] = [
    "explicit-specifier[opt]", "template-name", "`(`", "parameter-declaration-clause", "`)`", "`->`", "simple-template-id", "`;`",
]

gram_tree["concept-definition"] = [
    "`concept`", "concept-name", "`=`", "constraint-expression", "`;`",
]

gram_tree["concept-name"] = [
    "`identifier`",
]

gram_tree["typename-specifier"] = [
    ["`typename`", "nested-name-specifier", "`identifier`"],
    ["`typename`", "nested-name-specifier", "`template`[opt]", "simple-template-id"],
]

gram_tree["explicit-instantiation"] = [
    "`extern`[opt]", "`template`", "declaration",
]

gram_tree["explicit-specialization"] = [
    "`template`", "`<`", "`>`", "declaration",
]

gram_tree["try-block"] = [
    "`try`", "compound-statement", "handler-seq",
]

gram_tree["function-try-block"] = [
    "`try`", "ctor-initializer[opt]", "compound-statement", "handler-seq",
]

gram_tree["handler-seq"] = [
    "handler", "handler-seq[opt]",
]

gram_tree["handler"] = [
    "`catch`", "`(`", "exception-declaration", "`)`", "compound-statement",
]

gram_tree["exception-declaration"] = [
    ["attribute-specifier-seq[opt]", "type-specifier-seq", "declarator"],
    ["attribute-specifier-seq[opt]", "type-specifier-seq", "abstract-declarator[opt]"],
    ["`...`"],
]

gram_tree["noexcept-specifier"] = [
    ["`noexcept`", "`(`", "constant-expression", "`)`"],
    ["`noexcept`"],
]

gram_tree["preprocessing-file"] = [
    ["group[opt]"],
    ["module-file"],
]

gram_tree["module-file"] = [
    "pp-global-module-fragment[opt]", "pp-module", "group[opt]", "pp-private-module-fragment[opt]",
]

gram_tree["pp-global-module-fragment"] = [
    "`module`", "`;`", "new-line", "group[opt]",
]

gram_tree["pp-private-module-fragment"] = [
    "`module`", "`:`", "`private`",  "`;`", "new-line", "group[opt]",
]

gram_tree["group"] = [
    ["group-part"],
    ["group", "group-part"],
]

gram_tree["group-part"] = [
    ["control-line"],
    ["if-section"],
    ["text-line"],
    ["`#`", "conditionally-supported-directive"],
]

gram_tree["control-line"] = [
    ["`#`", "`include`", "pp-tokens", "new-line"],
    ["pp-import"],
    ["`#`", "`define`", "`identifier`", "replacement-list", "new-line"],
    ["`#`", "`define`", "`identifier`", "lparen", "identifier-list[opt]", "`)`", "replacement-list", "new-line"],
    ["`#`", "`define`", "`identifier`", "lparen", "`...`", "`)`", "replacement-list", "new-line"],
    ["`#`", "`define`", "`identifier`", "lparen", "identifier-list", "`...`"  ,"`)`", "replacement-list", "new-line"],
    ["`#`", "`undef`", "`identifier`", "new-line"],
    ["`#`", "`line`", "pp-tokens", "new-line"],
    ["`#`", "`error`", "pp-tokens[opt]", "new-line"],
    ["`#`", "`pragma`", "pp-tokens[opt]", "new-line"],
    ["`#`", "new-line"],
]

gram_tree["if-section"] = [
    "if-group", "elif-groups[opt]", "else-group[opt]", "endif-line",
]

gram_tree["if-group"] = [
    ["`#`", "`if`", "constant-expression", "new-line", "group[opt]"],
    ["`#`", "`ifdef`", "`identifier`", "new-line", "group[opt]"],
    ["`#`", "`ifndef`", "`identifier`", "new-line", "group[opt]"],
]

gram_tree["elif-groups"] = [
    ["elif-group"],
    ["elif-groups", "elif-group"],
]

gram_tree["elif-group"] = [
    "`#`", "`elif`", "constant-expression", "new-line", "group[opt]",
]

gram_tree["else-group"] = [
    "`#`", "`else`", "new-line", "group[opt]",
]

gram_tree["endif-line"] = [
    "`#`", "`endif`", "new-line",
]

gram_tree["text-line"] = [
    "pp-tokens[opt]", "new-line",
]

gram_tree["conditionally-supported-directive"] = [
    "pp-tokens", "new-line",
]

gram_tree["lparen"] = [
    # a ( character not immediately preceded by whitespace
]

gram_tree["identifier-list"] = [
    ["`identifier`"],
    ["identifier-list", "`,`", "`identifier`"],
]

gram_tree["replacement-list"] = [
    "pp-tokens[opt]",
]

gram_tree["pp-tokens"] = [
    ["preprocessing-token"],
    ["pp-tokens", "preprocessing-token"],
]

gram_tree["new-line"] = [
    # new line characters
]

gram_tree["defined-macro-expression"] = [
    ["`defined`", "`identifier`"],
    ["`defined`", "`(`", "`identifier`", "`)`"],
]

gram_tree["h-preprocessing-token"] = [
    # any preprocessing-token other than >
]

gram_tree["h-pp-tokens"] = [
    ["h-preprocessing-token"],
    ["h-pp-tokens", "h-preprocessing-token"],
]

gram_tree["header-name-tokens"] = [
    ["`string-literal`"],
    ["`<`", "h-pp-tokens", "`>`"],
]

gram_tree["has-include-expression"] = [
    ["`__has_include`", "`(`", "header-name", "`)`"],
    ["`__has_include`", "`(`", "header-name-tokens", "`)`"],
]

gram_tree["has-attribute-expression"] = [
    "`__has_cpp_attribute`", "`(`", "pp-tokens", "`)`",
]

gram_tree["pp-module"] = [
    "`export`[opt]", "`module`", "pp-tokens[opt]", "`;`", "new-line",
]

gram_tree["pp-import"] = [
    ["`export`[opt]", "`import`","header-name", "pp-tokens[opt]", "`;`", "new-line"],
    ["`export`[opt]", "`import`","header-name-tokens", "pp-tokens[opt]", "`;`", "new-line"],
    ["`export`[opt]", "`import`","pp-tokens", "`;`", "new-line"],
]

gram_tree["va-opt-replacement"] = [
    "`__va_opt__`", "`(`", "pp-tokens[opt]", "`)`",
]

gram_tree["hex-quad"] = [
    "hexadecimal-digit", "hexadecimal-digit", "hexadecimal-digit", "hexadecimal-digit",
]

gram_tree["universal-character-name"] = [
    ["`\\u`", "hex-quad"],
    ["`\\U`", "hex-quad", "hex-quad"],
]

gram_tree["preprocessing-token"] = [
    ["header-name"],
    ["`import`"],
    ["`module`"],
    ["`export`"],
    ["`identifier`"],
    ["pp-number"],
    ["`character-literal`"],
    ["`user-defined-character-literal`"],
    ["`string-literal`"],
    ["`user-defined-string-literal`"],
    ["preprocessing-op-or-punc"],
    # each non-whitespace character that cannot be one of the above
]

gram_tree["token"] = [
    ["`identifier`"],
    ["keyword"],
    ["literal"],
    ["operator-or-punctuator"],
]

gram_tree["header-name"] = [
    ["`<`", "h-char-sequence", "`>`"],
    ['`"`', "q-char-sequence", '`"`'],
]

gram_tree["h-char-sequence"] = [
    ["h-char"],
    ["h-char-sequence", "h-char"],
]

gram_tree["h-char"] = [
    # any member of the source character set except new-line and >
]

gram_tree["q-char-sequence"] = [
    ["q-char"],
    ["q-char-sequence", "q-char"],
]

gram_tree["q-char"] = [
    # any member of the source character set except new-line and "
]

gram_tree["pp-number"] = [
    ["digit"],
    ["`.`", "digit"],
    ["pp-number", "digit"],
    ["pp-number", "identifier-nondigit"],
    ["pp-number", "`'`", "digit"],
    ["pp-number", "`'`", "nondigit"],
    ["pp-number", "`e`", "sign"],
    ["pp-number", "`E`", "sign"],
    ["pp-number", "`p`", "sign"],
    ["pp-number", "`P`", "sign"],
    ["pp-number", "`.`"],
]



gram_tree["identifier-nondigit"] = [
    ["nondigit"],
    ["universal-character-name"],
]

gram_tree["nondigit"] = [
    # left lexer to parse it.
]

gram_tree["digit"] = [
    # left lexer to parse it.
]

gram_tree["keyword"] = [
    # left lexer to parse it.
]

gram_tree["preprocessing-op-or-punc"] = [
    ["preprocessing-operator"],
    ["operator-or-punctuator"],
]

gram_tree["preprocessing-operator"] = [
    ["`#`"], ["`##`"], ["`%:`"], ["`%:%:`"],
]

gram_tree["operator-or-punctuator"] = [
    ["`{`"], ["`}`"], ["`[`"], ["`]`"], ["`(`"], ["`)`"],
    ["`<:`"], ["`:>`"], ["`<%`"], ["`%>`"], ["`;`"], ["`:`"], ["`...`"],
    ["`?`"], ["`::`"], ["`.`"], ["`.*`"], ["`->`"], ["`->*`"], ["`~`"],
    ["`!`"], ["`+`"], ["`-`"], ["`*`"], ["`/`"], ["`%`"], ["`^`"], ["`&`"], ["`|`"],
    ["`=`"], ["`+=`"], ["`-=`"], ["`*=`"], ["`/=`"], ["`%=`"], ["`^=`"], ["`&=`"], ["`|=`"],
    ["`==`"], ["`!=`"], ["`<`"], ["`>`"], ["`<=`"], ["`>=`"], ["`<=>`"], ["`&&`"], ["`||`"],
    ["`<<`"], ["`>>`"], ["`<<=`"], ["`>>=`"], ["`++`"], ["`--`"], ["`,`"],
    ["`and`"], ["`or`"], ["`xor`"], ["`not`"], ["`bitand`"], ["`bitor`"], ["`compl`"],
    ["`and_eq`"], ["`or_eq`"], ["`xor_eq`"], ["`not_eq`"],
]

gram_tree["literal"] = [
    ["`integer-literal`"],
    ["`binary-literal`"],
    ["`octal-literal`"],
    ["`decimal-literal`"],
    ["`hexadecimal-literal`"],
    ["`character-literal`"],
    ["`floating-point-literal`"],
    ["`string-literal`"],
    ["boolean-literal"],
    ["pointer-literal"],
    ["user-defined-literal"],
]

gram_tree["binary-digit"] = [
    ["`0`"],
    ["`1`"],
]

gram_tree["octal-digit"] = [
    ["`0`"],
    ["`1`"],
    ["`2`"],
    ["`3`"],
    ["`4`"],
    ["`5`"],
    ["`6`"],
    ["`7`"],
]

gram_tree["nonzero-digit"] = [
    ["`1`"],
    ["`2`"],
    ["`3`"],
    ["`4`"],
    ["`5`"],
    ["`6`"],
    ["`7`"],
    ["`8`"],
    ["`9`"],
]

gram_tree["hexadecimal-prefix"] = [
    ["`0x`"], ["`0X`"],
]

gram_tree["hexadecimal-digit-sequence"] = [
    ["hexadecimal-digit"],
    ["hexadecimal-digit-sequence", "`'`[opt]", "hexadecimal-digit"],
]

gram_tree["hexadecimal-digit"] = [
    ["`0`"],
    ["`1`"],
    ["`2`"],
    ["`3`"],
    ["`4`"],
    ["`5`"],
    ["`6`"],
    ["`7`"],
    ["`8`"],
    ["`9`"],
    ["`A`"],
    ["`B`"],
    ["`C`"],
    ["`D`"],
    ["`E`"],
    ["`F`"],
]

gram_tree["integer-suffix"] = [
    ["unsigned-suffix", "long-suffix[opt]"],
    ["unsigned-suffix", "long-long-suffix[opt]"],
    ["long-suffix", "unsigned-suffix[opt]"],
    ["long-long-suffix", "unsigned-suffix[opt]"],
]

gram_tree["unsigned-suffix"] = [
    ["`u`"], ["`U`"],
]

gram_tree["long-suffix"] = [
    ["`l`"], ["`L`"],
]

gram_tree["long-long-suffix"] = [
    ["`ll`"], ["`LL`"],
]

gram_tree["encoding-prefix"] = [
    ["`u8`"],["`u`"], ["`U`"], ["`L`"],
]

gram_tree["c-char-sequence"] = [
    ["c-char"],
    ["c-char-sequence", "c-char"],
]

gram_tree["c-char"] = [
    # any member of the basic source character set except the `'`, `\``, or "new-line" character
    ["escape-sequence"],
    ["universal-character-name"],
]

gram_tree["escape-sequence"] = [
    ["simple-escape-sequence"],
    ["octal-escape-sequence"],
    ["hexadecimal-escape-sequence"],
]

gram_tree["simple-escape-sequence"] = [
    ["`\\'`"], ['`\\"`'], ["`\\?`"], ["`\\\\"],
    ["`\\a`"], ["`\\b`"], ["`\\f`"], ["`\\n`"], ["`\\r`"], ["`\\t`"], ["`\\v"],
]

gram_tree["octal-escape-sequence"] = [
    ['`\\`', "octal-digit"],
    ['`\\`', "octal-digit", "octal-digit"],
    ['`\\`', "octal-digit", "octal-digit", "octal-digit"],
]

gram_tree["hexadecimal-escape-sequence"] = [
    ["`\\x`", "hexadecimal-digit"],
    ["hexadecimal-escape-sequence", "hexadecimal-digit"],
]

gram_tree["decimal-floating-point-literal"] = [
    ["fractional-constant", "exponent-part[opt]", "floating-point-suffix[opt]"],
    ["digit-sequence", "exponent-part[opt]", "floating-point-suffix[opt]"],
]

gram_tree["hexadecimal-floating-point-literal"] = [
    ["hexadecimal-prefix", "hexadecimal-fractional-constant", "binary-exponent-part", "floating-point-suffix[opt]"],
    ["hexadecimal-prefix", "hexadecimal-digit-sequence", "binary-exponent-part", "floating-point-suffix[opt]"],
]

gram_tree["fractional-constant"] = [
    ["digit-sequence[opt]", "`.`", "digit-sequence"],
    ["digit-sequence", "`.`"],
]

gram_tree["hexadecimal-fractional-constant"] = [
    ["hexadecimal-digit-sequence[opt]", "`.`", "hexadecimal-digit-sequence"],
    ["hexadecimal-digit-sequence", "`.`"],
]

gram_tree["exponent-part"] = [
    ["`e`", "sign[opt]", "digit-sequence"],
    ["`E`", "sign[opt]", "digit-sequence"],
]

gram_tree["binary-exponent-part"] = [
    ["`p`", "sign[opt]", "digit-sequence"],
    ["`P`", "sign[opt]", "digit-sequence"],
]

gram_tree["sign"] = [
    ["`+`"], ["`-`"],
]

gram_tree["digit-sequence"] = [
    ["digit"],
    ["digit-sequence", "`'`[opt]", "digit"],
]

gram_tree["floating-point-suffix"] = [
    ["`f`"], ["`l`"], ["`F`"], ["`L`"],
]

gram_tree["s-char-sequence"] = [
    ["s-char"],
    ["s-char-sequence", "s-char"],
]

gram_tree["s-char"] = [
    # any member of the basic source character set except the `"`, `\``, or "new-line" character
    ["escape-sequence"],
    ["universal-character-name"],
]


gram_tree["r-char-sequence"] = [
    ["r-char"],
    ["r-char-sequence", "r-char"],
]

gram_tree["r-char"] = [
    # any member of the source character set, except a `)` followed by
    # the initial "d-char-sequence" (which may be empty) followed by a `"`.
]

gram_tree["d-char-sequence"] = [
    ["d-char"],
    ["d-char-sequence", "d-char"],
]

gram_tree["d-char"] = [
    # any member of the basic source character set except:
    # space, the `(`, `)`, `\`, and the control characters
    # representing horizontal tab, vertical tab, form feed, and newline.
]

gram_tree["boolean-literal"] = [
    ["`false`"],
    ["`true`"],
]

gram_tree["pointer-literal"] = [
    "`nullptr`",
]

gram_tree["user-defined-literal"] = [
    ["`user-defined-integer-literal`"],
    ["`user-defined-floating-point-literal`"],
    ["`user-defined-string-literal`"],
    ["`user-defined-character-literal`"],
]


gram_tree["ud-suffix"] = [
    "`identifier`",
]
