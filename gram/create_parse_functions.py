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

from config import gram_tree, sp_tokens
from re import sub
import subprocess
import sys
import os


def camel_case(s):
    s = sub(r"(_|-)+", " ", s).title().replace(" ", "")
    return ''.join([s[0].upper(), s[1:]])


diag_kinds = {}


def create_serial_parse_function(serial_idx, key, v, tag_name="TagName"):
    flg = True
    name = camel_case(k)
    tag_name = f""" {name}::kTag """

    content_init = ""
    content_type = f"using parse_func_type = SerialParseFunctions<"
    content_func_def = f"""SerialParseFunctions serial_funcs(
        ParseFunctionInputs(false ,calling_depth(), output.last_token_, output.cur_token_),"""
    for idx, s_ in enumerate(v):
        assert isinstance(s_, str)
        s, is_opt = remove_opt(s_)
        if s == key and idx == 0:
            assert (False)

    for s_ in v:
        assert isinstance(s_, str)
        s, is_opt = remove_opt(s_)
        opt_str = "false"
        if is_opt:
            opt_str = "true"
        if s.startswith('`'):
            if s not in sp_tokens:
                flg = False
                break
            content_type += f"""ParseFunction,"""
            content_func_def += f"""ParseFunction::
create_single_token_check({opt_str},calling_depth() + 1,
    token::details::TokenKind::{sp_tokens[s]},
    diag::DiagKind::{k.replace("-","_")}_expect_{sp_tokens[s]}),"""

            diag_kind = f"""{k.replace("-","_")}_expect_{sp_tokens[s]}"""
            if diag_kind not in diag_kinds:
                diag_kinds[diag_kind] = [s, k.replace("-", "_")]

        else:
            assert s in gram_tree
            content_type += f"{camel_case(s)},"
            content_func_def += f"{camel_case(s)}({opt_str},calling_depth() + 1),"
    content_func_def = content_func_def[:-1]
    content_type = content_type[:-1]
    comments = "// \t"
    for z in v:
        comments += f"""{z.replace("-","_")}, """
    comments = comments[:-2]

    content_op = f"""
    {content_func_def});
        static_assert(base::kNumberOfElements == 1);
     serial_funcs.executed_mask( decltype(serial_funcs)::base::bitset_type(this->executed_mask_.value() ) );
    output = serial_funcs();
    this->executed_mask(base::bitset_type(serial_funcs.executed_mask().value()));
    return output;
    """
    if not flg:
        content_func_def = ""
        content_op = f"unreachable({tag_name});return output;"

    return content_op, content_init, content_type + ">;", "", comments


def create_serial_in_parallel_function(s_v, k, flg, tidx):
    name = camel_case(k)
    tag_name = f""" {name}::kTag """
    assert isinstance(s_v, list)
    contents_type = f"SerialParseFunctions<"
    contents = f"""SerialParseFunctions(ParseFunctionInputs(
        false,calling_depth() + 1),"""
    for s_ in s_v:
        assert isinstance(s_, str)
        s, is_opt = remove_opt(s_)
        opt_str = "false"
        if is_opt:
            opt_str = "true"

        if s.startswith('`'):
            if s not in sp_tokens:
                flg = False
                break
            contents_type += f""" ParseFunction,"""
            contents += f"""ParseFunction::
create_single_token_check({opt_str},calling_depth() + 2,
    token::details::TokenKind::{sp_tokens[s]},
    diag::DiagKind::{k.replace("-","_")}_expect_{sp_tokens[s]}),"""

            diag_kind = f"""{k.replace("-","_")}_expect_{sp_tokens[s]}"""
            if diag_kind not in diag_kinds:
                diag_kinds[diag_kind] = [s, k.replace("-", "_")]

        else:
            assert s in gram_tree
            contents_type += f"{camel_case(s)},"
            contents += f"{camel_case(s)}({opt_str}, calling_depth() + 2),"
    contents = contents[:-1]
    contents_type = contents_type[:-1]
    contents_type += ">,"
    contents += "),"
    return contents, contents_type, flg


def create_parallel_function_normal(v, k, idx, offset, out_name="output"):
    name = camel_case(k)
    tag_name = f""" {name}::kTag """
    contents_type = f"""ParallelParseFunctions<{len(v)}, """
    content_func_def = f"""
     parallel_funcs_{idx}(ParseFunctionInputs(false,calling_depth(), output.last_token_,output.cur_token_),
     """
    flg = True
    for ii, s_v in enumerate(v):
        contents_, contents_type_, flg = create_serial_in_parallel_function(
            s_v, k, flg, ii)
        if not flg:
            break
        content_func_def += contents_
        contents_type += contents_type_
    content_func_def = content_func_def[:-1] + ");"
    contents_type = contents_type[:-1] + "> "
    content_op = f"""
    static_assert(base::kNumberOfElements >= {len(v)});
    using parallel_funcs_bitset_map = decltype(parallel_funcs_{idx})::base::bitset_type;
     parallel_funcs_{idx}.executed_mask( parallel_funcs_bitset_map(base::bitset_type::range<{offset}, {offset + len(v)}>(this->executed_mask_.value({offset})) ));
    {out_name} = parallel_funcs_{idx}();
    this->executed_mask(base::bitset_type(parallel_funcs_{idx}.executed_mask().value() ,{offset}));
    """
    content_init = ""
    return content_op, content_init, contents_type,  content_func_def, flg


def create_parallel_function(serial_idx, key, v):
    flg = True
    name = camel_case(key)
    tag_name = f""" {name}::kTag """
    recursive_eles = []
    non_recursive_eles = []
    for s_v in v:
        assert isinstance(s_v, list)
        flg_has_recursive = False
        for idx, s_ in enumerate(s_v):
            assert isinstance(s_, str)
            s, is_opt = remove_opt(s_)
            if s == key and idx == 0:
                flg_has_recursive = True
                break
        if not flg_has_recursive:
            non_recursive_eles.append(s_v)
        else:
            recursive_eles.append(s_v)

    assert (len(non_recursive_eles) > 0)

    if len(recursive_eles) == 0:
        content_op, content_init, content_type, content_func_def, flg = create_parallel_function_normal(
            v, k, 0, 0)
        content_op_out = None
        if not flg:
            content_op_out = f"unreachable({tag_name});return output;"
        else:
            content_op_out = f"""
            {content_type} {content_func_def}
            {content_op}
            return  output; """
        return content_op_out, content_init, content_type, "", flg
    else:
        content_non_op, content_non_init, content_non_type, content_non_func_def, flg = create_parallel_function_normal(
            non_recursive_eles, k, 0, 0)
        if not flg:
            content_non_op = f"unreachable({tag_name});return output;"
            return content_non_op, "", "", "", flg
        content_r_op, content_r_init, content_r_type, content_r_func_def, flg = create_parallel_function_normal(
            recursive_eles, k, 1, len(non_recursive_eles))
        if not flg:
            content_r_op = f"unreachable({tag_name});return output;"
            return content_r_op, "", "", "", flg
        content_op = f"""
        decltype(output) non_recursive_output;
        if(executed_mask_.start_idx() == 0)""" + "{\n" + f"""
            {content_non_type} {content_non_func_def}
            {content_non_op}
            if(!output.work_)""" + """{
                return output;
            }
            non_recursive_output = output;

        """ + "\n}\n" + f"""
            diag::infos() << basic::str::from({tag_name}, "into recursive. \\n");
            {content_r_type} {content_r_func_def}
            {content_r_op}""" + """
            if(!output.work_){
                return non_recursive_output;
            }
            return output;

        """
        content_func_def = f"""
        {content_non_func_def},
        {content_r_func_def}
        """
        content_type = f"""
        {content_non_type}
        {content_r_type}
        """
        return content_op, "", content_type, "", flg


def remove_opt(s_: str):
    s = s_
    if s_.endswith("[opt]"):
        s = s_.replace("[opt]", "")
    return s, s != s_


def write_to_file(src_file_name, the_contents, title, end="}"):
    with open(src_file_name, "w") as f:
        f.write(title + the_contents + end)
    encoding_py3 = {}
    if sys.version_info[0] >= 3:
        encoding_py3['encoding'] = 'utf-8'

    try:
        invocation = ["clang-format", src_file_name,
                      "-i", f'--style=file:{os.path.dirname(__file__)}/../.clang-format']

        proc = subprocess.Popen(
            invocation,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            universal_newlines=True,
            **encoding_py3)
    except OSError as exc:
        print("error.")
        assert (False)
    proc_stdout = proc.stdout
    proc_stderr = proc.stderr
    outs = list(proc_stdout.readlines())
    errs = list(proc_stderr.readlines())
    proc.wait()

    if proc.returncode:
        print("error.", subprocess.list2cmdline(
            invocation), proc.returncode, errs)
        assert (False)


if __name__ == "__main__":
    args = sys.argv[1:]
    all_content_out_path = args[0]
    all_header_content_out_path = args[1]
    all_header_kind_content_out_path = args[2]
    if len(args) > 3:
        all_single_src_out_path = args[3]

    kind_def_title = """/*
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

#ifndef PARSE_FUNC
#define PARSE_FUNC(FUNC)
#endif
     """
    header_contents = ""
    header_title = """/*
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
    """
    header_contents_template = """

#define ParseFunctionDef(NAME,TYPE, N)                                  \\
                                                                        \\
  class NAME : public ParseFunction<N> {                \\
   public:   \\
    constexpr static basic::mem::TraceTag::tag_type kTag = #NAME;  \\
    using base = ParseFunction< N>;                      \\
    constexpr static ParseFunctionKind kind = ParseFunctionKind::TYPE;                     \\
    ~NAME() = default;                                                  \\
    template <typename... Params>                                       \\
    explicit NAME(bool opt, Params... params) : base(opt, params...) {} \\
    template <typename... Params>                                       \\
    explicit NAME(Params... params) : base(params...) {}                \\
    explicit NAME(const ParseFunctionInputs& param)         \\
        : base(param) {}                                               \\
    ParseFunctionOutputs operator()() override;             \\
  };

___CONTENT_DEF___

    """
    header_content_types = ""
    header_content_defs = ""

    all_contents = ""
    title = """/*
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

#include "diag.h"
#include "parse_function/function.h"
#include "parse_function/parallel_function_impl.h"
#include "parse_function/serial_function_impl.h"
#include "token.h"

namespace lps::parser::details {

"""
    for k, v in gram_tree.items():
        name = camel_case(k)
        serial_idx = 0
        parallel_idx = 0

        define_tmplate = """
___CONTENT_COM___
    inline ParseFunctionOutputs ___NAME___::operator()()
    {
         auto output = base::operator()();
        if (!this->valid()) {
            return output;
        }
         ___CONTENT_OP___
    }
        """
        the_contents = define_tmplate.replace("___NAME___", f"{name}")

        comments = f"""
// {k}:
"""
        num_ele = 1

        if len(v) == 0:
            content_op = "{" + \
                f"""unreachable({name}::kTag);return base::operator()();  """ + "}"
            the_contents = the_contents.replace(
                "___CONTENT_OP___", f"{content_op}")
            the_contents = the_contents.replace("___CONTENT_COM___", comments)

        else:

            # If first element is `str`, then this grammar definition
            # has only one `SerialParseFunction`.

            if isinstance(v[0], str):
                content_op, content_init, content_type, content_func_def, content_common = create_serial_parse_function(
                    serial_idx, k, v, "TagName")
                the_contents = the_contents.replace(
                    "___CONTENT_OP___", f"{content_op}")
                the_contents = the_contents.replace(
                    "___CONTENT_FUNC_DEF___", f"{content_func_def}")
                the_contents = the_contents.replace(
                    "___CONTENT_TYPE___", f"{content_type}")
                the_contents = the_contents.replace(
                    "___CONTENT_COM___", comments + content_common)

            # Otherwise, it has several `ParallelParseFunction`.
            else:
                num_ele = len(v)
                content_op, _, content_type, content_func_def, flg = create_parallel_function(
                    serial_idx, k, v)
                content_common = ""

                for s_v in v:
                    content_common += "// \t"
                    for z in s_v:
                        content_common += f"""{z.replace("-","_")}, """
                    content_common = content_common[:-2] + "\n"
                content_common = content_common[:-1]
                the_contents = the_contents.replace(
                    "___CONTENT_OP___", f"{content_op}")
                the_contents = the_contents.replace(
                    "___CONTENT_FUNC_DEF___", f"{content_func_def}")
                the_contents = the_contents.replace(
                    "___CONTENT_TYPE___", f"{content_type}")
                the_contents = the_contents.replace(
                    "___CONTENT_COM___", comments + content_common)

                pass
        all_contents += the_contents
        header_content_defs += f"""ParseFunctionDef({camel_case(k)},k{camel_case(k)}, {num_ele});"""
        header_content_types += f"""    PARSE_FUNC(k{camel_case(k)})
        """
        if len(args) > 3:
            src_file_name = f"""{all_single_src_out_path}/{k.replace("-","_")}.cc"""
            write_to_file(src_file_name, the_contents, title)
    header_contents = header_contents_template.replace(
        "___CONTENT_DEF___",  f"{header_content_defs}")
    write_to_file(f"{all_content_out_path}", all_contents, title)
    write_to_file(f"{all_header_content_out_path}",
                  header_contents, header_title)
    write_to_file(f"{all_header_kind_content_out_path}", "\n" +
                  header_content_types, kind_def_title, "\n#undef PARSE_FUNC")

#     with open(f"{os.path.dirname(__file__)}/../.build/t.def", "w") as f:
#         for k, v in diag_kinds.items():
#             z = f"""DIAG({k}, "expected {v[0]} in {v[1]}.", "",
#         Level::kError)
# """
#             f.write(z)
