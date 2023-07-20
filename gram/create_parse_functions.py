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
    content_type = f"using parse_func_type = SerialParseFunctions<ParseFunctionKind::k{name},"
    content_func_def = f"""parse_func_type serial_funcs(context_,"{name}_serial",
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
create_single_token_check(context_, {opt_str},calling_depth() + 1,
    token::details::TokenKind::{sp_tokens[s]},
    diag::DiagKind::{k.replace("-","_")}_expect_{sp_tokens[s]}),"""

            diag_kind = f"""{k.replace("-","_")}_expect_{sp_tokens[s]}"""
            if diag_kind not in diag_kinds:
                diag_kinds[diag_kind] = [s, k.replace("-", "_")]

        else:
            assert s in gram_tree
            content_type += f"{camel_case(s)},"
            content_func_def += f"{camel_case(s)}(context_, {opt_str},calling_depth() + 1),"
    content_func_def = content_func_def[:-1]
    content_type = content_type[:-1]
    comments = "// \t"
    for z in v:
        comments += f"""{z.replace("-","_")}, """
    comments = comments[:-2]

    content_op = f"""
    {content_type + ">;"}
    {content_func_def});
     serial_funcs.opt_idx(this->opt_idx() );
    output = serial_funcs();
    this->opt_idx(serial_funcs.opt_idx());
       this->context_->save(this->cur_token(), output.cur_token_, this->kind(),
                            output.line_, output.len_);
    return output;
    """
    if not flg:
        content_func_def = ""
        content_op = f"unreachable({tag_name});return output;"

    return content_op, content_init, content_type + ">;", "", comments


def create_serial_in_parallel_function(s_v, k, flg, idx_str, tidx):
    name = camel_case(k)
    tag_name = f""" {name}::kTag """
    assert isinstance(s_v, list)
    contents_type = f"SerialParseFunctions<ParseFunctionKind::k{name},"
    contents = f"""(context_, "{name}_{idx_str}_serial_{tidx}", ParseFunctionInputs(
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
create_single_token_check(context_, {opt_str},calling_depth() + 1,
    token::details::TokenKind::{sp_tokens[s]},
    diag::DiagKind::{k.replace("-","_")}_expect_{sp_tokens[s]}),"""

            diag_kind = f"""{k.replace("-","_")}_expect_{sp_tokens[s]}"""
            if diag_kind not in diag_kinds:
                diag_kinds[diag_kind] = [s, k.replace("-", "_")]

        else:
            assert s in gram_tree
            contents_type += f"{camel_case(s)},"
            contents += f"{camel_case(s)}(context_, {opt_str}, calling_depth() + 1),"
    contents = contents[:-1]
    contents_type = contents_type[:-1]
    contents = contents_type + ">" + contents
    contents_type += ">,"
    contents += "),"
    return contents, contents_type, flg


def create_parallel_function_normal(v, k, idx, offset, out_name="output", type_recursive=False):
    name = camel_case(k)
    tag_name = f""" {name}::kTag """
    contents_type = f"""ParallelParseFunctions<ParseFunctionKind::k{name}, """
    sub_str = f"""parallel_{idx}"""
    if type_recursive:
        sub_str = f"""recursive_{idx}"""
        contents_type = f"""RecursiveParseFunctions<ParseFunctionKind::k{name},"""
    content_func_def = f"""
     parallel_funcs_{idx}(context_,"{name}_parallel_{idx}",ParseFunctionInputs(false,calling_depth(), output.last_token_,output.cur_token_),
     """
    if type_recursive:
        content_func_def = f"""
     recursive_funcs_{idx}(context_,"{name}_recursive", ParseFunctionInputs(false,calling_depth(), output.last_token_,output.cur_token_),
     """
    flg = True
    for ii, s_v in enumerate(v):
        contents_, contents_type_, flg = create_serial_in_parallel_function(
            s_v, k, flg, sub_str, ii)
        if not flg:
            break
        content_func_def += contents_
        contents_type += contents_type_
    content_func_def = content_func_def[:-1] + ");"
    contents_type = contents_type[:-1] + "> "

    content_op = f"""
     parallel_funcs_{idx}.opt_idx( this->opt_idx() );
    {out_name} = parallel_funcs_{idx}();
    this->opt_idx(parallel_funcs_{idx}.opt_idx());
if(parallel_funcs_{idx}.valid_outputs().empty()){{
 this->context_->save(this->cur_token(), {out_name}.cur_token_, this->kind());
}}else{{
 for (const auto& a : parallel_funcs_{idx}.valid_outputs()) {{
       this->context_->save(this->cur_token(), a.cur_token_, this->kind(),
                            a.line_, a.len_);
     }}
}}


    """
    if type_recursive:
        content_op = f"""{out_name} = recursive_funcs_{idx}();
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
            assert (len(s_v) > 1)
            recursive_eles.append(s_v[1:])

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
            recursive_eles, k, 1, len(non_recursive_eles), "output", True)
        if not flg:
            content_r_op = f"unreachable({tag_name});return output;"
            return content_r_op, "", "", "", flg
        content_op = f"""
        decltype(output) non_recursive_output;
        """ + "{\n" + f"""
            {content_non_type} {content_non_func_def}
            {content_non_op}
            if(!output.work_)""" + """{
                return output;
            }
            non_recursive_output = output;

        """ + "\n}\n" + f"""
            diag::infos() << basic::str::from({tag_name}, " into recursive. \\n");
            {content_r_type} {content_r_func_def}
            {content_r_op}""" + f"""
            if(!output.work_){{
                return non_recursive_output;
            }}

const auto* p_start = &context_->token_lists().at(start_token);
  const auto* p_end = &context_->token_lists().at(output.cur_token_);
  Line line{{
      p_start,
      p_end,
      this->kind(),
      token::details::TokenKind::unknown,
      token::TokenLists::len(p_start, p_end),
      this->calling_depth(),
      Line::segments_type()
  }};
  line.segments_.append(non_recursive_output.line_);
  line.segments_.append(output.line_);
  output.line_ = context_->paint(line);
 for (const auto& a : recursive_funcs_1.valid_outputs()) {{
       this->context_->save(this->cur_token(), a.cur_token_, this->kind(),
                            a.line_, a.len_);
     }}
            this->reset();
this->tried();
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
    all_header_diag_def_out_path = args[3]
    if len(args) > 4:
        all_single_src_out_path = args[4]

    diag_def_title = """/*
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

"""

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

#define ParseFunctionDef(NAME,TYPE)                                  \\
                                                                        \\
  class NAME : public ParseFunction {                \\
   public:   \\
    constexpr static basic::mem::TraceTag::tag_type kTag = #NAME;  \\
    using base = ParseFunction;                      \\
           ParseFunctionKind kind() override{\\
   constexpr static ParseFunctionKind kKind = ParseFunctionKind::TYPE;  \\
    return kKind;\\
  }\\
    ~NAME() = default;                                                  \\
    template <typename... Params>                                       \\
    explicit NAME(Context* context, bool opt, Params... params) : base(context, #NAME,opt, params...) {} \\
    template <typename... Params>                                       \\
    explicit NAME(Context* context,Params... params) : base(context,#NAME,params...) {}                \\
    explicit NAME(Context* context,const ParseFunctionInputs& param)         \\
        : base(context,#NAME,param) {}                                               \\
             void reset() override{\\
                if(this->cur_token().file_id() == 0){opt_idx_=0;return;} \\
                auto r = this->context_->saved(this->cur_token(), kind());\\
               if(r>=0){\\
opt_idx_ = r;\\
               }else if(r==-2|| r==-1){opt_idx_ = 0;}else{ unreachable(kTag);}\\
               \\
        }\\
    ParseFunctionOutputs operator()() ;             \\
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
#include "parse_function/recursive_function_impl.h"
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
     ParseFunctionOutputs ___NAME___::operator()()
    {
         auto output = base::operator()();
        if (!this->valid()) {
            return output;
        }

        auto start_token = output.cur_token_;
        auto saved_outputs= this->context_->saved(start_token, ParseFunctionKind::k___NAME___,this->opt_idx());
         if(saved_outputs.first){
            this->tried();
            return saved_outputs.second;
         }
         this->reset();
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
        header_content_defs += f"""ParseFunctionDef({camel_case(k)},k{camel_case(k)});"""
        header_content_types += f"""    PARSE_FUNC(k{camel_case(k)})
        """
        if len(args) > 4:
            src_file_name = f"""{all_single_src_out_path}/{k.replace("-","_")}.cc"""
            write_to_file(src_file_name, the_contents, title)
    header_contents = header_contents_template.replace(
        "___CONTENT_DEF___",  f"{header_content_defs}")
    write_to_file(f"{all_content_out_path}", all_contents, title)
    write_to_file(f"{all_header_content_out_path}",
                  header_contents, header_title)

    diag_def_contents = ""
    for k, v in diag_kinds.items():
        z = f"""DIAG({k}, "expected {v[0]} in {v[1]}.", "",
            Level::kError)
            """
        diag_def_contents += z

    write_to_file(f"{all_header_diag_def_out_path}", "\n" +
                  diag_def_contents, diag_def_title, "\n")

    write_to_file(f"{all_header_kind_content_out_path}", "\n" +
                  header_content_types, kind_def_title, "\n#undef PARSE_FUNC")
