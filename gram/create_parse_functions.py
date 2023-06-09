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
import subprocess,sys

def camel_case(s):
    s = sub(r"(_|-)+", " ", s).title().replace(" ", "")
    return ''.join([s[0].upper(), s[1:]])

diag_kinds = {}

def create_serial_parse_function(serial_idx, key, v, tag_name="TagName"):
    flg = True
    name = camel_case(k)
    tag_name = f""" {name}::kTag """

    content_init = ""
    content_type = f"using parse_func_type = SerialParseFunctions<{tag_name},"
    content_func_def = f"""SerialParseFunctions serial_funcs(
        ParseFunctionInputs<{tag_name}>(false, output.last_token_, output.cur_token_),"""
    for idx, s_ in enumerate(v):
        assert isinstance(s_, str)
        s, is_opt = remove_opt(s_)
        if s == key and idx == 0:
            assert(False)


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
            content_type += f"""ParseFunction<{tag_name}>,"""
            content_func_def += f"""ParseFunction<{tag_name}>::
create_single_token_check({opt_str},
    token::tok::TokenKind::{sp_tokens[s]},
    diag::DiagKind::{k.replace("-","_")}_expect_{sp_tokens[s]}),"""

            diag_kind = f"""{k.replace("-","_")}_expect_{sp_tokens[s]}"""
            if diag_kind not in diag_kinds:
                diag_kinds[diag_kind] = [s,k.replace("-","_")]

        else:
            assert s in gram_tree
            content_type += f"{camel_case(s)},"
            content_func_def += f"{camel_case(s)}({opt_str}),"
    content_func_def = content_func_def[:-1]
    content_type = content_type[:-1]
    comments = "// \t"
    for z in v:
        comments += f"""{z.replace("-","_")}, """
    comments = comments[:-2]


    content_op = f"""
    {content_func_def});
    return serial_funcs();
    """
    if not flg:
        content_func_def = ""
        content_op = f"unreachable({tag_name});return output;"

    return content_op,content_init,content_type + ">;" , "",comments


def create_serial_in_parallel_function(s_v, k, flg, tidx):
    name = camel_case(k)
    tag_name = f""" {name}::kTag """
    assert isinstance(s_v, list)
    contents_type = f"SerialParseFunctions<{tag_name},"
    contents = f"""SerialParseFunctions(ParseFunctionInputs<{tag_name}>(
        false),"""
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
            contents_type += f""" ParseFunction<{tag_name}>,"""
            contents += f"""ParseFunction<{tag_name}>::
create_single_token_check({opt_str},
    token::tok::TokenKind::{sp_tokens[s]},
    diag::DiagKind::{k.replace("-","_")}_expect_{sp_tokens[s]}),"""

            diag_kind = f"""{k.replace("-","_")}_expect_{sp_tokens[s]}"""
            if diag_kind not in diag_kinds:
                diag_kinds[diag_kind] = [s,k.replace("-","_")]

        else:
            assert s in gram_tree
            contents_type += f"{camel_case(s)},"
            contents += f"{camel_case(s)}({opt_str}),"
    contents = contents[:-1]
    contents_type = contents_type[:-1]
    contents_type += ">,"
    contents += "),"
    return contents, contents_type, flg

def create_parallel_function_normal(v,k, idx):
    name = camel_case(k)
    tag_name = f""" {name}::kTag """
    contents_type = f"""ParallelParseFunctions<{tag_name},{len(v)}, """
    content_func_def = f"""
     parallel_funcs_{idx}(ParseFunctionInputs<{tag_name}>(false,output.last_token_,output.cur_token_),
     """
    flg = True
    for ii,s_v in enumerate(v):
        contents_, contents_type_, flg = create_serial_in_parallel_function(s_v, k,flg, ii)
        if not flg:
            break
        content_func_def += contents_
        contents_type += contents_type_
    content_func_def = content_func_def[:-1] + ");"
    contents_type = contents_type[:-1] + "> "
    content_op = f"""
    static_assert(base::kNumberOfElements >= {len(v)});
     parallel_funcs_{idx}.executed_mask( decltype(parallel_funcs_{idx})::base::bitset_type(this->executed_mask_.value() ) );
    """
    content_init = ""
    return content_op,content_init, contents_type,  content_func_def, flg

def create_parallel_function(serial_idx,key, v):
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

    assert(len(non_recursive_eles) > 0)

    if len(recursive_eles) == 0:
        content_op,content_init,content_type, content_func_def, flg = create_parallel_function_normal(v,k,0)
        content_op_out = None
        if not flg:
            content_op_out = f"unreachable({tag_name});return output;"
        else:
            content_op_out = f"""
            {content_type} {content_func_def}
            {content_op}
            return    parallel_funcs_{0}(); """
        return content_op_out,content_init,content_type, "", flg
    else:
        content_non_op,content_non_init,content_non_type, content_non_func_def, flg = create_parallel_function_normal(non_recursive_eles,k,0)
        if not flg:
            content_non_op = f"unreachable({tag_name});return output;"
            return content_non_op,"", "","", flg
        content_r_op,content_r_init,content_r_type, content_r_func_def, flg = create_parallel_function_normal(recursive_eles,k,1)
        if not flg:
            content_r_op = f"unreachable({tag_name});return output;"
            return content_r_op,"", "", "", flg
        content_op = "{\n" + f"""
            {content_non_type} {content_non_func_def}
            {content_non_op}
            output = parallel_funcs_{0}();
            if(!output.work_)""" + """{
                return output;
            }
        """ + "\n}\n" + f"""
            {content_r_type} {content_r_func_def}
            {content_r_op}
            return parallel_funcs_{1}();
        """
        content_func_def = f"""
        {content_non_func_def},
        {content_r_func_def}
        """
        content_type = f"""
        {content_non_type}
        {content_r_type}
        """
        return content_op,"", content_type, "", flg


def remove_opt(s_:str):
    s = s_
    if s_.endswith("[opt]"):
        s = s_.replace("[opt]", "")
    return s, s != s_


def write_to_file(src_file_name, the_contents):
    with open(src_file_name, "w") as f:
        f.write(title + the_contents + "}")
    encoding_py3 = {}
    if sys.version_info[0] >= 3:
        encoding_py3['encoding'] = 'utf-8'

    try:
        invocation = ["clang-format", src_file_name,"-i",'--style=file:../.clang-format']

        proc = subprocess.Popen(
            invocation,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            universal_newlines=True,
            **encoding_py3)
    except OSError as exc:
        print("error.")
        assert(False)
    proc_stdout = proc.stdout
    proc_stderr = proc.stderr
    outs = list(proc_stdout.readlines())
    errs = list(proc_stderr.readlines())
    proc.wait()

    if proc.returncode:
        print("error.",subprocess.list2cmdline(invocation), proc.returncode,errs)
        assert(False)


if __name__ =="__main__":

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
#include "parser/function.h"
#include "token.h"

namespace lps::parser::details {

"""
    for k,v in gram_tree.items():
        name = camel_case(k)
        serial_idx = 0
        parallel_idx = 0


        define_tmplate = """
___CONTENT_COM___
    inline ParseFunctionOutputs<___NAME___::kTag> ___NAME___::operator()()
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
            content_op = "{" + f"""unreachable({name}::kTag);return base::operator()();  """ +"}"
            the_contents = the_contents.replace("___CONTENT_OP___", f"{content_op}")
            the_contents = the_contents.replace("___CONTENT_COM___", comments )

        else:

            # If first element is `str`, then this grammar definition
            # has only one `SerialParseFunction`.

            if isinstance(v[0], str):
                content_op,content_init,content_type, content_func_def,content_common = create_serial_parse_function(serial_idx, k, v, "TagName")
                the_contents = the_contents.replace("___CONTENT_OP___", f"{content_op}")
                the_contents = the_contents.replace("___CONTENT_FUNC_DEF___", f"{content_func_def}")
                the_contents = the_contents.replace("___CONTENT_TYPE___", f"{content_type}")
                the_contents = the_contents.replace("___CONTENT_COM___", comments  + content_common)




            # Otherwise, it has several `ParallelParseFunction`.
            else:
                num_ele = len(v)
                content_op,_, content_type, content_func_def, flg = create_parallel_function(serial_idx, k, v)
                content_common = ""

                for s_v in v:
                    content_common += "// \t"
                    for z in s_v:
                        content_common += f"""{z.replace("-","_")}, """
                    content_common = content_common[:-2] + "\n"
                content_common = content_common[:-1]
                the_contents = the_contents.replace("___CONTENT_OP___", f"{content_op}")
                the_contents = the_contents.replace("___CONTENT_FUNC_DEF___", f"{content_func_def}")
                the_contents = the_contents.replace("___CONTENT_TYPE___", f"{content_type}")
                the_contents = the_contents.replace("___CONTENT_COM___", comments  + content_common)

                pass
        all_contents += the_contents
        print(f"""ParseFunctionDef({camel_case(k)},{num_ele});""")
        src_file_name = f"""../.build/parser/{k.replace("-","_")}.cc"""
        #write_to_file(src_file_name, the_contents)
    write_to_file("../.build/function_impl.h", all_contents)

    with open("../.build/t.def", "w") as f:
        for k,v in diag_kinds.items():
            z = f"""DIAG({k}, "expected {v[0]} in {v[1]}.", "",
        Level::kError)
"""
            f.write(z)
