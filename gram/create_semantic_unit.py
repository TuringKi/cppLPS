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

from util import camel_case, remove_opt, write_to_file, underline_case


if __name__ == "__main__":
    args = sys.argv[1:]
    unit_h_out_path = args[0]

    unit_h_out_template = """/*
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

__CONTENT_UNIT_DECL__

__CONTENT_UNIT_DEF__

}
     """

    unit_def_template = """
class __NAME__ : public Unit, public HasElements {

 public:
 explicit __NAME__(const parser::details::Tree::Node* gram_node, basic::Vector<2, std::unique_ptr<Unit>>&& elements):Unit(gram_node), HasElements(std::move(elements)){{
    this->kind_ = parser::details::ParseFunctionKind::k__NAME__;
 }}
  void build() override { LPS_ERROR(kTag, "not implemented"); }
  void check() override { LPS_ERROR(kTag, "not implemented"); }
~__NAME__() override = default;

 private:
  constexpr static const char* kTag = "lps::sema::details::__NAME__";
};
      """

    case_template = f"""
      case parser::details::ParseFunctionKind::k__NAME__: {{
      return std::make_unique<details::unit::__NAME__>(&node,
          std::move(elements));
      break;
    }}
      """

    str_unit_decl = ""
    str_unit_def_all = ""
    case_strs = ""

    for k, v in gram_tree.items():
        name = camel_case(k)



        str_type = ""
        str_member = ""

        if len(v) == 0:
            # not valid semantic unit
            pass
        elif isinstance(v[0], str):
            # only one serial
            str_type += f"""struct Type{{"""
            str_type += "\n"
            for idx, s_ in enumerate(v):
                assert isinstance(s_, str)
                s, is_opt = remove_opt(s_)
                if s.startswith('`'):

                    str_type += f""" std::unique_ptr<Symbol> s{idx}_{{nullptr}};"""


                else:
                    k_name = camel_case(s)
                    k_underline_name = underline_case(s)
                    str_type += f""" std::unique_ptr<{k_name}> s{idx}_{{nullptr}};"""
                str_type += "\n"
            str_type += f"""}};"""
            str_member += f"""Type ele_; """
        else:
            str_member = f"""union {{"""
            for ii,tv in enumerate(v):
                str_type += f"""struct Type{ii}{{"""
                str_type += "\n"
                for idx, s_ in enumerate(tv):
                    assert isinstance(s_, str)
                    s, is_opt = remove_opt(s_)
                    if s.startswith('`'):

                        str_type += f""" std::unique_ptr<Symbol> s{idx}_{{nullptr}};"""


                    else:
                        k_name = camel_case(s)
                        k_underline_name = underline_case(s)
                        str_type += f""" std::unique_ptr<{k_name}> s{idx}_{{nullptr}};"""
                    str_type += "\n"
                str_type += f"""}};"""
                str_member += f"""Type{ii} v{ii}_; """

            str_member += f"""}} ele_;"""

        str_unit_decl += f"""class {name};"""
        str_unit_decl += "\n"
        str_unit_def = unit_def_template.replace("__TYPE__", str_type)
        str_unit_def = str_unit_def.replace("__MEMBER__", str_member)
        str_unit_def = str_unit_def.replace("__NAME__", name)
        case_strs += case_template.replace("__NAME__", name)
        str_unit_def_all += str_unit_def

    contents = unit_h_out_template.replace("__CONTENT_UNIT_DECL__", str_unit_decl)
    contents = contents.replace("__CONTENT_UNIT_DEF__", str_unit_def_all)


    write_to_file(unit_h_out_path, contents, "\n", "\n")
    write_to_file(unit_h_out_path + ".case", case_strs, "\n", "\n")
