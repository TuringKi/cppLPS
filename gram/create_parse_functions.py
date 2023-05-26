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

from config import gram_tree


def create_serial_parse_function():
    pass


def create_serial_parallel_function():
    pass

def remove_opt(s:str):
    if s.endswith("[opt]"):
        s = s.replace("[opt]", "")
    return s

if __name__ =="__main__":
    sp_tokens = {}
    for k,v in gram_tree.items():
        if len(v) == 0:
            print(k)
        else:
            # If first element is `str`, then this grammar definition
            # has only one `SerialParseFunction`.
            if isinstance(v[0], str):
                for s_ in v:
                    assert isinstance(s_, str)
                    s = remove_opt(s_)
                    if s.startswith('`'):
                        sp_tokens[s] = 1
                    else:
                        assert s in gram_tree
            # Otherwise, it has several `ParallelParseFunction`.
            else:
                for s_v in v:
                    assert isinstance(s_v, list)
                    for s_ in s_v:
                        assert isinstance(s_, str)
                        s = remove_opt(s_)
                        if s.startswith('`'):
                            sp_tokens[s] = 1
                        else:
                            assert s in gram_tree

                pass
    for k in sp_tokens.keys():
        print(f"\"{k}\":{None},")
