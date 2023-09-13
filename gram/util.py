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

from re import sub
import subprocess
import sys
import os


def camel_case(s):
    s = sub(r"(_|-)+", " ", s).title().replace(" ", "")
    return ''.join([s[0].upper(), s[1:]])

def underline_case(s):
    s = s.replace("-", "_").replace(" ", "_")
    return ''.join([s[0].lower(), s[1:]]) + "_"

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
    except OSError as _:
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
