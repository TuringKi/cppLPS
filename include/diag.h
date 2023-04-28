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

#include <map>
#include "basic/map.h"
#include "basic/vec.h"

namespace lps::diag {

enum DiagKind : uint16_t { kNone = 0, kBackslashNewlineSpace, kNum };

// The definition of this data structure can be found in the RFC(https://discourse.llvm.org/t/rfc-improving-clang-s-diagnostics/62584) for an reference:
// *Diagnostic category*	Error, warning, remark
// *Source location*
// *Summary*	Similar to current diagnostics, states what the problem is.
// *Reason*	Explains why the diagnostic was generated in a friendly manner.
// *Context*	Relevant source info such as considered overload resolution candidates, template backtraces, etc. These should be structured, rather than appearing as plaintext.
// *Potential fixes*
// *Reference material*	cppreference, C++ standard drafts, etc.
// *Glossary*	Separates identifying type aliases and template parameters from the message.
class Information {};

static constexpr std::array<std::pair<DiagKind, Information>,
                            DiagKind::kNum - 1>
    kLists = {{
        {DiagKind::kBackslashNewlineSpace, Information()},
    }};

static constexpr lps::basic::map::Map<DiagKind, Information, DiagKind::kNum - 1,
                                      meta::S("diag_map")>
    kMap{kLists};

void doing(const char* ptr, DiagKind kind);

};  // namespace lps::diag

namespace lps::basic::str::details {
inline std::ostream& operator<<(std::ostream& s,
                                const lps::diag::DiagKind& kind) {
  s << static_cast<uint16_t>(kind);
  return s;
}
}  // namespace lps::basic::str::details
