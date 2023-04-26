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

#include <cstdint>
#include "basic/exception.h"
namespace lps::token {

template <meta::Str TagName = meta::S("location")>
class Location {

 public:
  using id_type = uint32_t;
  [[nodiscard]] id_type offset() const { return id_ & ~kMask_; }
  Location<TagName> operator+(id_type l) {
    lps_assert(TagName, ((offset() + l) & kMask_) == 0 && "location overflow");
    Location<TagName> the_loc;
    the_loc.id = id_ + l;
    return the_loc;
  }
  [[nodiscard]] id_type raw() const { return id_; }

 private:
  id_type id_;
  const id_type kMask_ = 1ULL << (8 * sizeof(id_type) - 1);
};

namespace tok {

enum class TokenKind : uint16_t {
#define TOK(X) X,
#include "token/kinds.def"
  kNumTokens
};
};  // namespace tok

enum Flag : uint16_t {
  kStartOfLine = 0x01,
  kLeadingSpace = 0x02,
  kNeedClean = 0x04
};

template <meta::Str TagName>
struct Token {

 public:
  void set_flag(Flag flg) { flags_ |= flg; }

 private:
  typename Location<TagName + "_location">::id_type loc_;
  tok::TokenKind Kind_;

  uint16_t flags_;
};

}  // namespace lps::token
