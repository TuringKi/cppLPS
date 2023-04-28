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

#include <sys/types.h>
#include <array>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include "basic/meta.h"

namespace lps::basic::str {

namespace details {

template <auto N>
std::ostream& operator<<(std::ostream& s, const meta::Str<N>& arr) {
  if (meta::Str<N>::empty()) {
    return s;
  }
  s << std::string(arr.value, arr.value + N - 1);
  return s;
}

template <auto N>
std::ostream& operator<<(std::ostream& s, const std::array<char, N>& arr) {
  if (arr.empty()) {
    return s;
  }
  s << std::string(arr.begin(), arr.end() - 1);
  return s;
}

template <typename T>
std::ostream& operator<<(std::ostream& s, const std::vector<T>& arr) {
  if (arr.empty()) {
    return s;
  }
  const size_t max = 32;
  size_t max_idx = std::min(max, arr.size());
  s << "[";
  for (size_t i = 0; i < max_idx; i++) {
    if (i < max_idx - 1)
      s << arr[i] << ", ";
    else {
      if (arr.size() > max_idx) {
        s << arr[i] << ", ...";
      } else {
        s << arr[i];
      }
    }
  }
  s << "]";
  return s;
}

struct Empty {
  explicit operator const std::string&() const {
    static std::string empty;
    return empty;
  }
  explicit operator const char*() const { return ""; }
};

inline void str(std::ostream& s) {}

template <typename T>
inline void str(std::ostream& s, const T& t) {
  s << t;
}

template <>
inline void str<Empty>(std::ostream& s, const Empty& /*a*/) {}

template <typename T, typename... Args>
inline void str(std::ostream& s, const T& t, const Args&... args) {
  str(s, t);
  str(s, args...);
}

template <typename T>
struct Canonicalize {
  using type = const T&;
};

template <size_t N>
struct Canonicalize<char[N]> {
  using type = const char*;
};

template <typename... Args>
struct Warper final {
  inline static std::string call(const Args&... args) {
    std::ostringstream ss;
    details::str(ss, args...);
    return ss.str();
  }
};

template <>
struct Warper<std::string> final {
  static const std::string& call(const std::string& str) { return str; }
};

template <>
struct Warper<const char*> final {
  static const char* call(const char* str) { return str; }
};

template <>
struct Warper<> final {
  static Empty call() { return Empty(); }
};

}  // namespace details

template <typename... Args>
inline decltype(auto) from(const Args&... args) {
  return details::Warper<typename details::Canonicalize<Args>::type...>::call(
      args...);
}

namespace ascii {

enum CharType : uint16_t {
  kHorzWs = 0x0001,   // '\t', '\f', '\v'.  Note, no '\0'
  kVertWs = 0x0002,   // '\r', '\n'
  kSpace = 0x0004,    // ' '
  kDigit = 0x0008,    // 0-9
  kXLetter = 0x0010,  // a-f,A-F
  kUpper = 0x0020,    // A-Z
  kLower = 0x0040,    // a-z
  kUnder = 0x0080,    // _
  kDot = 0x0100,      // .
  kRawDel = 0x0200,   // {}[]#<>%:;?*+-/^&|~!=,"'
  kPunct = 0x0400     // `$@()
};

/* clang-format off */
const uint16_t kTable[256] = {
  // 0 NUL         1 SOH         2 STX         3 ETX
  // 4 EOT         5 ENQ         6 ACK         7 BEL
  0           , 0           , 0           , 0 ,
  0           , 0           , 0           , 0 ,
  // 8 BS          9 HT         10 NL         11 VT
  //12 NP         13 CR         14 SO         15 SI
  0           , kHorzWs, kVertWs, kHorzWs,
  kHorzWs, kVertWs, 0           , 0           ,
  //16 DLE        17 DC1        18 DC2        19 DC3
  //20 DC4        21 NAK        22 SYN        23 ETB
  0           , 0           , 0           , 0 ,
  0           , 0           , 0           , 0 ,
  //24 CAN        25 EM         26 SUB        27 ESC
  //28 FS         29 GS         30 RS         31 US
  0           , 0           , 0           , 0 ,
  0           , 0           , 0           , 0 ,
  //32 SP         33  !         34  "         35  #
  //36  $         37  %         38  &         39  '
  kSpace  , kRawDel , kRawDel , kRawDel ,
  kPunct  , kRawDel , kRawDel , kRawDel ,
  //40  (         41  )         42  *         43  +
  //44  ,         45  -         46  .         47  /
  kPunct  , kPunct  , kRawDel , kRawDel ,
  kRawDel , kRawDel , kDot , kRawDel ,
  //48  0         49  1         50  2         51  3
  //52  4         53  5         54  6         55  7
  kDigit  , kDigit  , kDigit  , kDigit  ,
  kDigit  , kDigit  , kDigit  , kDigit  ,
  //56  8         57  9         58  :         59  ;
  //60  <         61  =         62  >         63  ?
  kDigit  , kDigit  , kRawDel , kRawDel ,
  kRawDel , kRawDel , kRawDel , kRawDel ,
  //64  @         65  A         66  B         67  C
  //68  D         69  E         70  F         71  G
  kPunct  , kUpper , kUpper , kUpper ,
  kUpper , kUpper , kUpper , kUpper  ,
  //72  H         73  I         74  J         75  K
  //76  L         77  M         78  N         79  O
  kUpper  , kUpper  , kUpper  , kUpper  ,
  kUpper  , kUpper  , kUpper  , kUpper  ,
  //80  P         81  Q         82  R         83  S
  //84  T         85  U         86  V         87  W
  kUpper  , kUpper  , kUpper  , kUpper  ,
  kUpper  , kUpper  , kUpper  , kUpper  ,
  //88  X         89  Y         90  Z         91  [
  //92  \         93  ]         94  ^         95  _
  kUpper  , kUpper  , kUpper  , kRawDel ,
  kPunct  , kRawDel , kRawDel , kUnder  ,
  //96  `         97  a         98  b         99  c
  //100  d       101  e        102  f        103  g
  kPunct  , kLower , kLower , kLower ,
  kLower , kLower , kLower , kLower  ,
  //104  h       105  i        106  j        107  k
  //108  l       109  m        110  n        111  o
  kLower  , kLower  , kLower  , kLower  ,
  kLower  , kLower  , kLower  , kLower  ,
  //112  p       113  q        114  r        115  s
  //116  t       117  u        118  v        119  w
  kLower  , kLower  , kLower  , kLower  ,
  kLower  , kLower  , kLower  , kLower  ,
  //120  x       121  y        122  z        123  {
  //124  |       125  }        126  ~        127 DEL
  kLower  , kLower  , kLower  , kRawDel ,
  kRawDel , kRawDel , kRawDel , 0
};
/* clang-format on */

namespace is {

inline bool HorzWs(u_char c) {
  return (kTable[c] & (CharType::kHorzWs | CharType::kSpace)) != 0;
}

inline bool VertWs(u_char c) {
  return (kTable[c] & CharType::kVertWs) != 0;
}

inline bool Ws(u_char c) {
  return (kTable[c] &
          (CharType::kHorzWs | CharType::kVertWs | CharType::kSpace)) != 0;
}

inline bool Digit(u_char c) {
  return (kTable[c] & CharType::kDigit) != 0;
}

inline bool Letter(u_char c) {
  return (kTable[c] & (CharType::kLower | CharType::kUpper)) != 0;
}

inline bool Upper(u_char c) {
  return (kTable[c] & CharType::kUpper) != 0;
}

inline bool Lower(u_char c) {
  return (kTable[c] & CharType::kLower) != 0;
}

inline bool Dot(u_char c) {
  return (kTable[c] & CharType::kDot) != 0;
}

inline bool Alphanumeric(u_char c) {
  return (kTable[c] &
          (CharType::kDigit | CharType::kUpper | CharType::kLower)) != 0;
}

inline bool HexDigit(u_char c) {
  return (kTable[c] & (CharType::kDigit | CharType::kXLetter)) != 0;
}

inline bool Punctuation(u_char c) {
  return (kTable[c] & (CharType::kUnder | CharType::kDot | CharType::kRawDel |
                       CharType::kPunct)) != 0;
}

inline bool Printable(u_char c) {
  return (kTable[c] & (CharType::kUpper | CharType::kLower | CharType::kDot |
                       CharType::kPunct | CharType::kDigit | CharType::kUnder |
                       CharType::kRawDel | CharType::kSpace)) != 0;
}

// [a-zA-Z0-9_.]
inline bool PreprocessingNumberBody(u_char c) {
  return (kTable[c] & (CharType::kUpper | CharType::kLower | CharType::kDigit |
                       CharType::kUnder | CharType::kDot)) != 0;
}

inline bool RawStringDelimBody(u_char c) {
  return (kTable[c] &
          (CharType::kUpper | CharType::kLower | CharType::kDot |
           CharType::kDigit | CharType::kUnder | CharType::kRawDel)) != 0;
}

template <bool AllowDollar = false>
inline bool IdentStart(u_char c) {
  if (kTable[c] & (CharType::kUpper | CharType::kLower | CharType::kUnder))
    return true;
  return AllowDollar && c == '$';
}

template <bool AllowDollar = false>
inline bool IdentContinue(u_char c) {
  if (kTable[c] & (CharType::kUpper | CharType::kLower | CharType::kDigit |
                   CharType::kUnder))
    return true;
  return AllowDollar && c == '$';
}

inline bool NormalChar(u_char c) {
  return c != '?' && c != '\\';
}

}  // namespace is

}  // namespace ascii

}  // namespace lps::basic::str
