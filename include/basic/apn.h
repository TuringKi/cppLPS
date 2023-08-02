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

#include <limits.h>
#include <algorithm>
#include <bit>
#include <climits>
#include <cstdint>
#include <optional>
#include "basic/exception.h"
#include "basic/str.h"
#include "basic/vec.h"

// arbitrary precision numbers
namespace lps::basic::apn {
class Float;
namespace details {

template <typename T, std::size_t SizeOfT>
struct LeadingZerosCounter {
  static unsigned count(T val) {
    if (!val) {
      return std::numeric_limits<T>::digits;
    }
    unsigned zero_bits = 0;
    for (T shift = std::numeric_limits<T>::digits >> 1; shift; shift >>= 1) {
      T tmp = val >> shift;
      if (tmp) {
        val = tmp;
      } else {
        zero_bits |= shift;
      }
    }
    return zero_bits;
  }
};

template <typename T, std::size_t SizeOfT>
struct TrailingZerosCounter {
  static unsigned count(T Val) {

    if (!Val) {
      if (SizeOfT == 4) {
        return 32;
      }
      if (SizeOfT == 8) {
        return 64;
      }
      return std::numeric_limits<T>::digits;
    }

    if (Val & 0x1)
      return 0;

    // Bisection method.
    unsigned zero_bits = 0;
    T shift = std::numeric_limits<T>::digits >> 1;
    T mask = std::numeric_limits<T>::max() >> shift;
    while (shift) {
      if ((Val & mask) == 0) {
        Val >>= shift;
        zero_bits |= shift;
      }
      shift >>= 1;
      mask >>= shift;
    }
    return zero_bits;
  }
};

template <typename T>
[[nodiscard]] int countl_zero(T val) {
  static_assert(std::is_unsigned_v<T>,
                "Only unsigned integral types are allowed.");
  return LeadingZerosCounter<T, sizeof(T)>::count(val);
}
template <typename T>
[[nodiscard]] int countl_one(T Value) {
  static_assert(std::is_unsigned_v<T>,
                "Only unsigned integral types are allowed.");
  return countl_zero<T>(~Value);
}

template <typename T>
[[nodiscard]] int countr_zero(T val) {
  static_assert(std::is_unsigned_v<T>,
                "Only unsigned integral types are allowed.");
  return TrailingZerosCounter<T, sizeof(T)>::count(val);
}

template <typename T>
[[nodiscard]] int countr_one(T val) {
  static_assert(std::is_unsigned_v<T>,
                "Only unsigned integral types are allowed.");
  return countr_zero<T>(~val);
}

template <typename T, std::size_t SizeOfT>
struct PopulationCounter {
  static int count(T Value) {
    static_assert(SizeOfT <= 4, "not implemented!");
    uint32_t v = Value;
    v = v - ((v >> 1) & 0x55555555);
    v = (v & 0x33333333) + ((v >> 2) & 0x33333333);
    return static_cast<int>(((v + (v >> 4) & 0xF0F0F0F) * 0x1010101) >> 24);
  }
};

template <typename T>
struct PopulationCounter<T, 8> {
  static int count(T Value) {
    uint64_t v = Value;
    v = v - ((v >> 1) & 0x5555555555555555ULL);
    v = (v & 0x3333333333333333ULL) + ((v >> 2) & 0x3333333333333333ULL);
    v = (v + (v >> 4)) & 0x0F0F0F0F0F0F0F0FULL;
    return static_cast<int>(static_cast<uint64_t>(v * 0x0101010101010101ULL) >>
                            56);
  }
};

template <typename T, typename = std::enable_if_t<std::is_unsigned_v<T>>>
[[nodiscard]] inline int popcount(T Value) noexcept {
  return PopulationCounter<T, sizeof(T)>::count(Value);
}

enum class ZeroBehavior : uint8_t { kUndefined, kMax };

template <typename T>
T find_last_set(T val, ZeroBehavior zb = ZeroBehavior::kMax) {
  if (zb == ZeroBehavior::kMax && val == 0) {
    return std::numeric_limits<T>::max();
  }
  return countl_zero(val) ^ (std::numeric_limits<T>::digits - 1);
}

template <typename T>
T find_first_set(T val, ZeroBehavior zb = ZeroBehavior::kMax) {
  if (zb == ZeroBehavior::kMax && val == 0)
    return std::numeric_limits<T>::max();
  return countr_zero(val);
}

constexpr inline uint32_t hi_32(uint64_t Value) {
  return static_cast<uint32_t>(Value >> 32);
}

constexpr inline uint32_t lo_32(uint64_t Value) {
  return static_cast<uint32_t>(Value);
}

constexpr inline uint64_t make_64(uint32_t High, uint32_t Low) {
  return (static_cast<uint64_t>(High) << 32) | static_cast<uint64_t>(Low);
}

template <unsigned B>
constexpr inline int64_t sign_ext64(uint64_t x) {
  static_assert(B > 0, "Bit width can't be 0.");
  static_assert(B <= 64, "Bit width out of range.");
  return int64_t(x << (64 - B)) >> (64 - B);
}

inline int64_t sign_ext64(uint64_t X, unsigned B) {
  LPS_CHECK_ERROR("sign_ext64", B > 0, "Bit width can't be 0.");
  LPS_CHECK_ERROR("sign_ext64", B <= 64, "Bit width out of range.");
  return int64_t(X << (64 - B)) >> (64 - B);
}

constexpr inline uint64_t next_pow_2(uint64_t A) {
  A |= (A >> 1);
  A |= (A >> 2);
  A |= (A >> 4);
  A |= (A >> 8);
  A |= (A >> 16);
  A |= (A >> 32);
  return A + 1;
}

template <typename T, typename = std::enable_if_t<std::is_unsigned_v<T>>>
[[nodiscard]] constexpr inline bool has_single_bit(T v) noexcept {
  return (v != 0) && ((v & (v - 1)) == 0);
}

constexpr inline bool is_power2_64(uint64_t Value) {
  return has_single_bit(Value);
}

template <
    typename To, typename From,
    typename = std::enable_if_t<sizeof(To) == sizeof(From)>,
    typename = std::enable_if_t<std::is_trivially_constructible<To>::value>,
    typename = std::enable_if_t<std::is_trivially_copyable<To>::value>,
    typename = std::enable_if_t<std::is_trivially_copyable<From>::value>>
[[nodiscard]] inline To bit_cast(const From& from) noexcept {
  To to;
  std::memcpy(&to, &from, sizeof(To));
  return to;
}

inline double bits2double(uint64_t Bits) {
  static_assert(sizeof(uint64_t) == sizeof(double), "unexpected type sizes");
  return bit_cast<double>(Bits);
}

inline float bits2float(uint32_t Bits) {
  static_assert(sizeof(uint32_t) == sizeof(float), "unexpected type sizes");
  return bit_cast<float>(Bits);
}

inline uint64_t double2bits(double d) {
  static_assert(sizeof(uint64_t) == sizeof(double), "unexpected type sizes");
  return bit_cast<uint64_t>(d);
}

inline uint32_t float2bits(float f) {
  static_assert(sizeof(uint32_t) == sizeof(float), "unexpected type sizes");
  return bit_cast<uint32_t>(f);
}

class IEEEFloat;
}  // namespace details
class Int;

Int operator+(Int a, const Int& b);
Int operator+(const Int& a, Int&& b);
Int operator+(Int a, uint64_t other);
Int operator+(uint64_t other, Int b);
Int operator-(Int a, const Int& b);
Int operator-(const Int& a, Int&& b);
Int operator-(Int a, uint64_t other);
Int operator-(uint64_t other, Int b);
Int operator*(Int a, uint64_t other);
Int operator*(uint64_t other, Int b);
bool operator==(uint64_t V1, const Int& V2);
bool operator!=(uint64_t V1, const Int& V2);
Int operator~(Int v);
Int operator&(Int a, const Int& b);
Int operator&(const Int& a, Int&& b);
Int operator&(Int a, uint64_t other);
Int operator&(uint64_t other, Int b);
Int operator|(Int a, const Int& b);
Int operator|(const Int& a, Int&& b);
Int operator|(Int a, uint64_t other);
Int operator|(uint64_t other, Int b);
Int operator^(Int a, const Int& b);
Int operator^(const Int& a, Int&& b);
Int operator^(Int a, uint64_t other);
Int operator^(uint64_t other, Int b);
Int operator-(Int v);

class Int {
 public:
  friend class details::IEEEFloat;

  static constexpr const char* kTag = "Int";
  using word_type = uint64_t;
  enum class RoundingType : uint8_t { kUnknown = 0, kDown, kUp, kZero };
  static constexpr word_type kWordMax = ~static_cast<word_type>(0);
  static constexpr uint64_t kWordSize = sizeof(word_type);
  static constexpr uint64_t kBitsOfWord = kWordSize * CHAR_BIT;

  Int(uint64_t* val, uint64_t bits) : bits_(bits) { val_.pv_ = val; }

  Int(uint64_t bits, uint64_t val, bool is_signed = false) : bits_(bits) {
    if (is_single()) {
      val_.v_ = val;
      clear_bits();
    } else {
      init(val, is_signed);
    }
  }
  Int(uint64_t bits, StringRef str, uint8_t radix) : bits_(bits) {
    form(str, bits, radix);
  }

  Int(uint64_t bits, const char* str, uint8_t radix) : bits_(bits) {
    form(StringRef(str), bits, radix);
  }

  Int(uint64_t bits, const uint64_t* begin, uint64_t sz) : bits_(bits) {
    LPS_CHECK_ERROR(kTag, begin != nullptr && sz >= 1,
                    "not valid begin pointer or size.");
    if (is_single()) {
      val_.v_ = *begin;
    } else {
      val_.pv_ = alloc_and_set(n_words());
      uint64_t words = std::min<unsigned>(sz, n_words());
      memcpy(val_.pv_, begin, words * kWordSize);
    }
    clear_bits();
  }

  explicit Int() { val_.v_ = 0; }

  Int(const Int& other) : bits_(other.bits_) {
    if (is_single()) {
      val_.v_ = other.val_.v_;
    } else {
      init(other);
    }
  }

  Int(Int&& other) : bits_(other.bits_) {
    memcpy(&val_, &other.val_, sizeof(other));
    other.bits_ = 0;
  }

  ~Int() {
    if (!is_single()) {
      delete[] val_.pv_;
    }
  }

  [[nodiscard]] double bits2double() const {
    return details::bits2double(word(0));
  }

  [[nodiscard]] float bits2float() const {
    return details::bits2float(static_cast<uint32_t>(word(0)));
  }

  static Int double2bits(double v) {
    return Int(sizeof(double) * CHAR_BIT, details::double2bits(v));
  }

  static Int float2bits(float v) {
    return Int(sizeof(float) * CHAR_BIT, details::float2bits(v));
  }

  static Int all_ones(uint64_t bits) { return Int(bits, kWordMax, true); }

  static word_type increment(word_type* dst, uint64_t parts) {
    return add_parts(dst, 1, parts);
  }

  static word_type decrement(word_type* dst, uint64_t parts) {
    return sub_parts(dst, 1, parts);
  }

  static int compare(const word_type* lhs, const word_type* rhs,
                     uint64_t parts) {
    while (parts) {
      parts--;
      if (lhs[parts] != rhs[parts]) {
        return (lhs[parts] > rhs[parts]) ? 1 : -1;
      }
    }

    return 0;
  }

  static uint64_t sufficient_bits(StringRef str, uint8_t Radix) {
    LPS_CHECK_ERROR(kTag, !str.empty(), "invalid string length");
    uint64_t str_len = str.size();

    uint64_t is_neg = 0;
    if (str[0] == '-' || str[0] == '+') {
      is_neg = static_cast<uint64_t>(str[0] == '-');
      str_len--;
      LPS_CHECK_ERROR(kTag, str_len, "string is only a sign, needs a value.");
    }

    if (Radix == 2)
      return str_len + is_neg;
    if (Radix == 8)
      return str_len * 3 + is_neg;
    if (Radix == 16)
      return str_len * 4 + is_neg;

    if (Radix == 10)
      return (str_len == 1 ? 4 : str_len * 64 / 18) + is_neg;

    unreachable(kTag);
    return 0;
  }

  static unsigned bits_needed(const char* s, uint8_t radix) {
    StringRef str(s);
    auto sufficient = sufficient_bits(str, radix);

    if (radix == 2 || radix == 8 || radix == 16)
      return sufficient;

    size_t slen = str.size();

    StringRef::const_iterator p = str.begin();
    auto is_negative = static_cast<uint64_t>(*p == '-');
    if (*p == '-' || *p == '+') {
      p++;
      slen--;
      LPS_CHECK_ERROR(kTag, slen, "string is only a sign, needs a value.");
    }

    Int tmp(sufficient, StringRef(p, slen), radix);

    auto log = tmp.log2();
    if (log == static_cast<uint64_t>(-1)) {
      return is_negative + 1;
    }
    if (is_negative && tmp.is_power2()) {
      return is_negative + log;
    }
    { return is_negative + log + 1; }
  }

  static Int one_bit_set(uint64_t numBits, uint64_t BitNo) {
    Int res(numBits, 0);
    res.set_bit(BitNo);
    return res;
  }

  [[nodiscard]] bool bool_value() const { return !is_zero(); }

  [[nodiscard]] uint64_t log2() const { return active_bits() - 1; }
  [[nodiscard]] uint64_t ceil_log2() const {
    Int temp(*this);
    --temp;
    return temp.active_bits();
  }
  [[nodiscard]] uint64_t nearest_log2() const {

    if (bits_ == 1)
      return val_.v_ - 1;

    if (is_zero())
      return UINT64_MAX;

    uint64_t lg = log2();
    return lg + static_cast<uint64_t>((*this)[lg - 1]);
  }
  [[nodiscard]] int32_t exact_log2() const {
    if (!is_power2())
      return -1;
    return log2();
  }
  [[nodiscard]] bool is_power2() const {
    if (is_single()) {
      LPS_CHECK_ERROR(kTag, bits_, "zero width values not allowed");
      return details::is_power2_64(val_.v_);
    }
    return countp_slow_case() == 1;
  }
  void print(std::ostream& s, bool is_signed) const {
    String<40> str;
    this->string(str, 16, is_signed, false);
    s << str;
  }

  void set_bits(uint64_t loBit, uint64_t hiBit) {
    LPS_CHECK_ERROR(kTag, hiBit <= bits_, "hiBit out of range");
    LPS_CHECK_ERROR(kTag, loBit <= bits_, "loBit out of range");
    LPS_CHECK_ERROR(kTag, loBit <= hiBit, "loBit greater than hiBit");
    if (loBit == hiBit)
      return;
    if (loBit < kBitsOfWord && hiBit <= kBitsOfWord) {
      uint64_t mask = kWordMax >> (kBitsOfWord - (hiBit - loBit));
      mask <<= loBit;
      if (is_single())
        val_.v_ |= mask;
      else
        val_.pv_[0] |= mask;
    } else {
      set_bits_slow_case(loBit, hiBit);
    }
  }

  void set_bits_slow_case(uint64_t lo_bit, uint64_t hi_bit) {
    auto lo_word = which_word(lo_bit);
    auto hi_word = which_word(hi_bit);

    auto lo_mask = kWordMax << which_bit(lo_bit);

    auto hi_shift_amt = which_bit(hi_bit);
    if (hi_shift_amt != 0) {
      auto hi_mask = kWordMax >> (kBitsOfWord - hi_shift_amt);
      if (hi_word == lo_word)
        lo_mask &= hi_mask;
      else
        val_.pv_[hi_word] |= hi_mask;
    }
    val_.pv_[lo_word] |= lo_mask;

    for (unsigned word = lo_word + 1; word < hi_word; ++word)
      val_.pv_[word] = kWordMax;
  }

  void set_low_bits(uint64_t loBits) { return set_bits(0, loBits); }

  static Int low_bits_set(uint64_t numBits, uint64_t loBitsSet) {
    Int r(numBits, 0);
    r.set_low_bits(loBitsSet);
    return r;
  }

  [[nodiscard]] Int lo_bits(uint64_t n_bits) const {
    Int r(low_bits_set(bits_, n_bits));
    r &= *this;
    return r;
  }

  static inline void set_bit(word_type* parts, uint64_t bit) {
    parts[which_word(bit)] |= mask_bit(bit);
  }

  void set_bit(uint64_t bit_pos) {
    LPS_CHECK_ERROR(kTag, bit_pos < bits_, "bit position out of range");
    word_type mask = mask_bit(bit_pos);
    if (is_single())
      val_.v_ |= mask;
    else
      val_.pv_[which_word(bit_pos)] |= mask;
  }

  void clear_bit(uint64_t bit_pos) {
    LPS_CHECK_ERROR(kTag, bit_pos < bits_, "bit position out of range");
    word_type mask = ~mask_bit(bit_pos);
    if (is_single())
      val_.v_ &= mask;
    else
      val_.pv_[which_word(bit_pos)] &= mask;
  }

  [[nodiscard]] Int zext(uint64_t width) const {
    lps_assert(kTag, width >= bits_);

    if (width <= kBitsOfWord)
      return Int(width, val_.v_);

    if (width == bits_)
      return *this;

    Int result(alloc(n_words(width)), width);

    std::memcpy(result.val_.pv_, raw_data(), n_words() * kWordSize);

    std::memset(result.val_.pv_ + n_words(), 0,
                (result.n_words() - n_words()) * kWordSize);

    return result;
  }

  [[nodiscard]] Int sext(uint64_t width) const {
    lps_assert(kTag, width >= bits_);

    if (width <= kBitsOfWord)
      return Int(width, sign_ext64(val_.v_, bits_));

    if (width == bits_)
      return *this;

    Int result(alloc(n_words(width)), width);

    std::memcpy(result.val_.pv_, raw_data(), n_words() * kWordSize);

    result.val_.pv_[n_words() - 1] = sign_ext64(
        result.val_.pv_[n_words() - 1], ((bits_ - 1) % kBitsOfWord) + 1);

    std::memset(result.val_.pv_ + n_words(), is_negative() ? -1 : 0,
                (result.n_words() - n_words()) * kWordSize);
    result.clear_bits();
    return result;
  }

  static Int zero(uint64_t bits) { return Int(bits, 0); }

  static Int null_value(uint64_t bits) { return zero(bits); }

  static Int zero_width() { return zero(0); }

  [[nodiscard]] const uint64_t* raw_data() const {
    if (is_single())
      return &val_.v_;
    return &val_.pv_[0];
  }

  [[nodiscard]] uint64_t bits() const { return bits_; }

  [[nodiscard]] uint64_t zero_ext_value() const {
    if (is_single())
      return val_.v_;
    LPS_CHECK_ERROR(kTag, active_bits() <= 64, "too many bits for uint64_t");
    return val_.pv_[0];
  }

  [[nodiscard]] int64_t sign_ext_value() const {
    if (is_single())
      return sign_ext64(val_.v_, bits_);
    LPS_CHECK_ERROR(kTag, significant_bits() <= 64,
                    "too many bits for int64_t");
    return static_cast<int64_t>(val_.pv_[0]);
  }

  [[nodiscard]] Int shl(uint64_t shiftAmt) const {
    Int r(*this);
    r <<= shiftAmt;
    return r;
  }

  [[nodiscard]] Int ashr(uint64_t ShiftAmt) const {
    Int r(*this);
    r.ashr_inplace(ShiftAmt);
    return r;
  }

  [[nodiscard]] Int lshr(uint64_t shift_amt) const {
    Int r(*this);
    r.lshr_inplace(shift_amt);
    return r;
  }

  [[nodiscard]] Int shl(const Int& ShiftAmt) const {
    Int r(*this);
    r <<= ShiftAmt;
    return r;
  }

  [[nodiscard]] Int udiv(const Int& other) const {
    LPS_CHECK_ERROR(kTag, bits_ == other.bits_, "bit widths must be the same");

    if (is_single()) {
      LPS_CHECK_ERROR(kTag, other.val_.v_ != 0, "divide by zero?");
      return Int(bits_, val_.v_ / other.val_.v_);
    }

    unsigned lhs_words = n_words(active_bits());
    unsigned rhs_bits = other.active_bits();
    unsigned rhs_words = n_words(rhs_bits);
    LPS_CHECK_ERROR(kTag, rhs_words, "divide by zero?");

    if (!lhs_words)
      // 0 / X ===> 0
      return Int(bits_, 0);
    if (rhs_bits == 1)
      // X / 1 ===> X
      return *this;
    if (lhs_words < rhs_words || this->ult(other))
      // X / Y ===> 0, iff X < Y
      return Int(bits_, 0);
    if (*this == other)
      // X / X ===> 1
      return Int(bits_, 1);
    if (lhs_words == 1)  // rhsWords is 1 if lhsWords is 1.
      // All high words are zero, just use native divide
      return Int(bits_, this->val_.pv_[0] / other.val_.pv_[0]);

    // We have to compute it the hard way. Invoke the Knuth divide algorithm.
    Int quotient(bits_, 0);
    div(val_.pv_, lhs_words, other.val_.pv_, rhs_words, quotient.val_.pv_,
        nullptr);
    return quotient;
  }

  [[nodiscard]] Int udiv(uint64_t other) const {
    LPS_CHECK_ERROR(kTag, other != 0, "divide by zero?");

    // First, deal with the easy case
    if (is_single())
      return Int(bits_, val_.v_ / other);

    // Get some facts about the LHS words.
    uint64_t lhs_words = n_words(active_bits());

    // Deal with some degenerate cases
    if (!lhs_words)
      // 0 / X ===> 0
      return Int(bits_, 0);
    if (other == 1)
      // X / 1 ===> X
      return *this;
    if (this->ult(other))
      // X / Y ===> 0, iff X < Y
      return Int(bits_, 0);
    if (*this == other)
      // X / X ===> 1
      return Int(bits_, 1);
    if (lhs_words == 1)  // rhsWords is 1 if lhsWords is 1.
      // All high words are zero, just use native divide
      return Int(bits_, val_.pv_[0] / other);

    // We have to compute it the hard way. Invoke the Knuth divide algorithm.
    Int quotient(bits_, 0);  // to hold result.
    div(val_.pv_, lhs_words, &other, 1, quotient.val_.pv_, nullptr);
    return quotient;
  }

  [[nodiscard]] Int sdiv(const Int& other) const {
    if (is_negative()) {
      if (other.is_negative())
        return (-(*this)).udiv(-other);
      return -((-(*this)).udiv(other));
    }
    if (other.is_negative())
      return -(this->udiv(-other));
    return this->udiv(other);
  }

  [[nodiscard]] Int sdiv(int64_t other) const {
    if (is_negative()) {
      if (other < 0)
        return (-(*this)).udiv(-other);
      return -((-(*this)).udiv(other));
    }
    if (other < 0)
      return -(this->udiv(-other));
    return this->udiv(other);
  }

  [[nodiscard]] Int urem(const Int& other) const {
    LPS_CHECK_ERROR(kTag, bits_ == other.bits_, "bit widths must be the same");
    if (is_single()) {
      LPS_CHECK_ERROR(kTag, other.val_.v_ != 0, "Remainder by zero?");
      return Int(bits_, val_.v_ % other.val_.v_);
    }
    unsigned lhs_words = n_words(active_bits());
    unsigned rhs_bits = other.active_bits();
    unsigned rhs_words = n_words(rhs_bits);
    LPS_CHECK_ERROR(kTag, rhs_words,
                    "Performing remainder operation by zero ???");

    // Check the degenerate cases
    if (lhs_words == 0)
      // 0 % Y ===> 0
      return Int(bits_, 0);
    if (rhs_bits == 1)
      // X % 1 ===> 0
      return Int(bits_, 0);
    if (lhs_words < rhs_words || this->ult(other))
      // X % Y ===> X, iff X < Y
      return *this;
    if (*this == other)
      // X % X == 0;
      return Int(bits_, 0);
    if (lhs_words == 1)
      return Int(bits_, val_.pv_[0] % other.val_.pv_[0]);

    Int remainder(bits_, 0);
    div(val_.pv_, lhs_words, other.val_.pv_, rhs_words, nullptr,
        remainder.val_.pv_);
    return remainder;
  }

  [[nodiscard]] uint64_t urem(uint64_t rhs) const {
    LPS_CHECK_ERROR(kTag, rhs != 0, "Remainder by zero?");

    if (is_single())
      return val_.v_ % rhs;

    // Get some facts about the LHS
    unsigned lhs_words = n_words(active_bits());

    // Check the degenerate cases
    if (lhs_words == 0)
      // 0 % Y ===> 0
      return 0;
    if (rhs == 1)
      // X % 1 ===> 0
      return 0;
    if (this->ult(rhs))
      // X % Y ===> X, iff X < Y
      return zero_ext_value();
    if (*this == rhs)
      // X % X == 0;
      return 0;
    if (lhs_words == 1)
      // All high words are zero, just use native remainder
      return val_.pv_[0] % rhs;

    // We have to compute it the hard way. Invoke the Knuth divide algorithm.
    uint64_t remainder;
    div(val_.pv_, lhs_words, &rhs, 1, nullptr, &remainder);
    return remainder;
  }

  [[nodiscard]] Int srem(const Int& other) const {
    if (is_negative()) {
      if (other.is_negative())
        return -((-(*this)).urem(-other));
      return -((-(*this)).urem(other));
    }
    if (other.is_negative())
      return this->urem(-other);
    return this->urem(other);
  }

  [[nodiscard]] int64_t srem(int64_t other) const {
    if (is_negative()) {
      if (other < 0)
        return -((-(*this)).urem(-other));
      return -((-(*this)).urem(other));
    }
    if (other < 0)
      return this->urem(-other);
    return this->urem(other);
  }

  [[nodiscard]] bool is_max_signed_value() const {
    if (is_single()) {
      LPS_CHECK_ERROR(kTag, bits_, "zero width values not allowed");
      return val_.v_ == ((static_cast<word_type>(1) << (bits_ - 1)) - 1);
    }
    return !is_negative() && countr_one_slow_case() == bits_ - 1;
  }

  [[nodiscard]] bool is_min_signed_value() const {
    if (is_single()) {
      LPS_CHECK_ERROR(kTag, bits_, "zero width values not allowed");
      return val_.v_ == (static_cast<word_type>(1) << (bits_ - 1));
    }
    return is_negative() && countr_zero_slow_case() == bits_ - 1;
  }

  [[nodiscard]] uint64_t n_words() const { return n_words(bits_); }
  static unsigned n_words(uint64_t bits) {
    return (bits + kBitsOfWord - 1) / kBitsOfWord;
  }
  [[nodiscard]] bool is_single() const { return bits_ <= kBitsOfWord; }
  [[nodiscard]] bool is_negative() const { return (*this)[bits_ - 1]; }
  [[nodiscard]] bool is_nonnegative() const { return !is_negative(); }
  [[nodiscard]] bool is_sign_bits() const { return (*this)[bits_ - 1]; }
  [[nodiscard]] uint64_t n_sign_bits() const {
    return is_negative() ? countl_one() : countl_zero();
  }
  [[nodiscard]] uint64_t limited_value(uint64_t limit = UINT64_MAX) const {
    return ugt(limit) ? limit : zero_ext_value();
  }
  [[nodiscard]] bool is_all_one() const {
    if (bits_ == 0)
      return true;
    if (is_single())
      return val_.v_ == kWordMax >> (kBitsOfWord - bits_);
    return countr_one_slow_case() == bits_;
  }

  [[nodiscard]] uint64_t significant_bits() const {
    return bits_ - n_sign_bits() + 1;
  }

  [[nodiscard]] bool is_int_n(uint64_t N) const { return active_bits() <= N; }

  [[nodiscard]] bool is_signed_int_n(uint64_t N) const {
    return significant_bits() <= N;
  }

  [[nodiscard]] uint64_t min_signed_bit() const { return significant_bits(); }

  [[nodiscard]] word_type word(uint64_t pos) const {
    return is_single() ? val_.v_ : val_.pv_[which_word(pos)];
  }

  void string(vec::details::TemplateCommon<char>& str, unsigned radix,
              bool Signed, bool format_asc_literal = false) const {
    LPS_CHECK_ERROR(kTag,
                    (radix == 10 || radix == 8 || radix == 16 || radix == 2),
                    "Radix should be 2, 8, 10, 16.");

    const char* prefix = "";
    if (format_asc_literal) {
      switch (radix) {
        case 2:
          prefix = "0b";
          break;
        case 8:
          prefix = "0";
          break;
        case 10:
          break;
        case 16:
          prefix = "0x";
          break;
        default:
          unreachable(kTag);
      }
    }

    if (is_zero()) {
      while (*prefix) {
        str.append(*prefix);
        ++prefix;
      };
      str.append('0');
      return;
    }

    static const char kDigits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    if (is_single()) {
      char buffer[65];
      char* buffer_ptr = std::end(buffer);

      uint64_t n;
      if (!Signed) {
        n = zero_ext_value();
      } else {
        int64_t ii = sign_ext_value();
        if (ii >= 0) {
          n = ii;
        } else {
          str.append('-');
          n = -static_cast<uint64_t>(ii);
        }
      }

      while (*prefix) {
        str.append(*prefix);
        ++prefix;
      };

      while (n) {
        *--buffer_ptr = kDigits[n % radix];
        n /= radix;
      }
      {
        for (const auto* p = buffer_ptr; p != std::end(buffer); p++) {
          str.append(*p);
        }
      }

      return;
    }

    Int tmp(*this);

    if (Signed && is_negative()) {
      tmp.negate();
      str.append('-');
    }

    while (*prefix) {
      str.append(*prefix);
      ++prefix;
    };

    auto start_dig = str.size();

    if (radix == 2 || radix == 8 || radix == 16) {
      unsigned shift_size = (radix == 16 ? 4 : (radix == 8 ? 3 : 1));
      unsigned mask_size = radix - 1;

      while (!tmp.is_zero()) {
        uint64_t digit = (tmp.raw_data()[0]) & mask_size;
        str.append(kDigits[digit]);
        tmp.lshr_inplace(shift_size);
      }
    } else {
      while (!tmp.is_zero()) {
        uint64_t digit;
        udivrem(tmp, radix, tmp, digit);
        LPS_CHECK_ERROR(kTag, digit < radix, "divide failed");
        str.append(kDigits[digit]);
      }
    }

    std::reverse(str.begin() + start_dig, str.end());
  }

  bool operator!() const { return is_zero(); }

  Int& operator=(const Int& rhs) {

    if (is_single() && rhs.is_single()) {
      val_.v_ = rhs.val_.v_;
      bits_ = rhs.bits_;
      return *this;
    }

    assign_slow_case(rhs);
    return *this;
  }

  Int& operator=(Int&& that) {

    if (this == &that)
      return *this;

    lps_assert(kTag, this != &that);
    if (!is_single())
      delete[] val_.pv_;

    memcpy(&val_, &that.val_, sizeof(val_));

    bits_ = that.bits_;
    that.bits_ = 0;
    return *this;
  }

  Int& operator=(uint64_t rhs) {
    if (is_single()) {
      val_.v_ = rhs;
      clear_bits();
      return *this;
    }
    val_.pv_[0] = rhs;
    memset(val_.pv_ + 1, 0, (n_words() - 1) * kWordSize);
    return *this;
  }
  // Int operator-() const {
  //   auto o = *this;
  //   o.negate();
  //   return o;
  // }

  Int& operator+=(const Int& other) { return add(other); }
  Int& operator+=(uint64_t other) { return add(other); }
  Int& operator-=(const Int& other) { return sub(other); }
  Int& operator-=(uint64_t other) { return sub(other); }
  Int& operator*=(const Int& other) {
    *this = *this * other;
    return *this;
  }

  Int& operator*=(uint64_t other) {
    if (is_single()) {
      val_.v_ *= other;
    } else {
      mul_parts(val_.pv_, val_.pv_, other, 0, n_words(), n_words(), false);
    }
    return clear_bits();
  }
  Int operator*(const Int& other) const {
    lps_assert(kTag, bits_ == other.bits_);
    if (is_single())
      return Int(bits_, val_.v_ * other.val_.v_);
    Int r(alloc(n_words()), bits());
    mul(r.val_.pv_, val_.pv_, other.val_.pv_, n_words());
    r.clear_bits();
    return r;
  }

  bool operator==(const Int& other) const {
    LPS_CHECK_ERROR(
        kTag, bits_ == other.bits_ && "comparison requires equal bit widths");
    if (is_single())
      return val_.v_ == other.val_.v_;
    return equal_slow_case(other);
  }

  Int& operator++() {
    if (is_single())
      ++val_.v_;
    else
      increment(val_.pv_, n_words());
    return clear_bits();
  }

  Int& operator--() {
    if (is_single())
      --val_.v_;
    else
      decrement(val_.pv_, n_words());
    return clear_bits();
  }

  Int& operator<<=(unsigned shift_amt) {
    LPS_CHECK_ERROR(kTag, shift_amt <= bits_, "invalid shift amount");
    if (is_single()) {
      if (shift_amt == bits_)
        val_.v_ = 0;
      else
        val_.v_ <<= shift_amt;
      return clear_bits();
    }
    shift_left(val_.pv_, n_words(), shift_amt);
    clear_bits();
    return *this;
  }

  Int& operator<<=(const Int& shit_amt) {
    *this <<= shit_amt.limited_value(bits_);
    return *this;
  }

  Int operator<<(const Int& bits) const { return shl(bits); }
  Int operator<<(uint64_t bits) const { return shl(bits); }

  void flip() {
    if (is_single()) {
      val_.v_ ^= kWordMax;
      clear_bits();
    } else {
      complement(val_.pv_, n_words());
      clear_bits();
    }
  }

  [[nodiscard]] Int trunc(uint64_t width) const {
    LPS_CHECK_ERROR(kTag, width <= bits_, "invalid Int Truncate request");

    if (width <= kBitsOfWord)
      return Int(width, raw_data()[0]);

    if (width == bits_)
      return *this;

    Int result(alloc(n_words(width)), width);

    uint64_t i;
    for (i = 0; i != width / kBitsOfWord; i++)
      result.val_.pv_[i] = val_.pv_[i];

    uint64_t bits = (0 - width) % kBitsOfWord;
    if (bits != 0)
      result.val_.pv_[i] = val_.pv_[i] << bits >> bits;

    return result;
  }

  [[nodiscard]] uint64_t active_bits() const { return bits_ - countl_zero(); }
  [[nodiscard]] uint64_t significand_bits() const {
    return bits_ - n_sign_bits() + 1;
  }

  template <auto FV, auto FPV>
  Int& op_logical(const Int& other) {
    LPS_CHECK_ERROR(kTag, bits_ == other.bits_, "bit widths must be the same");
    if (is_single())
      val_.v_ = FV(val_.v_, other.val_.v_);
    else
      FPV(*this, other);
    return *this;
  }

  Int& operator&=(const Int& other) {
    return op_logical<[](word_type a, word_type b) -> word_type {
      return a & b;
    },
                      and_assign_slow_case>(other);
  }

  Int& operator&=(uint64_t other) {
    if (is_single()) {
      val_.v_ &= other;
      return *this;
    }
    val_.pv_[0] &= other;
    memset(val_.pv_ + 1, 0, (n_words() - 1) * kWordSize);
    return *this;
  }

  Int& operator|=(const Int& other) {
    return op_logical<[](word_type a, word_type b) -> word_type {
      return a | b;
    },
                      or_assign_slow_case>(other);
  }

  Int& operator|=(uint64_t other) {
    if (is_single()) {
      val_.v_ |= other;
      return clear_bits();
    }
    val_.pv_[0] |= other;
    return *this;
  }

  Int& operator^=(const Int& other) {
    return op_logical<[](word_type a, word_type b) -> word_type {
      return a ^ b;
    },
                      xor_assign_slow_case>(other);
  }

  Int& operator^=(uint64_t other) {
    if (is_single()) {
      val_.v_ ^= other;
      return clear_bits();
    }
    val_.pv_[0] ^= other;
    return *this;
  }

  bool operator==(uint64_t other) const {
    return (is_single() || active_bits() <= 64) && zero_ext_value() == other;
  }
  bool operator!=(const Int& other) const { return !((*this) == other); }

  bool operator!=(uint64_t Val) const { return !((*this) == Val); }

  void negate() {
    flip_bits();
    ++(*this);
  }

  static Int max_value(uint64_t numBits) { return all_ones(numBits); }

  [[nodiscard]] Int trunc_usat(uint64_t width) const {
    LPS_CHECK_ERROR(kTag, width <= bits_, "invalid Int Truncate request");
    if (is_int_n(width))
      return trunc(width);
    return max_value(width);
  }

  [[nodiscard]] Int trunc_ssat(uint64_t width) const {
    LPS_CHECK_ERROR(kTag, width <= bits_, "invalid Int Truncate request");

    if (is_signed_int_n(width))
      return trunc(width);

    return is_negative() ? signed_min_value(width) : signed_max_value(width);
  }

  bool operator[](uint64_t bit_pos) const {
    LPS_CHECK_ERROR(kTag, bit_pos < bits(), "bit position out of bounds!");
    return (mask_bit(bit_pos) & word(bit_pos)) != 0;
  }

 protected:
  Int& add(const Int& other) {
    lps_assert(kTag, bits_ == other.bits_);
    if (is_single())
      val_.v_ += other.val_.v_;
    else
      add(val_.pv_, other.val_.pv_, 0, n_words());
    return clear_bits();
  }
  Int& add(uint64_t other) {
    if (is_single())
      val_.v_ += other;
    else
      add_parts(val_.pv_, other, n_words());
    return clear_bits();
  }

  Int& sub(const Int& other) {
    lps_assert(kTag, bits_ == other.bits_);
    if (is_single())
      val_.v_ -= other.val_.v_;
    else
      sub(val_.pv_, other.val_.pv_, 0, n_words());
    return clear_bits();
  }
  Int& sub(uint64_t other) {
    if (is_single())
      val_.v_ -= other;
    else
      sub_parts(val_.pv_, other, n_words());
    return clear_bits();
  }

  template <auto F>
  static void op_assign_slow_case(Int& me, const Int& other) {
    lps_assert(kTag, !me.is_single() && !other.is_single());
    word_type* dst = me.val_.pv_;
    word_type* rhs = other.val_.pv_;
    for (size_t i = 0, e = me.n_words(); i != e; ++i)
      dst[i] = F(dst[i], rhs[i]);
  }
  static void and_assign_slow_case(Int& me, const Int& rhs) {
    op_assign_slow_case<[](word_type a, word_type b) -> word_type {
      return a & b;
    }>(me, rhs);
  }

  static void or_assign_slow_case(Int& me, const Int& rhs) {
    op_assign_slow_case<[](word_type a, word_type b) -> word_type {
      return a | b;
    }>(me, rhs);
  }

  static void xor_assign_slow_case(Int& me, const Int& rhs) {
    op_assign_slow_case<[](word_type a, word_type b) -> word_type {
      return a ^ b;
    }>(me, rhs);
  }

  [[nodiscard]] bool equal_slow_case(const Int& other) const {
    return std::equal(val_.pv_, val_.pv_ + n_words(), other.val_.pv_);
  }

  void reallocate(uint64_t bits) {
    if (n_words() == n_words(bits)) {
      bits_ = bits;
      return;
    }

    if (!is_single())
      delete[] val_.pv_;

    bits_ = bits;

    if (!is_single())
      val_.pv_ = alloc(n_words());
  }

  void assign_slow_case(const Int& rhs) {

    if (this == &rhs)
      return;

    reallocate(rhs.bits());

    if (is_single())
      val_.v_ = rhs.val_.v_;
    else
      memcpy(val_.pv_, rhs.val_.pv_, n_words() * kWordSize);
  }

  [[nodiscard]] uint64_t countl_zero_slow_case() const {
    uint64_t count = 0;
    for (int i = n_words() - 1; i >= 0; --i) {
      uint64_t v = val_.pv_[i];
      if (v == 0)
        count += kBitsOfWord;
      else {
        count += details::countl_zero(v);
        break;
      }
    }
    uint64_t mod = bits_ % kBitsOfWord;
    count -= mod > 0 ? kBitsOfWord - mod : 0;
    return count;
  }
  [[nodiscard]] uint64_t countl_zero() const {
    if (is_single()) {
      uint64_t unused_bits = kBitsOfWord - bits_;
      return details::countl_zero(val_.v_) - unused_bits;
    }
    return countl_zero_slow_case();
  }

  [[nodiscard]] uint64_t countp_slow_case() const {
    uint64_t c = 0;
    for (uint64_t i = 0; i < n_words(); ++i)
      c += details::popcount(val_.pv_[i]);
    return c;
  }

  [[nodiscard]] uint64_t countl_one_slow_case() const {
    uint64_t high_word_bits = bits_ % kBitsOfWord;
    uint64_t shift;
    if (!high_word_bits) {
      high_word_bits = kBitsOfWord;
      shift = 0;
    } else {
      shift = kBitsOfWord - high_word_bits;
    }
    int i = n_words() - 1;
    uint64_t count = details::countl_one(val_.pv_[i] << shift);
    if (count == high_word_bits) {
      for (i--; i >= 0; --i) {
        if (val_.pv_[i] == kWordMax)
          count += kBitsOfWord;
        else {
          count += details::countl_one(val_.pv_[i]);
          break;
        }
      }
    }
    return count;
  }

  [[nodiscard]] uint64_t countr_zero_slow_case() const {
    uint64_t count = 0;
    uint64_t i = 0;
    for (; i < n_words() && val_.pv_[i] == 0; ++i)
      count += kBitsOfWord;
    if (i < n_words())
      count += details::countr_zero(val_.pv_[i]);
    return std::min(count, bits_);
  }

  [[nodiscard]] uint64_t countr_one_slow_case() const {
    uint64_t count = 0;
    uint64_t i = 0;
    for (; i < n_words() && val_.pv_[i] == kWordMax; ++i)
      count += kBitsOfWord;
    if (i < n_words())
      count += details::countr_one(val_.pv_[i]);
    lps_assert(kTag, count <= bits_);
    return count;
  }

  [[nodiscard]] uint64_t countl_one() const {
    if (is_single()) {
      uint64_t unused_bits = kBitsOfWord - bits_;
      return details::countl_one(val_.v_ << (kBitsOfWord - bits_));
    }
    return countl_one_slow_case();
  }

  [[nodiscard]] uint64_t countr_zero() const {
    if (is_single()) {
      unsigned tzeros = details::countr_zero(val_.v_);
      return (tzeros > bits_ ? bits_ : tzeros);
    }
    return countr_zero_slow_case();
  }

  static inline word_type* alloc_and_set(uint64_t n) {
    auto* result = new word_type[n];
    memset(result, 0, n * sizeof(word_type));
    return result;
  }
  inline static uint64_t* alloc(uint64_t n) { return new uint64_t[n]; }

  Int& clear_bits() {
    uint64_t word_bits = ((bits_ - 1) % kBitsOfWord) + 1;
    uint64_t mask = kWordMax >> (kBitsOfWord - word_bits);
    LPS_CHECK_ERROR(kTag, bits_ > 0, "bits should larger than 0");

    if (is_single()) {
      val_.v_ &= mask;
    } else {
      val_.pv_[n_words() - 1] &= mask;
    }
    return *this;
  }

  void init(uint64_t val, bool is_signed) {
    val_.pv_ = alloc_and_set(n_words());
    val_.pv_[0] = val;
    if (is_signed && static_cast<int64_t>(val) < 0) {
      for (unsigned i = 1; i < n_words(); ++i) {
        val_.pv_[i] = kWordMax;
      }
    }

    clear_bits();
  }

  void init(const Int& other) {
    val_.pv_ = alloc(n_words());
    memcpy(val_.pv_, other.val_.pv_, n_words() * kWordSize);
  }

  inline static unsigned digit(char c, uint8_t radix) {
    uint64_t r;

    if (radix == 16) {
      r = c - '0';
      if (r <= 9)
        return r;

      r = c - 'A';
      if (r <= radix - 11U)
        return r + 10;

      r = c - 'a';
      if (r <= radix - 11U)
        return r + 10;

      radix = 10;
    }

    r = c - '0';
    if (r < radix)
      return r;

    return -1U;
  }

  void form(const basic::StringRef& str, uint64_t bits, uint8_t radix) {
    lps_assert(kTag, !str.empty());
    lps_assert(kTag, radix == 2 || radix == 8 || radix == 10 || radix == 16);
    const auto* p = str.begin();
    size_t slen = str.size();
    bool neg = *p == '-';
    if (*p == '-' || *p == '+') {
      p++;
      slen--;
      LPS_CHECK_ERROR(kTag, slen, "needs a value.");
    }
    LPS_CHECK_ERROR(kTag, (slen <= bits || radix != 2),
                    "Insufficient bit width");
    LPS_CHECK_ERROR(kTag, ((slen - 1) * 3 <= bits || radix != 8),
                    "Insufficient bit width");
    LPS_CHECK_ERROR(kTag, ((slen - 1) * 4 <= bits || radix != 16),
                    "Insufficient bit width");
    LPS_CHECK_ERROR(kTag, (((slen - 1) * 64) / 22 <= bits || radix != 10),
                    "Insufficient bit width");

    if (is_single()) {
      val_.v_ = 0;
    } else {
      val_.pv_ = alloc_and_set(n_words());
    }

    unsigned shift = (radix == 16 ? 4 : radix == 8 ? 3 : radix == 2 ? 1 : 0);

    // Enter digit traversal loop
    for (const auto* e = str.end(); p != e; ++p) {
      if (*e == '\'') {
        continue;
      }
      unsigned t_digit = digit(*p, radix);
      LPS_CHECK_ERROR(kTag, t_digit < radix,
                      "Invalid character in digit string");

      // Shift or multiply the value by the radix
      if (slen > 1) {
        if (shift)
          *this <<= shift;
        else
          *this *= radix;
      }

      *this += t_digit;
    }
    if (neg)
      this->negate();
  }
  static inline uint64_t which_word(uint64_t pos) { return pos / kBitsOfWord; }
  static inline uint64_t which_bit(uint64_t pos) { return pos % kBitsOfWord; }
  static inline uint64_t mask_bit(uint64_t pos) {
    return 1ULL << which_bit(pos);
  }
  static inline word_type low_bit_mask(uint64_t bits) {
    lps_assert(kTag, bits != 0 && bits <= kBitsOfWord);
    return kWordMax >> (kBitsOfWord - bits);
  }

  static void clear_bit(word_type* parts, uint64_t bit) {
    parts[which_word(bit)] &= ~mask_bit(bit);
  }
  static inline word_type low_half(word_type part) {
    return part & low_bit_mask(kBitsOfWord / 2);
  }

  static inline word_type high_half(word_type part) {
    return part >> (kBitsOfWord / 2);
  }

  template <auto BOp0, auto BOp1, auto BOp2>
  static word_type op_with_carry(word_type* dst, const word_type* rhs,
                                 word_type c, uint64_t parts) {
    lps_assert(kTag, c <= 1);
    for (uint64_t i = 0; i < parts; i++) {
      word_type l = dst[i];
      if (c) {
        dst[i] = BOp0(dst[i], rhs[i] + 1);
        c = static_cast<word_type>(BOp1(dst[i], l));
      } else {
        dst[i] = BOp0(dst[i], rhs[i]);
        c = static_cast<word_type>(BOp2(dst[i], l));
      }
    }
    return c;
  }

  static word_type add(word_type* dst, const word_type* rhs, word_type c,
                       uint64_t parts) {
    return op_with_carry<
        [](word_type& a, const word_type& b) -> word_type& { return a += b; },
        [](word_type& a, const word_type& b) -> bool { return a <= b; },
        [](word_type& a, const word_type& b) -> bool {
          return a < b;
        }>(dst, rhs, c, parts);
  }
  static word_type sub(word_type* dst, const word_type* rhs, word_type c,
                       uint64_t parts) {
    return op_with_carry<
        [](word_type& a, const word_type& b) -> word_type& { return a -= b; },
        [](word_type& a, const word_type& b) -> bool { return a >= b; },
        [](word_type& a, const word_type& b) -> bool {
          return (a > b);
        }>(dst, rhs, c, parts);
  }

  static word_type add_parts(word_type* dst, word_type src, uint64_t parts) {
    for (uint64_t i = 0; i < parts; ++i) {
      dst[i] += src;
      if (dst[i] >= src)
        return 0;
      src = 1;
    }

    return 1;
  }

  static word_type sub_parts(word_type* dst, word_type src, uint64_t parts) {
    for (uint64_t i = 0; i < parts; ++i) {
      word_type d = dst[i];
      dst[i] -= src;
      if (src <= d)
        return 0;
      src = 1;
    }

    return 1;
  }

  static int mul_parts(word_type* dst, const word_type* src,
                       word_type multiplier, word_type carry,
                       uint64_t src_parts, uint64_t dst_parts, bool add) {
    lps_assert(kTag, dst <= src || dst >= src + src_parts);
    lps_assert(kTag, dst_parts <= src_parts + 1);

    uint64_t n = std::min(dst_parts, src_parts);

    for (uint64_t i = 0; i < n; i++) {
      word_type src_part = src[i];
      word_type low;
      word_type mid;
      word_type high;
      if (multiplier == 0 || src_part == 0) {
        low = carry;
        high = 0;
      } else {
        low = low_half(src_part) * low_half(multiplier);
        high = high_half(src_part) * high_half(multiplier);

        mid = low_half(src_part) * high_half(multiplier);
        high += high_half(mid);
        mid <<= kBitsOfWord / 2;
        if (low + mid < low)
          high++;
        low += mid;

        mid = high_half(src_part) * low_half(multiplier);
        high += high_half(mid);
        mid <<= kBitsOfWord / 2;
        if (low + mid < low)
          high++;
        low += mid;

        if (low + carry < low)
          high++;
        low += carry;
      }

      if (add) {
        if (low + dst[i] < low)
          high++;
        dst[i] += low;
      } else
        dst[i] = low;

      carry = high;
    }

    if (src_parts < dst_parts) {
      lps_assert(kTag, src_parts + 1 == dst_parts);
      dst[src_parts] = carry;
      return 0;
    }

    if (carry) {
      return 1;
    }

    if (multiplier) {
      for (unsigned i = dst_parts; i < src_parts; i++) {
        if (src[i]) {
          return 1;
        }
      }
    }
    return 0;
  }

  static void full_mul(word_type* dst, const word_type* lhs,
                       const word_type* rhs, uint64_t lhsParts,
                       uint64_t rhsParts) {
    if (lhsParts > rhsParts)
      return full_mul(dst, rhs, lhs, rhsParts, lhsParts);

    lps_assert(kTag, dst != lhs && dst != rhs);

    set(dst, 0, rhsParts);

    for (unsigned i = 0; i < lhsParts; i++)
      mul_parts(&dst[i], rhs, lhs[i], 0, rhsParts, rhsParts + 1, true);
  }

  static void assign(word_type* dst, const word_type* src, uint64_t parts) {
    for (uint64_t i = 0; i < parts; i++) {
      dst[i] = src[i];
    }
  }

  static bool is_zero(const word_type* src, uint64_t parts) {
    for (uint64_t i = 0; i < parts; i++) {
      if (src[i]) {
        return false;
      }
    }
    return true;
  }

  [[nodiscard]] bool is_zero() const {
    if (is_single())
      return val_.v_ == 0;
    return countl_zero_slow_case() == bits_;
  }

  static void shift_left(word_type* dst, uint64_t words, uint64_t count) {
    if (!count) {
      return;
    }

    uint64_t word_shift = std::min(count / kBitsOfWord, words);
    uint64_t bit_shift = count % kBitsOfWord;

    if (bit_shift == 0) {
      std::memmove(dst + word_shift, dst, (words - word_shift) * kWordSize);
    } else {
      while (words-- > word_shift) {
        dst[words] = dst[words - word_shift] << bit_shift;
        if (words > word_shift) {
          dst[words] |=
              dst[words - word_shift - 1] >> (kBitsOfWord - bit_shift);
        }
      }
    }

    std::memset(dst, 0, word_shift * kWordSize);
  }

  static void shift_right(word_type* dst, uint64_t words, uint64_t count) {
    if (!count) {
      return;
    }

    uint64_t word_shift = std::min(count / kBitsOfWord, words);
    uint64_t bit_shift = count % kBitsOfWord;

    uint64_t words2move = words - word_shift;

    if (bit_shift == 0) {
      std::memmove(dst, dst + word_shift, words2move * kWordSize);
    } else {
      for (unsigned i = 0; i != words2move; ++i) {
        dst[i] = dst[i + word_shift] >> bit_shift;
        if (i + 1 != words2move) {
          dst[i] |= dst[i + word_shift + 1] << (kBitsOfWord - bit_shift);
        }
      }
    }
    std::memset(dst + words2move, 0, word_shift * kWordSize);
  }

  static void extract(word_type* dst, uint64_t n_dst, const word_type* src,
                      uint64_t src_bits, uint64_t src_LSB) {
    uint64_t dst_parts = (src_bits + kBitsOfWord - 1) / kBitsOfWord;
    lps_assert(kTag, dst_parts <= n_dst);

    uint64_t first_src_part = src_LSB / kBitsOfWord;
    assign(dst, src + first_src_part, dst_parts);

    uint64_t shift = src_LSB % kBitsOfWord;
    shift_right(dst, dst_parts, shift);

    unsigned n = dst_parts * kBitsOfWord - shift;
    if (n < src_bits) {
      word_type mask = low_bit_mask(src_bits - n);
      dst[dst_parts - 1] |=
          ((src[first_src_part + dst_parts] & mask) << n % kBitsOfWord);
    } else if (n > src_bits) {
      if (src_bits % kBitsOfWord)
        dst[dst_parts - 1] &= low_bit_mask(src_bits % kBitsOfWord);
    }

    while (dst_parts < n_dst)
      dst[dst_parts++] = 0;
  }

  [[nodiscard]] Int extract_bits(uint64_t num_bits,
                                 uint64_t bit_position) const {
    LPS_ERROR(kTag, bit_position < bits_ &&
                        (num_bits + bit_position) <= bits_ &&
                        "Illegal bit extraction");

    if (is_single()) {
      return Int(num_bits, val_.v_ >> bit_position);
    }

    uint64_t lo_bit = which_bit(bit_position);
    uint64_t lo_word = which_bit(bit_position);
    uint64_t hi_word = which_bit(bit_position + num_bits - 1);

    if (lo_word == hi_word) {
      return Int(num_bits, val_.pv_[lo_word] >> lo_bit);
    }

    if (lo_bit == 0) {
      return Int(num_bits, val_.pv_ + lo_word, 1 + hi_word - lo_word);
    }

    // General case - shift + copy source words directly into place.
    Int result(num_bits, 0);
    uint64_t num_src_words = n_words();
    uint64_t num_dst_words = result.n_words();

    uint64_t* dst_ptr = result.is_single() ? &result.val_.v_ : result.val_.pv_;
    for (uint64_t word = 0; word < num_dst_words; ++word) {
      uint64_t w0 = val_.pv_[lo_word + word];
      uint64_t w1 = (lo_word + word + 1) < num_src_words
                        ? val_.pv_[lo_word + word + 1]
                        : 0;
      dst_ptr[word] = (w0 >> lo_bit) | (w1 << (kBitsOfWord - lo_bit));
    }

    return result.clear_bits();
  }

  static int extract_bit(const word_type* parts, uint64_t bit) {
    return static_cast<int>((parts[which_word(bit)] & mask_bit(bit)) != 0);
  }

  static uint64_t partMSB(word_type value) {
    return details::find_last_set(value);
  }
  static uint64_t partLSB(word_type value) {
    return details::find_first_set(value);
  }

  static uint64_t LSB(const word_type* parts, uint64_t n) {
    for (unsigned i = 0; i < n; i++) {
      if (parts[i] != 0) {
        auto lsb = partLSB(parts[i]);
        return lsb + i * kBitsOfWord;
      }
    }

    return -1ULL;
  }

  static void set_leastSignificant_bits(word_type* dst, uint64_t parts,
                                        uint64_t bits) {
    uint64_t i = 0;
    while (bits > kBitsOfWord) {
      dst[i++] = ~static_cast<word_type>(0);
      bits -= kBitsOfWord;
    }

    if (bits)
      dst[i++] = ~static_cast<word_type>(0) >> (kBitsOfWord - bits);

    while (i < parts)
      dst[i++] = 0;
  }

  static void negate(word_type* dst, uint64_t parts) {
    complement(dst, parts);
    increment(dst, parts);
  }

  static uint64_t MSB(const word_type* parts, uint64_t n) {
    do {
      --n;
      if (parts[n] != 0) {
        uint64_t msb = partMSB(parts[n]);
        return msb + n * kBitsOfWord;
      }
    } while (n);

    return -1ULL;
  }

  static void set(word_type* dst, word_type part, uint64_t parts) {
    lps_assert(kTag, parts > 0);
    dst[0] = part;
    for (uint64_t i = 1; i < parts; i++) {
      dst[i] = 0;
    }
  }

  static int mul(word_type* dst, const word_type* lhs, const word_type* rhs,
                 uint64_t parts) {
    lps_assert(kTag, dst != lhs && dst != rhs);

    int overflow = 0;
    set(dst, 0, parts);

    for (uint64_t i = 0; i < parts; i++) {
      overflow |= mul_parts(&dst[i], lhs, rhs[i], 0, parts, parts - i, true);
    }

    LPS_CHECK_ERROR(kTag, overflow == 0, "operation * overflow!");

    return overflow;
  }

  static void complement(word_type* dst, uint64_t parts) {
    for (uint64_t i = 0; i < parts; i++)
      dst[i] = ~dst[i];
  }

  void flip_bits_slow_case() {
    complement(val_.pv_, n_words());
    clear_bits();
  }

  void flip_bits() {
    if (is_single()) {
      val_.v_ ^= kWordMax;
      clear_bits();
    } else {
      flip_bits_slow_case();
    }
  }

  static bool div(word_type* lhs, const word_type* rhs, word_type* remainder,
                  word_type* srhs, uint64_t parts) {
    lps_assert(kTag, lhs != remainder && lhs != srhs && remainder != srhs);

    uint64_t shift_count = MSB(rhs, parts) + 1;
    if (shift_count == 0) {
      return true;
    }

    shift_count = parts * kBitsOfWord - shift_count;
    uint64_t n = shift_count / kBitsOfWord;
    word_type mask = static_cast<word_type>(1) << (shift_count % kBitsOfWord);

    assign(srhs, rhs, parts);
    shift_left(srhs, parts, shift_count);
    assign(remainder, lhs, parts);
    set(lhs, 0, parts);

    for (;;) {
      int compare_result = compare(remainder, srhs, parts);
      if (compare_result >= 0) {
        sub(remainder, srhs, 0, parts);
        lhs[n] |= mask;
      }

      if (shift_count == 0) {
        break;
      }
      shift_count--;
      shift_right(srhs, parts, 1);
      if ((mask >>= 1) == 0) {
        mask = static_cast<word_type>(1) << (kBitsOfWord - 1);
        n--;
      }
    }

    return false;
  }

  [[nodiscard]] int compare(const Int& other) const {
    LPS_CHECK_ERROR(kTag, bits_ == other.bits_,
                    "bit widths must be same for comparison");
    if (is_single())
      return val_.v_ < other.val_.v_ ? -1 : val_.v_ > other.val_.v_;

    return compare(val_.pv_, other.val_.pv_, n_words());
  }

  static int64_t sign_ext64(uint64_t x, uint64_t b) {
    LPS_CHECK_ERROR(kTag, b > 0, "bits width can't be 0.");
    LPS_CHECK_ERROR(kTag, b <= 64, "bits width out of range.");
    return static_cast<int64_t>(x << (64 - b)) >> (64 - b);
  }

  [[nodiscard]] int compare_signed(const Int& other) const {
    LPS_CHECK_ERROR(kTag, bits_ == other.bits_,
                    "bit widths must be same for comparison");
    if (is_single()) {
      int64_t lhs_sext = sign_ext64(val_.v_, bits_);
      int64_t rhs_sext = sign_ext64(other.val_.v_, bits_);
      return lhs_sext < rhs_sext ? -1 : lhs_sext > rhs_sext;
    }

    bool lhs_neg = is_negative();
    bool rhs_neg = other.is_negative();

    if (lhs_neg != rhs_neg)
      return lhs_neg ? -1 : 1;

    return compare(val_.pv_, other.val_.pv_, n_words());
  }

  [[nodiscard]] bool eq(const Int& other) const { return (*this) == other; }

  [[nodiscard]] bool ne(const Int& other) const { return !((*this) == other); }

  [[nodiscard]] Int relative_lshr(int64_t RelativeShift) const {
    return RelativeShift > 0 ? lshr(RelativeShift) : shl(-RelativeShift);
  }

  [[nodiscard]] Int relative_lshl(int64_t RelativeShift) const {
    return relative_lshr(-RelativeShift);
  }

  [[nodiscard]] Int relative_ashr(int64_t RelativeShift) const {
    return RelativeShift > 0 ? ashr(RelativeShift) : shl(-RelativeShift);
  }

  [[nodiscard]] Int relative_ashl(int64_t RelativeShift) const {
    return relative_ashr(-RelativeShift);
  }

  void ashr_inplace(uint64_t shift_amt) {
    lps_assert(kTag, shift_amt <= bits_);
    if (is_single()) {
      int64_t s_ext_val = sign_ext64(val_.v_, bits_);
      if (shift_amt == bits_)
        val_.v_ = s_ext_val >> (kBitsOfWord - 1);
      else
        val_.v_ = s_ext_val >> shift_amt;
      clear_bits();
      return;
    }
    ashr_Slow_case(shift_amt);
  }

  void lshr_inplace(uint64_t Shift_amt) {
    LPS_CHECK_ERROR(kTag, Shift_amt <= bits_, "invalid shift amount");
    if (is_single()) {
      if (Shift_amt == bits_)
        val_.v_ = 0;
      else
        val_.v_ >>= Shift_amt;
      return;
    }
    lshr_slow_case(Shift_amt);
  }

  void lshr_slow_case(unsigned Shift_amt) {
    shift_right(val_.pv_, n_words(), Shift_amt);
  }

  static void knuth_div(uint32_t* u, uint32_t* v, uint32_t* q, uint32_t* r,
                        unsigned m, unsigned n);

  static void div(const word_type* lhs, uint64_t lhs_words,
                  const word_type* rhs, uint64_t rhs_words, word_type* quotient,
                  word_type* rem) {

    LPS_CHECK_ERROR(kTag, lhs_words >= rhs_words, "fractional result");

    uint64_t n = rhs_words * 2;
    uint64_t m = (lhs_words * 2) - n;

    uint32_t space[128];
    uint32_t* u = nullptr;
    uint32_t* v = nullptr;
    uint32_t* q = nullptr;
    uint32_t* r = nullptr;
    if ((rem ? 4 : 3) * n + 2 * m + 1 <= 128) {
      u = &space[0];
      v = &space[m + n + 1];
      q = &space[(m + n + 1) + n];
      if (rem)
        r = &space[(m + n + 1) + n + (m + n)];
    } else {
      u = new uint32_t[m + n + 1];
      v = new uint32_t[n];
      q = new uint32_t[m + n];
      if (rem)
        r = new uint32_t[n];
    }

    // Initialize the dividend
    memset(u, 0, (m + n + 1) * sizeof(uint32_t));
    for (unsigned i = 0; i < lhs_words; ++i) {
      uint64_t tmp = lhs[i];
      u[i * 2] = details::lo_32(tmp);
      u[i * 2 + 1] = details::hi_32(tmp);
    }
    u[m + n] = 0;  // this extra word is for "spill" in the Knuth algorithm.

    // Initialize the divisor
    memset(v, 0, (n) * sizeof(uint32_t));
    for (unsigned i = 0; i < rhs_words; ++i) {
      uint64_t tmp = rhs[i];
      v[i * 2] = details::lo_32(tmp);
      v[i * 2 + 1] = details::hi_32(tmp);
    }

    // initialize the quotient and remainder
    memset(q, 0, (m + n) * sizeof(uint32_t));
    if (rem)
      memset(r, 0, n * sizeof(uint32_t));

    // Now, adjust m and n for the Knuth division. n is the number of words in
    // the divisor. m is the number of words by which the dividend exceeds the
    // divisor (i.e. m+n is the length of the dividend). These sizes must not
    // contain any zero words or the Knuth algorithm fails.
    for (unsigned i = n; i > 0 && v[i - 1] == 0; i--) {
      n--;
      m++;
    }
    for (unsigned i = m + n; i > 0 && u[i - 1] == 0; i--)
      m--;

    // If we're left with only a single word for the divisor, Knuth doesn't work
    // so we implement the short division algorithm here. This is much simpler
    // and faster because we are certain that we can divide a 64-bit quantity
    // by a 32-bit quantity at hardware speed and short division is simply a
    // series of such operations. This is just like doing short division but we
    // are using base 2^32 instead of base 10.
    LPS_CHECK_ERROR(kTag, n != 0, "divide by zero?");
    if (n == 1) {
      uint32_t divisor = v[0];
      uint32_t remainder = 0;
      for (int i = m; i >= 0; i--) {
        uint64_t partial_dividend = details::make_64(remainder, u[i]);
        if (partial_dividend == 0) {
          q[i] = 0;
          remainder = 0;
        } else if (partial_dividend < divisor) {
          q[i] = 0;
          remainder = details::lo_32(partial_dividend);
        } else if (partial_dividend == divisor) {
          q[i] = 1;
          remainder = 0;
        } else {
          q[i] = details::lo_32(partial_dividend / divisor);
          remainder = details::lo_32(partial_dividend - (q[i] * divisor));
        }
      }
      if (r)
        r[0] = remainder;
    } else {
      // Now we're ready to invoke the Knuth classical divide algorithm. In this
      // case n > 1.
      knuth_div(u, v, q, r, m, n);
    }

    // If the caller wants the quotient
    if (quotient) {
      for (unsigned i = 0; i < lhs_words; ++i)
        quotient[i] = details::make_64(q[i * 2 + 1], q[i * 2]);
    }

    // If the caller wants the remainder
    if (rem) {
      for (unsigned i = 0; i < rhs_words; ++i)
        rem[i] = details::make_64(r[i * 2 + 1], r[i * 2]);
    }

    // Clean up the memory we allocated.
    if (u != &space[0]) {
      delete[] u;
      delete[] v;
      delete[] q;
      delete[] r;
    }
  }

  static uint64_t rotate_modulo(uint64_t bits_, const Int& rotate_amt) {
    if (bits_ == 0)
      return 0;
    uint64_t rot_bits = rotate_amt.bits();
    Int rot = rotate_amt;
    if (rot_bits < bits_) {
      rot = rotate_amt.zext(bits_);
    }
    rot = rot.urem(Int(rot.bits(), bits_));
    return rot.limited_value(bits_);
  }

 public:
  static void udivrem(const Int& LHS, const Int& rhs, Int& Quotient,
                      Int& Remainder) {
    LPS_CHECK_ERROR(kTag, LHS.bits_ == rhs.bits_,
                    "bit widths must be the same");
    unsigned bits = LHS.bits_;

    if (LHS.is_single()) {
      LPS_CHECK_ERROR(kTag, rhs.val_.v_ != 0, "divide by zero?");
      uint64_t quot_val = LHS.val_.v_ / rhs.val_.v_;
      uint64_t rem_val = LHS.val_.v_ % rhs.val_.v_;
      Quotient = Int(bits, quot_val);
      Remainder = Int(bits, rem_val);
      return;
    }

    // Get some size facts about the dividend and divisor
    unsigned lhs_words = n_words(LHS.active_bits());
    unsigned rhs_bits = rhs.active_bits();
    unsigned rhs_words = n_words(rhs_bits);
    LPS_CHECK_ERROR(kTag, rhs_words, "Performing divrem operation by zero ???");

    // Check the degenerate cases
    if (lhs_words == 0) {
      Quotient = Int(bits, 0);   // 0 / Y ===> 0
      Remainder = Int(bits, 0);  // 0 % Y ===> 0
      return;
    }

    if (rhs_bits == 1) {
      Quotient = LHS;            // X / 1 ===> X
      Remainder = Int(bits, 0);  // X % 1 ===> 0
    }

    if (lhs_words < rhs_words || LHS.ult(rhs)) {
      Remainder = LHS;          // X % Y ===> X, iff X < Y
      Quotient = Int(bits, 0);  // X / Y ===> 0, iff X < Y
      return;
    }

    if (LHS == rhs) {
      Quotient = Int(bits, 1);   // X / X ===> 1
      Remainder = Int(bits, 0);  // X % X ===> 0;
      return;
    }

    // Make sure there is enough space to hold the results.
    // NOTE: This assumes that reallocate won't affect any bits if it doesn't
    // change the size. This is necessary if Quotient or Remainder is aliased
    // with LHS or rhs.
    Quotient.reallocate(bits);
    Remainder.reallocate(bits);

    if (lhs_words == 1) {  // rhsWords is 1 if lhsWords is 1.
      // There is only one word to consider so use the native versions.
      uint64_t lhs_value = LHS.val_.pv_[0];
      uint64_t rhs_value = rhs.val_.pv_[0];
      Quotient = lhs_value / rhs_value;
      Remainder = lhs_value % rhs_value;
      return;
    }

    // Okay, lets do it the long way
    div(LHS.val_.pv_, lhs_words, rhs.val_.pv_, rhs_words, Quotient.val_.pv_,
        Remainder.val_.pv_);
    // Clear the rest of the Quotient and Remainder.
    std::memset(Quotient.val_.pv_ + lhs_words, 0,
                (n_words(bits) - lhs_words) * kWordSize);
    std::memset(Remainder.val_.pv_ + rhs_words, 0,
                (n_words(bits) - rhs_words) * kWordSize);
  }

  static void udivrem(const Int& LHS, uint64_t rhs, Int& Quotient,
                      uint64_t& Remainder) {
    LPS_CHECK_ERROR(kTag, rhs != 0, "Divide by zero?");
    unsigned bits = LHS.bits_;

    // First, deal with the easy case
    if (LHS.is_single()) {
      uint64_t quot_val = LHS.val_.v_ / rhs;
      Remainder = LHS.val_.v_ % rhs;
      Quotient = Int(bits, quot_val);
      return;
    }

    // Get some size facts about the dividend and divisor
    unsigned lhs_words = n_words(LHS.active_bits());

    // Check the degenerate cases
    if (lhs_words == 0) {
      Quotient = Int(bits, 0);  // 0 / Y ===> 0
      Remainder = 0;            // 0 % Y ===> 0
      return;
    }

    if (rhs == 1) {
      Quotient = LHS;  // X / 1 ===> X
      Remainder = 0;   // X % 1 ===> 0
      return;
    }

    if (LHS.ult(rhs)) {
      Remainder = LHS.zero_ext_value();  // X % Y ===> X, iff X < Y
      Quotient = Int(bits, 0);           // X / Y ===> 0, iff X < Y
      return;
    }

    if (LHS == rhs) {
      Quotient = Int(bits, 1);  // X / X ===> 1
      Remainder = 0;            // X % X ===> 0;
      return;
    }

    // Make sure there is enough space to hold the results.
    // NOTE: This assumes that reallocate won't affect any bits if it doesn't
    // change the size. This is necessary if Quotient is aliased with LHS.
    Quotient.reallocate(bits);

    if (lhs_words == 1) {  // rhsWords is 1 if lhsWords is 1.
      // There is only one word to consider so use the native versions.
      uint64_t lhs_value = LHS.val_.pv_[0];
      Quotient = lhs_value / rhs;
      Remainder = lhs_value % rhs;
      return;
    }

    // Okay, lets do it the long way
    div(LHS.val_.pv_, lhs_words, &rhs, 1, Quotient.val_.pv_, &Remainder);
    // Clear the rest of the Quotient.
    std::memset(Quotient.val_.pv_ + lhs_words, 0,
                (n_words(bits) - lhs_words) * kWordSize);
  }

  [[nodiscard]] bool ult(const Int& other) const { return compare(other) < 0; }

  [[nodiscard]] bool ult(uint64_t other) const {
    return (is_single() || active_bits() <= 64) && zero_ext_value() < other;
  }

  [[nodiscard]] bool slt(const Int& other) const {
    return compare_signed(other) < 0;
  }

  [[nodiscard]] bool slt(int64_t other) const {
    return (!is_single() && significant_bits() > 64) ? is_negative()
                                                     : sign_ext_value() < other;
  }

  [[nodiscard]] bool ule(const Int& other) const { return compare(other) <= 0; }

  [[nodiscard]] bool ule(uint64_t other) const { return !ugt(other); }

  [[nodiscard]] bool sle(const Int& other) const {
    return compare_signed(other) <= 0;
  }

  [[nodiscard]] bool sle(uint64_t other) const { return !sgt(other); }

  [[nodiscard]] bool ugt(const Int& other) const { return !ule(other); }

  [[nodiscard]] bool ugt(uint64_t other) const {

    return (!is_single() && active_bits() > 64) || zero_ext_value() > other;
  }

  [[nodiscard]] bool sgt(const Int& other) const { return !sle(other); }

  [[nodiscard]] bool sgt(int64_t rhs) const {
    return (!is_single() && significant_bits() > 64) ? !is_negative()
                                                     : sign_ext_value() > rhs;
  }

  [[nodiscard]] bool uge(const Int& other) const { return !ult(other); }

  [[nodiscard]] bool uge(uint64_t other) const { return !ult(other); }

  [[nodiscard]] bool sge(const Int& other) const { return !slt(other); }

  [[nodiscard]] bool sge(int64_t other) const { return !slt(other); }

  [[nodiscard]] Int zext_or_trunc(uint64_t width) const {
    if (bits_ < width)
      return zext(width);
    if (bits_ > width)
      return trunc(width);
    return *this;
  }

  [[nodiscard]] Int sext_or_trunc(unsigned width) const {
    if (bits_ < width)
      return sext(width);
    if (bits_ > width)
      return trunc(width);
    return *this;
  }

  static Int splat(uint64_t new_len, const Int& v) {
    LPS_CHECK_ERROR(kTag, new_len >= v.bits(),
                    "can't splat to smaller bit width!");

    Int val = v.zext(new_len);
    for (uint64_t i = v.bits(); i < new_len; i <<= 1)
      val |= val << i;

    return val;
  }

  [[nodiscard]] Int rotl(const Int& rotate_amt) const {
    return rotl(rotate_modulo(bits_, rotate_amt));
  }

  [[nodiscard]] Int rotl(uint64_t rotate_amt) const {
    if (bits_ == 0)
      return *this;
    rotate_amt %= bits_;
    if (rotate_amt == 0)
      return *this;
    return shl(rotate_amt) | lshr(bits_ - rotate_amt);
  }

  [[nodiscard]] Int rotr(const Int& rotate_amt) const {
    return rotr(rotate_modulo(bits_, rotate_amt));
  }

  [[nodiscard]] Int rotr(uint64_t rotate_amt) const {
    if (bits_ == 0)
      return *this;
    rotate_amt %= bits_;
    if (rotate_amt == 0)
      return *this;
    return lshr(rotate_amt) | shl(bits_ - rotate_amt);
  }

  static void sdivrem(const Int& lhs, const Int& rhs, Int& quotient,
                      Int& remainder) {
    if (lhs.is_negative()) {
      if (rhs.is_negative())
        Int::udivrem(-lhs, -rhs, quotient, remainder);
      else {
        Int::udivrem(-lhs, rhs, quotient, remainder);
        quotient.negate();
      }
      remainder.negate();
    } else if (rhs.is_negative()) {
      Int::udivrem(lhs, -rhs, quotient, remainder);
      quotient.negate();
    } else {
      Int::udivrem(lhs, rhs, quotient, remainder);
    }
  }

  static void sdivrem(const Int& lhs, int64_t rhs, Int& quotient,
                      int64_t& remainder) {
    uint64_t r = remainder;
    if (lhs.is_negative()) {
      if (rhs < 0)
        Int::udivrem(-lhs, -rhs, quotient, r);
      else {
        Int::udivrem(-lhs, rhs, quotient, r);
        quotient.negate();
      }
      r = -r;
    } else if (rhs < 0) {
      Int::udivrem(lhs, -rhs, quotient, r);
      quotient.negate();
    } else {
      Int::udivrem(lhs, rhs, quotient, r);
    }
    remainder = r;
  }

  Int sadd_ov(const Int& rhs, bool& overflow) const {
    Int res = *this + rhs;
    overflow = is_nonnegative() == rhs.is_nonnegative() &&
               res.is_nonnegative() != is_nonnegative();
    return res;
  }

  Int uadd_ov(const Int& rhs, bool& overflow) const {
    Int res = *this + rhs;
    overflow = res.ult(rhs);
    return res;
  }

  Int ssub_ov(const Int& rhs, bool& overflow) const {
    Int res = *this - rhs;
    overflow = is_nonnegative() != rhs.is_nonnegative() &&
               res.is_nonnegative() != is_nonnegative();
    return res;
  }

  Int usub_ov(const Int& rhs, bool& overflow) const {
    Int res = *this - rhs;
    overflow = res.ugt(*this);
    return res;
  }

  Int sdiv_ov(const Int& rhs, bool& overflow) const {
    // MININT/-1  -->  overflow.
    overflow = is_min_signed_value() && rhs.is_all_one();
    return sdiv(rhs);
  }

  Int smul_ov(const Int& rhs, bool& overflow) const {
    Int res = *this * rhs;
    if (rhs != 0)
      overflow =
          res.sdiv(rhs) != *this || (is_min_signed_value() && rhs.is_all_one());
    else
      overflow = false;
    return res;
  }

  Int umul_ov(const Int& rhs, bool& overflow) const {
    if (countl_zero() + rhs.countl_zero() + 2 <= bits_) {
      overflow = true;
      return *this * rhs;
    }

    Int res = lshr(1) * rhs;
    overflow = res.is_negative();
    res <<= 1;
    if ((*this)[0]) {
      res += rhs;
      if (res.ult(rhs))
        overflow = true;
    }
    return res;
  }

  Int sshl_ov(const Int& ShAmt, bool& overflow) const {
    overflow = ShAmt.uge(bits_);
    if (overflow)
      return Int(bits_, 0);

    if (is_nonnegative())
      overflow = ShAmt.uge(countl_zero());
    else
      overflow = ShAmt.uge(countl_one());

    return *this << ShAmt;
  }

  Int ushl_ov(const Int& sh_amt, bool& overflow) const {
    overflow = sh_amt.uge(bits());
    if (overflow)
      return Int(bits_, 0);

    overflow = sh_amt.ugt(countl_zero());

    return *this << sh_amt;
  }

  static Int signed_min_value(uint64_t bits) {
    Int r(bits, 0);
    r.set_bit(bits - 1);
    return r;
  }

  static Int min_value(uint64_t n) { return Int(n, 0); }

  static Int signed_max_value(uint64_t numBits) {
    Int r = all_ones(numBits);
    r.clear_bit(numBits - 1);
    return r;
  }

  [[nodiscard]] Int sadd_sat(const Int& rhs) const {
    bool overflow;
    Int res = sadd_ov(rhs, overflow);
    if (!overflow)
      return res;

    return is_negative() ? signed_min_value(bits_) : signed_max_value(bits_);
  }

  [[nodiscard]] Int uadd_sat(const Int& rhs) const {
    bool overflow;
    Int res = uadd_ov(rhs, overflow);
    if (!overflow)
      return res;

    return all_ones(bits_);
  }

  [[nodiscard]] Int ssub_sat(const Int& rhs) const {
    bool overflow;
    Int res = ssub_ov(rhs, overflow);
    if (!overflow)
      return res;

    return is_negative() ? signed_min_value(bits_) : signed_max_value(bits_);
  }

  [[nodiscard]] Int usub_sat(const Int& rhs) const {
    bool overflow;
    Int res = usub_ov(rhs, overflow);
    if (!overflow)
      return res;

    return Int(bits_, 0);
  }

  [[nodiscard]] Int smul_sat(const Int& rhs) const {
    bool overflow;
    Int res = smul_ov(rhs, overflow);
    if (!overflow)
      return res;

    bool res_neg = is_negative() ^ rhs.is_negative();

    return res_neg ? signed_min_value(bits_) : signed_max_value(bits_);
  }

  [[nodiscard]] Int umul_sat(const Int& rhs) const {
    bool overflow;
    Int res = umul_ov(rhs, overflow);
    if (!overflow)
      return res;

    return all_ones(bits_);
  }

  [[nodiscard]] Int sshl_sat(const Int& rhs) const {
    bool overflow;
    Int res = sshl_ov(rhs, overflow);
    if (!overflow)
      return res;

    return is_negative() ? signed_min_value(bits_) : signed_max_value(bits_);
  }

  [[nodiscard]] Int ushl_sat(const Int& rhs) const {
    bool overflow;
    Int res = ushl_ov(rhs, overflow);
    if (!overflow)
      return res;

    return all_ones(bits_);
  }

 protected:
  void ashr_Slow_case(uint64_t ShiftAmt) {

    if (!ShiftAmt)
      return;

    bool negative = is_negative();

    uint64_t word_shift = ShiftAmt / kBitsOfWord;
    uint64_t bit_shift = ShiftAmt % kBitsOfWord;

    uint64_t words_2_move = n_words() - word_shift;
    if (words_2_move != 0) {

      val_.pv_[n_words() - 1] =
          sign_ext64(val_.pv_[n_words() - 1], ((bits_ - 1) % kBitsOfWord) + 1);

      if (bit_shift == 0) {
        std::memmove(val_.pv_, val_.pv_ + word_shift, words_2_move * kWordSize);
      } else {
        for (uint64_t i = 0; i != words_2_move - 1; ++i)
          val_.pv_[i] =
              (val_.pv_[i + word_shift] >> bit_shift) |
              (val_.pv_[i + word_shift + 1] << (kBitsOfWord - bit_shift));

        val_.pv_[words_2_move - 1] =
            val_.pv_[word_shift + words_2_move - 1] >> bit_shift;

        val_.pv_[words_2_move - 1] =
            sign_ext64(val_.pv_[words_2_move - 1], kBitsOfWord - bit_shift);
      }
    }

    std::memset(val_.pv_ + words_2_move, negative ? -1 : 0,
                word_shift * kWordSize);
    clear_bits();
  }

  void shift() {}

  union {
    word_type v_;
    word_type* pv_;
  } val_;

  uint64_t bits_{1};
};

inline std::ostream& operator<<(std::ostream& s, const Int& val) {
  val.print(s, true);
  return s;
}

class SInt : public Int {
 public:
  explicit SInt() = default;

  explicit SInt(uint64_t bits, bool is_unsigned = true)
      : Int(bits, 0), is_unsigned_(is_unsigned) {}

  explicit SInt(Int I, bool is_unsigned = true)
      : Int(std::move(I)), is_unsigned_(is_unsigned) {}
  explicit SInt(const char* str) : SInt(StringRef(str)) {}
  explicit SInt(StringRef str) {
    LPS_CHECK_ERROR(kTag, !str.empty(), "invalid string length");

    auto n_bits = ((str.size() * 64) / 19) + 2;
    Int tmp(n_bits, str, 10);
    if (str[0] == '-') {
      auto min_bits = tmp.significand_bits();
      if (min_bits < n_bits)
        tmp = tmp.trunc(std::max<uint64_t>(1, min_bits));
      *this = SInt(tmp, false);
      return;
    }
    auto active_bits = tmp.active_bits();
    if (active_bits < n_bits)
      tmp = tmp.trunc(std::max<unsigned>(1, active_bits));

    *this = SInt(tmp, true);
  }

  [[nodiscard]] bool is_signed() const { return !is_unsigned_; }
  [[nodiscard]] bool is_unsigned() const { return is_unsigned_; }
  [[nodiscard]] int64_t ext_value() const {
    LPS_CHECK_ERROR(kTag, is_representable_by_int64(),
                    "Too many bits for int64_t");
    return is_signed() ? sign_ext_value() : zero_ext_value();
  }

  static SInt min_value(uint32_t n, bool un_signed) {
    return SInt(un_signed ? Int::min_value(n) : Int::signed_min_value(n),
                un_signed);
  }

  static SInt max_value(uint32_t n, bool un_signed) {
    return SInt(un_signed ? Int::max_value(n) : Int::signed_max_value(n),
                un_signed);
  }

  void is_signed(bool v) { is_unsigned_ = !v; }
  void is_unsigned(bool v) { is_unsigned_ = v; }

  [[nodiscard]] bool is_negative() const {
    return is_signed() && Int::is_negative();
  }

  [[nodiscard]] bool is_nonnegative() const { return !is_negative(); }

  [[nodiscard]] bool is_strictly_positive() const {
    return is_nonnegative() && !is_zero();
  }

  SInt& operator=(Int other) {
    Int::operator=(std::move(other));
    return *this;
  }

  SInt& operator=(uint64_t other) {
    Int::operator=(other);
    return *this;
  }

  [[nodiscard]] bool is_representable_by_int64() const {
    return is_signed() ? is_signed_int_n(64) : is_int_n(63);
  }

  [[nodiscard]] std::optional<int64_t> try_ext_value() const {
    return is_representable_by_int64() ? std::optional<int64_t>(ext_value())
                                       : std::nullopt;
  }

  [[nodiscard]] SInt trunc(uint64_t width) const {
    return SInt(Int::trunc(width), is_unsigned_);
  }

  [[nodiscard]] SInt extend(uint64_t width) const {
    if (is_unsigned_)
      return SInt(zext(width), is_unsigned_);
    return SInt(sext(width), is_unsigned_);
  }

  [[nodiscard]] Int ext_or_trunc(uint64_t width) const {
    if (is_unsigned_)
      return SInt(zext_or_trunc(width), is_unsigned_);
    return SInt(sext_or_trunc(width), is_unsigned_);
  }

  [[nodiscard]] SInt relative_shr(unsigned Amt) const {
    return is_unsigned_ ? SInt(relative_lshr(Amt), true)
                        : SInt(relative_ashr(Amt), false);
  }

  const SInt& operator%=(const SInt& other) {
    LPS_CHECK_ERROR(kTag, is_unsigned_ == other.is_unsigned_,
                    "signedness mismatch!");
    if (is_unsigned_)
      *this = urem(other);
    else
      *this = srem(other);
    return *this;
  }

  const SInt& operator/=(const SInt& other) {
    LPS_CHECK_ERROR(kTag, is_unsigned_ == other.is_unsigned_,
                    "signedness mismatch!");
    if (is_unsigned_)
      *this = udiv(other);
    else
      *this = sdiv(other);
    return *this;
  }

  SInt operator%(const SInt& other) const {
    LPS_CHECK_ERROR(kTag, is_unsigned_ == other.is_unsigned_,
                    "signedness mismatch!");
    return is_unsigned_ ? SInt(urem(other), true) : SInt(srem(other), false);
  }
  SInt operator/(const SInt& other) const {
    LPS_CHECK_ERROR(kTag, is_unsigned_ == other.is_unsigned_,
                    "signedness mismatch!");
    return is_unsigned_ ? SInt(udiv(other), true) : SInt(sdiv(other), false);
  }

  SInt operator>>(unsigned Amt) const {
    return is_unsigned_ ? SInt(lshr(Amt), true) : SInt(ashr(Amt), false);
  }

  SInt& operator>>=(unsigned Amt) {
    if (is_unsigned_)
      lshr_inplace(Amt);
    else
      ashr_inplace(Amt);
    return *this;
  }

  inline bool operator<(const SInt& other) const {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    return is_unsigned_ ? ult(other) : slt(other);
  }
  inline bool operator>(const SInt& other) const {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    return is_unsigned_ ? ugt(other) : sgt(other);
  }
  inline bool operator<=(const SInt& other) const {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    return is_unsigned_ ? ule(other) : sle(other);
  }
  inline bool operator>=(const SInt& other) const {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    return is_unsigned_ ? uge(other) : sge(other);
  }
  inline bool operator==(const SInt& other) const {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    return eq(other);
  }
  inline bool operator!=(const SInt& other) const {
    return !((*this) == other);
  }

  bool operator==(int64_t other) const {
    return compare_values(*this, get(other)) == 0;
  }

  bool operator!=(int64_t other) const {
    return compare_values(*this, get(other)) != 0;
  }
  bool operator<=(int64_t other) const {
    return compare_values(*this, get(other)) <= 0;
  }
  bool operator>=(int64_t other) const {
    return compare_values(*this, get(other)) >= 0;
  }
  bool operator<(int64_t other) const {
    return compare_values(*this, get(other)) < 0;
  }
  bool operator>(int64_t other) const {
    return compare_values(*this, get(other)) > 0;
  }

  SInt operator<<(unsigned Bits) const {
    return SInt(static_cast<const Int&>(*this) << Bits, is_unsigned_);
  }
  SInt& operator<<=(unsigned Amt) {
    static_cast<Int&>(*this) <<= Amt;
    return *this;
  }

  SInt& operator++() {
    ++(static_cast<Int&>(*this));
    return *this;
  }
  SInt& operator--() {
    --(static_cast<Int&>(*this));
    return *this;
  }
  SInt operator++(int) {
    return SInt(++static_cast<Int&>(*this), is_unsigned_);
  }
  SInt operator--(int) {
    return SInt(--static_cast<Int&>(*this), is_unsigned_);
  }
  SInt operator-() const {
    return SInt(-static_cast<const Int&>(*this), is_unsigned_);
  }
  SInt& operator+=(const SInt& other) {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    static_cast<Int&>(*this) += other;
    return *this;
  }
  SInt& operator-=(const SInt& other) {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    static_cast<Int&>(*this) -= other;
    return *this;
  }
  SInt& operator*=(const SInt& other) {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    static_cast<Int&>(*this) *= other;
    return *this;
  }
  SInt& operator&=(const SInt& other) {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    static_cast<Int&>(*this) &= other;
    return *this;
  }
  SInt& operator|=(const SInt& other) {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    static_cast<Int&>(*this) |= other;
    return *this;
  }
  SInt& operator^=(const SInt& other) {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    static_cast<Int&>(*this) ^= other;
    return *this;
  }

  SInt operator&(const SInt& other) const {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    return SInt(static_cast<const Int&>(*this) & other, is_unsigned_);
  }

  SInt operator|(const SInt& other) const {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    return SInt(static_cast<const Int&>(*this) | other, is_unsigned_);
  }

  SInt operator^(const SInt& other) const {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    return SInt(static_cast<const Int&>(*this) ^ other, is_unsigned_);
  }

  SInt operator*(const SInt& other) const {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    return SInt(static_cast<const Int&>(*this) * other, is_unsigned_);
  }
  SInt operator+(const SInt& other) const {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    return SInt(static_cast<const Int&>(*this) + other, is_unsigned_);
  }
  SInt operator-(const SInt& other) const {
    LPS_CHECK_ERROR(
        kTag, is_unsigned_ == other.is_unsigned_ && "signedness mismatch!");
    return SInt(static_cast<const Int&>(*this) - other, is_unsigned_);
  }
  SInt operator~() const {
    return SInt(~static_cast<const Int&>(*this), is_unsigned_);
  }

  static SInt get(int64_t X) { return SInt(Int(64, X), false); }
  static SInt get_unsigned(uint64_t X) { return SInt(Int(64, X), true); }

  static int compare_values(const SInt& I1, const SInt& I2) {
    if (I1.bits() == I2.bits() && I1.is_signed() == I2.is_signed())
      return I1.is_unsigned_ ? I1.compare(I2) : I1.compare_signed(I2);

    if (I1.bits() > I2.bits())
      return compare_values(I1, I2.extend(I1.bits()));
    if (I2.bits() > I1.bits())
      return compare_values(I1.extend(I2.bits()), I2);

    if (I1.is_signed()) {
      LPS_CHECK_ERROR(kTag, !I2.is_signed(), "expected signed mismatch");
      if (I1.is_negative())
        return -1;
    } else {
      LPS_CHECK_ERROR(kTag, I2.is_signed(), "expected signed mismatch");
      if (I2.is_negative())
        return 1;
    }

    return I1.compare(I2);
  }

 private:
  bool is_unsigned_{false};
};

inline bool operator==(int64_t V1, const SInt& V2) {
  return V2 == V1;
}
inline bool operator!=(int64_t V1, const SInt& V2) {
  return V2 != V1;
}
inline bool operator<=(int64_t V1, const SInt& V2) {
  return V2 >= V1;
}
inline bool operator>=(int64_t V1, const SInt& V2) {
  return V2 <= V1;
}
inline bool operator<(int64_t V1, const SInt& V2) {
  return V2 > V1;
}
inline bool operator>(int64_t V1, const SInt& V2) {
  return V2 < V1;
}

namespace details {

struct FloatSemantics;

enum class FloatNonFiniteBehavior : uint8_t {
  kUnknown = 0,
  kIEEE754,
  kNanOnly,
};

class FloatBase {
 public:
  static constexpr const char* kTag = "FloatBase";
  using int_part_type = Int::word_type;
  using exp_part_type = int32_t;
  static constexpr uint64_t kIntPartBits = Int::kBitsOfWord;
  enum class Semantics : uint8_t {
    kUnknown = 0,
    kIEEEhalf,
    kBFloat,
    kIEEEsingle,
    kIEEEdouble,
    kIEEEquad,
    kMaxSemantics = kIEEEquad,
  };
  enum class LostFraction : uint8_t {
    kUnknown = 0,
    kExactlyZero,
    kLessThanHalf,
    kExactlyHalf,
    kMoreThanHalf
  };

  enum class FloatCategory : uint8_t {
    kUnknown = 0,
    kInfinity,
    kNaN,
    kNormal,
    kZero
  };

  enum class RoundingMode : int8_t {
    kUnknown = -1,
    kTowardZero = 0,
    kNearestTiesToEven = 1,
    kTowardPositive = 2,
    kTowardNegative = 3,
    kNearestTiesToAway = 4,
    kDynamic = 7,
  };

  enum class Compareresult : uint8_t {
    kUnknown = 0,
    kLessThan,
    kEqual,
    kGreaterThan,
    kUnordered
  };

  enum class OpStatus : uint8_t {
    kOK = 0x00,
    kInvalidOp = 0x01,
    kDivByZero = 0x02,
    kOverflow = 0x04,
    kUnderflow = 0x08,
    kInexact = 0x10
  };

  enum class IlogbErrorKinds {
    kZero = INT_MIN + 1,
    kNaN = INT_MIN,
    kInf = INT_MAX
  };

  static const FloatSemantics& Semantics2FloatSemantics(Semantics S);
  static Semantics FloatSemantics2Semantics(const FloatSemantics& Sem);
  static const FloatSemantics& IEEEhalf();
  static const FloatSemantics& BFloat();
  static const FloatSemantics& IEEEsingle();
  static const FloatSemantics& IEEEdouble();
  static const FloatSemantics& IEEEquad();

  static const unsigned int kMaxExponent = 16383;
  static const unsigned int kMaxPrecision = 113;
  static const unsigned int kMaxPowerOfFiveExponent =
      kMaxExponent + kMaxPrecision - 1;
  static const unsigned int kMaxPowerOfFiveParts =
      2 + ((kMaxPowerOfFiveExponent * 815) / (351 * FloatBase::kIntPartBits));
};

inline std::ostream& operator<<(std::ostream& s, const FloatBase::OpStatus& v) {
  switch (v) {
    case FloatBase::OpStatus::kOK:
      s << "OK";
      break;
    case FloatBase::OpStatus::kInvalidOp:
      s << "InvalidOp";
      break;
    case FloatBase::OpStatus::kDivByZero:
      s << "DivByZero";
      break;
    case FloatBase::OpStatus::kOverflow:
      s << "Overflow";
      break;
    case FloatBase::OpStatus::kUnderflow:
      s << "Underflow";
      break;
    case FloatBase::OpStatus::kInexact:
      s << "Inexact";
      break;
  }
  return s;
}

struct FloatSemantics {
  FloatBase::exp_part_type max_exp_;
  FloatBase::exp_part_type min_exp_;
  uint64_t precision_;
  uint64_t bits_;
  FloatNonFiniteBehavior non_finite_behavior_{FloatNonFiniteBehavior::kIEEE754};

  [[nodiscard]] bool is_representable_by(const FloatSemantics& other) const {
    return max_exp_ <= other.max_exp_ && min_exp_ >= other.min_exp_ &&
           precision_ <= other.precision_;
  }
};

enum class UninitializedTag : uint8_t { kUninitialized };

class IEEEFloat : public FloatBase {
 public:
  friend class Float;
  explicit IEEEFloat(const FloatSemantics& sem) {
    init(&sem);
    make_zero(false);
  }
  IEEEFloat(const FloatSemantics& sem, int_part_type value) {
    init(&sem);
    sign_ = 0;
    category_ = FloatCategory::kNormal;
    Int::set(significand_parts(), 0, n_parts());
    exp_ = sem.precision_ - 1;
    significand_parts()[0] = value;
    normalize(RoundingMode::kNearestTiesToEven, LostFraction::kExactlyZero);
  }
  IEEEFloat(const FloatSemantics& sem, UninitializedTag) : IEEEFloat(sem) {}
  IEEEFloat(const FloatSemantics& sem, const Int& api) {
    init_from_int(&sem, api);
  }
  explicit IEEEFloat(double d);
  explicit IEEEFloat(float f);
  IEEEFloat(const IEEEFloat& other) {
    init(other.sem_);
    assign(other);
  }
  IEEEFloat(IEEEFloat&& other);

  [[nodiscard]] Int bit_cast_to_int() const;

  [[nodiscard]] float to_float() const {
    LPS_CHECK_ERROR(kTag, sem_ == &IEEEsingle(),
                    "Float semantics are not IEEEsingle");
    Int api = bit_cast_to_int();
    return api.bits2float();
  }

  [[nodiscard]] double to_double() const {
    LPS_CHECK_ERROR(kTag, sem_ == &IEEEdouble(),
                    "Float semantics are not IEEEdouble");
    Int api = bit_cast_to_int();
    return api.bits2double();
  }

  OpStatus next(bool nex_down) {
    if (nex_down)
      change_sign();
    OpStatus result = OpStatus::kOK;
    switch (category_) {
      case FloatCategory::kInfinity:
        if (!is_negative())
          break;
        make_largest(true);
        break;
      case FloatCategory::kNaN:

        if (is_signaling()) {
          result = OpStatus::kInvalidOp;
          // For consistency, propagate the sign of the sNaN to the qNaN.
          make_NaN(false, is_negative(), nullptr);
        }
        break;
      case FloatCategory::kZero:
        make_smallest(false);
        break;
      case FloatCategory::kNormal:
        if (is_smallest() && is_negative()) {
          Int::set(significand_parts(), 0, n_parts());
          category_ = FloatCategory::kZero;
          exp_ = 0;
          break;
        }

        if (is_largest() && !is_negative()) {
          if (sem_->non_finite_behavior_ == FloatNonFiniteBehavior::kNanOnly) {
            make_NaN();
            break;
          }
          Int::set(significand_parts(), 0, n_parts());
          category_ = FloatCategory::kInfinity;
          exp_ = sem_->max_exp_ + 1;
          break;
        }

        if (is_negative()) {

          bool will_cross =
              exp_ != sem_->min_exp_ && is_significand_all_zeros();

          int_part_type* parts = significand_parts();
          Int::decrement(parts, n_parts());

          if (will_cross) {
            Int::set_bit(parts, sem_->precision_ - 1);
            exp_--;
          }
        } else {
          bool will_cross = !is_denormal() && is_significand_all_ones();

          if (will_cross) {
            int_part_type* parts = significand_parts();
            Int::set(parts, 0, n_parts());
            Int::set_bit(parts, sem_->precision_ - 1);
            LPS_CHECK_ERROR(
                kTag,
                exp_ != sem_->max_exp_ &&
                    "We can not increment an exponent beyond the maxExponent "
                    "allowed by the given floating point semantics.");
            exp_++;
          } else {
            increment_significand();
          }
        }
        break;
      case FloatBase::FloatCategory::kUnknown:
        break;
      default:
        unreachable(kTag);
    }
    if (nex_down)
      change_sign();

    return result;
  }

 private:
  static void append(basic::vec::details::TemplateCommon<char>& buf,
                     StringRef str) {
    for (const auto& a : str) {
      buf.append(a);
    }
  }

  static void adjust_to_precision(Int& significand, int& exp,
                                  uint64_t format_precision) {
    auto bits = significand.active_bits();

    auto bits_required = (format_precision * 196 + 58) / 59;

    if (bits <= bits_required)
      return;
    auto tens_removable = (bits - bits_required) * 59 / 196;
    if (!tens_removable)
      return;

    exp += tens_removable;
    Int divisor(significand.bits(), 1);
    Int powten(significand.bits(), 10);
    while (true) {
      if (tens_removable & 1)
        divisor *= powten;
      tens_removable >>= 1;
      if (!tens_removable)
        break;
      powten *= powten;
    }
    significand = significand.udiv(divisor);
    significand = significand.trunc(significand.active_bits());
  }

  static void adjust_to_precision(
      basic::vec::details::TemplateCommon<char>& buffer, int& exp,
      uint64_t format_precision) {
    auto n = buffer.size();
    if (n <= format_precision)
      return;
    auto first_significant = n - format_precision;

    if (buffer[first_significant - 1] < '5') {
      while (first_significant < n && buffer[first_significant] == '0')
        first_significant++;

      exp += first_significant;
      buffer.erase(buffer.data(), &buffer[first_significant]);
      return;
    }

    for (unsigned I = first_significant; I != n; ++I) {
      if (buffer[I] == '9') {
        first_significant++;
      } else {
        buffer[I]++;
        break;
      }
    }

    if (first_significant == n) {
      exp += first_significant;
      buffer.clear();
      buffer.append('1');
      return;
    }

    exp += first_significant;
    buffer.erase(buffer.data(), &buffer[first_significant]);
  }

 public:
  void string(basic::vec::details::TemplateCommon<char>& str,
              unsigned format_precision, unsigned format_max_padding,
              bool truncate_zero) const {
    switch (category_) {
      case FloatCategory::kInfinity:
        if (is_negative())
          return append(str, "-Inf");
        else
          return append(str, "+Inf");

      case FloatCategory::kNaN:
        return append(str, "NaN");

      case FloatCategory::kZero:
        if (is_negative())
          str.append('-');

        if (!format_max_padding) {
          if (truncate_zero)
            append(str, "0.0E+0");
          else {
            append(str, "0.0");
            if (format_precision > 1)
              str.append(format_precision - 1, '0');
            append(str, "e+00");
          }
        } else
          str.append('0');
        return;

      case FloatCategory::kNormal:
        break;
      case FloatBase::FloatCategory::kUnknown:
        unreachable(kTag);
    }

    if (is_negative())
      str.append('-');

    int exp = exp_ - (static_cast<int>(sem_->precision_) - 1);
    Int significand(sem_->precision_, significand_parts(),
                    n_parts_of_bits(sem_->precision_));

    if (!format_precision) {
      format_precision = 2 + sem_->precision_ * 59 / 196;
    }

    int trailing_zeros = significand.countr_zero();
    exp += trailing_zeros;
    significand.lshr_inplace(trailing_zeros);

    if (exp == 0) {
    } else if (exp > 0) {
      significand = significand.zext(sem_->precision_ + exp);
      significand <<= exp;
      exp = 0;
    } else {
      int tmp_exp = -exp;
      unsigned precision = sem_->precision_ + (137 * tmp_exp + 136) / 59;

      significand = significand.zext(precision);
      Int five_to_the_i(precision, 5);
      while (true) {
        if (tmp_exp & 1)
          significand *= five_to_the_i;

        tmp_exp >>= 1;
        if (!tmp_exp)
          break;
        five_to_the_i *= five_to_the_i;
      }
    }

    adjust_to_precision(significand, exp, format_precision);

    Vector<256, char> buffer;

    auto precision = significand.bits();
    if (precision < 4) {
      precision = 4;
      significand = significand.zext(precision);
    }
    Int ten(precision, 10);
    Int digit(precision, 0);

    bool in_trail = true;
    while (significand != 0) {
      Int::udivrem(significand, ten, significand, digit);

      unsigned d = digit.zero_ext_value();
      if (in_trail && !d)
        exp++;
      else {
        buffer.append(static_cast<char>('0' + d));
        in_trail = false;
      }
    }

    LPS_CHECK_ERROR(kTag, !buffer.empty(), "no characters in buffer!");

    adjust_to_precision(buffer, exp, format_precision);

    auto n_digits = buffer.size();

    bool format_scientific;
    if (!format_max_padding)
      format_scientific = true;
    else {
      if (exp >= 0) {
        format_scientific =
            (exp > format_max_padding || n_digits + exp > format_precision);
      } else {
        auto msd = exp + static_cast<int>(n_digits - 1);
        if (msd >= 0) {
          format_scientific = false;
        } else {
          format_scientific = (-msd) > format_max_padding;
        }
      }
    }

    if (format_scientific) {
      exp += (n_digits - 1);

      str.append(buffer[n_digits - 1]);
      str.append('.');
      if (n_digits == 1 && truncate_zero)
        str.append('0');
      else
        for (unsigned i = 1; i != n_digits; ++i)
          str.append(buffer[n_digits - 1 - i]);
      if (!truncate_zero && format_precision > n_digits - 1)
        str.append(format_precision - n_digits + 1, '0');
      str.append(truncate_zero ? 'E' : 'e');

      str.append(exp >= 0 ? '+' : '-');
      if (exp < 0)
        exp = -exp;
      Vector<6, char> exp_buf;
      do {
        exp_buf.append(static_cast<char>('0' + (exp % 10)));
        exp /= 10;
      } while (exp > 0);

      if (!truncate_zero && exp_buf.size() < 2)
        exp_buf.append('0');
      for (unsigned i = 0, e = exp_buf.size(); i != e; ++i)
        str.append(exp_buf[e - 1 - i]);
      return;
    }
    if (exp >= 0) {
      for (unsigned i = 0; i != n_digits; ++i)
        str.append(buffer[n_digits - 1 - i]);
      for (unsigned i = 0; i != static_cast<unsigned>(exp); ++i)
        str.append('0');
      return;
    }

    int n_whole_digits = exp + static_cast<int>(n_digits);

    unsigned i = 0;
    if (n_whole_digits > 0) {
      for (; i != static_cast<unsigned>(n_whole_digits); ++i)
        str.append(buffer[n_digits - i - 1]);
      str.append('.');
    } else {
      unsigned n_zeros = 1 + static_cast<unsigned>(-n_whole_digits);

      str.append('0');
      str.append('.');
      for (unsigned z = 1; z != n_zeros; ++z)
        str.append('0');
    }

    for (; i != n_digits; ++i)
      str.append(buffer[n_digits - i - 1]);
  }

  [[nodiscard]] bool is_signaling() const {
    if (!isNaN())
      return false;
    if (sem_->non_finite_behavior_ == FloatNonFiniteBehavior::kNanOnly)
      return false;
    return Int::extract_bit(significand_parts(), sem_->precision_ - 2) == 0;
  }

  IEEEFloat& operator=(IEEEFloat&& rhs);
  IEEEFloat& operator=(const IEEEFloat& rhs) {
    if (this != &rhs) {
      if (sem_ != rhs.sem_) {
        free_significand();
        init(rhs.sem_);
      }
      assign(rhs);
    }

    return *this;
  }
  ~IEEEFloat() { free_significand(); }

  void change_sign() { sign_ = !sign_; }
  [[nodiscard]] bool is_negative() const { return sign_; }
  [[nodiscard]] bool isNaN() const { return category_ == FloatCategory::kNaN; }
  [[nodiscard]] bool is_finite() const { return !isNaN() && !is_infinity(); }
  [[nodiscard]] bool is_infinity() const {
    return category_ == FloatCategory::kInfinity;
  }
  [[nodiscard]] bool is_zero() const {
    return category_ == FloatCategory::kZero;
  }
  [[nodiscard]] bool is_nonzero() const {
    return category_ != FloatCategory::kZero;
  }
  [[nodiscard]] bool is_finite_nonzero() const {
    return is_finite() && !is_zero();
  }
  [[nodiscard]] bool is_positive_zero() const {
    return is_zero() && !is_negative();
  }
  [[nodiscard]] bool is_negative_zero() const {
    return is_zero() && is_negative();
  }
  [[nodiscard]] bool is_denormal() const {
    return is_finite_nonzero() && (exp_ == sem_->min_exp_) &&
           (Int::extract_bit(significand_parts(), sem_->precision_ - 1) == 0);
  }

  [[nodiscard]] bool is_significand_all_zeros_except_LSB() const {
    const auto* parts = significand_parts();
    const auto n_parts = n_parts_of_bits(sem_->precision_);
    for (unsigned i = 0; i < n_parts - 1; i++) {
      if (parts[i])
        return false;
    }
    const auto n_high_bits = n_parts * kIntPartBits - sem_->precision_ + 1;
    return parts[n_parts - 1] == static_cast<int_part_type>(1)
                                     << (kIntPartBits - n_high_bits);
  }

  [[nodiscard]] bool is_smallest_normalized() const {
    return category() == FloatCategory::kNormal && exp_ == sem_->min_exp_ &&
           is_significand_all_zeros_except_LSB();
  }

  [[nodiscard]] bool is_smallest() const {
    return is_finite_nonzero() && exp_ == sem_->min_exp_ &&
           MSB_significand() == 0;
  }

  [[nodiscard]] bool is_largest() const {
    if (sem_->non_finite_behavior_ == FloatNonFiniteBehavior::kNanOnly) {
      return is_finite_nonzero() && exp_ == sem_->max_exp_ &&
             is_significand_all_ones_except_LSB();
    }
    return is_finite_nonzero() && exp_ == sem_->max_exp_ &&
           is_significand_all_ones();
  }

  [[nodiscard]] FloatCategory category() const { return category_; }
  [[nodiscard]] const FloatSemantics* sem() const { return sem_; }

  OpStatus add(const IEEEFloat& rhs, RoundingMode rounding_mode) {
    return add_or_sub<0>(rhs, rounding_mode);
  }

  OpStatus sub(const IEEEFloat& rhs, RoundingMode rounding_mode) {
    return add_or_sub<1>(rhs, rounding_mode);
  }

  OpStatus mul(const IEEEFloat& rhs, RoundingMode rounding_mode) {
    OpStatus fs;

    sign_ ^= rhs.sign_;
    fs = mul_specials(rhs);

    if (is_finite_nonzero()) {
      LostFraction lost_fraction = mul_significand(rhs);
      fs = normalize(rounding_mode, lost_fraction);
      if (lost_fraction != LostFraction::kExactlyZero)
        fs = static_cast<OpStatus>(static_cast<uint8_t>(fs) |
                                   static_cast<uint8_t>(OpStatus::kInexact));
    }
    return fs;
  }

  OpStatus div(const IEEEFloat& rhs, RoundingMode rounding_mode) {
    OpStatus fs;

    sign_ ^= rhs.sign_;
    fs = div_specials(rhs);

    if (is_finite_nonzero()) {
      LostFraction lost_fraction = div_significand(rhs);
      fs = normalize(rounding_mode, lost_fraction);
      if (lost_fraction != LostFraction::kExactlyZero)
        fs = static_cast<OpStatus>(static_cast<uint8_t>(fs) |
                                   static_cast<uint8_t>(OpStatus::kInexact));
    }

    return fs;
  }

  OpStatus rem(const IEEEFloat& rhs) {
    OpStatus fs;
    uint8_t origin_sign = sign_;

    fs = rem_specials(rhs);
    if (fs != OpStatus::kDivByZero)
      return fs;

    fs = OpStatus::kOK;

    IEEEFloat p2 = rhs;
    if (p2.add(rhs, RoundingMode::kNearestTiesToEven) == OpStatus::kOK) {
      fs = mod(p2);
      lps_assert(kTag, fs == OpStatus::kOK);
    }

    // Lets work with absolute numbers.
    IEEEFloat pp = rhs;
    pp.sign_ = false;
    sign_ = false;

    bool loses_info;
    FloatSemantics extended_semantics = *sem_;
    extended_semantics.max_exp_++;
    extended_semantics.min_exp_--;
    extended_semantics.precision_ += 2;

    IEEEFloat vex = *this;
    fs = vex.convert(extended_semantics, RoundingMode::kNearestTiesToEven,
                     &loses_info);
    lps_assert(kTag, fs == OpStatus::kOK && !loses_info);
    IEEEFloat pex = pp;
    fs = pex.convert(extended_semantics, RoundingMode::kNearestTiesToEven,
                     &loses_info);
    lps_assert(kTag, fs == OpStatus::kOK && !loses_info);

    fs = vex.add(vex, RoundingMode::kNearestTiesToEven);
    lps_assert(kTag, fs == OpStatus::kOK);

    if (vex.compare(pex) == Compareresult::kGreaterThan) {
      fs = sub(pp, RoundingMode::kNearestTiesToEven);
      lps_assert(kTag, fs == OpStatus::kOK);

      fs = vex.sub(pex, RoundingMode::kNearestTiesToEven);
      lps_assert(kTag, fs == OpStatus::kOK);
      fs = vex.sub(pex, RoundingMode::kNearestTiesToEven);
      lps_assert(kTag, fs == OpStatus::kOK);

      Compareresult result = vex.compare(pex);
      if (result == Compareresult::kGreaterThan ||
          result == Compareresult::kEqual) {
        fs = sub(pp, RoundingMode::kNearestTiesToEven);
        lps_assert(kTag, fs == OpStatus::kOK);
      }
    }

    if (is_zero())
      sign_ = origin_sign;
    else
      sign_ ^= origin_sign;
    return fs;
  }

  OpStatus mod(const IEEEFloat& rhs) {
    OpStatus fs;
    fs = mod_specials(rhs);
    unsigned int original_sign = sign_;

    while (is_finite_nonzero() && rhs.is_finite_nonzero() &&
           compare_abs(rhs) != Compareresult::kLessThan) {
      int exp = ilogb(*this) - ilogb(rhs);
      IEEEFloat v = scalbn(rhs, exp, RoundingMode::kNearestTiesToEven);

      if (v.isNaN() || compare_abs(v) == Compareresult::kLessThan)
        v = scalbn(rhs, exp - 1, RoundingMode::kNearestTiesToEven);
      v.sign_ = sign_;

      fs = sub(v, RoundingMode::kNearestTiesToEven);
      lps_assert(kTag, fs == OpStatus::kOK);
    }
    if (is_zero())
      sign_ = original_sign;  // fmod requires this
    return fs;
  }

  bool exact_inverse(Float* inv) const;

  template <uint64_t P0, uint64_t P1, uint64_t P2, uint64_t P3, uint64_t P4,
            uint64_t P5, uint64_t P6>
  [[nodiscard]] Int convert_x_to_int() const {

    lps_assert(kTag, n_parts() == P0);

    uint64_t exp;
    uint64_t significand;

    if (is_finite_nonzero()) {
      exp = exp_ + P1;
      significand = *significand_parts();
      if (exp == 1 && !(significand & P2))
        exp = 0;
    } else if (category_ == FloatCategory::kZero) {
      exp = 0;
      significand = 0;
    } else if (category_ == FloatCategory::kInfinity) {
      exp = P3;
      significand = 0;
    } else {
      LPS_CHECK_ERROR(kTag, category_ == FloatCategory::kNaN,
                      "Unknown category!");
      exp = P3;
      significand = *significand_parts();
    }

    return Int(P4, ((static_cast<uint64_t>(sign_ & 1ULL) << (P4 - 1)) |
                    ((exp & P3) << P5) | (significand & P6)));
  }

  [[nodiscard]] Int convert_half_to_int() const {
    return convert_x_to_int<1, 15, 0x400U, 0x1fU, 16, 10, 0x3ffU>();
  }
  [[nodiscard]] Int convert_bfloat_to_int() const {
    return convert_x_to_int<1, 127, 0x80U, 0xffU, 16, 7, 0x7fU>();
  }
  [[nodiscard]] Int convert_float_to_int() const {
    return convert_x_to_int<1, 127, 0x800000U, 0xffU, 32, 23, 0x7fffffU>();
  }
  [[nodiscard]] Int convert_double_to_int() const {
    return convert_x_to_int<1, 1023, 0x10000000000000LL, 0x7ffU, 64, 52,
                            0xfffffffffffffLL>();
  }
  [[nodiscard]] Int convert_quad_to_int() const {
    lps_assert(kTag, n_parts() == 2);

    uint64_t exp;
    uint64_t significand;
    uint64_t significand2;

    if (is_finite_nonzero()) {
      exp = exp + 16383;
      significand = significand_parts()[0];
      significand2 = significand_parts()[1];
      if (exp == 1 && !(significand2 & 0x1000000000000LL))
        exp = 0;
    } else if (category_ == FloatCategory::kZero) {
      exp = 0;
      significand = significand2 = 0;
    } else if (category_ == FloatCategory::kInfinity) {
      exp = 0x7fff;
      significand = significand2 = 0;
    } else {
      LPS_CHECK_ERROR(kTag, category_ == FloatCategory::kNaN,
                      "unknown category!");
      exp = 0x7fff;
      significand = significand_parts()[0];
      significand2 = significand_parts()[1];
    }

    uint64_t words[2];
    words[0] = significand;
    words[1] = (static_cast<uint64_t>(sign_ & 1) << 63) |
               ((exp & 0x7fff) << 48) | (significand2 & 0xffffffffffffLL);

    return Int(128, words, 2);
  }

  OpStatus convert(const FloatSemantics& to_sem, RoundingMode rounding_mode,
                   bool* loses_info) {
    LostFraction lf;
    uint64_t new_n_parts;
    uint64_t old_n_parts;
    OpStatus fs;
    int shift;
    const FloatSemantics& from_sem = *sem_;
    bool is_signal = is_signaling();

    lf = LostFraction::kExactlyZero;
    new_n_parts = n_parts_of_bits(to_sem.precision_ + 1);
    old_n_parts = n_parts();
    shift = to_sem.precision_ - from_sem.precision_;

    bool x86_special_nan = false;

    if (shift < 0 && is_finite_nonzero()) {
      int omsb = MSB_significand() + 1;
      int exp_change = omsb - from_sem.precision_;
      if (exp_ + exp_change < to_sem.min_exp_)
        exp_change = to_sem.min_exp_ - exp_;
      if (exp_change < shift)
        exp_change = shift;
      if (exp_change < 0) {
        shift -= exp_change;
        exp_ += exp_change;
      } else if (omsb <= -shift) {
        exp_change = omsb + shift - 1;
        shift -= exp_change;
        exp_ += exp_change;
      }
    }

    if (shift < 0 &&
        (is_finite_nonzero() ||
         (category_ == FloatCategory::kNaN &&
          sem_->non_finite_behavior_ != FloatNonFiniteBehavior::kNanOnly)))
      lf = shift_right(significand_parts(), old_n_parts, -shift);

    if (new_n_parts > old_n_parts) {
      int_part_type* new_parts;
      new_parts = new int_part_type[new_n_parts];
      Int::set(new_parts, 0, new_n_parts);
      if (is_finite_nonzero() || isNaN())
        Int::assign(new_parts, significand_parts(), old_n_parts);
      free_significand();
      significand_.parts_ = new_parts;
    } else if (new_n_parts == 1 && old_n_parts != 1) {
      int_part_type new_part = 0;
      if (is_finite_nonzero() || isNaN())
        new_part = significand_parts()[0];
      free_significand();
      significand_.part_ = new_part;
    }

    sem_ = &to_sem;

    if (shift > 0 && (is_finite_nonzero() || isNaN()))
      Int::shift_left(significand_parts(), new_n_parts, shift);

    if (is_finite_nonzero()) {
      fs = normalize(rounding_mode, lf);
      *loses_info = (fs != OpStatus::kOK);
    } else if (isNaN()) {
      if (sem_->non_finite_behavior_ == FloatNonFiniteBehavior::kNanOnly) {
        *loses_info =
            sem_->non_finite_behavior_ != FloatNonFiniteBehavior::kNanOnly;
        make_NaN(false, sign_);
        return is_signal ? OpStatus::kInvalidOp : OpStatus::kOK;
      }

      *loses_info = lf != LostFraction::kExactlyZero || x86_special_nan;

      if (is_signal) {
        make_quiet();
        fs = OpStatus::kInvalidOp;
      } else {
        fs = OpStatus::kOK;
      }
    } else if (category_ == FloatCategory::kInfinity &&
               sem_->non_finite_behavior_ == FloatNonFiniteBehavior::kNanOnly) {
      make_NaN(false, sign_);
      *loses_info = true;
      fs = OpStatus::kInexact;
    } else {
      *loses_info = false;
      fs = OpStatus::kOK;
    }

    return fs;
  }

  void make_smallest_normalized(bool neg) {
    category_ = FloatCategory::kNormal;
    zero_significand();
    sign_ = neg;
    exp_ = sem_->min_exp_;
    Int::set_bit(significand_parts(), sem_->precision_ - 1);
  }

  void make_zero(bool neg) {
    category_ = FloatCategory::kZero;
    sign_ = neg;
    exp_ = exp_zero();
    Int::set(significand_parts(), 0, n_parts());
  }

  [[nodiscard]] exp_part_type exp_inf() const { return sem_->max_exp_ + 1; }
  [[nodiscard]] exp_part_type exp_zero() const { return sem_->min_exp_ - 1; }

  void make_largest(bool neg) {
    category_ = FloatCategory::kNormal;
    sign_ = neg;
    exp_ = sem_->max_exp_;
    int_part_type* significand = significand_parts();
    uint64_t n_part = n_parts();
    memset(significand, 0xFF, sizeof(int_part_type) * (n_part - 1));
    const uint64_t n_unused_high_bits =
        n_part * kIntPartBits - sem_->precision_;
    significand[n_part - 1] =
        (n_unused_high_bits < kIntPartBits)
            ? (~static_cast<int_part_type>(0) >> n_unused_high_bits)
            : 0;

    if (sem_->non_finite_behavior_ == FloatNonFiniteBehavior::kNanOnly)
      significand[0] &= ~static_cast<int_part_type>(1);
  }

  void make_smallest(bool neg) {
    category_ = FloatCategory::kNormal;
    sign_ = neg;
    exp_ = sem_->min_exp_;
    Int::set(significand_parts(), 1, n_parts());
  }

  void make_inf(bool neg) {
    if (sem_->non_finite_behavior_ == FloatNonFiniteBehavior::kNanOnly) {
      make_NaN(false, neg);
      return;
    }
    category_ = FloatCategory::kInfinity;
    sign_ = neg;
    exp_ = exp_inf();
    Int::set(significand_parts(), 0, n_parts());
  }

  void make_NaN(bool s_NaN = false, bool negative = false,
                const Int* fill = nullptr) {
    category_ = FloatCategory::kNaN;
    sign_ = negative;
    exp_ = exp_NaN();

    int_part_type* significand = significand_parts();
    uint64_t num_parts = n_parts();

    Int fill_storage;
    if (sem_->non_finite_behavior_ == FloatNonFiniteBehavior::kNanOnly) {
      s_NaN = false;
      fill_storage = Int::all_ones(sem_->precision_ - 1);
      fill = &fill_storage;
    }

    if (!fill || fill->n_words() < num_parts)
      Int::set(significand, 0, num_parts);
    if (fill) {
      Int::assign(significand, fill->raw_data(),
                  std::min(fill->n_words(), num_parts));

      uint64_t bits_preserve = sem_->precision_ - 1;
      uint64_t part = bits_preserve / 64;
      bits_preserve %= 64;
      significand[part] &= ((1ULL << bits_preserve) - 1);
      for (part++; part != num_parts; ++part)
        significand[part] = 0;
    }

    uint64_t q_nan_bit = sem_->precision_ - 2;

    if (s_NaN) {
      Int::clear_bit(significand, q_nan_bit);
      if (Int::is_zero(significand, num_parts))
        Int::set_bit(significand, q_nan_bit - 1);
    } else {
      Int::set_bit(significand, q_nan_bit);
    }
  }

  static uint64_t power5(int_part_type* dst, uint64_t power) {
    static const int_part_type kFirstEightPowers[] = {1,   5,    25,    125,
                                                      625, 3125, 15625, 78125};
    int_part_type pow5s[kMaxPowerOfFiveParts * 2 + 5];
    pow5s[0] = 78125 * 5;

    uint64_t parts_count[16] = {1};
    int_part_type scratch[kMaxPowerOfFiveParts];
    int_part_type* p1;
    int_part_type* p2;
    int_part_type* pow5;
    uint64_t result;
    lps_assert(kTag, power <= kMaxExponent);

    p1 = dst;
    p2 = scratch;

    *p1 = kFirstEightPowers[power & 7];
    power >>= 3;

    result = 1;
    pow5 = pow5s;

    for (uint64_t n = 0; power; power >>= 1, n++) {
      uint64_t pc;

      pc = parts_count[n];
      if (pc == 0) {
        pc = parts_count[n - 1];
        Int::full_mul(pow5, pow5 - pc, pow5 - pc, pc, pc);
        pc *= 2;
        if (pow5[pc - 1] == 0)
          pc--;
        parts_count[n] = pc;
      }

      if (power & 1) {
        int_part_type* tmp;

        Int::full_mul(p2, p1, pow5, result, pc);
        result += pc;
        if (p2[result - 1] == 0)
          result--;

        tmp = p1;
        p1 = p2;
        p2 = tmp;
      }

      pow5 += pc;
    }

    if (p1 != dst)
      Int::assign(dst, p1, result);

    return result;
  }

#define PackCategoriesIntoKey(_lhs, _rhs) \
  (static_cast<int>((_lhs)) * 4 + static_cast<int>((_rhs)))

  [[nodiscard]] Compareresult compare(const IEEEFloat& rhs) const {
    Compareresult result;

    lps_assert(kTag, sem_ == rhs.sem_);

    switch (PackCategoriesIntoKey(category_, rhs.category_)) {
      default:
        unreachable(kTag);

      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kZero):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kNaN):
      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kNaN):
      case PackCategoriesIntoKey(FloatCategory::kNormal, FloatCategory::kNaN):
      case PackCategoriesIntoKey(FloatCategory::kInfinity, FloatCategory::kNaN):
        return Compareresult::kUnordered;

      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kZero):
      case PackCategoriesIntoKey(FloatCategory::kNormal, FloatCategory::kZero):
        if (sign_)
          return Compareresult::kLessThan;
        else
          return Compareresult::kGreaterThan;

      case PackCategoriesIntoKey(FloatCategory::kNormal,
                                 FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kZero,
                                 FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kNormal):
        if (rhs.sign_)
          return Compareresult::kGreaterThan;
        else
          return Compareresult::kLessThan;

      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kInfinity):
        if (sign_ == rhs.sign_)
          return Compareresult::kEqual;
        else if (sign_)
          return Compareresult::kLessThan;
        else
          return Compareresult::kGreaterThan;

      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kZero):
        return Compareresult::kEqual;

      case PackCategoriesIntoKey(FloatCategory::kNormal,
                                 FloatCategory::kNormal):
        break;
    }

    /* Two normal numbers.  Do they have the same sign?  */
    if (sign_ != rhs.sign_) {
      if (sign_)
        result = Compareresult::kLessThan;
      else
        result = Compareresult::kGreaterThan;
    } else {
      result = compare_abs(rhs);

      if (sign_) {
        if (result == Compareresult::kLessThan)
          result = Compareresult::kGreaterThan;
        else if (result == Compareresult::kGreaterThan)
          result = Compareresult::kLessThan;
      }
    }

    return result;
  }

  OpStatus round2int(RoundingMode rounding_mode) {
    OpStatus fs;

    if (is_infinity())
      return OpStatus::kOK;

    if (isNaN()) {
      if (is_signaling()) {
        make_quiet();

        return OpStatus::kInvalidOp;
      }
      return OpStatus::kOK;
    }

    if (is_zero()) {

      return OpStatus::kOK;
    }

    if (exp_ + 1 >= static_cast<int32_t>(sem_->precision_))
      return OpStatus::kOK;

    Int int_const(next_pow_2(sem_->precision_), 1);
    int_const <<= sem_->precision_ - 1;
    IEEEFloat magic_const(*sem_);
    fs = magic_const.convert_from_Int(int_const, false,
                                      RoundingMode::kNearestTiesToEven);
    lps_assert(kTag, fs == OpStatus::kOK);
    magic_const.sign_ = sign_;

    bool input_sign = is_negative();

    fs = add(magic_const, rounding_mode);

    sub(magic_const, rounding_mode);

    if (input_sign != is_negative())
      sign_ = !sign_;

    return fs;
  }

  [[nodiscard]] bool is_int() const {
    if (!is_finite())
      return false;
    IEEEFloat truncated = *this;
    truncated.round2int(RoundingMode::kTowardZero);
    return compare(truncated) == Compareresult::kEqual;
  }

  [[nodiscard]] bool bitwise_equal(const IEEEFloat& rhs) const {
    if (this == &rhs)
      return true;
    if (sem_ != rhs.sem_ || category_ != rhs.category_ || sign_ != rhs.sign_)
      return false;
    if (category_ == FloatCategory::kZero ||
        category_ == FloatCategory::kInfinity)
      return true;

    if (is_finite_nonzero() && exp_ != rhs.exp_)
      return false;

    return std::equal(significand_parts(), significand_parts() + n_parts(),
                      rhs.significand_parts());
  }

  OpStatus fused_mul_add(const IEEEFloat& multiplicand, const IEEEFloat& addend,
                         RoundingMode rounding_mode) {
    OpStatus fs;
    sign_ ^= multiplicand.sign_;

    if (is_finite_nonzero() && multiplicand.is_finite_nonzero() &&
        addend.is_finite()) {
      LostFraction lost_fraction;

      lost_fraction = mul_significand(multiplicand, addend);
      fs = normalize(rounding_mode, lost_fraction);
      if (lost_fraction != LostFraction::kExactlyZero)
        fs = static_cast<OpStatus>(static_cast<uint8_t>(fs) |
                                   static_cast<uint8_t>(OpStatus::kInexact));

      if (category_ == FloatCategory::kZero &&
          !(static_cast<uint8_t>(fs) &
            static_cast<uint8_t>(OpStatus::kUnderflow)) &&
          sign_ != addend.sign_)
        sign_ = (rounding_mode == RoundingMode::kTowardNegative);
    } else {
      fs = mul_specials(multiplicand);

      if (fs == OpStatus::kOK)
        fs = add_or_sub<0>(addend, rounding_mode);
    }

    return fs;
  }

  OpStatus sign_extended_int(
      basic::vec::details::TemplateCommon<int_part_type>& parts, uint64_t width,
      bool is_signed, RoundingMode rounding_mode, bool* isExact) const {
    LostFraction lost_fraction;
    const int_part_type* src;
    uint64_t truncated_bits;

    *isExact = false;

    if (category_ == FloatCategory::kInfinity ||
        category_ == FloatCategory::kNaN)
      return OpStatus::kInvalidOp;

    auto dst_n_parts = n_parts_of_bits(width);
    LPS_CHECK_ERROR(kTag, dst_n_parts <= parts.size(), "integer too big");

    if (category_ == FloatCategory::kZero) {
      Int::set(parts.data(), 0, dst_n_parts);
      *isExact = (sign_ == 0U);
      return OpStatus::kOK;
    }

    src = significand_parts();

    if (exp_ < 0) {
      Int::set(parts.data(), 0, dst_n_parts);
      truncated_bits = sem_->precision_ - 1ULL - exp_;
    } else {
      auto bits = exp_ + 1ULL;
      if (bits > width)
        return OpStatus::kInvalidOp;
      if (bits < sem_->precision_) {
        truncated_bits = sem_->precision_ - bits;
        Int::extract(parts.data(), dst_n_parts, src, bits, truncated_bits);
      } else {
        Int::extract(parts.data(), dst_n_parts, src, sem_->precision_, 0);
        Int::shift_left(parts.data(), dst_n_parts, bits - sem_->precision_);
        truncated_bits = 0;
      }
    }

    if (truncated_bits) {
      lost_fraction =
          lost_fraction_through_truncation(src, n_parts(), truncated_bits);
      if (lost_fraction != LostFraction::kExactlyZero &&
          round_away_from_zero(rounding_mode, lost_fraction, truncated_bits)) {
        if (Int::increment(parts.data(), dst_n_parts))
          return OpStatus::kInvalidOp;
      }
    } else {
      lost_fraction = LostFraction::kExactlyZero;
    }

    unsigned int omsb = Int::MSB(parts.data(), dst_n_parts) + 1;

    if (sign_) {
      if (!is_signed) {
        if (omsb != 0)
          return OpStatus::kInvalidOp;
      } else {
        if (omsb == width && Int::LSB(parts.data(), dst_n_parts) + 1 != omsb)
          return OpStatus::kInvalidOp;

        if (omsb > width)
          return OpStatus::kInvalidOp;
      }

      Int::negate(parts.data(), dst_n_parts);
    } else {
      if (omsb >= width + static_cast<uint64_t>(!is_signed))
        return OpStatus::kInvalidOp;
    }

    if (lost_fraction == LostFraction::kExactlyZero) {
      *isExact = true;
      return OpStatus::kOK;
    }
    return OpStatus::kInexact;
  }

  OpStatus integer(basic::vec::details::TemplateCommon<int_part_type>& parts,
                   uint64_t width, bool is_signed, RoundingMode rounding_mode,
                   bool* is_exact) const {
    auto fs =
        sign_extended_int(parts, width, is_signed, rounding_mode, is_exact);
    if (fs == OpStatus::kInvalidOp) {
      uint64_t bits;
      auto n_dst_parts = n_parts_of_bits(width);
      LPS_CHECK_ERROR(kTag, n_dst_parts <= parts.size(), "integer too big");

      if (category_ == FloatCategory::kNaN)
        bits = 0;
      else if (sign_)
        bits = static_cast<uint64_t>(is_signed);
      else
        bits = width - static_cast<uint64_t>(is_signed);

      Int::set_leastSignificant_bits(parts.data(), n_dst_parts, bits);
      if (sign_ && is_signed)
        Int::shift_left(parts.data(), n_dst_parts, width - 1);
    }

    return fs;
  }

  std::optional<OpStatus> form(StringRef str, RoundingMode rounding_mode) {
    if (str.empty())
      return {};

    if (from_string_specials(str))
      return OpStatus::kOK;

    StringRef::const_iterator p = str.begin();
    size_t slen = str.size();
    sign_ = *p == '-' ? 1 : 0;
    if (*p == '-' || *p == '+') {
      p++;
      slen--;
      if (!slen)
        return {};
    }

    if (slen >= 2 && p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
      if (slen == 2)
        return {};
      return from_hex_decimal_string(StringRef(p + 2, slen - 2), rounding_mode);
    }

    return from_decimal_string(StringRef(p, slen), rounding_mode);
  }

 private:
  void init(const FloatSemantics* sem) {
    sem_ = sem;
    auto count = n_parts();
    if (count > 1)
      significand_.parts_ = new int_part_type[count];
  }

  bool from_string_specials(StringRef str) {
    const size_t min_name_size = 3;

    if (str.size() < min_name_size)
      return false;

    if (str.eq("inf") || str.eq("INFINITY") || str.eq("+Inf")) {
      make_inf(false);
      return true;
    }

    bool is_negative = str.front() == '-';
    if (is_negative) {
      str = str.drop_front();
      if (str.size() < min_name_size)
        return false;

      if (str.eq("inf") || str.eq("INFINITY") || str.eq("Inf")) {
        make_inf(true);
        return true;
      }
    }

    bool is_signaling = str.front() == 's' || str.front() == 'S';
    if (is_signaling) {
      str = str.drop_front();
      if (str.size() < min_name_size)
        return false;
    }

    if (str.starts_with("nan") || str.starts_with("NaN")) {
      str = str.drop_front(3);

      if (str.empty()) {
        make_NaN(is_signaling, is_negative);
        return true;
      }

      if (str.front() == '(') {
        if (str.size() <= 2 || str.back() != ')')
          return false;

        str = str.slice(1, str.size() - 1);
      }

      unsigned radix = 10;
      if (str[0] == '0') {
        if (str.size() > 1 && tolower(str[1]) == 'x') {
          str = str.drop_front(2);
          radix = 16;
        } else
          radix = 8;
      }

      Int pay_load;
      if (!str.as_int(radix, pay_load)) {
        make_NaN(is_signaling, is_negative, &pay_load);
        return true;
      }
    }

    return false;
  }

  void zero_significand() {
    Int::set(significand_parts(), 0, n_parts());
  }

  static std::optional<StringRef::const_iterator> skip_Leading_zeros_and_dot(
      StringRef::const_iterator begin, StringRef::const_iterator end,
      StringRef::const_iterator* dot) {
    StringRef::const_iterator p = begin;
    *dot = end;
    while (p != end && *p == '0')
      p++;

    if (p != end && *p == '.') {
      *dot = p++;

      if (end - begin == 1)
        return {};

      while (p != end && *p == '0')
        p++;
    }

    return p;
  }

  static std::optional<LostFraction> trailing_hex_decimal_fraction(
      StringRef::const_iterator p, StringRef::const_iterator end,
      uint64_t digit_Val) {
    uint64_t hex_digit;

    if (digit_Val > 8)
      return LostFraction::kMoreThanHalf;
    if (digit_Val < 8 && digit_Val > 0)
      return LostFraction::kLessThanHalf;

    while (p != end && (*p == '0' || *p == '.'))
      p++;

    if (p == end)
      return {};

    hex_digit = str::hex_digit(*p);

    if (hex_digit == -1U)
      return digit_Val == 0 ? LostFraction::kExactlyZero
                            : LostFraction::kExactlyHalf;

    return digit_Val == 0 ? LostFraction::kLessThanHalf
                          : LostFraction::kMoreThanHalf;
  }

  static inline unsigned int dec_digit(unsigned int c) {
    return c - '0';
  }

  static std::optional<int> total_exp(StringRef::const_iterator p,
                                      StringRef::const_iterator end,
                                      int exp_adjustment) {
    int unsigned_exp;
    bool negative;
    bool overflow;
    int exponent = 0;

    if (p == end)
      return {};

    negative = *p == '-';
    if (*p == '-' || *p == '+') {
      p++;
      if (p == end)
        return {};
    }

    unsigned_exp = 0;
    overflow = false;
    for (; p != end; ++p) {
      unsigned int value;

      value = dec_digit(*p);
      if (value >= 10U)
        return {};

      unsigned_exp = unsigned_exp * 10 + value;
      if (unsigned_exp > 32767) {
        overflow = true;
        break;
      }
    }

    if (exp_adjustment > 32767 || exp_adjustment < -32768)
      overflow = true;

    if (!overflow) {
      exponent = unsigned_exp;
      if (negative)
        exponent = -exponent;
      exponent += exp_adjustment;
      if (exponent > 32767 || exponent < -32768)
        overflow = true;
    }

    if (overflow)
      exponent = negative ? -32768 : 32767;

    return exponent;
  }

  std::optional<OpStatus> from_hex_decimal_string(StringRef s,
                                                  RoundingMode rounding_mode) {
    auto lost_fraction = LostFraction::kExactlyZero;

    category_ = FloatCategory::kNormal;
    zero_significand();
    exp_ = 0;

    int_part_type* significand = significand_parts();
    unsigned parts_count = n_parts();
    unsigned bit_pos = parts_count * kIntPartBits;
    bool computed_trailing_fraction = false;

    const auto* begin = s.begin();
    const auto* end = s.end();
    StringRef::const_iterator dot;
    auto ptr_or_none = skip_Leading_zeros_and_dot(begin, end, &dot);
    if (!ptr_or_none)
      return {};
    const auto* p = *ptr_or_none;
    const auto* first_significant_digit = p;

    while (p != end) {
      int_part_type hex_value;

      if (*p == '.') {
        if (dot != end)
          return {};
        dot = p++;
        continue;
      }

      hex_value = str::hex_digit(*p);
      if (hex_value == -1U)
        break;
      p++;
      if (bit_pos) {
        bit_pos -= 4;
        hex_value <<= bit_pos % kIntPartBits;
        significand[bit_pos / kIntPartBits] |= hex_value;
      } else if (!computed_trailing_fraction) {
        auto fract_or_none = trailing_hex_decimal_fraction(p, end, hex_value);
        if (!fract_or_none)
          return {};
        lost_fraction = *fract_or_none;
        computed_trailing_fraction = true;
      }
    }

    if (p == end)
      return {};
    if (*p != 'p' && *p != 'P')
      return {};
    if (p == begin)
      return {};
    if (dot != end && p - begin == 1)
      return {};

    if (p != first_significant_digit) {
      int exp_adjustment;

      if (dot == end)
        dot = p;

      exp_adjustment = static_cast<int>(dot - first_significant_digit);
      if (exp_adjustment < 0)
        exp_adjustment++;
      exp_adjustment = exp_adjustment * 4 - 1;

      exp_adjustment += sem_->precision_;
      exp_adjustment -= parts_count * kIntPartBits;

      auto exp_or_none = total_exp(p + 1, end, exp_adjustment);
      if (!exp_or_none)
        return {};
      exp_ = *exp_or_none;
    }

    return normalize(rounding_mode, lost_fraction);
  }

  static std::optional<int> read_exp(StringRef::const_iterator begin,
                                     StringRef::const_iterator end) {
    bool is_neg;
    unsigned int abs_exp;
    const unsigned int overlarge_exp = 24000;
    StringRef::const_iterator p = begin;

    if (p == end || ((*p == '-' || *p == '+') && (p + 1) == end)) {
      return 0;
    }

    is_neg = (*p == '-');
    if (*p == '-' || *p == '+') {
      p++;
      if (p == end)
        return {};
    }

    abs_exp = dec_digit(*p++);
    if (abs_exp >= 10U)
      return {};

    for (; p != end; ++p) {
      unsigned int value;

      value = dec_digit(*p);
      if (value >= 10U)
        return {};

      abs_exp = abs_exp * 10U + value;
      if (abs_exp >= overlarge_exp) {
        abs_exp = overlarge_exp;
        break;
      }
    }

    if (is_neg)
      return -(int)abs_exp;
    else
      return (int)abs_exp;
  }

  struct DecimalInfo {
    const char* first_sig_{nullptr};
    const char* last_sig_{nullptr};
    int exp_{0};
    int normalized_exp_{0};
  };

  static bool interpret_decimal(StringRef::const_iterator begin,
                                StringRef::const_iterator end, DecimalInfo* D) {
    StringRef::const_iterator dot = end;

    auto ptr_or_none = skip_Leading_zeros_and_dot(begin, end, &dot);
    if (!ptr_or_none)
      return false;
    StringRef::const_iterator p = *ptr_or_none;
    D->first_sig_ = p;

    for (; p != end; ++p) {
      if (*p == '.') {
        if (dot != end)
          return false;
        dot = p++;
        if (p == end)
          break;
      }
      if (dec_digit(*p) >= 10U)
        break;
    }

    if (p != end) {
      if (*p != 'e' && *p != 'E')
        return false;
      if (p == begin)
        return false;
      if (dot != end && p - begin == 1)
        return false;

      auto exp_or_none = read_exp(p + 1, end);
      if (!exp_or_none)
        return false;
      D->exp_ = *exp_or_none;

      if (dot == end)
        dot = p;
    }

    if (p != D->first_sig_) {
      if (p != begin) {
        do
          do
            p--;
          while (p != begin && *p == '0');
        while (p != begin && *p == '.');
      }

      D->exp_ += static_cast<exp_part_type>((dot - p) - (dot > p));
      D->normalized_exp_ = (D->exp_ + static_cast<exp_part_type>(
                                          (p - D->first_sig_) -
                                          (dot > D->first_sig_ && dot < p)));
    }

    D->last_sig_ = p;
    return true;
  }

  static void set_least_significant_bits(Int::word_type* dst, uint64_t parts,
                                         uint64_t bits) {
    uint64_t i = 0;
    while (bits > Int::kBitsOfWord) {
      dst[i++] = ~static_cast<Int::word_type>(0);
      bits -= Int::kBitsOfWord;
    }
    if (bits)
      dst[i++] = ~static_cast<Int::word_type>(0) >> (Int::kBitsOfWord - bits);
    while (i < parts)
      dst[i++] = 0;
  }

  void significand_zero() {
    Int::set(significand_parts(), 0, n_parts());
  }

  static uint64_t hu_err_bound(bool inexact_multiply, uint64_t hu_err1,
                               uint64_t hu_err2) {
    lps_assert(kTag, hu_err1 < 2 || hu_err2 < 2 || (hu_err1 + hu_err2 < 8));

    if (hu_err1 + hu_err2 == 0)
      return static_cast<int>(inexact_multiply) * 2;
    return static_cast<uint64_t>(inexact_multiply) + 2 * (hu_err1 + hu_err2);
  }

  static int_part_type ulps_from_boundary(const int_part_type* parts,
                                          uint64_t bits, bool isNearest) {
    uint64_t count;
    uint64_t part_bits;
    int_part_type part;
    int_part_type boundary;

    lps_assert(kTag, bits != 0);

    bits--;
    count = bits / kIntPartBits;
    part_bits = bits % kIntPartBits + 1;

    part = parts[count] &
           (~static_cast<int_part_type>(0) >> (kIntPartBits - part_bits));

    if (isNearest)
      boundary = static_cast<int_part_type>(1) << (part_bits - 1);
    else
      boundary = 0;

    if (count == 0) {
      if (part - boundary <= boundary - part)
        return part - boundary;
      return boundary - part;
    }

    if (part == boundary) {
      while (--count)
        if (parts[count])
          return ~static_cast<int_part_type>(0);

      return parts[0];
    }
    if (part == boundary - 1) {
      while (--count)
        if (~parts[count])
          return ~static_cast<int_part_type>(0);

      return -parts[0];
    }

    return ~static_cast<int_part_type>(0);
  }

  OpStatus round_significand_with_exp(const int_part_type* decSigParts,
                                      uint64_t sigPartCount, int exp,
                                      RoundingMode rounding_mode) {
    uint64_t parts;
    uint64_t pow5_part_count;
    FloatSemantics calc_sem = {32767, -32767, 0, 0};
    int_part_type pow5_parts[kMaxPowerOfFiveParts];

    bool is_nearest = (rounding_mode == RoundingMode::kNearestTiesToEven ||
                       rounding_mode == RoundingMode::kNearestTiesToAway);

    parts = n_parts_of_bits(sem_->precision_ + 11);

    pow5_part_count = power5(pow5_parts, exp >= 0 ? exp : -exp);

    for (;; parts *= 2) {
      OpStatus sig_status;
      OpStatus pow_status;
      uint64_t excess_precision;
      uint64_t truncated_bits;

      calc_sem.precision_ = parts * kIntPartBits - 1;
      excess_precision = calc_sem.precision_ - sem_->precision_;
      truncated_bits = excess_precision;

      IEEEFloat dec_sig(calc_sem, UninitializedTag::kUninitialized);
      dec_sig.make_zero(sign_ != 0U);
      IEEEFloat pow5(calc_sem);

      sig_status = dec_sig.convert_from_unsigned_parts(
          decSigParts, sigPartCount, RoundingMode::kNearestTiesToEven);
      pow_status = pow5.convert_from_unsigned_parts(
          pow5_parts, pow5_part_count, RoundingMode::kNearestTiesToEven);
      dec_sig.exp_ += exp;

      LostFraction calc_lost_fraction;
      int_part_type hu_err;
      int_part_type hu_distance;
      unsigned int pow_hu_err;

      if (exp >= 0) {
        calc_lost_fraction = dec_sig.mul_significand(pow5);
        pow_hu_err = static_cast<unsigned int>(pow_status != OpStatus::kOK);
      } else {
        calc_lost_fraction = dec_sig.div_significand(pow5);
        if (dec_sig.exp_ < sem_->min_exp_) {
          excess_precision += (sem_->min_exp_ - dec_sig.exp_);
          truncated_bits = excess_precision;
          if (excess_precision > calc_sem.precision_)
            excess_precision = calc_sem.precision_;
        }

        pow_hu_err = (pow_status == OpStatus::kOK &&
                      calc_lost_fraction == LostFraction::kExactlyZero)
                         ? 0
                         : 2;
      }

      lps_assert(kTag, Int::extract_bit(dec_sig.significand_parts(),
                                        calc_sem.precision_ - 1) == 1);

      hu_err = hu_err_bound(calc_lost_fraction != LostFraction::kExactlyZero,
                            static_cast<uint64_t>(sig_status != OpStatus::kOK),
                            pow_hu_err);
      hu_distance = 2 * ulps_from_boundary(dec_sig.significand_parts(),
                                           excess_precision, is_nearest);

      if (hu_distance >= hu_err) {
        Int::extract(significand_parts(), n_parts(),
                     dec_sig.significand_parts(),
                     calc_sem.precision_ - excess_precision, excess_precision);

        exp_ = (dec_sig.exp_ + sem_->precision_ -
                (calc_sem.precision_ - excess_precision));
        calc_lost_fraction = lost_fraction_through_truncation(
            dec_sig.significand_parts(), dec_sig.n_parts(), truncated_bits);
        return normalize(rounding_mode, calc_lost_fraction);
      }
    }
  }

  std::optional<OpStatus> from_decimal_string(StringRef str,
                                              RoundingMode rounding_mode) {
    DecimalInfo d;
    OpStatus fs;

    StringRef::const_iterator p = str.begin();
    if (!interpret_decimal(p, str.end(), &d))
      return {};

    if (d.first_sig_ == str.end() || dec_digit(*d.first_sig_) >= 10U) {
      category_ = FloatCategory::kZero;
      fs = OpStatus::kOK;
    } else if (d.normalized_exp_ - 1 > INT_MAX / 42039) {
      fs = handle_overflow(rounding_mode);
    } else if (d.normalized_exp_ - 1 < INT_MIN / 42039 ||
               (d.normalized_exp_ + 1) * 28738 <=
                   8651 *
                       (sem_->min_exp_ - static_cast<int>(sem_->precision_))) {
      category_ = FloatCategory::kNormal;
      significand_zero();
      fs = normalize(rounding_mode, LostFraction::kLessThanHalf);
    } else if ((d.normalized_exp_ - 1) * 42039 >= 12655 * sem_->max_exp_) {
      fs = handle_overflow(rounding_mode);
    } else {
      int_part_type* dec_significand;
      unsigned int part_count;

      part_count = static_cast<unsigned int>(d.last_sig_ - d.first_sig_) + 1;
      part_count = n_parts_of_bits(1 + 196 * part_count / 59);
      dec_significand = new int_part_type[part_count + 1];
      part_count = 0;

      do {
        int_part_type dec_val;
        int_part_type val;
        int_part_type multiplier;

        val = 0;
        multiplier = 1;

        do {
          if (*p == '.') {
            p++;
            if (p == str.end()) {
              break;
            }
          }
          dec_val = dec_digit(*p++);
          if (dec_val >= 10U) {
            delete[] dec_significand;
            return {};
          }
          multiplier *= 10;
          val = val * 10 + dec_val;
        } while (p <= d.last_sig_ &&
                 multiplier <= (~static_cast<int_part_type>(0) - 9) / 10);

        Int::mul_parts(dec_significand, dec_significand, multiplier, val,
                       part_count, part_count + 1, false);

        if (dec_significand[part_count])
          part_count++;
      } while (p <= d.last_sig_);

      category_ = FloatCategory::kNormal;
      fs = round_significand_with_exp(dec_significand, part_count, d.exp_,
                                      rounding_mode);

      delete[] dec_significand;
    }

    return fs;
  }

  template <uint64_t P0, uint64_t P1, uint64_t P2, uint64_t P3, uint64_t P4,
            uint64_t P5>
  void init_from_word(const Int& api, const FloatSemantics& sem) {
    uint64_t i = *api.raw_data();
    uint64_t exp = (i >> P0) & P1;
    uint64_t significand = i & P2;

    init(&sem);
    lps_assert(kTag, n_parts() == 1);

    sign_ = i >> P3;

    if (exp == 0 && significand == 0) {
      make_zero(sign_);
    } else if (exp == P1 && significand == 0) {
      make_inf(sign_);
    } else if (exp == P1 && significand != 0) {
      category_ = FloatCategory::kNaN;
      exp_ = exp_NaN();
      *significand_parts() = significand;
    } else {
      category_ = FloatCategory::kNormal;
      exp_ = static_cast<exp_part_type>(exp - P4);
      *significand_parts() = significand;
      if (exp == 0)
        exp_ = static_cast<exp_part_type>(-P4 + 1);
      else
        *significand_parts() |= P5;
    }
  }

  void init_from_int(const FloatSemantics* Sem, const Int& api);
  void init_from_half_int(const Int& api);
  void init_from_bfloat_int(const Int& api);
  void init_from_float_int(const Int& api);
  void init_from_double_int(const Int& api);
  void init_from_quad_int(const Int& api);

  static inline uint64_t n_parts_of_bits(unsigned int bits) {
    return ((bits) + kIntPartBits - 1) / kIntPartBits;
  }

  [[nodiscard]] uint64_t n_parts() const {
    return n_parts_of_bits(sem_->precision_ + 1);
  }

  [[nodiscard]] const int_part_type* significand_parts() const {
    return const_cast<IEEEFloat*>(this)->significand_parts();
  }

  int_part_type* significand_parts() {
    if (n_parts() > 1)
      return significand_.parts_;
    return &significand_.part_;
  }

  static LostFraction lost_fraction_through_truncation(
      const int_part_type* parts, uint64_t partCount, uint64_t bits) {
    uint64_t lsb = Int::LSB(parts, partCount);

    if (bits <= lsb)
      return LostFraction::kExactlyZero;
    if (bits == lsb + 1)
      return LostFraction::kExactlyHalf;
    if (bits <= partCount * kIntPartBits && Int::extract_bit(parts, bits - 1))
      return LostFraction::kMoreThanHalf;

    return LostFraction::kLessThanHalf;
  }

  OpStatus convert_from_unsigned_parts(const int_part_type* src, uint64_t n_src,
                                       RoundingMode rounding_mode) {

    LostFraction lost_fraction;

    category_ = FloatCategory::kNormal;
    auto omsb = Int::MSB(src, n_src) + 1;
    auto* dst = significand_parts();
    auto n_dst = n_parts();
    auto precision = sem_->precision_;

    if (precision <= omsb) {
      exp_ = omsb - 1;
      lost_fraction =
          lost_fraction_through_truncation(src, n_src, omsb - precision);
      Int::extract(dst, n_dst, src, precision, omsb - precision);
    } else {
      exp_ = precision - 1;
      lost_fraction = LostFraction::kExactlyZero;
      Int::extract(dst, n_dst, src, omsb, 0);
    }

    return normalize(rounding_mode, lost_fraction);
  }

  static LostFraction shift_right(int_part_type* dst, uint64_t parts,
                                  uint64_t bits) {
    LostFraction lost_fraction;
    lost_fraction = lost_fraction_through_truncation(dst, parts, bits);
    Int::shift_right(dst, parts, bits);
    return lost_fraction;
  }

  LostFraction shift_significand_right(uint64_t bits) {

    lps_assert(kTag, (exp_part_type)(exp_ + bits) >= exp_);

    exp_ += bits;

    return shift_right(significand_parts(), n_parts(), bits);
  }

  void shift_significand_left(unsigned int bits) {
    lps_assert(kTag, bits < sem_->precision_);

    if (bits) {
      unsigned int np = n_parts();
      Int::shift_left(significand_parts(), np, bits);
      exp_ -= bits;

      lps_assert(kTag, !Int::is_zero(significand_parts(), np));
    }
  }

  [[nodiscard]] Compareresult compare_abs(const IEEEFloat& rhs) const {
    int compare;

    lps_assert(kTag, sem_ == rhs.sem_);
    lps_assert(kTag, is_finite_nonzero());
    lps_assert(kTag, rhs.is_finite_nonzero());

    compare = exp_ - rhs.exp_;

    if (compare == 0) {
      compare =
          Int::compare(significand_parts(), rhs.significand_parts(), n_parts());
    }

    if (compare > 0)
      return Compareresult::kGreaterThan;
    if (compare < 0)
      return Compareresult::kLessThan;
    return Compareresult::kEqual;
  }

  void copy_significand(const IEEEFloat& rhs) {
    lps_assert(kTag, is_finite_nonzero() || isNaN());
    lps_assert(kTag, rhs.n_parts() >= n_parts());
    Int::assign(significand_parts(), rhs.significand_parts(), n_parts());
  }

  int_part_type sub_significand(const IEEEFloat& rhs, int_part_type borrow) {
    int_part_type* parts;

    parts = significand_parts();

    lps_assert(kTag, sem_ == rhs.sem_);
    lps_assert(kTag, exp_ == rhs.exp_);

    return Int::sub(parts, rhs.significand_parts(), borrow, n_parts());
  }

  int_part_type add_significand(const IEEEFloat& rhs) {
    int_part_type* parts;

    parts = significand_parts();

    lps_assert(kTag, sem_ == rhs.sem_);
    lps_assert(kTag, exp_ == rhs.exp_);
    return Int::add(parts, rhs.significand_parts(), 0, n_parts());
  }

  [[nodiscard]] uint64_t MSB_significand() const {
    return Int::MSB(significand_parts(), n_parts());
  }
  [[nodiscard]] uint64_t LSB_significand() const {
    return Int::LSB(significand_parts(), n_parts());
  }

  void assign(const IEEEFloat& rhs) {
    lps_assert(kTag, sem_ == rhs.sem_);

    sign_ = rhs.sign_;
    category_ = rhs.category_;
    exp_ = rhs.exp_;
    if (is_finite_nonzero() || isNaN())
      copy_significand(rhs);
  }

  [[nodiscard]] exp_part_type exp_NaN() const {
    if (sem_->non_finite_behavior_ == FloatNonFiniteBehavior::kNanOnly)
      return sem_->max_exp_;
    return sem_->max_exp_ + 1;
  }

  void make_quiet() {
    lps_assert(kTag, isNaN());
    if (sem_->non_finite_behavior_ != FloatNonFiniteBehavior::kNanOnly)
      Int::set_bit(significand_parts(), sem_->precision_ - 2);
  }

  template <int8_t Type = 0>
  OpStatus add_or_sub_specials(const IEEEFloat& rhs) {
    bool subtract = Type == 1;
    switch (PackCategoriesIntoKey(category_, rhs.category_)) {
      default:
        unreachable(kTag);

      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kNaN):
      case PackCategoriesIntoKey(FloatCategory::kNormal, FloatCategory::kNaN):
      case PackCategoriesIntoKey(FloatCategory::kInfinity, FloatCategory::kNaN):
        assign(rhs);
        [[fallthrough]];
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kZero):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kNaN):
        if (is_signaling()) {
          make_quiet();
          return OpStatus::kInvalidOp;
        }
        return rhs.is_signaling() ? OpStatus::kInvalidOp : OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kNormal, FloatCategory::kZero):
      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kZero):
        return OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kNormal,
                                 FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kZero,
                                 FloatCategory::kInfinity):
        category_ = FloatCategory::kInfinity;
        sign_ = rhs.sign_ ^ subtract;
        return OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kNormal):
        assign(rhs);
        sign_ = rhs.sign_ ^ subtract;
        return OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kZero):
        /* Sign depends on rounding mode; handled by caller.  */
        return OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kInfinity):
        if (((sign_ ^ rhs.sign_) != 0) != subtract) {
          make_NaN();
          return OpStatus::kInvalidOp;
        }

        return OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kNormal,
                                 FloatCategory::kNormal):
        return OpStatus::kDivByZero;
    }
  }

  template <int8_t Type = 0>
  LostFraction add_or_sub_significand(const IEEEFloat& rhs) {
    int_part_type carry;
    LostFraction lost_fraction;
    int bits;

    bool subtract = Type == 1;
    subtract ^= static_cast<bool>(sign_ ^ rhs.sign_);

    bits = exp_ - rhs.exp_;

    if (subtract) {
      IEEEFloat temp_rhs(rhs);

      if (bits == 0)
        lost_fraction = LostFraction::kExactlyZero;
      else if (bits > 0) {
        lost_fraction = temp_rhs.shift_significand_right(bits - 1);
        shift_significand_left(1);
      } else {
        lost_fraction = shift_significand_right(-bits - 1);
        temp_rhs.shift_significand_left(1);
      }

      if (compare_abs(temp_rhs) == Compareresult::kLessThan) {
        carry = temp_rhs.sub_significand(
            *this, lost_fraction != LostFraction::kExactlyZero);
        copy_significand(temp_rhs);
        sign_ = !sign_;
      } else {
        carry = sub_significand(temp_rhs,
                                lost_fraction != LostFraction::kExactlyZero);
      }

      if (lost_fraction == LostFraction::kLessThanHalf)
        lost_fraction = LostFraction::kMoreThanHalf;
      else if (lost_fraction == LostFraction::kMoreThanHalf)
        lost_fraction = LostFraction::kLessThanHalf;

      lps_assert(kTag, !carry);
    } else {
      if (bits > 0) {
        IEEEFloat temp_rhs(rhs);

        lost_fraction = temp_rhs.shift_significand_right(bits);
        carry = add_significand(temp_rhs);
      } else {
        lost_fraction = shift_significand_right(-bits);
        carry = add_significand(rhs);
      }
      lps_assert(kTag, !carry);
    }

    return lost_fraction;
  }
  [[nodiscard]] bool needs_cleanup() const {
    return n_parts() > 1;
  }
  void free_significand() {
    if (needs_cleanup())
      delete[] significand_.parts_;
  }

  OpStatus convert_from_Int(const Int& Val, bool is_signed,
                            RoundingMode rounding_mode) {
    auto n_part = Val.n_words();
    Int api = Val;

    sign_ = false;
    if (is_signed && api.is_negative()) {
      sign_ = true;
      api = -api;
    }

    return convert_from_unsigned_parts(api.raw_data(), n_part, rounding_mode);
  }

  LostFraction mul_significand(const IEEEFloat& rhs, IEEEFloat addend) {
    uint64_t omsb;  // One, not zero, based MSB.
    uint64_t parts_count;
    uint64_t new_parts_count;
    uint64_t precision;
    int_part_type* lh_significand;
    int_part_type scratch[4];
    int_part_type* full_significand;
    LostFraction lost_fraction;
    bool ignored;

    lps_assert(kTag, sem_ == rhs.sem_);

    precision = sem_->precision_;

    new_parts_count = n_parts_of_bits(precision * 2 + 1);

    if (new_parts_count > 4)
      full_significand = new int_part_type[new_parts_count];
    else
      full_significand = scratch;

    lh_significand = significand_parts();
    parts_count = n_parts();

    Int::full_mul(full_significand, lh_significand, rhs.significand_parts(),
                  parts_count, parts_count);

    lost_fraction = LostFraction::kExactlyZero;
    omsb = Int::MSB(full_significand, new_parts_count) + 1;
    exp_ += rhs.exp_;

    exp_ += 2;

    if (addend.is_nonzero()) {

      Significand save_significand = significand_;
      const FloatSemantics* saved_sem = sem_;
      FloatSemantics extended_sem;
      OpStatus status;
      uint64_t extended_precision;

      extended_precision = 2 * precision + 1;
      if (omsb != extended_precision - 1) {
        lps_assert(kTag, extended_precision > omsb);
        Int::shift_left(full_significand, new_parts_count,
                        (extended_precision - 1) - omsb);
        exp_ -= (extended_precision - 1) - omsb;
      }

      /* Create new semantics.  */
      extended_sem = *sem_;
      extended_sem.precision_ = extended_precision;

      if (new_parts_count == 1)
        significand_.part_ = full_significand[0];
      else
        significand_.parts_ = full_significand;
      sem_ = &extended_sem;

      IEEEFloat extended_addend(addend);
      status = extended_addend.convert(extended_sem, RoundingMode::kTowardZero,
                                       &ignored);
      lps_assert(kTag, status == OpStatus::kOK);

      lost_fraction = extended_addend.shift_significand_right(1);
      LPS_CHECK_ERROR(
          kTag, lost_fraction == LostFraction::kExactlyZero,
          "Lost precision while shifting addend for fused-multiply-add.");

      lost_fraction = add_or_sub_significand<0>(extended_addend);

      if (new_parts_count == 1)
        full_significand[0] = significand_.part_;
      significand_ = save_significand;
      sem_ = saved_sem;

      omsb = Int::MSB(full_significand, new_parts_count) + 1;
    }

    exp_ -= precision + 1;

    // expected.
    if (omsb > precision) {
      uint64_t bits;
      uint64_t significant_parts;
      LostFraction lf;

      bits = omsb - precision;
      significant_parts = n_parts_of_bits(omsb);
      lf = shift_right(full_significand, significant_parts, bits);
      lost_fraction = combine_LostFractions(lf, lost_fraction);
      exp_ += bits;
    }

    Int::assign(lh_significand, full_significand, parts_count);

    if (new_parts_count > 4)
      delete[] full_significand;

    return lost_fraction;
  }

  LostFraction div_significand(const IEEEFloat& rhs) {
    uint64_t bit;
    uint64_t i;
    uint64_t parts_count;
    const int_part_type* rhs_significand;
    int_part_type* lhs_significand;
    int_part_type* dividend;
    int_part_type* divisor;
    int_part_type scratch[4];
    LostFraction lost_fraction;

    lps_assert(kTag, sem_ == rhs.sem_);

    lhs_significand = significand_parts();
    rhs_significand = rhs.significand_parts();
    parts_count = n_parts();

    if (parts_count > 2)
      dividend = new int_part_type[parts_count * 2];
    else
      dividend = scratch;

    divisor = dividend + parts_count;

    for (i = 0; i < parts_count; i++) {
      dividend[i] = lhs_significand[i];
      divisor[i] = rhs_significand[i];
      lhs_significand[i] = 0;
    }

    exp_ -= rhs.exp_;

    unsigned int precision = sem_->precision_;

    /* Normalize the divisor.  */
    bit = precision - Int::MSB(divisor, parts_count) - 1;
    if (bit) {
      exp_ += bit;
      Int::shift_left(divisor, parts_count, bit);
    }

    /* Normalize the dividend.  */
    bit = precision - Int::MSB(dividend, parts_count) - 1;
    if (bit) {
      exp_ -= bit;
      Int::shift_left(dividend, parts_count, bit);
    }

    if (Int::compare(dividend, divisor, parts_count) < 0) {
      exp_--;
      Int::shift_left(dividend, parts_count, 1);
      lps_assert(kTag, Int::compare(dividend, divisor, parts_count) >= 0);
    }

    for (bit = precision; bit; bit -= 1) {
      if (Int::compare(dividend, divisor, parts_count) >= 0) {
        Int::sub(dividend, divisor, 0, parts_count);
        Int::set_bit(lhs_significand, bit - 1);
      }

      Int::shift_left(dividend, parts_count, 1);
    }

    /* Figure out the lost fraction.  */
    int cmp = Int::compare(dividend, divisor, parts_count);

    if (cmp > 0)
      lost_fraction = LostFraction::kMoreThanHalf;
    else if (cmp == 0)
      lost_fraction = LostFraction::kExactlyHalf;
    else if (Int::is_zero(dividend, parts_count))
      lost_fraction = LostFraction::kExactlyZero;
    else
      lost_fraction = LostFraction::kLessThanHalf;

    if (parts_count > 2)
      delete[] dividend;

    return lost_fraction;
  }

  void increment_significand() {
    auto carry = Int::increment(significand_parts(), n_parts());
    lps_assert(kTag, carry == 0);
  }

  OpStatus handle_overflow(RoundingMode rounding_mode) {
    /* Infinity?  */
    if (rounding_mode == RoundingMode::kNearestTiesToEven ||
        rounding_mode == RoundingMode::kNearestTiesToAway ||
        (rounding_mode == RoundingMode::kTowardPositive && !sign_) ||
        (rounding_mode == RoundingMode::kTowardNegative && sign_)) {
      if (sem_->non_finite_behavior_ == FloatNonFiniteBehavior::kNanOnly)
        make_NaN(false, sign_);
      else
        category_ = FloatCategory::kInfinity;
      return static_cast<OpStatus>(static_cast<uint8_t>(OpStatus::kOverflow) |
                                   static_cast<uint8_t>(OpStatus::kInexact));
    }

    category_ = FloatCategory::kNormal;
    exp_ = sem_->max_exp_;
    set_least_significant_bits(significand_parts(), n_parts(),
                               sem_->precision_);
    if (sem_->non_finite_behavior_ == FloatNonFiniteBehavior::kNanOnly)
      Int::clear_bit(significand_parts(), 0);

    return OpStatus::kInexact;
  }

  static LostFraction combine_LostFractions(LostFraction moreSignificant,
                                            LostFraction lessSignificant) {
    if (lessSignificant != LostFraction::kExactlyZero) {
      if (moreSignificant == LostFraction::kExactlyZero)
        moreSignificant = LostFraction::kLessThanHalf;
      else if (moreSignificant == LostFraction::kExactlyHalf)
        moreSignificant = LostFraction::kMoreThanHalf;
    }

    return moreSignificant;
  }

  [[nodiscard]] bool is_significand_all_ones_except_LSB() const {

    const int_part_type* parts = significand_parts();

    if (parts[0] & 1)
      return false;

    const uint64_t part_count = n_parts_of_bits(sem_->precision_);
    for (uint64_t i = 0; i < part_count - 1; i++) {
      if (~parts[i] & ~uint64_t{!i})
        return false;
    }

    const unsigned num_high_bits =
        part_count * kIntPartBits - sem_->precision_ + 1;
    LPS_CHECK_ERROR(kTag, num_high_bits <= kIntPartBits && num_high_bits > 0,
                    "can not have more high bits to fill than kIntPartBits");
    const int_part_type higt_bit_fill = ~static_cast<int_part_type>(0)
                                        << (kIntPartBits - num_high_bits);
    if (~(parts[part_count - 1] | higt_bit_fill | 0x1))
      return false;

    return true;
  }

  [[nodiscard]] bool is_significand_all_ones() const {
    const auto* parts = significand_parts();
    const unsigned n_parts = n_parts_of_bits(sem_->precision_);
    for (unsigned i = 0; i < n_parts - 1; i++)
      if (~parts[i])
        return false;

    const unsigned n_high_bits = n_parts * kIntPartBits - sem_->precision_ + 1;
    LPS_CHECK_ERROR(kTag, n_high_bits <= kIntPartBits && n_high_bits > 0,
                    "Can not have more high bits to fill than kIntPartBits");
    const int_part_type hight_bits_fill = ~static_cast<int_part_type>(0)
                                          << (kIntPartBits - n_high_bits);

    return (~(parts[n_parts - 1] | hight_bits_fill)) == 0U;
  }

  [[nodiscard]] bool is_significand_all_zeros() const {
    const auto* parts = significand_parts();
    const unsigned n_parts = n_parts_of_bits(sem_->precision_);

    for (unsigned i = 0; i < n_parts - 1; i++)
      if (parts[i])
        return false;

    // Compute how many bits are used in the final word.
    const unsigned num_high_bits =
        n_parts * kIntPartBits - sem_->precision_ + 1;
    LPS_CHECK_ERROR(kTag, num_high_bits < kIntPartBits &&
                              "Can not have more high bits to "
                              "clear than kIntPartBits");
    const int_part_type high_bit_mask =
        ~static_cast<int_part_type>(0) >> num_high_bits;

    return (parts[n_parts - 1] & high_bit_mask) == 0;
  }

  [[nodiscard]] bool round_away_from_zero(RoundingMode rounding_mode,
                                          LostFraction lost_fraction,
                                          uint64_t bit) const {
    lps_assert(kTag, is_finite_nonzero() || category_ == FloatCategory::kZero);

    lps_assert(kTag, lost_fraction != LostFraction::kExactlyZero);

    switch (rounding_mode) {
      case RoundingMode::kNearestTiesToAway:
        return lost_fraction == LostFraction::kExactlyHalf ||
               lost_fraction == LostFraction::kMoreThanHalf;

      case RoundingMode::kNearestTiesToEven:
        if (lost_fraction == LostFraction::kMoreThanHalf)
          return true;

        /* Our zeroes don't have a significand to test.  */
        if (lost_fraction == LostFraction::kExactlyHalf &&
            category_ != FloatCategory::kZero)
          return Int::extract_bit(significand_parts(), bit) != 0;

        return false;

      case RoundingMode::kTowardZero:
        return false;

      case RoundingMode::kTowardPositive:
        return !sign_;

      case RoundingMode::kTowardNegative:
        return sign_;

      default:
        break;
    }
    unreachable(kTag);
    return false;
  }

  OpStatus normalize(RoundingMode rounding_mode, LostFraction lost_fraction) {
    uint64_t omsb;
    int exp_change;

    if (!is_finite_nonzero())
      return OpStatus::kOK;

    omsb = MSB_significand() + 1;

    if (omsb) {
      exp_change = omsb - sem_->precision_;
      if (exp_ + exp_change > sem_->max_exp_)
        return handle_overflow(rounding_mode);

      if (exp_ + exp_change < sem_->min_exp_)
        exp_change = sem_->min_exp_ - exp_;

      if (exp_change < 0) {
        lps_assert(kTag, lost_fraction == LostFraction::kExactlyZero);
        shift_significand_left(-exp_change);
        return OpStatus::kOK;
      }

      if (exp_change > 0) {
        LostFraction lf;

        lf = shift_significand_right(exp_change);

        lost_fraction = combine_LostFractions(lf, lost_fraction);

        /* Keep OMSB up-to-date.  */
        if (omsb > static_cast<uint64_t>(exp_change))
          omsb -= exp_change;
        else
          omsb = 0;
      }
    }

    if (sem_->non_finite_behavior_ == FloatNonFiniteBehavior::kNanOnly &&
        exp_ == sem_->max_exp_ && is_significand_all_ones())
      return handle_overflow(rounding_mode);

    if (lost_fraction == LostFraction::kExactlyZero) {
      if (omsb == 0)
        category_ = FloatCategory::kZero;

      return OpStatus::kOK;
    }

    if (round_away_from_zero(rounding_mode, lost_fraction, 0)) {
      if (omsb == 0)
        exp_ = sem_->min_exp_;

      increment_significand();
      omsb = MSB_significand() + 1;

      if (omsb == sem_->precision_ + 1) {

        if (exp_ == sem_->max_exp_) {
          category_ = FloatCategory::kInfinity;

          return static_cast<OpStatus>(
              static_cast<uint8_t>(OpStatus::kOverflow) |
              static_cast<uint8_t>(OpStatus::kInexact));
        }

        shift_significand_right(1);

        return OpStatus::kInexact;
      }

      if (sem_->non_finite_behavior_ == FloatNonFiniteBehavior::kNanOnly &&
          exp_ == sem_->max_exp_ && is_significand_all_ones())
        return handle_overflow(rounding_mode);
    }

    if (omsb == sem_->precision_)
      return OpStatus::kInexact;

    lps_assert(kTag, omsb < sem_->precision_);

    if (omsb == 0)
      category_ = FloatCategory::kZero;

    return static_cast<OpStatus>(static_cast<uint8_t>(OpStatus::kUnderflow) |
                                 static_cast<uint8_t>(OpStatus::kInexact));
  }

  template <int8_t Type = 0>
  OpStatus add_or_sub(const IEEEFloat& rhs, RoundingMode rounding_mode) {
    OpStatus fs;

    fs = add_or_sub_specials<Type>(rhs);
    if (fs == OpStatus::kDivByZero) {
      LostFraction lost_fraction;

      lost_fraction = add_or_sub_significand<Type>(rhs);
      fs = normalize(rounding_mode, lost_fraction);

      lps_assert(kTag, category_ != FloatCategory::kZero ||
                           lost_fraction == LostFraction::kExactlyZero);
    }

    if (category_ == FloatCategory::kZero) {
      if (rhs.category_ != FloatCategory::kZero ||
          (sign_ == rhs.sign_) == (Type == 1))
        sign_ = (rounding_mode == RoundingMode::kTowardNegative);
    }
    return fs;
  }

  LostFraction mul_significand(const IEEEFloat& rhs) {
    return mul_significand(rhs, IEEEFloat(*sem_));
  }

 public:
  static int ilogb(const IEEEFloat& Arg) {
    if (Arg.isNaN())
      return static_cast<int>(IlogbErrorKinds::kNaN);
    if (Arg.is_zero())
      return static_cast<int>(IlogbErrorKinds::kZero);
    if (Arg.is_infinity())
      return static_cast<int>(IlogbErrorKinds::kInf);
    if (!Arg.is_denormal())
      return Arg.exp_;

    IEEEFloat normalized(Arg);
    int significand_bits = Arg.sem_->precision_ - 1;

    normalized.exp_ += significand_bits;
    normalized.normalize(IEEEFloat::RoundingMode::kNearestTiesToEven,
                         LostFraction::kExactlyZero);
    return normalized.exp_ - significand_bits;
  }

  static IEEEFloat scalbn(IEEEFloat X, int Exp, RoundingMode rm) {
    auto max_exp = X.sem_->max_exp_;
    auto min_exp = X.sem_->min_exp_;

    int significand_bits = X.sem_->precision_ - 1;
    int max_increment = max_exp - (min_exp - significand_bits) + 1;

    X.exp_ += std::min(std::max(Exp, -max_increment - 1), max_increment);
    X.normalize(rm, LostFraction::kExactlyZero);
    if (X.isNaN())
      X.make_quiet();
    return X;
  }

  static IEEEFloat frexp(const IEEEFloat& Val, int& Exp, RoundingMode rm) {
    Exp = ilogb(Val);

    if (static_cast<IlogbErrorKinds>(Exp) == IlogbErrorKinds::kNaN) {
      IEEEFloat quiet(Val);
      quiet.make_quiet();
      return quiet;
    }

    if (static_cast<IlogbErrorKinds>(Exp) == IlogbErrorKinds::kInf)
      return Val;

    Exp = static_cast<IlogbErrorKinds>(Exp) == IlogbErrorKinds::kZero ? 0
                                                                      : Exp + 1;
    return scalbn(Val, -Exp, rm);
  }

 private:
  OpStatus mul_specials(const IEEEFloat& rhs) {
    switch (PackCategoriesIntoKey(category_, rhs.category_)) {
      default:
        unreachable(kTag);

      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kNaN):
      case PackCategoriesIntoKey(FloatCategory::kNormal, FloatCategory::kNaN):
      case PackCategoriesIntoKey(FloatCategory::kInfinity, FloatCategory::kNaN):
        assign(rhs);
        sign_ = false;
        [[fallthrough]];
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kZero):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kNaN):
        sign_ ^= rhs.sign_;  // restore the original sign
        if (is_signaling()) {
          make_quiet();
          return OpStatus::kInvalidOp;
        }
        return rhs.is_signaling() ? OpStatus::kInvalidOp : OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kNormal,
                                 FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kInfinity):
        category_ = FloatCategory::kInfinity;
        return OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kNormal, FloatCategory::kZero):
      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kZero):
        category_ = FloatCategory::kZero;
        return OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kZero,
                                 FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kZero):
        make_NaN();
        return OpStatus::kInvalidOp;

      case PackCategoriesIntoKey(FloatCategory::kNormal,
                                 FloatCategory::kNormal):
        return OpStatus::kOK;
    }
  }

  OpStatus div_specials(const IEEEFloat& rhs) {
    switch (PackCategoriesIntoKey(category_, rhs.category_)) {
      default:
        unreachable(kTag);

      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kNaN):
      case PackCategoriesIntoKey(FloatCategory::kNormal, FloatCategory::kNaN):
      case PackCategoriesIntoKey(FloatCategory::kInfinity, FloatCategory::kNaN):
        assign(rhs);
        sign_ = false;
        [[fallthrough]];
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kZero):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kNaN):
        sign_ ^= rhs.sign_;  // restore the original sign
        if (is_signaling()) {
          make_quiet();
          return OpStatus::kInvalidOp;
        }
        return rhs.is_signaling() ? OpStatus::kInvalidOp : OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kZero):
      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kZero,
                                 FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kNormal):
        return OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kNormal,
                                 FloatCategory::kInfinity):
        category_ = FloatCategory::kZero;
        return OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kNormal, FloatCategory::kZero):
        if (sem_->non_finite_behavior_ == FloatNonFiniteBehavior::kNanOnly)
          make_NaN(false, sign_);
        else
          category_ = FloatCategory::kInfinity;
        return OpStatus::kDivByZero;

      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kZero):
        make_NaN();
        return OpStatus::kInvalidOp;

      case PackCategoriesIntoKey(FloatCategory::kNormal,
                                 FloatCategory::kNormal):
        return OpStatus::kOK;
    }
  }

  OpStatus mod_specials(const IEEEFloat& rhs) {
    switch (PackCategoriesIntoKey(category_, rhs.category_)) {
      default:
        unreachable(kTag);

      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kNaN):
      case PackCategoriesIntoKey(FloatCategory::kNormal, FloatCategory::kNaN):
      case PackCategoriesIntoKey(FloatCategory::kInfinity, FloatCategory::kNaN):
        assign(rhs);
        [[fallthrough]];
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kZero):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kNaN):
        if (is_signaling()) {
          make_quiet();
          return OpStatus::kInvalidOp;
        }
        return rhs.is_signaling() ? OpStatus::kInvalidOp : OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kZero,
                                 FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kNormal,
                                 FloatCategory::kInfinity):
        return OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kNormal, FloatCategory::kZero):
      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kZero):
      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kZero):
        make_NaN();
        return OpStatus::kInvalidOp;

      case PackCategoriesIntoKey(FloatCategory::kNormal,
                                 FloatCategory::kNormal):
        return OpStatus::kOK;
    }
  }

  OpStatus rem_specials(const IEEEFloat& rhs) {
    switch (PackCategoriesIntoKey(category_, rhs.category_)) {
      default:
        unreachable(kTag);

      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kNaN):
      case PackCategoriesIntoKey(FloatCategory::kNormal, FloatCategory::kNaN):
      case PackCategoriesIntoKey(FloatCategory::kInfinity, FloatCategory::kNaN):
        assign(rhs);
        [[fallthrough]];
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kZero):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kNaN, FloatCategory::kNaN):
        if (is_signaling()) {
          make_quiet();
          return OpStatus::kInvalidOp;
        }
        return rhs.is_signaling() ? OpStatus::kInvalidOp : OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kZero,
                                 FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kNormal,
                                 FloatCategory::kInfinity):
        return OpStatus::kOK;

      case PackCategoriesIntoKey(FloatCategory::kNormal, FloatCategory::kZero):
      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kZero):
      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kNormal):
      case PackCategoriesIntoKey(FloatCategory::kInfinity,
                                 FloatCategory::kInfinity):
      case PackCategoriesIntoKey(FloatCategory::kZero, FloatCategory::kZero):
        make_NaN();
        return OpStatus::kInvalidOp;

      case PackCategoriesIntoKey(FloatCategory::kNormal,
                                 FloatCategory::kNormal):
        return OpStatus::
            kDivByZero;  // fake status, indicating this is not a special case
    }
  }

#undef PackCategoriesIntoKey

  void initialize(const FloatSemantics* our_semantics) {
    sem_ = our_semantics;
    auto count = n_parts();
    if (count > 1) {
      significand_.parts_ = new int_part_type[count];
    }
  }

  const FloatSemantics* sem_;
  union Significand {
    int_part_type part_;
    int_part_type* parts_;
  } significand_;

  exp_part_type exp_;

  FloatCategory category_;
  uint8_t sign_;
};
}  // namespace details

class Float {
 public:
  static constexpr const char* kTag = "Float";

  explicit Float(const details::FloatSemantics& sem) : val_(sem) {}
  Float(const details::FloatSemantics& sem, StringRef S) : Float(sem) {
    auto status_or_none =
        form(S, details::FloatBase::RoundingMode::kNearestTiesToEven);
    LPS_CHECK_ERROR(kTag, status_or_none,
                    "Invalid floating point representation");
  }

  explicit Float(details::IEEEFloat F, const details::FloatSemantics& S)
      : val_(std::move(F), S) {}

  Float(const details::FloatSemantics& sem, details::FloatBase::int_part_type I)
      : val_(sem, I) {}
  template <typename T,
            typename = std::enable_if_t<std::is_floating_point<T>::value>>
  Float(const details::FloatSemantics& sem, T V) = delete;
  Float(const details::FloatSemantics& sem, details::UninitializedTag)
      : val_(sem, details::UninitializedTag::kUninitialized) {}
  Float(const details::FloatSemantics& sem, const Int& I) : val_(sem, I) {}
  explicit Float(double d)
      : val_(details::IEEEFloat(d), details::FloatBase::IEEEdouble()) {}
  explicit Float(float f)
      : val_(details::IEEEFloat(f), details::FloatBase::IEEEsingle()) {}
  Float(const Float& RHS) = default;
  Float(Float&& RHS) = default;

  ~Float() = default;

  template <bool IsSigned>
  static Float xNaN(const details::FloatSemantics& sem, bool negative = false,
                    const Int* payload = nullptr) {
    Float val(sem, details::UninitializedTag::kUninitialized);
    val.make_NaN(IsSigned, negative, payload);
    return val;
  }

  static Float qNaN(const details::FloatSemantics& sem, bool negative = false,
                    const Int* payload = nullptr) {
    return xNaN<false>(sem, negative, payload);
  }

  static Float sNaN(const details::FloatSemantics& sem, bool negative = false,
                    const Int* payload = nullptr) {
    return xNaN<true>(sem, negative, payload);
  }

  static Float NaN(const details::FloatSemantics& Sem, bool negative = false,
                   uint64_t payload = 0) {
    if (payload) {
      Int p(64, payload);
      return qNaN(Sem, negative, &p);
    }
    return qNaN(Sem, negative, nullptr);
  }

  static Float inf(const details::FloatSemantics& sem, bool neg = false) {
    Float val(sem, details::UninitializedTag::kUninitialized);
    val.make_inf(neg);
    return val;
  }

  static Float smallest_normalized(const details::FloatSemantics& sem,
                                   bool neg = false) {
    Float val(sem, details::UninitializedTag::kUninitialized);
    val.make_smallest_normalized(neg);
    return val;
  }

  static Float largest(const details::FloatSemantics& sem, bool neg = false) {
    Float val(sem, details::UninitializedTag::kUninitialized);
    val.make_largest(neg);
    return val;
  }

  static Float smallest(const details::FloatSemantics& sem, bool neg = false) {
    Float val(sem, details::UninitializedTag::kUninitialized);
    val.make_smallest(neg);
    return val;
  }

  static Float zero(const details::FloatSemantics& sem, bool negative = false) {
    Float val(sem, details::UninitializedTag::kUninitialized);
    val.make_zero(negative);
    return val;
  }

  static Float all_ones(const details::FloatSemantics& sem) {
    return Float(sem, Int::all_ones(sem.bits_));
  }

  [[nodiscard]] const details::FloatSemantics& sem() const {
    return *val_.sem_;
  }

  Float operator-() const {
    Float r(*this);
    r.change_sign();
    return r;
  }

  Float operator+(const Float& other) const {
    Float r(*this);
    (void)r.add(other, details::FloatBase::RoundingMode::kNearestTiesToEven);
    return r;
  }

  Float operator-(const Float& other) const {
    Float r(*this);
    (void)r.sub(other, details::FloatBase::RoundingMode::kNearestTiesToEven);
    return r;
  }

  Float operator*(const Float& other) const {
    Float r(*this);
    (void)r.mul(other, details::FloatBase::RoundingMode::kNearestTiesToEven);
    return r;
  }

  Float operator/(const Float& other) const {
    Float r(*this);
    (void)r.div(other, details::FloatBase::RoundingMode::kNearestTiesToEven);
    return r;
  }

  bool operator==(const Float& RHS) const {
    return compare(RHS) == details::FloatBase::Compareresult::kEqual;
  }

  bool operator!=(const Float& RHS) const {
    return compare(RHS) != details::FloatBase::Compareresult::kEqual;
  }

  bool operator<(const Float& RHS) const {
    return compare(RHS) == details::FloatBase::Compareresult::kLessThan;
  }

  bool operator>(const Float& RHS) const {
    return compare(RHS) == details::FloatBase::Compareresult::kGreaterThan;
  }

  bool operator<=(const Float& RHS) const {
    details::FloatBase::Compareresult res = compare(RHS);
    return res == details::FloatBase::Compareresult::kLessThan ||
           res == details::FloatBase::Compareresult::kEqual;
  }

  bool operator>=(const Float& RHS) const {
    details::FloatBase::Compareresult res = compare(RHS);
    return res == details::FloatBase::Compareresult::kGreaterThan ||
           res == details::FloatBase::Compareresult::kEqual;
  }

#define DISPATCH_ON_SEMANTICS(METHOD_CALL)     \
  do {                                         \
    if (usesLayout<details::IEEEFloat>(sem())) \
      return val_.float_.METHOD_CALL;          \
    unreachable(kTag);                         \
  } while (false)

  std::optional<details::FloatBase::OpStatus> form(
      StringRef str, details::FloatBase::RoundingMode rm) {
    DISPATCH_ON_SEMANTICS(form(str, rm));
    return {};
  }

  [[nodiscard]] details::FloatBase::OpStatus next(bool next_down) {
    DISPATCH_ON_SEMANTICS(next(next_down));
    return details::FloatBase::OpStatus::kInvalidOp;
  }

  [[nodiscard]] bool is_zero() const {
    DISPATCH_ON_SEMANTICS(is_zero());
    return false;
  }

  [[nodiscard]] bool is_denormal() const {
    DISPATCH_ON_SEMANTICS(is_denormal());
    return false;
  }

  [[nodiscard]] bool is_smallest_normalized() const {
    DISPATCH_ON_SEMANTICS(is_smallest_normalized());
    return false;
  }

  [[nodiscard]] bool is_finite_nonzero() const {
    return is_finite() && !is_zero();
  }

  [[nodiscard]] bool is_finite() const {
    return !isNaN() && !is_infinity();
  }

  [[nodiscard]] bool is_normal() const {
    return !is_denormal() && is_finite_nonzero();
  }

  [[nodiscard]] bool is_negative() const {
    DISPATCH_ON_SEMANTICS(is_negative());
    return false;
  }
  [[nodiscard]] details::FloatBase::FloatCategory category() const {
    DISPATCH_ON_SEMANTICS(category());
    return details::FloatBase::FloatCategory::kUnknown;
  }

  void make_zero(bool neg) {
    DISPATCH_ON_SEMANTICS(make_zero(neg));
  }

  void make_smallest_normalized(bool neg) {
    DISPATCH_ON_SEMANTICS(make_smallest_normalized(neg));
  }

  void make_inf(bool neg) {
    DISPATCH_ON_SEMANTICS(make_inf(neg));
  }

  void make_largest(bool neg) {
    DISPATCH_ON_SEMANTICS(make_largest(neg));
  }

  void make_smallest(bool neg) {
    DISPATCH_ON_SEMANTICS(make_smallest(neg));
  }

  void make_NaN(bool SNaN, bool Neg, const Int* fill) {
    DISPATCH_ON_SEMANTICS(make_NaN(SNaN, Neg, fill));
  }

  [[nodiscard]] Int bit_cast_to_int() const {
    DISPATCH_ON_SEMANTICS(bit_cast_to_int());
    return Int();
  }

  bool exact_inverse(Float* inv) const {
    DISPATCH_ON_SEMANTICS(exact_inverse(inv));
    return false;
  }

  details::FloatBase::OpStatus round2int(details::FloatBase::RoundingMode rm) {
    DISPATCH_ON_SEMANTICS(round2int(rm));
    return details::FloatBase::OpStatus::kInvalidOp;
  }

  void string(basic::vec::details::TemplateCommon<char>& Str,
              unsigned FormatPrecision = 0, unsigned FormatMaxPadding = 3,
              bool TruncateZero = true) const {
    DISPATCH_ON_SEMANTICS(
        string(Str, FormatPrecision, FormatMaxPadding, TruncateZero));
  }

  details::FloatBase::OpStatus integer(
      basic::vec::details::TemplateCommon<details::FloatBase::int_part_type>&
          Input,
      uint64_t Width, bool IsSigned, details::FloatBase::RoundingMode RM,
      bool* IsExact) const {
    DISPATCH_ON_SEMANTICS(integer(Input, Width, IsSigned, RM, IsExact));
    return details::FloatBase::OpStatus::kInvalidOp;
  }

  details::FloatBase::OpStatus integer(
      SInt& result, details::FloatBase::RoundingMode rounding_mode,
      bool* isExact) const {
    auto bits = result.bits();
    basic::Vector<4, uint64_t> parts(result.n_words());
    details::FloatBase::OpStatus status =
        integer(parts, bits, result.is_signed(), rounding_mode, isExact);
    result = Int(bits, parts.data(), parts.size());
    return status;
  }

  friend int ilogb(const Float& Arg) {
    return details::IEEEFloat::ilogb(Arg.IEEE());
  }

  friend inline Float scalbn(Float x, int exp,
                             details::FloatBase::RoundingMode RM) {
    LPS_CHECK_ERROR(kTag, Float::usesLayout<details::IEEEFloat>(x.sem()),
                    "not supported semantics");
    return Float(details::IEEEFloat::scalbn(x.val_.float_, exp, RM), x.sem());
  }

  friend inline Float frexp(const Float& x, int& exp,
                            details::FloatBase::RoundingMode rm) {
    LPS_CHECK_ERROR(kTag, Float::usesLayout<details::IEEEFloat>(x.sem()),
                    "not supported semantics");
    return Float(details::IEEEFloat::frexp(x.val_.float_, exp, rm), x.sem());
  }

  [[nodiscard]] bool bitwise_equal(const Float& RHS) const {
    if (&sem() != &RHS.sem())
      return false;
    DISPATCH_ON_SEMANTICS(bitwise_equal(RHS.val_.float_));
    return false;
  }

  [[nodiscard]] bool is_signaling() const {
    return IEEE().is_signaling();
  }

  [[nodiscard]] bool is_int() const {
    DISPATCH_ON_SEMANTICS(is_int());
    return false;
  }

  [[nodiscard]] bool is_pos_zero() const {
    return is_zero() && !is_negative();
  }
  [[nodiscard]] bool is_neg_zero() const {
    return is_zero() && is_negative();
  }
  [[nodiscard]] bool is_pos_infinity() const {
    return is_infinity() && !is_negative();
  }
  [[nodiscard]] bool is_neg_infinity() const {
    return is_infinity() && is_negative();
  }
  [[nodiscard]] bool isNaN() const {
    return category() == details::FloatBase::FloatCategory::kNaN;
  }
  [[nodiscard]] bool is_infinity() const {
    return category() == details::FloatBase::FloatCategory::kInfinity;
  }

  Float& operator=(const Float&) = default;
  Float& operator=(Float&&) = default;

  [[nodiscard]] details::FloatBase::Compareresult compare(
      const Float& rhs) const {
    LPS_CHECK_ERROR(
        kTag, &sem() == &rhs.sem(),
        "should only call on two IEEEFloat with the same semantics");
    DISPATCH_ON_SEMANTICS(compare(rhs.val_.float_));
    return details::FloatBase::Compareresult::kUnknown;
  }

  details::FloatBase::OpStatus add(
      const Float& rhs,
      details::FloatBase::RoundingMode rm =
          details::FloatBase::RoundingMode::kNearestTiesToEven) {
    LPS_CHECK_ERROR(
        kTag, &sem() == &rhs.sem(),
        "should only call on two IEEEFloat with the same semantics");
    DISPATCH_ON_SEMANTICS(add(rhs.val_.float_, rm));
    return details::FloatBase::OpStatus::kInvalidOp;
  }
  details::FloatBase::OpStatus sub(
      const Float& rhs,
      details::FloatBase::RoundingMode rm =
          details::FloatBase::RoundingMode::kNearestTiesToEven) {
    LPS_CHECK_ERROR(
        kTag, &sem() == &rhs.sem(),
        "should only call on two IEEEFloat with the same semantics");
    DISPATCH_ON_SEMANTICS(sub(rhs.val_.float_, rm));
    return details::FloatBase::OpStatus::kInvalidOp;
  }
  details::FloatBase::OpStatus mul(
      const Float& rhs,
      details::FloatBase::RoundingMode rm =
          details::FloatBase::RoundingMode::kNearestTiesToEven) {
    LPS_CHECK_ERROR(
        kTag, &sem() == &rhs.sem(),
        "should only call on two IEEEFloat with the same semantics");
    DISPATCH_ON_SEMANTICS(mul(rhs.val_.float_, rm));
    return details::FloatBase::OpStatus::kInvalidOp;
  }
  details::FloatBase::OpStatus div(
      const Float& rhs,
      details::FloatBase::RoundingMode rm =
          details::FloatBase::RoundingMode::kNearestTiesToEven) {
    LPS_CHECK_ERROR(
        kTag, &sem() == &rhs.sem(),
        "should only call on two IEEEFloat with the same semantics");
    DISPATCH_ON_SEMANTICS(div(rhs.val_.float_, rm));
    return details::FloatBase::OpStatus::kInvalidOp;
  }
  details::FloatBase::OpStatus rem(const Float& rhs) {
    LPS_CHECK_ERROR(
        kTag, &sem() == &rhs.sem(),
        "should only call on two IEEEFloat with the same semantics");
    DISPATCH_ON_SEMANTICS(rem(rhs.val_.float_));
    return details::FloatBase::OpStatus::kInvalidOp;
  }
  details::FloatBase::OpStatus mod(const Float& rhs) {
    LPS_CHECK_ERROR(
        kTag, &sem() == &rhs.sem(),
        "should only call on two IEEEFloat with the same semantics");
    DISPATCH_ON_SEMANTICS(mod(rhs.val_.float_));
    return details::FloatBase::OpStatus::kInvalidOp;
  }
  details::FloatBase::OpStatus fused_mul_add(
      const Float& multiplicand, const Float& addend,
      details::FloatBase::RoundingMode rm) {
    LPS_CHECK_ERROR(
        kTag, &sem() == &multiplicand.sem(),
        "should only call on two IEEEFloat with the same semantics");
    LPS_CHECK_ERROR(
        kTag, &sem() == &addend.sem(),
        "should only call on two IEEEFloat with the same semantics");
    DISPATCH_ON_SEMANTICS(
        fused_mul_add(multiplicand.val_.float_, addend.val_.float_, rm));
    return details::FloatBase::OpStatus::kInvalidOp;
  }

  details::FloatBase::OpStatus convert(const details::FloatSemantics& to_sem,
                                       details::FloatBase::RoundingMode rm,
                                       bool* losesInfo) {
    if (&sem() == &to_sem) {
      *losesInfo = false;
      return details::FloatBase::OpStatus::kOK;
    }
    if (usesLayout<details::IEEEFloat>(sem()) &&
        usesLayout<details::IEEEFloat>(to_sem))
      return val_.float_.convert(to_sem, rm, losesInfo);

    unreachable(kTag);
    return details::FloatBase::OpStatus::kInvalidOp;
  }

  [[nodiscard]] float to_float() const;
  [[nodiscard]] double to_double() const;

  static Float copy_sign(Float val, const Float& sign) {
    val.copy_sign(sign);
    return val;
  }

  void clear_sign() {
    if (is_negative())
      change_sign();
  }

  void copy_sign(const Float& other) {
    if (is_negative() != other.is_negative())
      change_sign();
  }

  void change_sign() {
    DISPATCH_ON_SEMANTICS(change_sign());
  }

 private:
#undef DISPATCH_ON_SEMANTICS

  union Storage {
    const details::FloatSemantics* sem_;
    details::IEEEFloat float_;

    explicit Storage(details::IEEEFloat F, const details::FloatSemantics& S) {
      if (usesLayout<details::IEEEFloat>(S)) {
        new (&float_) details::IEEEFloat(std::move(F));
        return;
      }
      unreachable(kTag);
    }

    template <typename... ArgTypes>
    explicit Storage(const details::FloatSemantics& Semantics,
                     ArgTypes&&... Args) {
      if (usesLayout<details::IEEEFloat>(Semantics)) {
        new (&float_)
            details::IEEEFloat(Semantics, std::forward<ArgTypes>(Args)...);
        return;
      }
      unreachable(kTag);
    }

    ~Storage() {
      if (usesLayout<details::IEEEFloat>(*sem_)) {
        float_.~IEEEFloat();
        return;
      }
      unreachable(kTag);
    }

    Storage(const Storage& rhs) {
      if (usesLayout<details::IEEEFloat>(*rhs.sem_)) {
        new (this) details::IEEEFloat(rhs.float_);
        return;
      }
      unreachable(kTag);
    }

    Storage(Storage&& rhs) {
      if (usesLayout<details::IEEEFloat>(*rhs.sem_)) {
        new (this) details::IEEEFloat(std::move(rhs.float_));
        return;
      }
      unreachable(kTag);
    }

    Storage& operator=(const Storage& rhs) {
      if (usesLayout<details::IEEEFloat>(*sem_) &&
          usesLayout<details::IEEEFloat>(*rhs.sem_)) {
        float_ = rhs.float_;
      } else if (this != &rhs) {
        this->~Storage();
        new (this) Storage(rhs);
      }
      return *this;
    }

    Storage& operator=(Storage&& rhs) {
      if (usesLayout<details::IEEEFloat>(*sem_) &&
          usesLayout<details::IEEEFloat>(*rhs.sem_)) {
        float_ = std::move(rhs.float_);
      } else if (this != &rhs) {
        this->~Storage();
        new (this) Storage(std::move(rhs));
      }
      return *this;
    }
  } val_;

  [[nodiscard]] const details::IEEEFloat& IEEE() const {
    if (usesLayout<details::IEEEFloat>(*val_.sem_))
      return val_.float_;

    unreachable(kTag);
    return val_.float_;
  }

  template <typename T>
  static bool usesLayout(const details::FloatSemantics&) {
    static_assert(std::is_same<T, details::IEEEFloat>::value);
    //todo(@mxlol233): add other format?
    return true;
  }
};

inline Float minnum(const Float& A, const Float& B) {
  if (A.isNaN())
    return B;
  if (B.isNaN())
    return A;
  return B < A ? B : A;
}

inline Float maxnum(const Float& A, const Float& B) {
  if (A.isNaN())
    return B;
  if (B.isNaN())
    return A;
  return A < B ? B : A;
}

inline Float minimum(const Float& A, const Float& B) {
  if (A.isNaN())
    return A;
  if (B.isNaN())
    return B;
  if (A.is_zero() && B.is_zero() && (A.is_negative() != B.is_negative()))
    return A.is_negative() ? A : B;
  return B < A ? B : A;
}

inline Float maximum(const Float& A, const Float& B) {
  if (A.isNaN())
    return A;
  if (B.isNaN())
    return B;
  if (A.is_zero() && B.is_zero() && (A.is_negative() != B.is_negative()))
    return A.is_negative() ? B : A;
  return A < B ? B : A;
}

inline Float abs(Float X) {
  X.clear_sign();
  return X;
}

inline Float neg(Float X) {
  X.change_sign();
  return X;
}

}  // namespace lps::basic::apn
