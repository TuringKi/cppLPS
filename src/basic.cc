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

#include "basic/apn.h"
#include "basic/exception.h"
#include "basic/file.h"
#include "basic/str.h"
#include "lex/base.h"
#include "src.h"
#include "tu.h"

namespace lps::basic {

FileVisitor::FileVisitor(
    const char* start, const char* end,
    const base::check_eof_callback_type& check_eof_callback, uint32_t file_id)
    : base(start, end, std::move(check_eof_callback), file_id) {
  eof_ = 0;
}

FileVisitor::~FileVisitor() {}
const char* FileVisitor::cur() const {
  if (pos_ > len() || start_ > end_) {
    return &eof_;
  }
  return start_ + pos_;
}

void FileVisitor::vertws_skipping() {
  if (flg_skip_vertws_) {
    skipping();
    return;
  }
  vertws_skip(true);
  skipping();
  vertws_skip(false);
}
void FileVisitor::horzws_skipping() {
  if (flg_skip_horzws_) {
    skipping();
    return;
  }
  horzws_skip(true);
  skipping();
  horzws_skip(false);
}

namespace apn {
namespace details {
static const FloatSemantics kSemIEEEhalf = {15, -14, 11, 16};
static const FloatSemantics kSemBFloat = {127, -126, 8, 16};
static const FloatSemantics kSemIEEEsingle = {127, -126, 24, 32};
static const FloatSemantics kSemIEEEdouble = {1023, -1022, 53, 64};
static const FloatSemantics kSemIEEEquad = {16383, -16382, 113, 128};
static const FloatSemantics kSemBogus = {0, 0, 0, 0};

const FloatSemantics& FloatBase::Semantics2FloatSemantics(
    FloatBase::Semantics s) {
  switch (s) {
    case Semantics::kIEEEhalf:
      return IEEEhalf();
    case Semantics::kBFloat:
      return BFloat();
    case Semantics::kIEEEsingle:
      return IEEEsingle();
    case Semantics::kIEEEdouble:
      return IEEEdouble();
    case Semantics::kIEEEquad:
      return IEEEquad();
    default:
      LPS_ERROR(kTag, "Unrecognised floating semantics");
  }
  LPS_ERROR(kTag, "Unrecognised floating semantics");
  static FloatSemantics empty;
  return empty;
}
FloatBase::Semantics FloatBase::FloatSemantics2Semantics(
    const FloatSemantics& Sem) {
  if (&Sem == &FloatBase::IEEEhalf())
    return Semantics::kIEEEhalf;
  if (&Sem == &FloatBase::BFloat())
    return Semantics::kBFloat;
  if (&Sem == &FloatBase::IEEEsingle())
    return Semantics::kIEEEsingle;
  if (&Sem == &FloatBase::IEEEdouble())
    return Semantics::kIEEEdouble;
  if (&Sem == &FloatBase::IEEEquad())
    return Semantics::kIEEEquad;
  LPS_ERROR(kTag, "Unknown floating semantics");
  return Semantics::kUnknown;
}

const FloatSemantics& FloatBase::IEEEhalf() {
  return kSemIEEEhalf;
}
const FloatSemantics& FloatBase::BFloat() {
  return kSemBFloat;
}
const FloatSemantics& FloatBase::IEEEsingle() {
  return kSemIEEEsingle;
}
const FloatSemantics& FloatBase::IEEEdouble() {
  return kSemIEEEdouble;
}
const FloatSemantics& FloatBase::IEEEquad() {
  return kSemIEEEquad;
}
IEEEFloat& IEEEFloat::operator=(IEEEFloat&& other) {
  free_significand();

  sem_ = other.sem_;
  significand_ = other.significand_;
  exp_ = other.exp_;
  category_ = other.category_;
  sign_ = other.sign_;

  other.sem_ = &kSemBogus;
  return *this;
}

}  // namespace details

void Int::knuth_div(uint32_t* u, uint32_t* v, uint32_t* q, uint32_t* r,
                    unsigned m, unsigned n) {
  LPS_CHECK_ERROR(kTag, u, "must provide dividend");
  LPS_CHECK_ERROR(kTag, v, "must provide divisor");
  LPS_CHECK_ERROR(kTag, q, "must provide quotient");
  LPS_CHECK_ERROR(kTag, u != v && u != q && v != q,
                  "must use different memory");
  LPS_CHECK_ERROR(kTag, n > 1, "n must be > 1");

  // b denotes the base of the number system. In our case b is 2^32.
  const uint64_t b = static_cast<uint64_t>(1) << 32;

  // The DEBUG macros here tend to be spam in the debug output if you're not
  // debugging this code. Disable them unless KNUTH_DEBUG is defined.

  // D1. [Normalize.] Set d = b / (v[n-1] + 1) and multiply all the digits of
  // u and v by d. Note that we have taken Knuth's advice here to use a power
  // of 2 value for d such that d * v[n-1] >= b/2 (b is the base). A power of
  // 2 allows us to shift instead of multiply and it is easy to determine the
  // shift amount from the leading zeros.  We are basically normalizing the u
  // and v so that its high bits are shifted to the top of v's range without
  // overflow. Note that this can require an extra word in u so that u must
  // be of length m+n+1.
  unsigned shift = details::countl_zero(v[n - 1]);
  uint32_t v_carry = 0;
  uint32_t u_carry = 0;
  if (shift) {
    for (unsigned i = 0; i < m + n; ++i) {
      uint32_t u_tmp = u[i] >> (32 - shift);
      u[i] = (u[i] << shift) | u_carry;
      u_carry = u_tmp;
    }
    for (unsigned i = 0; i < n; ++i) {
      uint32_t v_tmp = v[i] >> (32 - shift);
      v[i] = (v[i] << shift) | v_carry;
      v_carry = v_tmp;
    }
  }
  u[m + n] = u_carry;

  // D2. [Initialize j.]  Set j to m. This is the loop counter over the places.
  int j = m;
  do {

    // D3. [Calculate q'.].
    //     Set qp = (u[j+n]*b + u[j+n-1]) / v[n-1]. (qp=qprime=q')
    //     Set rp = (u[j+n]*b + u[j+n-1]) % v[n-1]. (rp=rprime=r')
    // Now test if qp == b or qp*v[n-2] > b*rp + u[j+n-2]; if so, decrease
    // qp by 1, increase rp by v[n-1], and repeat this test if rp < b. The test
    // on v[n-2] determines at high speed most of the cases in which the trial
    // value qp is one too large, and it eliminates all cases where qp is two
    // too large.
    uint64_t dividend = details::make_64(u[j + n], u[j + n - 1]);
    uint64_t qp = dividend / v[n - 1];
    uint64_t rp = dividend % v[n - 1];
    if (qp == b || qp * v[n - 2] > b * rp + u[j + n - 2]) {
      qp--;
      rp += v[n - 1];
      if (rp < b && (qp == b || qp * v[n - 2] > b * rp + u[j + n - 2]))
        qp--;
    }
    // D4. [Multiply and subtract.] Replace (u[j+n]u[j+n-1]...u[j]) with
    // (u[j+n]u[j+n-1]..u[j]) - qp * (v[n-1]...v[1]v[0]). This computation
    // consists of a simple multiplication by a one-place number, combined with
    // a subtraction.
    // The digits (u[j+n]...u[j]) should be kept positive; if the result of
    // this step is actually negative, (u[j+n]...u[j]) should be left as the
    // true value plus b**(n+1), namely as the b's complement of
    // the true value, and a "borrow" to the left should be remembered.
    int64_t borrow = 0;
    for (unsigned i = 0; i < n; ++i) {
      uint64_t p = (qp) * static_cast<uint64_t>(v[i]);
      int64_t subres =
          static_cast<int64_t>(u[j + i]) - borrow - details::lo_32(p);
      u[j + i] = details::lo_32(subres);
      borrow = details::hi_32(p) - details::hi_32(subres);
    }
    bool is_neg = u[j + n] < borrow;
    u[j + n] -= details::lo_32(borrow);

    // D5. [Test remainder.] Set q[j] = qp. If the result of step D4 was
    // negative, go to step D6; otherwise go on to step D7.
    q[j] = details::lo_32(qp);
    if (is_neg) {
      // D6. [Add back]. The probability that this step is necessary is very
      // small, on the order of only 2/b. Make sure that test data accounts for
      // this possibility. Decrease q[j] by 1
      q[j]--;
      // and add (0v[n-1]...v[1]v[0]) to (u[j+n]u[j+n-1]...u[j+1]u[j]).
      // A carry will occur to the left of u[j+n], and it should be ignored
      // since it cancels with the borrow that occurred in D4.
      bool carry = false;
      for (unsigned i = 0; i < n; i++) {
        uint32_t limit = std::min(u[j + i], v[i]);
        u[j + i] += v[i] + static_cast<unsigned int>(carry);
        carry = u[j + i] < limit || (carry && u[j + i] == limit);
      }
      u[j + n] += static_cast<unsigned int>(carry);
    }

    // D7. [Loop on j.]  Decrease j by one. Now if j >= 0, go back to D3.
  } while (--j >= 0);

  // D8. [Unnormalize]. Now q[...] is the desired quotient, and the desired
  // remainder may be obtained by dividing u[...] by d. If r is non-null we
  // compute the remainder (urem uses this).
  if (r) {
    // The value d is expressed by the "shift" value above since we avoided
    // multiplication by d by using a shift left. So, all we have to do is
    // shift right here.
    if (shift) {
      uint32_t carry = 0;

      for (int i = n - 1; i >= 0; i--) {
        r[i] = (u[i] >> shift) | carry;
        carry = u[i] << (32 - shift);
      }
    } else {
      for (int i = n - 1; i >= 0; i--) {
        r[i] = u[i];
      }
    }
  }
}

Int operator-(Int v) {
  v.negate();
  return v;
}

Int operator+(Int a, const Int& b) {
  a += b;
  return a;
}

Int operator+(const Int& a, Int&& b) {
  b += a;
  return std::move(b);
}

Int operator+(Int a, uint64_t other) {
  a += other;
  return a;
}

Int operator+(uint64_t other, Int b) {
  b += other;
  return b;
}

Int operator-(Int a, const Int& b) {
  a -= b;
  return a;
}

Int operator-(const Int& a, Int&& b) {
  b.negate();
  b += a;
  return std::move(b);
}

Int operator-(Int a, uint64_t other) {
  a -= other;
  return a;
}

Int operator-(uint64_t other, Int b) {
  b.negate();
  b += other;
  return b;
}

Int operator*(Int a, uint64_t other) {
  a *= other;
  return a;
}

Int operator*(uint64_t other, Int b) {
  b *= other;
  return b;
}

bool operator==(uint64_t V1, const Int& V2) {
  return V2 == V1;
}

bool operator!=(uint64_t V1, const Int& V2) {
  return V2 != V1;
}

Int operator~(Int v) {
  v.flip();
  return v;
}

Int operator&(Int a, const Int& b) {
  a &= b;
  return a;
}

Int operator&(const Int& a, Int&& b) {
  b &= a;
  return std::move(b);
}

Int operator&(Int a, uint64_t other) {
  a &= other;
  return a;
}

Int operator&(uint64_t other, Int b) {
  b &= other;
  return b;
}

Int operator|(Int a, const Int& b) {
  a |= b;
  return a;
}

Int operator|(const Int& a, Int&& b) {
  b |= a;
  return std::move(b);
}

Int operator|(Int a, uint64_t other) {
  a |= other;
  return a;
}

Int operator|(uint64_t other, Int b) {
  b |= other;
  return b;
}

Int operator^(Int a, const Int& b) {
  a ^= b;
  return a;
}

Int operator^(const Int& a, Int&& b) {
  b ^= a;
  return std::move(b);
}

Int operator^(Int a, uint64_t other) {
  a ^= other;
  return a;
}

Int operator^(uint64_t other, Int b) {
  b ^= other;
  return b;
}

namespace details {

void IEEEFloat::init_from_half_int(const Int& api) {
  return init_from_word<10, 0x1f, 0x3ff, 15, 15, 0x400>(api, kSemIEEEhalf);
}

void IEEEFloat::init_from_bfloat_int(const Int& api) {
  return init_from_word<7, 0xff, 0x7f, 15, 127, 0x80>(api, kSemBFloat);
}

void IEEEFloat::init_from_float_int(const Int& api) {
  return init_from_word<23, 0xff, 0x7fffff, 31, 127, 0x800000>(api,
                                                               kSemIEEEsingle);
}

void IEEEFloat::init_from_double_int(const Int& api) {
  return init_from_word<52, 0x7ff, 0xfffffffffffffLL, 63, 1023,
                        0x10000000000000LL>(api, kSemIEEEdouble);
}

void IEEEFloat::init_from_quad_int(const Int& api) {
  uint64_t i1 = api.raw_data()[0];
  uint64_t i2 = api.raw_data()[1];
  uint64_t exp = (i2 >> 48) & 0x7fff;
  uint64_t significand = i1;
  uint64_t significand2 = i2 & 0xffffffffffffLL;

  initialize(&kSemIEEEquad);
  lps_assert(kTag, n_parts() == 2);

  sign_ = static_cast<unsigned int>(i2 >> 63);
  if (exp == 0 && (significand == 0 && significand2 == 0)) {
    make_zero(sign_ != 0U);
  } else if (exp == 0x7fff && (significand == 0 && significand2 == 0)) {
    make_inf(sign_ != 0U);
  } else if (exp == 0x7fff && (significand != 0 || significand2 != 0)) {
    category_ = FloatCategory::kNaN;
    exp_ = exp_NaN();
    significand_parts()[0] = significand;
    significand_parts()[1] = significand2;
  } else {
    category_ = FloatCategory::kNormal;
    exp_ = exp - 16383;
    significand_parts()[0] = significand;
    significand_parts()[1] = significand2;
    if (exp == 0)
      exp_ = -16382;
    else
      significand_parts()[1] |= 0x1000000000000LL;
  }
}

void IEEEFloat::init_from_int(const FloatSemantics* Sem, const Int& api) {
  lps_assert(kTag, api.bits() == Sem->bits_);
  if (Sem == &kSemIEEEhalf)
    return init_from_half_int(api);
  if (Sem == &kSemBFloat)
    return init_from_bfloat_int(api);
  if (Sem == &kSemIEEEsingle)
    return init_from_float_int(api);
  if (Sem == &kSemIEEEdouble)
    return init_from_double_int(api);
  if (Sem == &kSemIEEEquad)
    return init_from_quad_int(api);

  unreachable(kTag);
}

IEEEFloat::IEEEFloat(double d) {
  init_from_int(&kSemIEEEdouble, Int::double2bits(d));
}

IEEEFloat::IEEEFloat(float f) {
  init_from_int(&kSemIEEEsingle, Int::float2bits(f));
}

IEEEFloat::IEEEFloat(IEEEFloat&& other) : sem_(&kSemBogus) {
  *this = std::move(other);
}

bool IEEEFloat::exact_inverse(Float* inv) const {
  if (!is_finite_nonzero())
    return false;

  if (LSB_significand() != sem_->precision_ - 1)
    return false;

  IEEEFloat reciprocal(*sem_, 1ULL);
  if (reciprocal.div(*this, RoundingMode::kNearestTiesToEven) != OpStatus::kOK)
    return false;

  if (reciprocal.is_denormal())
    return false;

  assert(reciprocal.is_finite_nonzero() &&
         reciprocal.LSB_significand() == reciprocal.sem_->precision_ - 1);

  if (inv)
    *inv = Float(reciprocal, *sem_);

  return true;
}

Int IEEEFloat::bit_cast_to_int() const {

  if (sem_ == &kSemIEEEhalf)
    return convert_half_to_int();

  if (sem_ == &kSemBFloat)
    return convert_bfloat_to_int();

  if (sem_ == &kSemIEEEsingle)
    return convert_float_to_int();

  if (sem_ == &kSemIEEEdouble)
    return convert_double_to_int();

  if (sem_ == &kSemIEEEquad)
    return convert_quad_to_int();

  unreachable(kTag);
  return Int();
}

}  // namespace details

float Float::to_float() const {
  if (&sem() == &details::kSemIEEEsingle)
    return IEEE().to_float();
  LPS_CHECK_ERROR(kTag, sem().is_representable_by(details::kSemIEEEsingle),
                  "float semantics is not representable by IEEEsingle");
  Float tmp = *this;
  bool loses_info;
  auto st = tmp.convert(details::kSemIEEEsingle,
                        details::FloatBase::RoundingMode::kNearestTiesToEven,
                        &loses_info);
  LPS_CHECK_ERROR(
      kTag,
      !(static_cast<uint8_t>(st) &
        static_cast<uint8_t>(details::FloatBase::OpStatus::kInexact)) &&
          !loses_info,
      "unexpected imprecision");
  return tmp.IEEE().to_float();
}

double Float::to_double() const {
  if (&sem() == &details::kSemIEEEdouble)
    return IEEE().to_double();
  LPS_CHECK_ERROR(kTag, sem().is_representable_by(details::kSemIEEEdouble),
                  "Float semantics is not representable by IEEEdouble");
  Float tmp = *this;
  bool loses_info;
  auto st = tmp.convert(details::kSemIEEEdouble,
                        details::FloatBase::RoundingMode::kNearestTiesToEven,
                        &loses_info);
  LPS_CHECK_ERROR(
      kTag,
      !(static_cast<uint8_t>(st) &
        static_cast<uint8_t>(details::FloatBase::OpStatus::kInexact)) &&
          !loses_info,
      "unexpected imprecision");

  return tmp.IEEE().to_double();
}

}  // namespace apn

namespace vec {

static uint64_t auto_radix(StringRef& Str) {
  if (Str.empty())
    return 10;

  if (Str.starts_with("0x") || Str.starts_with("0X")) {
    Str = Str.substr(2);
    return 16;
  }

  if (Str.starts_with("0b") || Str.starts_with("0B")) {
    Str = Str.substr(2);
    return 2;
  }

  if (Str.starts_with("0o")) {
    Str = Str.substr(2);
    return 8;
  }

  if (Str[0] == '0' && Str.size() > 1 && str::ascii::is::Digit(Str[1])) {
    Str = Str.substr(1);
    return 8;
  }

  return 10;
}

}  // namespace vec

bool StringRef::as_int(uint64_t radix, apn::Int& result) const {
  StringRef str = *this;

  if (radix == 0)
    radix = vec::auto_radix(str);

  lps_assert(tag_, radix > 1 && radix <= 36);

  if (str.empty())
    return true;

  while (!str.empty() && str.front() == '0')
    str = str.substr(1);

  if (str.empty()) {
    result = apn::Int(64, 0);
    return false;
  }

  unsigned log2_radix = 0;
  while ((1U << log2_radix) < radix)
    log2_radix++;
  bool is_power2_radix = ((1U << log2_radix) == radix);

  auto bits = log2_radix * str.size();
  if (bits < result.bits())
    bits = result.bits();
  else if (bits > result.bits())
    result = result.zext(bits);

  apn::Int radix_int;
  apn::Int char_int;
  if (!is_power2_radix) {
    radix_int = apn::Int(bits, radix);
    char_int = apn::Int(bits, 0);
  }

  result = 0;
  while (!str.empty()) {
    unsigned char_val;
    if (str[0] >= '0' && str[0] <= '9')
      char_val = str[0] - '0';
    else if (str[0] >= 'a' && str[0] <= 'z')
      char_val = str[0] - 'a' + 10;
    else if (str[0] >= 'A' && str[0] <= 'Z')
      char_val = str[0] - 'A' + 10;
    else
      return true;

    if (char_val >= radix)
      return true;

    if (is_power2_radix) {
      result <<= log2_radix;
      result |= char_val;
    } else {
      result *= radix_int;
      char_int = char_val;
      result += char_int;
    }

    str = str.substr(1);
  }

  return false;
}
}  // namespace lps::basic
