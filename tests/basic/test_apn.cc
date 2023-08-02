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

#include <math.h>
#include "basic/apn.h"
#include "unittest.h"

using namespace lps::basic::apn;

TEST(IntTest, ValueInit) {
  Int zero = Int();
  EXPECT_TRUE(!zero);
  EXPECT_TRUE(!zero.zext(64));
  EXPECT_TRUE(!zero.sext(64));
}

TEST(IntTest, ShiftLeftByZero) {
  Int one = Int::zero(65) + 1;
  Int shl = one.shl(0);
  EXPECT_TRUE(shl[0]);
  EXPECT_FALSE(shl[1]);
}

TEST(IntTest, i64ArithmeticRightShiftNegative) {
  const Int neg_one(64, static_cast<uint64_t>(-1), true);
  EXPECT_EQ(neg_one, neg_one.ashr(7));
}

TEST(IntTest, iOne) {
  const Int neg_two(1, static_cast<uint64_t>(-2), true);
  const Int neg_one(1, static_cast<uint64_t>(-1), true);
  const Int zero(1, 0);
  const Int one(1, 1);
  const Int two(1, 2);

  EXPECT_EQ(0, neg_two.sign_ext_value());
  EXPECT_EQ(-1, neg_one.sign_ext_value());
  EXPECT_EQ(1U, neg_one.zero_ext_value());
  EXPECT_EQ(0U, zero.zero_ext_value());
  EXPECT_EQ(-1, one.sign_ext_value());
  EXPECT_EQ(1U, one.zero_ext_value());
  EXPECT_EQ(0U, two.zero_ext_value());
  EXPECT_EQ(0, two.sign_ext_value());

  // Basic equalities for 1-bit values.
  EXPECT_EQ(zero, two);
  EXPECT_EQ(zero, neg_two);
  EXPECT_EQ(one, neg_one);
  EXPECT_EQ(two, neg_two);

  // Min/max signed values.
  EXPECT_TRUE(zero.is_max_signed_value());
  EXPECT_FALSE(one.is_max_signed_value());
  EXPECT_FALSE(zero.is_min_signed_value());
  EXPECT_TRUE(one.is_min_signed_value());

  // Additions.
  EXPECT_EQ(two, one + one);
  EXPECT_EQ(zero, neg_one + one);
  EXPECT_EQ(neg_two, neg_one + neg_one);

  // Subtractions.
  EXPECT_EQ(neg_two, neg_one - one);
  EXPECT_EQ(two, one - neg_one);
  EXPECT_EQ(zero, one - one);

  // And
  EXPECT_EQ(zero, zero & zero);
  EXPECT_EQ(zero, one & zero);
  EXPECT_EQ(zero, zero & one);
  EXPECT_EQ(one, one & one);
  EXPECT_EQ(zero, zero & zero);
  EXPECT_EQ(zero, neg_one & zero);
  EXPECT_EQ(zero, zero & neg_one);
  EXPECT_EQ(neg_one, neg_one & neg_one);

  // Or
  EXPECT_EQ(zero, zero | zero);
  EXPECT_EQ(one, one | zero);
  EXPECT_EQ(one, zero | one);
  EXPECT_EQ(one, one | one);
  EXPECT_EQ(zero, zero | zero);
  EXPECT_EQ(neg_one, neg_one | zero);
  EXPECT_EQ(neg_one, zero | neg_one);
  EXPECT_EQ(neg_one, neg_one | neg_one);

  // Xor
  EXPECT_EQ(zero, zero ^ zero);
  EXPECT_EQ(one, one ^ zero);
  EXPECT_EQ(one, zero ^ one);
  EXPECT_EQ(zero, one ^ one);
  EXPECT_EQ(zero, zero ^ zero);
  EXPECT_EQ(neg_one, neg_one ^ zero);
  EXPECT_EQ(neg_one, zero ^ neg_one);
  EXPECT_EQ(zero, neg_one ^ neg_one);

  // Shifts.
  EXPECT_EQ(zero, one << one);
  EXPECT_EQ(one, one << zero);
  EXPECT_EQ(zero, one.shl(1));
  EXPECT_EQ(one, one.shl(0));
  EXPECT_EQ(zero, one.lshr(1));
  EXPECT_EQ(one, one.ashr(1));

  // Rotates.
  EXPECT_EQ(one, one.rotl(0));
  EXPECT_EQ(one, one.rotl(1));
  EXPECT_EQ(one, one.rotr(0));
  EXPECT_EQ(one, one.rotr(1));

  // Multiplies.
  EXPECT_EQ(neg_one, neg_one * one);
  EXPECT_EQ(neg_one, one * neg_one);
  EXPECT_EQ(one, neg_one * neg_one);
  EXPECT_EQ(one, one * one);

  // Divides.
  EXPECT_EQ(neg_one, one.sdiv(neg_one));
  EXPECT_EQ(neg_one, neg_one.sdiv(one));
  EXPECT_EQ(one, neg_one.sdiv(neg_one));
  EXPECT_EQ(one, one.sdiv(one));

  EXPECT_EQ(neg_one, one.udiv(neg_one));
  EXPECT_EQ(neg_one, neg_one.udiv(one));
  EXPECT_EQ(one, neg_one.udiv(neg_one));
  EXPECT_EQ(one, one.udiv(one));

  // rems.
  EXPECT_EQ(zero, neg_one.srem(one));
  EXPECT_EQ(zero, neg_one.urem(one));
  EXPECT_EQ(zero, one.srem(neg_one));

  // sdivrem
  {
    Int q(8, 0);
    Int r(8, 0);
    Int one(8, 1);
    Int two(8, 2);
    Int nine(8, 9);
    Int four(8, 4);

    EXPECT_EQ(nine.srem(two), one);
    EXPECT_EQ(nine.srem(-two), one);
    EXPECT_EQ((-nine).srem(two), -one);
    EXPECT_EQ((-nine).srem(-two), -one);

    Int::sdivrem(nine, two, q, r);
    EXPECT_EQ(four, q);
    EXPECT_EQ(one, r);
    Int::sdivrem(-nine, two, q, r);
    EXPECT_EQ(-four, q);
    EXPECT_EQ(-one, r);
    Int::sdivrem(nine, -two, q, r);
    EXPECT_EQ(-four, q);
    EXPECT_EQ(one, r);
    Int::sdivrem(-nine, -two, q, r);
    EXPECT_EQ(four, q);
    EXPECT_EQ(-one, r);
  }
}

TEST(IntTest, compare) {
  std::array<Int, 5> test_vals{{
      Int{16, 2},
      Int{16, 1},
      Int{16, 0},
      Int{16, static_cast<uint64_t>(-1), true},
      Int{16, static_cast<uint64_t>(-2), true},
  }};

  for (auto& arg1 : test_vals)
    for (auto& arg2 : test_vals) {
      auto uv1 = arg1.zero_ext_value();
      auto uv2 = arg2.zero_ext_value();
      auto sv1 = arg1.sign_ext_value();
      auto sv2 = arg2.sign_ext_value();

      EXPECT_EQ(uv1 < uv2, arg1.ult(arg2));
      EXPECT_EQ(uv1 <= uv2, arg1.ule(arg2));
      EXPECT_EQ(uv1 > uv2, arg1.ugt(arg2));
      EXPECT_EQ(uv1 >= uv2, arg1.uge(arg2));

      EXPECT_EQ(sv1 < sv2, arg1.slt(arg2));
      EXPECT_EQ(sv1 <= sv2, arg1.sle(arg2));
      EXPECT_EQ(sv1 > sv2, arg1.sgt(arg2));
      EXPECT_EQ(sv1 >= sv2, arg1.sge(arg2));

      EXPECT_EQ(uv1 < uv2, arg1.ult(uv2));
      EXPECT_EQ(uv1 <= uv2, arg1.ule(uv2));
      EXPECT_EQ(uv1 > uv2, arg1.ugt(uv2));
      EXPECT_EQ(uv1 >= uv2, arg1.uge(uv2));

      EXPECT_EQ(sv1 < sv2, arg1.slt(sv2));
      EXPECT_EQ(sv1 <= sv2, arg1.sle(sv2));
      EXPECT_EQ(sv1 > sv2, arg1.sgt(sv2));
      EXPECT_EQ(sv1 >= sv2, arg1.sge(sv2));
    }
}

TEST(IntTest, compareWithRawIntegers) {
  EXPECT_TRUE(!Int(8, 1).uge(256));
  EXPECT_TRUE(!Int(8, 1).ugt(256));
  EXPECT_TRUE(Int(8, 1).ule(256));
  EXPECT_TRUE(Int(8, 1).ult(256));
  EXPECT_TRUE(!Int(8, 1).sge(256));
  EXPECT_TRUE(!Int(8, 1).sgt(256));
  EXPECT_TRUE(Int(8, 1).sle(256));
  EXPECT_TRUE(Int(8, 1).slt(256));
  EXPECT_TRUE(!(Int(8, 0) == 256));
  EXPECT_TRUE(Int(8, 0) != 256);
  EXPECT_TRUE(!(Int(8, 1) == 256));
  EXPECT_TRUE(Int(8, 1) != 256);

  auto uint64max = UINT64_MAX;
  auto int64max = INT64_MAX;
  auto int64min = INT64_MIN;

  auto u64 = Int{128, uint64max};
  auto s64 = Int{128, static_cast<uint64_t>(int64max), true};
  auto big = u64 + 1;

  EXPECT_TRUE(u64.uge(uint64max));
  EXPECT_TRUE(!u64.ugt(uint64max));
  EXPECT_TRUE(u64.ule(uint64max));
  EXPECT_TRUE(!u64.ult(uint64max));
  EXPECT_TRUE(u64.sge(int64max));
  EXPECT_TRUE(u64.sgt(int64max));
  EXPECT_TRUE(!u64.sle(int64max));
  EXPECT_TRUE(!u64.slt(int64max));
  EXPECT_TRUE(u64.sge(int64min));
  EXPECT_TRUE(u64.sgt(int64min));
  EXPECT_TRUE(!u64.sle(int64min));
  EXPECT_TRUE(!u64.slt(int64min));

  EXPECT_TRUE(u64 == uint64max);
  EXPECT_TRUE(u64 != int64max);
  EXPECT_TRUE(u64 != int64min);

  EXPECT_TRUE(!s64.uge(uint64max));
  EXPECT_TRUE(!s64.ugt(uint64max));
  EXPECT_TRUE(s64.ule(uint64max));
  EXPECT_TRUE(s64.ult(uint64max));
  EXPECT_TRUE(s64.sge(int64max));
  EXPECT_TRUE(!s64.sgt(int64max));
  EXPECT_TRUE(s64.sle(int64max));
  EXPECT_TRUE(!s64.slt(int64max));
  EXPECT_TRUE(s64.sge(int64min));
  EXPECT_TRUE(s64.sgt(int64min));
  EXPECT_TRUE(!s64.sle(int64min));
  EXPECT_TRUE(!s64.slt(int64min));

  EXPECT_TRUE(s64 != uint64max);
  EXPECT_TRUE(s64 == int64max);
  EXPECT_TRUE(s64 != int64min);

  EXPECT_TRUE(big.uge(uint64max));
  EXPECT_TRUE(big.ugt(uint64max));
  EXPECT_TRUE(!big.ule(uint64max));
  EXPECT_TRUE(!big.ult(uint64max));
  EXPECT_TRUE(big.sge(int64max));
  EXPECT_TRUE(big.sgt(int64max));
  EXPECT_TRUE(!big.sle(int64max));
  EXPECT_TRUE(!big.slt(int64max));
  EXPECT_TRUE(big.sge(int64min));
  EXPECT_TRUE(big.sgt(int64min));
  EXPECT_TRUE(!big.sle(int64min));
  EXPECT_TRUE(!big.slt(int64min));

  EXPECT_TRUE(big != uint64max);
  EXPECT_TRUE(big != int64max);
  EXPECT_TRUE(big != int64min);
}

TEST(IntTest, compareWithInt64Min) {
  int64_t edge = INT64_MIN;
  int64_t edge_p1 = edge + 1;
  int64_t edge_m1 = INT64_MAX;
  auto a = Int{64, static_cast<uint64_t>(edge), true};

  EXPECT_TRUE(!a.slt(edge));
  EXPECT_TRUE(a.sle(edge));
  EXPECT_TRUE(!a.sgt(edge));
  EXPECT_TRUE(a.sge(edge));
  EXPECT_TRUE(a.slt(edge_p1));
  EXPECT_TRUE(a.sle(edge_p1));
  EXPECT_TRUE(!a.sgt(edge_p1));
  EXPECT_TRUE(!a.sge(edge_p1));
  EXPECT_TRUE(a.slt(edge_m1));
  EXPECT_TRUE(a.sle(edge_m1));
  EXPECT_TRUE(!a.sgt(edge_m1));
  EXPECT_TRUE(!a.sge(edge_m1));
}

TEST(IntTest, compareWithHalfInt64Max) {
  uint64_t edge = 0x4000000000000000;
  uint64_t edge_p1 = edge + 1;
  uint64_t edge_m1 = edge - 1;
  auto a = Int{64, edge};

  EXPECT_TRUE(!a.ult(edge));
  EXPECT_TRUE(a.ule(edge));
  EXPECT_TRUE(!a.ugt(edge));
  EXPECT_TRUE(a.uge(edge));
  EXPECT_TRUE(a.ult(edge_p1));
  EXPECT_TRUE(a.ule(edge_p1));
  EXPECT_TRUE(!a.ugt(edge_p1));
  EXPECT_TRUE(!a.uge(edge_p1));
  EXPECT_TRUE(!a.ult(edge_m1));
  EXPECT_TRUE(!a.ule(edge_m1));
  EXPECT_TRUE(a.ugt(edge_m1));
  EXPECT_TRUE(a.uge(edge_m1));

  EXPECT_TRUE(!a.slt(edge));
  EXPECT_TRUE(a.sle(edge));
  EXPECT_TRUE(!a.sgt(edge));
  EXPECT_TRUE(a.sge(edge));
  EXPECT_TRUE(a.slt(edge_p1));
  EXPECT_TRUE(a.sle(edge_p1));
  EXPECT_TRUE(!a.sgt(edge_p1));
  EXPECT_TRUE(!a.sge(edge_p1));
  EXPECT_TRUE(!a.slt(edge_m1));
  EXPECT_TRUE(!a.sle(edge_m1));
  EXPECT_TRUE(a.sgt(edge_m1));
  EXPECT_TRUE(a.sge(edge_m1));
}

TEST(IntTest, compareLargeIntegers) {
  // Make sure all the combinations of signed comparisons work with big ints.
  auto one = Int{128, static_cast<uint64_t>(1), true};
  auto two = Int{128, static_cast<uint64_t>(2), true};
  auto minus_one = Int{128, static_cast<uint64_t>(-1), true};
  auto minus_two = Int{128, static_cast<uint64_t>(-2), true};

  EXPECT_TRUE(!one.slt(one));
  EXPECT_TRUE(!two.slt(one));
  EXPECT_TRUE(minus_one.slt(one));
  EXPECT_TRUE(minus_two.slt(one));

  EXPECT_TRUE(one.slt(two));
  EXPECT_TRUE(!two.slt(two));
  EXPECT_TRUE(minus_one.slt(two));
  EXPECT_TRUE(minus_two.slt(two));

  EXPECT_TRUE(!one.slt(minus_one));
  EXPECT_TRUE(!two.slt(minus_one));
  EXPECT_TRUE(!minus_one.slt(minus_one));
  EXPECT_TRUE(minus_two.slt(minus_one));

  EXPECT_TRUE(!one.slt(minus_two));
  EXPECT_TRUE(!two.slt(minus_two));
  EXPECT_TRUE(!minus_one.slt(minus_two));
  EXPECT_TRUE(!minus_two.slt(minus_two));
}

TEST(IntTest, binaryOpsWithRawIntegers) {
  // Single word check.
  uint64_t e1 = 0x2CA7F46BF6569915ULL;
  Int a1(64, e1);

  EXPECT_EQ(a1 & e1, e1);
  EXPECT_EQ(a1 & 0, 0);
  EXPECT_EQ(a1 & 1, 1);
  EXPECT_EQ(a1 & 5, 5);
  EXPECT_EQ(a1 & UINT64_MAX, e1);

  EXPECT_EQ(a1 | e1, e1);
  EXPECT_EQ(a1 | 0, e1);
  EXPECT_EQ(a1 | 1, e1);
  EXPECT_EQ(a1 | 2, e1 | 2);
  EXPECT_EQ(a1 | UINT64_MAX, UINT64_MAX);

  EXPECT_EQ(a1 ^ e1, 0);
  EXPECT_EQ(a1 ^ 0, e1);
  EXPECT_EQ(a1 ^ 1, e1 ^ 1);
  EXPECT_EQ(a1 ^ 7, e1 ^ 7);
  EXPECT_EQ(a1 ^ UINT64_MAX, ~e1);

  // Multiword check.
  uint64_t n = 0xEB6EB136591CBA21ULL;
  Int::word_type e2[4] = {n, 0x7B9358BD6A33F10AULL, 0x7E7FFA5EADD8846ULL,
                          0x305F341CA00B613DULL};
  Int a2(Int::kBitsOfWord * 4, e2, 4);

  EXPECT_EQ(a2 & n, n);
  EXPECT_EQ(a2 & 0, 0);
  EXPECT_EQ(a2 & 1, 1);
  EXPECT_EQ(a2 & 5, 1);
  EXPECT_EQ(a2 & UINT64_MAX, n);

  EXPECT_EQ(a2 | n, a2);
  EXPECT_EQ(a2 | 0, a2);
  EXPECT_EQ(a2 | 1, a2);
  EXPECT_EQ(a2 | 2, a2 + 2);
  EXPECT_EQ(a2 | UINT64_MAX, a2 - n + UINT64_MAX);

  EXPECT_EQ(a2 ^ n, a2 - n);
  EXPECT_EQ(a2 ^ 0, a2);
  EXPECT_EQ(a2 ^ 1, a2 - 1);
  EXPECT_EQ(a2 ^ 7, a2 + 5);
  EXPECT_EQ(a2 ^ UINT64_MAX, a2 - n + ~n);
}

TEST(IntTest, rvalue_arithmetic) {

  auto rvalue = [](const char* hex, uint64_t const*& raw_data) {
    Int v(129, hex, 16);
    raw_data = v.raw_data();
    return v;
  };

  Int one(129, "1", 16);
  Int two(129, "2", 16);
  Int three(129, "3", 16);
  Int minus_one = -one;

  const uint64_t* raw_data_l = nullptr;
  const uint64_t* raw_data_r = nullptr;

  {
    // 1 + 1 = 2
    Int add_ll = one + one;
    EXPECT_EQ(add_ll, two);

    Int add_lr = one + rvalue("1", raw_data_r);
    EXPECT_EQ(add_lr, two);
    EXPECT_EQ(add_lr.raw_data(), raw_data_r);

    Int add_rl = rvalue("1", raw_data_l) + one;
    EXPECT_EQ(add_rl, two);
    EXPECT_EQ(add_rl.raw_data(), raw_data_l);

    Int add_rr = rvalue("1", raw_data_l) + rvalue("1", raw_data_r);
    EXPECT_EQ(add_rr, two);
    EXPECT_EQ(add_rr.raw_data(), raw_data_r);

    // LValue's and constants
    Int add_lk = one + 1;
    EXPECT_EQ(add_lk, two);

    Int add_kl = 1 + one;
    EXPECT_EQ(add_kl, two);

    // RValue's and constants
    Int add_rk = rvalue("1", raw_data_l) + 1;
    EXPECT_EQ(add_rk, two);
    EXPECT_EQ(add_rk.raw_data(), raw_data_l);

    Int add_kr = 1 + rvalue("1", raw_data_r);
    EXPECT_EQ(add_kr, two);
    EXPECT_EQ(add_kr.raw_data(), raw_data_r);
  }

  {
    // 0x0,FFFF...FFFF + 0x2 = 0x100...0001
    Int all_ones(129, "0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16);
    Int high_one_low_one(129, "100000000000000000000000000000001", 16);

    Int add_ll = all_ones + two;
    EXPECT_EQ(add_ll, high_one_low_one);

    Int add_lr = all_ones + rvalue("2", raw_data_r);
    EXPECT_EQ(add_lr, high_one_low_one);
    EXPECT_EQ(add_lr.raw_data(), raw_data_r);

    Int add_rl = rvalue("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", raw_data_l) + two;
    EXPECT_EQ(add_rl, high_one_low_one);
    EXPECT_EQ(add_rl.raw_data(), raw_data_l);

    Int add_rr = rvalue("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", raw_data_l) +
                 rvalue("2", raw_data_r);
    EXPECT_EQ(add_rr, high_one_low_one);
    EXPECT_EQ(add_rr.raw_data(), raw_data_r);

    // LValue's and constants
    Int add_lk = all_ones + 2;
    EXPECT_EQ(add_lk, high_one_low_one);

    Int add_kl = 2 + all_ones;
    EXPECT_EQ(add_kl, high_one_low_one);

    // RValue's and constants
    Int add_rk = rvalue("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", raw_data_l) + 2;
    EXPECT_EQ(add_rk, high_one_low_one);
    EXPECT_EQ(add_rk.raw_data(), raw_data_l);

    Int add_kr = 2 + rvalue("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", raw_data_r);
    EXPECT_EQ(add_kr, high_one_low_one);
    EXPECT_EQ(add_kr.raw_data(), raw_data_r);
  }

  {
    // 2 - 1 = 1
    Int sub_ll = two - one;
    EXPECT_EQ(sub_ll, one);

    Int sub_lr = two - rvalue("1", raw_data_r);
    EXPECT_EQ(sub_lr, one);
    EXPECT_EQ(sub_lr.raw_data(), raw_data_r);

    Int sub_rl = rvalue("2", raw_data_l) - one;
    EXPECT_EQ(sub_rl, one);
    EXPECT_EQ(sub_rl.raw_data(), raw_data_l);

    Int sub_rr = rvalue("2", raw_data_l) - rvalue("1", raw_data_r);
    EXPECT_EQ(sub_rr, one);
    EXPECT_EQ(sub_rr.raw_data(), raw_data_r);

    // LValue's and constants
    Int sub_lk = two - 1;
    EXPECT_EQ(sub_lk, one);

    Int SubKL = 2 - one;
    EXPECT_EQ(SubKL, one);

    // RValue's and constants
    Int SubRK = rvalue("2", raw_data_l) - 1;
    EXPECT_EQ(SubRK, one);
    EXPECT_EQ(SubRK.raw_data(), raw_data_l);

    Int SubKR = 2 - rvalue("1", raw_data_r);
    EXPECT_EQ(SubKR, one);
    EXPECT_EQ(SubKR.raw_data(), raw_data_r);
  }

  {
    // 0x100...0001 - 0x0,FFFF...FFFF = 0x2
    Int all_ones(129, "0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16);
    Int high_one_low_one(129, "100000000000000000000000000000001", 16);

    Int sub_ll = high_one_low_one - all_ones;
    EXPECT_EQ(sub_ll, two);

    Int SubLR = high_one_low_one -
                rvalue("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", raw_data_r);
    EXPECT_EQ(SubLR, two);
    EXPECT_EQ(SubLR.raw_data(), raw_data_r);

    Int sub_rl =
        rvalue("100000000000000000000000000000001", raw_data_l) - all_ones;
    EXPECT_EQ(sub_rl, two);
    EXPECT_EQ(sub_rl.raw_data(), raw_data_l);

    Int SubRR = rvalue("100000000000000000000000000000001", raw_data_l) -
                rvalue("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", raw_data_r);
    EXPECT_EQ(SubRR, two);
    EXPECT_EQ(SubRR.raw_data(), raw_data_r);

    // LValue's and constants
    // 0x100...0001 - 0x2 = 0x0,FFFF...FFFF
    Int SubLK = high_one_low_one - 2;
    EXPECT_EQ(SubLK, all_ones);

    // 2 - (-1) = 3
    Int SubKL = 2 - minus_one;
    EXPECT_EQ(SubKL, three);

    // RValue's and constants
    // 0x100...0001 - 0x2 = 0x0,FFFF...FFFF
    Int SubRK = rvalue("100000000000000000000000000000001", raw_data_l) - 2;
    EXPECT_EQ(SubRK, all_ones);
    EXPECT_EQ(SubRK.raw_data(), raw_data_l);

    Int SubKR = 2 - rvalue("1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", raw_data_r);
    EXPECT_EQ(SubKR, three);
    EXPECT_EQ(SubKR.raw_data(), raw_data_r);
  }
}

TEST(IntTest, rvalue_bitwise) {

  auto rvalue = [](const char* HexString, uint64_t const*& RawData) {
    Int V(129, HexString, 16);
    RawData = V.raw_data();
    return V;
  };

  Int Ten(129, "A", 16);
  Int Twelve(129, "C", 16);

  const uint64_t* RawDataL = nullptr;
  const uint64_t* RawDataR = nullptr;

  {
    // 12 & 10 = 8
    Int AndLL = Ten & Twelve;
    EXPECT_EQ(AndLL, 0x8);

    Int AndLR = Ten & rvalue("C", RawDataR);
    EXPECT_EQ(AndLR, 0x8);
    EXPECT_EQ(AndLR.raw_data(), RawDataR);

    Int AndRL = rvalue("A", RawDataL) & Twelve;
    EXPECT_EQ(AndRL, 0x8);
    EXPECT_EQ(AndRL.raw_data(), RawDataL);

    Int AndRR = rvalue("A", RawDataL) & rvalue("C", RawDataR);
    EXPECT_EQ(AndRR, 0x8);
    EXPECT_EQ(AndRR.raw_data(), RawDataR);

    // LValue's and constants
    Int AndLK = Ten & 0xc;
    EXPECT_EQ(AndLK, 0x8);

    Int AndKL = 0xa & Twelve;
    EXPECT_EQ(AndKL, 0x8);

    // RValue's and constants
    Int AndRK = rvalue("A", RawDataL) & 0xc;
    EXPECT_EQ(AndRK, 0x8);
    EXPECT_EQ(AndRK.raw_data(), RawDataL);

    Int AndKR = 0xa & rvalue("C", RawDataR);
    EXPECT_EQ(AndKR, 0x8);
    EXPECT_EQ(AndKR.raw_data(), RawDataR);
  }

  {
    // 12 | 10 = 14
    Int OrLL = Ten | Twelve;
    EXPECT_EQ(OrLL, 0xe);

    Int OrLR = Ten | rvalue("C", RawDataR);
    EXPECT_EQ(OrLR, 0xe);
    EXPECT_EQ(OrLR.raw_data(), RawDataR);

    Int OrRL = rvalue("A", RawDataL) | Twelve;
    EXPECT_EQ(OrRL, 0xe);
    EXPECT_EQ(OrRL.raw_data(), RawDataL);

    Int OrRR = rvalue("A", RawDataL) | rvalue("C", RawDataR);
    EXPECT_EQ(OrRR, 0xe);
    EXPECT_EQ(OrRR.raw_data(), RawDataR);

    // LValue's and constants
    Int OrLK = Ten | 0xc;
    EXPECT_EQ(OrLK, 0xe);

    Int OrKL = 0xa | Twelve;
    EXPECT_EQ(OrKL, 0xe);

    // RValue's and constants
    Int OrRK = rvalue("A", RawDataL) | 0xc;
    EXPECT_EQ(OrRK, 0xe);
    EXPECT_EQ(OrRK.raw_data(), RawDataL);

    Int OrKR = 0xa | rvalue("C", RawDataR);
    EXPECT_EQ(OrKR, 0xe);
    EXPECT_EQ(OrKR.raw_data(), RawDataR);
  }

  {
    // 12 ^ 10 = 6
    Int XorLL = Ten ^ Twelve;
    EXPECT_EQ(XorLL, 0x6);

    Int XorLR = Ten ^ rvalue("C", RawDataR);
    EXPECT_EQ(XorLR, 0x6);
    EXPECT_EQ(XorLR.raw_data(), RawDataR);

    Int XorRL = rvalue("A", RawDataL) ^ Twelve;
    EXPECT_EQ(XorRL, 0x6);
    EXPECT_EQ(XorRL.raw_data(), RawDataL);

    Int XorRR = rvalue("A", RawDataL) ^ rvalue("C", RawDataR);
    EXPECT_EQ(XorRR, 0x6);
    EXPECT_EQ(XorRR.raw_data(), RawDataR);

    // LValue's and constants
    Int XorLK = Ten ^ 0xc;
    EXPECT_EQ(XorLK, 0x6);

    Int XorKL = 0xa ^ Twelve;
    EXPECT_EQ(XorKL, 0x6);

    // RValue's and constants
    Int XorRK = rvalue("A", RawDataL) ^ 0xc;
    EXPECT_EQ(XorRK, 0x6);
    EXPECT_EQ(XorRK.raw_data(), RawDataL);

    Int XorKR = 0xa ^ rvalue("C", RawDataR);
    EXPECT_EQ(XorKR, 0x6);
    EXPECT_EQ(XorKR.raw_data(), RawDataR);
  }
}

TEST(IntTest, rvalue_invert) {
  // Lamdba to return an Int by value, but also provide the raw value of the
  // allocated data.
  auto getRValue = [](const char* HexString, uint64_t const*& RawData) {
    Int V(129, HexString, 16);
    RawData = V.raw_data();
    return V;
  };

  Int One(129, 1);
  Int NegativeTwo(129, -2ULL, true);

  const uint64_t* RawData = nullptr;

  {
    // ~1 = -2
    Int NegL = ~One;
    EXPECT_EQ(NegL, NegativeTwo);

    Int NegR = ~getRValue("1", RawData);
    EXPECT_EQ(NegR, NegativeTwo);
    EXPECT_EQ(NegR.raw_data(), RawData);
  }
}

TEST(IntTest, mul_clear) {
  Int ValA(65, -1ULL);
  Int ValB(65, 4);
  Int ValC(65, 0);
  ValC = ValA * ValB;
  ValA *= ValB;
  lps::basic::String<16> StrA, StrC;
  ValA.string(StrA, 10, false);
  ValC.string(StrC, 10, false);
  EXPECT_EQ(StrA, StrC);
}

// Tests different div/rem varaints using scheme (a * b + c) / a
void testDiv(Int a, Int b, Int c) {
  EXPECT_TRUE(a.uge(b));  // Must: a >= b
  EXPECT_TRUE(a.ugt(c));  // Must: a > c

  auto p = a * b + c;

  auto q = p.udiv(a);
  auto r = p.urem(a);
  EXPECT_EQ(b, q);
  EXPECT_EQ(c, r);
  Int::udivrem(p, a, q, r);
  EXPECT_EQ(b, q);
  EXPECT_EQ(c, r);
  q = p.sdiv(a);
  r = p.srem(a);
  EXPECT_EQ(b, q);
  EXPECT_EQ(c, r);
  Int::sdivrem(p, a, q, r);
  EXPECT_EQ(b, q);
  EXPECT_EQ(c, r);

  if (b.ugt(c)) {  // Test also symmetric case
    q = p.udiv(b);
    r = p.urem(b);
    EXPECT_EQ(a, q);
    EXPECT_EQ(c, r);
    Int::udivrem(p, b, q, r);
    EXPECT_EQ(a, q);
    EXPECT_EQ(c, r);
    q = p.sdiv(b);
    r = p.srem(b);
    EXPECT_EQ(a, q);
    EXPECT_EQ(c, r);
    Int::sdivrem(p, b, q, r);
    EXPECT_EQ(a, q);
    EXPECT_EQ(c, r);
  }
}

TEST(IntTest, divrem_big1) {
  // Tests KnuthDiv rare step D6
  testDiv({256, "1ffffffffffffffff", 16}, {256, "1ffffffffffffffff", 16},
          {256, 0});
}

TEST(IntTest, divrem_big2) {
  // Tests KnuthDiv rare step D6
  testDiv({1024,
           "112233ceff"
           "cecece000000ffffffffffffffffffff"
           "ffffffffffffffffffffffffffffffff"
           "ffffffffffffffffffffffffffffffff"
           "ffffffffffffffffffffffffffffff33",
           16},
          {1024,
           "111111ffffffffffffffff"
           "ffffffffffffffffffffffffffffffff"
           "fffffffffffffffffffffffffffffccf"
           "ffffffffffffffffffffffffffffff00",
           16},
          {1024, 7919});
}

TEST(IntTest, divrem_big3) {
  // Tests KnuthDiv case without shift
  testDiv({256, "80000001ffffffffffffffff", 16},
          {256, "ffffffffffffff0000000", 16}, {256, 4219});
}

TEST(IntTest, divrem_big4) {
  // Tests heap allocation in divide() enfoced by huge numbers
  testDiv(Int{4096, 5}.shl(2001), Int{4096, 1}.shl(2000), Int{4096, 4219 * 13});
}

TEST(IntTest, divrem_big5) {
  // Tests one word divisor case of divide()
  testDiv(Int{1024, 19}.shl(811), Int{1024, 4356013},  // one word
          Int{1024, 1});
}

TEST(IntTest, divrem_big6) {
  // Tests some rare "borrow" cases in D4 step
  testDiv(Int{512, "ffffffffffffffff00000000000000000000000001", 16},
          Int{512, "10000000000000001000000000000001", 16},
          Int{512, "10000000000000000000000000000000", 16});
}

TEST(IntTest, divrem_big7) {
  // Yet another test for KnuthDiv rare step D6.
  testDiv({224, "800000008000000200000005", 16}, {224, "fffffffd", 16},
          {224, "80000000800000010000000f", 16});
}

void testDiv(Int a, uint64_t b, Int c) {
  auto p = a * b + c;

  Int q;
  uint64_t r;
  // Unsigned division will only work if our original number wasn't negative.
  if (!a.is_negative()) {
    q = p.udiv(b);
    r = p.urem(b);
    EXPECT_EQ(a, q);
    EXPECT_EQ(c, r);
    Int::udivrem(p, b, q, r);
    EXPECT_EQ(a, q);
    EXPECT_EQ(c, r);
  }
  q = p.sdiv(b);
  r = p.srem(b);
  EXPECT_EQ(a, q);
  if (c.is_negative()) {
    EXPECT_EQ(-c, -r);
  } else {
    EXPECT_EQ(c, r);
  }
  int64_t sr;
  Int::sdivrem(p, b, q, sr);
  EXPECT_EQ(a, q);
  if (c.is_negative()) {
    EXPECT_EQ(-c, -sr);  // Need to negate so the uint64_t compare will work.
  } else {
    EXPECT_EQ(c, sr);
  }
}

TEST(IntTest, divremuint) {
  // Single word Int
  testDiv(Int{64, 9}, 2, Int{64, 1});

  // Single word negative Int
  testDiv(-Int{64, 9}, 2, -Int{64, 1});

  // Multiword dividend with only one significant word.
  testDiv(Int{256, 9}, 2, Int{256, 1});

  // Negative dividend.
  testDiv(-Int{256, 9}, 2, -Int{256, 1});

  // Multiword dividend
  testDiv(Int{1024, 19}.shl(811),
          4356013,  // one word
          Int{1024, 1});
}

TEST(IntTest, fromString) {
  EXPECT_EQ(Int(32, 0), Int(32, "0", 2));
  EXPECT_EQ(Int(32, 1), Int(32, "1", 2));
  EXPECT_EQ(Int(32, 2), Int(32, "10", 2));
  EXPECT_EQ(Int(32, 3), Int(32, "11", 2));
  EXPECT_EQ(Int(32, 4), Int(32, "100", 2));

  EXPECT_EQ(Int(32, 0), Int(32, "+0", 2));
  EXPECT_EQ(Int(32, 1), Int(32, "+1", 2));
  EXPECT_EQ(Int(32, 2), Int(32, "+10", 2));
  EXPECT_EQ(Int(32, 3), Int(32, "+11", 2));
  EXPECT_EQ(Int(32, 4), Int(32, "+100", 2));

  EXPECT_EQ(Int(32, uint64_t(-0LL)), Int(32, "-0", 2));
  EXPECT_EQ(Int(32, uint64_t(-1LL)), Int(32, "-1", 2));
  EXPECT_EQ(Int(32, uint64_t(-2LL)), Int(32, "-10", 2));
  EXPECT_EQ(Int(32, uint64_t(-3LL)), Int(32, "-11", 2));
  EXPECT_EQ(Int(32, uint64_t(-4LL)), Int(32, "-100", 2));

  EXPECT_EQ(Int(32, 0), Int(32, "0", 8));
  EXPECT_EQ(Int(32, 1), Int(32, "1", 8));
  EXPECT_EQ(Int(32, 7), Int(32, "7", 8));
  EXPECT_EQ(Int(32, 8), Int(32, "10", 8));
  EXPECT_EQ(Int(32, 15), Int(32, "17", 8));
  EXPECT_EQ(Int(32, 16), Int(32, "20", 8));

  EXPECT_EQ(Int(32, +0), Int(32, "+0", 8));
  EXPECT_EQ(Int(32, +1), Int(32, "+1", 8));
  EXPECT_EQ(Int(32, +7), Int(32, "+7", 8));
  EXPECT_EQ(Int(32, +8), Int(32, "+10", 8));
  EXPECT_EQ(Int(32, +15), Int(32, "+17", 8));
  EXPECT_EQ(Int(32, +16), Int(32, "+20", 8));

  EXPECT_EQ(Int(32, uint64_t(-0LL)), Int(32, "-0", 8));
  EXPECT_EQ(Int(32, uint64_t(-1LL)), Int(32, "-1", 8));
  EXPECT_EQ(Int(32, uint64_t(-7LL)), Int(32, "-7", 8));
  EXPECT_EQ(Int(32, uint64_t(-8LL)), Int(32, "-10", 8));
  EXPECT_EQ(Int(32, uint64_t(-15LL)), Int(32, "-17", 8));
  EXPECT_EQ(Int(32, uint64_t(-16LL)), Int(32, "-20", 8));

  EXPECT_EQ(Int(32, 0), Int(32, "0", 10));
  EXPECT_EQ(Int(32, 1), Int(32, "1", 10));
  EXPECT_EQ(Int(32, 9), Int(32, "9", 10));
  EXPECT_EQ(Int(32, 10), Int(32, "10", 10));
  EXPECT_EQ(Int(32, 19), Int(32, "19", 10));
  EXPECT_EQ(Int(32, 20), Int(32, "20", 10));

  EXPECT_EQ(Int(32, uint64_t(-0LL)), Int(32, "-0", 10));
  EXPECT_EQ(Int(32, uint64_t(-1LL)), Int(32, "-1", 10));
  EXPECT_EQ(Int(32, uint64_t(-9LL)), Int(32, "-9", 10));
  EXPECT_EQ(Int(32, uint64_t(-10LL)), Int(32, "-10", 10));
  EXPECT_EQ(Int(32, uint64_t(-19LL)), Int(32, "-19", 10));
  EXPECT_EQ(Int(32, uint64_t(-20LL)), Int(32, "-20", 10));

  EXPECT_EQ(Int(32, 0), Int(32, "0", 16));
  EXPECT_EQ(Int(32, 1), Int(32, "1", 16));
  EXPECT_EQ(Int(32, 15), Int(32, "F", 16));
  EXPECT_EQ(Int(32, 16), Int(32, "10", 16));
  EXPECT_EQ(Int(32, 31), Int(32, "1F", 16));
  EXPECT_EQ(Int(32, 32), Int(32, "20", 16));

  EXPECT_EQ(Int(32, uint64_t(-0LL)), Int(32, "-0", 16));
  EXPECT_EQ(Int(32, uint64_t(-1LL)), Int(32, "-1", 16));
  EXPECT_EQ(Int(32, uint64_t(-15LL)), Int(32, "-F", 16));
  EXPECT_EQ(Int(32, uint64_t(-16LL)), Int(32, "-10", 16));
  EXPECT_EQ(Int(32, uint64_t(-31LL)), Int(32, "-1F", 16));
  EXPECT_EQ(Int(32, uint64_t(-32LL)), Int(32, "-20", 16));
}

TEST(IntTest, SaturatingMath) {
  Int AP_10 = Int(8, 10);
  Int AP_42 = Int(8, 42);
  Int AP_100 = Int(8, 100);
  Int AP_200 = Int(8, 200);

  EXPECT_EQ(Int(8, 100), AP_100.trunc_usat(8));
  EXPECT_EQ(Int(7, 100), AP_100.trunc_usat(7));
  EXPECT_EQ(Int(6, 63), AP_100.trunc_usat(6));
  EXPECT_EQ(Int(5, 31), AP_100.trunc_usat(5));

  EXPECT_EQ(Int(8, 200), AP_200.trunc_usat(8));
  EXPECT_EQ(Int(7, 127), AP_200.trunc_usat(7));
  EXPECT_EQ(Int(6, 63), AP_200.trunc_usat(6));
  EXPECT_EQ(Int(5, 31), AP_200.trunc_usat(5));

  EXPECT_EQ(Int(8, 42), AP_42.trunc_ssat(8));
  EXPECT_EQ(Int(7, 42), AP_42.trunc_ssat(7));
  EXPECT_EQ(Int(6, 31), AP_42.trunc_ssat(6));
  EXPECT_EQ(Int(5, 15), AP_42.trunc_ssat(5));

  EXPECT_EQ(Int(8, -56), AP_200.trunc_ssat(8));
  EXPECT_EQ(Int(7, -56), AP_200.trunc_ssat(7));
  EXPECT_EQ(Int(6, -32), AP_200.trunc_ssat(6));
  EXPECT_EQ(Int(5, -16), AP_200.trunc_ssat(5));

  EXPECT_EQ(Int(8, 200), AP_100.uadd_sat(AP_100));
  EXPECT_EQ(Int(8, 255), AP_100.uadd_sat(AP_200));
  EXPECT_EQ(Int(8, 255), Int(8, 255).uadd_sat(Int(8, 255)));

  EXPECT_EQ(Int(8, 110), AP_10.sadd_sat(AP_100));
  EXPECT_EQ(Int(8, 127), AP_100.sadd_sat(AP_100));
  EXPECT_EQ(Int(8, -128), (-AP_100).sadd_sat(-AP_100));
  EXPECT_EQ(Int(8, -128), Int(8, -128).sadd_sat(Int(8, -128)));

  EXPECT_EQ(Int(8, 90), AP_100.usub_sat(AP_10));
  EXPECT_EQ(Int(8, 0), AP_100.usub_sat(AP_200));
  EXPECT_EQ(Int(8, 0), Int(8, 0).usub_sat(Int(8, 255)));

  EXPECT_EQ(Int(8, -90), AP_10.ssub_sat(AP_100));
  EXPECT_EQ(Int(8, 127), AP_100.ssub_sat(-AP_100));
  EXPECT_EQ(Int(8, -128), (-AP_100).ssub_sat(AP_100));
  EXPECT_EQ(Int(8, -128), Int(8, -128).ssub_sat(Int(8, 127)));

  EXPECT_EQ(Int(8, 250), Int(8, 50).umul_sat(Int(8, 5)));
  EXPECT_EQ(Int(8, 255), Int(8, 50).umul_sat(Int(8, 6)));
  EXPECT_EQ(Int(8, 255), Int(8, -128).umul_sat(Int(8, 3)));
  EXPECT_EQ(Int(8, 255), Int(8, 3).umul_sat(Int(8, -128)));
  EXPECT_EQ(Int(8, 255), Int(8, -128).umul_sat(Int(8, -128)));

  EXPECT_EQ(Int(8, 125), Int(8, 25).smul_sat(Int(8, 5)));
  EXPECT_EQ(Int(8, 127), Int(8, 25).smul_sat(Int(8, 6)));
  EXPECT_EQ(Int(8, 127), Int(8, 127).smul_sat(Int(8, 127)));
  EXPECT_EQ(Int(8, -125), Int(8, -25).smul_sat(Int(8, 5)));
  EXPECT_EQ(Int(8, -125), Int(8, 25).smul_sat(Int(8, -5)));
  EXPECT_EQ(Int(8, 125), Int(8, -25).smul_sat(Int(8, -5)));
  EXPECT_EQ(Int(8, 125), Int(8, 25).smul_sat(Int(8, 5)));
  EXPECT_EQ(Int(8, -128), Int(8, -25).smul_sat(Int(8, 6)));
  EXPECT_EQ(Int(8, -128), Int(8, 25).smul_sat(Int(8, -6)));
  EXPECT_EQ(Int(8, 127), Int(8, -25).smul_sat(Int(8, -6)));
  EXPECT_EQ(Int(8, 127), Int(8, 25).smul_sat(Int(8, 6)));

  EXPECT_EQ(Int(8, 128), Int(8, 4).ushl_sat(Int(8, 5)));
  EXPECT_EQ(Int(8, 255), Int(8, 4).ushl_sat(Int(8, 6)));
  EXPECT_EQ(Int(8, 128), Int(8, 1).ushl_sat(Int(8, 7)));
  EXPECT_EQ(Int(8, 255), Int(8, 1).ushl_sat(Int(8, 8)));
  EXPECT_EQ(Int(8, 255), Int(8, -128).ushl_sat(Int(8, 2)));
  EXPECT_EQ(Int(8, 255), Int(8, 64).ushl_sat(Int(8, 2)));
  EXPECT_EQ(Int(8, 255), Int(8, 64).ushl_sat(Int(8, -2)));

  EXPECT_EQ(Int(8, 64), Int(8, 4).sshl_sat(Int(8, 4)));
  EXPECT_EQ(Int(8, 127), Int(8, 4).sshl_sat(Int(8, 5)));
  EXPECT_EQ(Int(8, 127), Int(8, 1).sshl_sat(Int(8, 8)));
  EXPECT_EQ(Int(8, -64), Int(8, -4).sshl_sat(Int(8, 4)));
  EXPECT_EQ(Int(8, -128), Int(8, -4).sshl_sat(Int(8, 5)));
  EXPECT_EQ(Int(8, -128), Int(8, -4).sshl_sat(Int(8, 6)));
  EXPECT_EQ(Int(8, -128), Int(8, -1).sshl_sat(Int(8, 7)));
  EXPECT_EQ(Int(8, -128), Int(8, -1).sshl_sat(Int(8, 8)));
}

TEST(IntTest, FromArray) {
  lps::basic::Vector<1, uint64_t> aa;
  aa.append(1);
  EXPECT_EQ(Int(32, uint64_t(1)), Int(32, aa.data(), 1));
}

TEST(IntTest, StringBitsNeeded2) {
  EXPECT_EQ(1U, Int::bits_needed("0", 2));
  EXPECT_EQ(1U, Int::bits_needed("1", 2));
  EXPECT_EQ(2U, Int::bits_needed("10", 2));
  EXPECT_EQ(2U, Int::bits_needed("11", 2));
  EXPECT_EQ(3U, Int::bits_needed("100", 2));

  EXPECT_EQ(1U, Int::bits_needed("+0", 2));
  EXPECT_EQ(1U, Int::bits_needed("+1", 2));
  EXPECT_EQ(2U, Int::bits_needed("+10", 2));
  EXPECT_EQ(2U, Int::bits_needed("+11", 2));
  EXPECT_EQ(3U, Int::bits_needed("+100", 2));

  EXPECT_EQ(2U, Int::bits_needed("-0", 2));
  EXPECT_EQ(2U, Int::bits_needed("-1", 2));
  EXPECT_EQ(3U, Int::bits_needed("-10", 2));
  EXPECT_EQ(3U, Int::bits_needed("-11", 2));
  EXPECT_EQ(4U, Int::bits_needed("-100", 2));
}

TEST(IntTest, StringBitsNeeded8) {
  EXPECT_EQ(3U, Int::bits_needed("0", 8));
  EXPECT_EQ(3U, Int::bits_needed("7", 8));
  EXPECT_EQ(6U, Int::bits_needed("10", 8));
  EXPECT_EQ(6U, Int::bits_needed("17", 8));
  EXPECT_EQ(6U, Int::bits_needed("20", 8));

  EXPECT_EQ(3U, Int::bits_needed("+0", 8));
  EXPECT_EQ(3U, Int::bits_needed("+7", 8));
  EXPECT_EQ(6U, Int::bits_needed("+10", 8));
  EXPECT_EQ(6U, Int::bits_needed("+17", 8));
  EXPECT_EQ(6U, Int::bits_needed("+20", 8));

  EXPECT_EQ(4U, Int::bits_needed("-0", 8));
  EXPECT_EQ(4U, Int::bits_needed("-7", 8));
  EXPECT_EQ(7U, Int::bits_needed("-10", 8));
  EXPECT_EQ(7U, Int::bits_needed("-17", 8));
  EXPECT_EQ(7U, Int::bits_needed("-20", 8));
}

TEST(IntTest, StringBitsNeeded10) {
  EXPECT_EQ(1U, Int::bits_needed("0", 10));
  EXPECT_EQ(2U, Int::bits_needed("3", 10));
  EXPECT_EQ(4U, Int::bits_needed("9", 10));
  EXPECT_EQ(4U, Int::bits_needed("10", 10));
  EXPECT_EQ(5U, Int::bits_needed("19", 10));
  EXPECT_EQ(5U, Int::bits_needed("20", 10));

  EXPECT_EQ(1U, Int::bits_needed("+0", 10));
  EXPECT_EQ(4U, Int::bits_needed("+9", 10));
  EXPECT_EQ(4U, Int::bits_needed("+10", 10));
  EXPECT_EQ(5U, Int::bits_needed("+19", 10));
  EXPECT_EQ(5U, Int::bits_needed("+20", 10));

  EXPECT_EQ(2U, Int::bits_needed("-0", 10));
  EXPECT_EQ(5U, Int::bits_needed("-9", 10));
  EXPECT_EQ(5U, Int::bits_needed("-10", 10));
  EXPECT_EQ(6U, Int::bits_needed("-19", 10));
  EXPECT_EQ(6U, Int::bits_needed("-20", 10));

  EXPECT_EQ(1U, Int::bits_needed("-1", 10));
  EXPECT_EQ(2U, Int::bits_needed("-2", 10));
  EXPECT_EQ(3U, Int::bits_needed("-4", 10));
  EXPECT_EQ(4U, Int::bits_needed("-8", 10));
  EXPECT_EQ(5U, Int::bits_needed("-16", 10));
  EXPECT_EQ(6U, Int::bits_needed("-23", 10));
  EXPECT_EQ(6U, Int::bits_needed("-32", 10));
  EXPECT_EQ(7U, Int::bits_needed("-64", 10));
  EXPECT_EQ(8U, Int::bits_needed("-127", 10));
  EXPECT_EQ(8U, Int::bits_needed("-128", 10));
  EXPECT_EQ(9U, Int::bits_needed("-255", 10));
  EXPECT_EQ(9U, Int::bits_needed("-256", 10));
  EXPECT_EQ(10U, Int::bits_needed("-512", 10));
  EXPECT_EQ(11U, Int::bits_needed("-1024", 10));
  EXPECT_EQ(12U, Int::bits_needed("-1025", 10));
}

TEST(IntTest, StringBitsNeeded16) {
  EXPECT_EQ(4U, Int::bits_needed("0", 16));
  EXPECT_EQ(4U, Int::bits_needed("F", 16));
  EXPECT_EQ(8U, Int::bits_needed("10", 16));
  EXPECT_EQ(8U, Int::bits_needed("1F", 16));
  EXPECT_EQ(8U, Int::bits_needed("20", 16));

  EXPECT_EQ(4U, Int::bits_needed("+0", 16));
  EXPECT_EQ(4U, Int::bits_needed("+F", 16));
  EXPECT_EQ(8U, Int::bits_needed("+10", 16));
  EXPECT_EQ(8U, Int::bits_needed("+1F", 16));
  EXPECT_EQ(8U, Int::bits_needed("+20", 16));

  EXPECT_EQ(5U, Int::bits_needed("-0", 16));
  EXPECT_EQ(5U, Int::bits_needed("-F", 16));
  EXPECT_EQ(9U, Int::bits_needed("-10", 16));
  EXPECT_EQ(9U, Int::bits_needed("-1F", 16));
  EXPECT_EQ(9U, Int::bits_needed("-20", 16));
}

TEST(IntTest, string) {
  lps::basic::String<16> ss;
  bool is_signed;

  Int(8, 0).string(ss, 2, true, true);
  EXPECT_EQ(std::string(ss), "0b0");
  ss.clear();
  Int(8, 0).string(ss, 8, true, true);
  EXPECT_EQ(std::string(ss), "00");
  ss.clear();
  Int(8, 0).string(ss, 10, true, true);
  EXPECT_EQ(std::string(ss), "0");
  ss.clear();
  Int(8, 0).string(ss, 16, true, true);
  EXPECT_EQ(std::string(ss), "0x0");
  ss.clear();

  is_signed = false;
  Int(8, 255, is_signed).string(ss, 2, is_signed, true);
  EXPECT_EQ(std::string(ss), "0b11111111");
  ss.clear();
  Int(8, 255, is_signed).string(ss, 8, is_signed, true);
  EXPECT_EQ(std::string(ss), "0377");
  ss.clear();
  Int(8, 255, is_signed).string(ss, 10, is_signed, true);
  EXPECT_EQ(std::string(ss), "255");
  ss.clear();
  Int(8, 255, is_signed).string(ss, 16, is_signed, true);
  EXPECT_EQ(std::string(ss), "0xFF");
  ss.clear();

  is_signed = true;
  Int(8, 255, is_signed).string(ss, 2, is_signed, true);
  EXPECT_EQ(std::string(ss), "-0b1");
  ss.clear();
  Int(8, 255, is_signed).string(ss, 8, is_signed, true);
  EXPECT_EQ(std::string(ss), "-01");
  ss.clear();
  Int(8, 255, is_signed).string(ss, 10, is_signed, true);
  EXPECT_EQ(std::string(ss), "-1");
  ss.clear();
  Int(8, 255, is_signed).string(ss, 16, is_signed, true);
  EXPECT_EQ(std::string(ss), "-0x1");
  ss.clear();
}

TEST(IntTest, Log2) {
  EXPECT_EQ(Int(15, 7).log2(), 2U);
  EXPECT_EQ(Int(15, 7).ceil_log2(), 3U);
  EXPECT_EQ(Int(15, 7).exact_log2(), -1);
  EXPECT_EQ(Int(15, 8).log2(), 3U);
  EXPECT_EQ(Int(15, 8).ceil_log2(), 3U);
  EXPECT_EQ(Int(15, 8).exact_log2(), 3);
  EXPECT_EQ(Int(15, 9).log2(), 3U);
  EXPECT_EQ(Int(15, 9).ceil_log2(), 4U);
  EXPECT_EQ(Int(15, 9).exact_log2(), -1);
}

TEST(IntTest, Rotate) {
  EXPECT_EQ(Int(8, 1), Int(8, 1).rotl(0));
  EXPECT_EQ(Int(8, 2), Int(8, 1).rotl(1));
  EXPECT_EQ(Int(8, 4), Int(8, 1).rotl(2));
  EXPECT_EQ(Int(8, 16), Int(8, 1).rotl(4));
  EXPECT_EQ(Int(8, 1), Int(8, 1).rotl(8));

  EXPECT_EQ(Int(8, 16), Int(8, 16).rotl(0));
  EXPECT_EQ(Int(8, 32), Int(8, 16).rotl(1));
  EXPECT_EQ(Int(8, 64), Int(8, 16).rotl(2));
  EXPECT_EQ(Int(8, 1), Int(8, 16).rotl(4));
  EXPECT_EQ(Int(8, 16), Int(8, 16).rotl(8));

  EXPECT_EQ(Int(32, 2), Int(32, 1).rotl(33));
  EXPECT_EQ(Int(32, 2), Int(32, 1).rotl(Int(32, 33)));

  EXPECT_EQ(Int(32, 2), Int(32, 1).rotl(33));
  EXPECT_EQ(Int(32, 2), Int(32, 1).rotl(Int(32, 33)));
  EXPECT_EQ(Int(32, 2), Int(32, 1).rotl(Int(33, 33)));
  EXPECT_EQ(Int(32, (1 << 8)), Int(32, 1).rotl(Int(32, 40)));
  EXPECT_EQ(Int(32, (1 << 30)), Int(32, 1).rotl(Int(31, 30)));
  EXPECT_EQ(Int(32, (1 << 31)), Int(32, 1).rotl(Int(31, 31)));

  EXPECT_EQ(Int(32, 1), Int(32, 1).rotl(Int(1, 0)));
  EXPECT_EQ(Int(32, 2), Int(32, 1).rotl(Int(1, 1)));

  EXPECT_EQ(Int(32, 16), Int(32, 1).rotl(Int(3, 4)));

  EXPECT_EQ(Int(32, 1), Int(32, 1).rotl(Int(64, 64)));
  EXPECT_EQ(Int(32, 2), Int(32, 1).rotl(Int(64, 65)));

  EXPECT_EQ(Int(7, 24), Int(7, 3).rotl(Int(7, 3)));
  EXPECT_EQ(Int(7, 24), Int(7, 3).rotl(Int(7, 10)));
  EXPECT_EQ(Int(7, 24), Int(7, 3).rotl(Int(5, 10)));
  EXPECT_EQ(Int(7, 6), Int(7, 3).rotl(Int(12, 120)));

  EXPECT_EQ(Int(8, 16), Int(8, 16).rotr(0));
  EXPECT_EQ(Int(8, 8), Int(8, 16).rotr(1));
  EXPECT_EQ(Int(8, 4), Int(8, 16).rotr(2));
  EXPECT_EQ(Int(8, 1), Int(8, 16).rotr(4));
  EXPECT_EQ(Int(8, 16), Int(8, 16).rotr(8));

  EXPECT_EQ(Int(8, 1), Int(8, 1).rotr(0));
  EXPECT_EQ(Int(8, 128), Int(8, 1).rotr(1));
  EXPECT_EQ(Int(8, 64), Int(8, 1).rotr(2));
  EXPECT_EQ(Int(8, 16), Int(8, 1).rotr(4));
  EXPECT_EQ(Int(8, 1), Int(8, 1).rotr(8));

  EXPECT_EQ(Int(32, (1 << 31)), Int(32, 1).rotr(33));
  EXPECT_EQ(Int(32, (1 << 31)), Int(32, 1).rotr(Int(32, 33)));

  EXPECT_EQ(Int(32, (1 << 31)), Int(32, 1).rotr(33));
  EXPECT_EQ(Int(32, (1 << 31)), Int(32, 1).rotr(Int(32, 33)));
  EXPECT_EQ(Int(32, (1 << 31)), Int(32, 1).rotr(Int(33, 33)));
  EXPECT_EQ(Int(32, (1 << 24)), Int(32, 1).rotr(Int(32, 40)));

  EXPECT_EQ(Int(32, (1 << 2)), Int(32, 1).rotr(Int(31, 30)));
  EXPECT_EQ(Int(32, (1 << 1)), Int(32, 1).rotr(Int(31, 31)));

  EXPECT_EQ(Int(32, 1), Int(32, 1).rotr(Int(1, 0)));
  EXPECT_EQ(Int(32, (1 << 31)), Int(32, 1).rotr(Int(1, 1)));

  EXPECT_EQ(Int(32, (1 << 28)), Int(32, 1).rotr(Int(3, 4)));

  EXPECT_EQ(Int(32, 1), Int(32, 1).rotr(Int(64, 64)));
  EXPECT_EQ(Int(32, (1 << 31)), Int(32, 1).rotr(Int(64, 65)));

  EXPECT_EQ(Int(7, 48), Int(7, 3).rotr(Int(7, 3)));
  EXPECT_EQ(Int(7, 48), Int(7, 3).rotr(Int(7, 10)));
  EXPECT_EQ(Int(7, 48), Int(7, 3).rotr(Int(5, 10)));
  EXPECT_EQ(Int(7, 65), Int(7, 3).rotr(Int(12, 120)));

  Int Big(256, "00004000800000000000000000003fff8000000000000003", 16);
  Int Rot(256, "3fff80000000000000030000000000000000000040008000", 16);
  EXPECT_EQ(Rot, Big.rotr(144));

  EXPECT_EQ(Int(32, 8), Int(32, 1).rotl(Big));
  EXPECT_EQ(Int(32, (1 << 29)), Int(32, 1).rotr(Big));
}

TEST(IntTest, Splat) {
  Int ValA(8, 0x01);
  EXPECT_EQ(ValA, Int::splat(8, ValA));
  EXPECT_EQ(Int(64, 0x0101010101010101ULL), Int::splat(64, ValA));

  Int ValB(3, 5);
  EXPECT_EQ(Int(4, 0xD), Int::splat(4, ValB));
  EXPECT_EQ(Int(15, 0xDB6D), Int::splat(15, ValB));
}

TEST(IntTest, tcDecrement) {
  // Test single word decrement.

  // No out borrow.
  {
    Int::word_type singleWord = ~Int::word_type(0) << (Int::kBitsOfWord - 1);
    Int::word_type carry = Int::decrement(&singleWord, 1);
    EXPECT_EQ(carry, Int::word_type(0));
    EXPECT_EQ(singleWord, ~Int::word_type(0) >> 1);
  }

  // With out borrow.
  {
    Int::word_type singleWord = 0;
    Int::word_type carry = Int::decrement(&singleWord, 1);
    EXPECT_EQ(carry, Int::word_type(1));
    EXPECT_EQ(singleWord, ~Int::word_type(0));
  }

  // Test multiword decrement.

  // No across word borrow, no out borrow.
  {
    Int::word_type test[4] = {0x1, 0x1, 0x1, 0x1};
    Int::word_type expected[4] = {0x0, 0x1, 0x1, 0x1};
    Int::decrement(test, 4);
    EXPECT_EQ(Int::compare(test, expected, 4), 0);
  }

  // 1 across word borrow, no out borrow.
  {
    Int::word_type test[4] = {0x0, 0xF, 0x1, 0x1};
    Int::word_type expected[4] = {~Int::word_type(0), 0xE, 0x1, 0x1};
    Int::word_type carry = Int::decrement(test, 4);
    EXPECT_EQ(carry, Int::word_type(0));
    EXPECT_EQ(Int::compare(test, expected, 4), 0);
  }

  // 2 across word borrow, no out borrow.
  {
    Int::word_type test[4] = {0x0, 0x0, 0xC, 0x1};
    Int::word_type expected[4] = {~Int::word_type(0), ~Int::word_type(0), 0xB,
                                  0x1};
    Int::word_type carry = Int::decrement(test, 4);
    EXPECT_EQ(carry, Int::word_type(0));
    EXPECT_EQ(Int::compare(test, expected, 4), 0);
  }

  // 3 across word borrow, no out borrow.
  {
    Int::word_type test[4] = {0x0, 0x0, 0x0, 0x1};
    Int::word_type expected[4] = {~Int::word_type(0), ~Int::word_type(0),
                                  ~Int::word_type(0), 0x0};
    Int::word_type carry = Int::decrement(test, 4);
    EXPECT_EQ(carry, Int::word_type(0));
    EXPECT_EQ(Int::compare(test, expected, 4), 0);
  }

  // 3 across word borrow, with out borrow.
  {
    Int::word_type test[4] = {0x0, 0x0, 0x0, 0x0};
    Int::word_type expected[4] = {~Int::word_type(0), ~Int::word_type(0),
                                  ~Int::word_type(0), ~Int::word_type(0)};
    Int::word_type carry = Int::decrement(test, 4);
    EXPECT_EQ(carry, Int::word_type(1));
    EXPECT_EQ(Int::compare(test, expected, 4), 0);
  }
}

TEST(IntTest, arrayAccess) {
  // Single word check.
  uint64_t E1 = 0x2CA7F46BF6569915ULL;
  Int A1(64, E1);
  for (unsigned i = 0, e = 64; i < e; ++i) {
    EXPECT_EQ(bool(E1 & (1ULL << i)), A1[i]);
  }

  // Multiword check.
  Int::word_type E2[4] = {0xEB6EB136591CBA21ULL, 0x7B9358BD6A33F10AULL,
                          0x7E7FFA5EADD8846ULL, 0x305F341CA00B613DULL};
  Int A2(Int::kBitsOfWord * 4, E2, 4);
  for (unsigned i = 0; i < 4; ++i) {
    for (unsigned j = 0; j < Int::kBitsOfWord; ++j) {
      EXPECT_EQ(bool(E2[i] & (1ULL << j)), A2[i * Int::kBitsOfWord + j]);
    }
  }
}

TEST(IntTest, LargeIntConstruction) {
  // Check that we can properly construct very large Int. It is very
  // unlikely that people will ever do this, but it is a legal input,
  // so we should not crash on it.
  Int A9(UINT32_MAX, 0);
  EXPECT_FALSE(A9.bool_value());
}

TEST(IntTest, nearest_log2) {
  // Single word check.

  // Test round up.
  uint64_t I1 = 0x1800001;
  Int A1(64, I1);
  EXPECT_EQ(A1.nearest_log2(), A1.ceil_log2());

  // Test round down.
  uint64_t I2 = 0x1000011;
  Int A2(64, I2);
  EXPECT_EQ(A2.nearest_log2(), A2.log2());

  // Test ties round up.
  uint64_t I3 = 0x1800000;
  Int A3(64, I3);
  EXPECT_EQ(A3.nearest_log2(), A3.ceil_log2());

  // Multiple word check.

  // Test round up.
  Int::word_type I4[4] = {0x0, 0xF, 0x18, 0x0};
  Int A4(Int::kBitsOfWord * 4, I4, 4);
  EXPECT_EQ(A4.nearest_log2(), A4.ceil_log2());

  // Test round down.
  Int::word_type I5[4] = {0x0, 0xF, 0x10, 0x0};
  Int A5(Int::kBitsOfWord * 4, I5, 5);
  EXPECT_EQ(A5.nearest_log2(), A5.log2());

  // Test ties round up.
  uint64_t I6[4] = {0x0, 0x0, 0x0, 0x18};
  Int A6(Int::kBitsOfWord * 4, I6, 4);
  EXPECT_EQ(A6.nearest_log2(), A6.ceil_log2());

  // Test BitWidth == 1 special cases.
  Int A7(1, 1);
  EXPECT_EQ(A7.nearest_log2(), 0ULL);
  Int A8(1, 0);
  EXPECT_EQ(A8.nearest_log2(), UINT64_MAX);

  // Test the zero case when we have a bit width large enough such
  // that the bit width is larger than UINT32_MAX-1.
  Int A9(UINT64_MAX, 0);
  EXPECT_EQ(A9.nearest_log2(), UINT64_MAX);
}

TEST(IntTest, divrem_simple) {
  // Test simple cases.
  Int A(65, 2), B(65, 2);
  Int Q, R;

  // X / X
  Int::sdivrem(A, B, Q, R);
  EXPECT_EQ(Q, Int(65, 1));
  EXPECT_EQ(R, Int(65, 0));
  Int::udivrem(A, B, Q, R);
  EXPECT_EQ(Q, Int(65, 1));
  EXPECT_EQ(R, Int(65, 0));

  // 0 / X
  Int O(65, 0);
  Int::sdivrem(O, B, Q, R);
  EXPECT_EQ(Q, Int(65, 0));
  EXPECT_EQ(R, Int(65, 0));
  Int::udivrem(O, B, Q, R);
  EXPECT_EQ(Q, Int(65, 0));
  EXPECT_EQ(R, Int(65, 0));

  // X / 1
  Int I(65, 1);
  Int::sdivrem(A, I, Q, R);
  EXPECT_EQ(Q, A);
  EXPECT_EQ(R, Int(65, 0));
  Int::udivrem(A, I, Q, R);
  EXPECT_EQ(Q, A);
  EXPECT_EQ(R, Int(65, 0));
}

TEST(SIntTest, MoveTest) {
  SInt a(32, true);
  EXPECT_TRUE(a.is_unsigned());

  SInt b(128, false);
  a = b;
  EXPECT_FALSE(a.is_unsigned());

  const SInt& c(b);
  EXPECT_FALSE(c.is_unsigned());

  Int wide(256, 0);
  const uint64_t* bits = wide.raw_data();
  SInt d(std::move(wide));
  EXPECT_TRUE(d.is_unsigned());
  EXPECT_EQ(bits, d.raw_data());

  a = SInt(64, true);
  EXPECT_TRUE(a.is_unsigned());

  wide = Int(128, 1);
  bits = wide.raw_data();
  a = std::move(wide);
  EXPECT_TRUE(a.is_unsigned());
  EXPECT_EQ(bits, a.raw_data());
}

TEST(SIntTest, get) {
  EXPECT_TRUE(SInt::get(7).is_signed());
  EXPECT_EQ(64U, SInt::get(7).bits());
  EXPECT_EQ(7U, SInt::get(7).zero_ext_value());
  EXPECT_EQ(7, SInt::get(7).sign_ext_value());
  EXPECT_TRUE(SInt::get(-7).is_signed());
  EXPECT_EQ(64U, SInt::get(-7).bits());
  EXPECT_EQ(-7, SInt::get(-7).sign_ext_value());
  EXPECT_EQ(UINT64_C(0) - 7, SInt::get(-7).zero_ext_value());
}

TEST(SIntTest, ext_value) {
  EXPECT_TRUE(SInt(Int(3, 7), true).is_unsigned());
  EXPECT_TRUE(SInt(Int(3, 7), false).is_signed());
  EXPECT_TRUE(SInt(Int(4, 7), true).is_unsigned());
  EXPECT_TRUE(SInt(Int(4, 7), false).is_signed());
  EXPECT_TRUE(SInt(Int(4, -7), true).is_unsigned());
  EXPECT_TRUE(SInt(Int(4, -7), false).is_signed());
  EXPECT_EQ(7, SInt(Int(3, 7), true).ext_value());
  EXPECT_EQ(-1, SInt(Int(3, 7), false).ext_value());
  EXPECT_EQ(7, SInt(Int(4, 7), true).ext_value());
  EXPECT_EQ(7, SInt(Int(4, 7), false).ext_value());
  EXPECT_EQ(9, SInt(Int(4, -7), true).ext_value());
  EXPECT_EQ(-7, SInt(Int(4, -7), false).ext_value());
}

TEST(SIntTest, try_ext_value) {
  EXPECT_EQ(-7, SInt(Int(64, -7), false).try_ext_value().value_or(42));
  EXPECT_EQ(42, SInt(Int(128, -7), false).try_ext_value().value_or(42));
  EXPECT_EQ(-1, SInt(Int::all_ones(128), false).try_ext_value().value_or(42));
  EXPECT_EQ(42, SInt(Int(64, -7), true).try_ext_value().value_or(42));
  EXPECT_EQ(1, SInt(Int(128, 1), true).try_ext_value().value_or(42));
  EXPECT_EQ(42, SInt(Int::all_ones(128), true).try_ext_value().value_or(42));
}

TEST(SIntTest, compare_values) {
  auto U = [](uint64_t V) {
    return SInt::get_unsigned(V);
  };
  auto S = [](int64_t V) {
    return SInt::get(V);
  };

  // Bit-width matches and is-signed.
  EXPECT_TRUE(SInt::compare_values(S(7), S(8)) < 0);
  EXPECT_TRUE(SInt::compare_values(S(8), S(7)) > 0);
  EXPECT_TRUE(SInt::compare_values(S(7), S(7)) == 0);
  EXPECT_TRUE(SInt::compare_values(S(-7), S(8)) < 0);
  EXPECT_TRUE(SInt::compare_values(S(8), S(-7)) > 0);
  EXPECT_TRUE(SInt::compare_values(S(-7), S(-7)) == 0);
  EXPECT_TRUE(SInt::compare_values(S(-7), S(-8)) > 0);
  EXPECT_TRUE(SInt::compare_values(S(-8), S(-7)) < 0);
  EXPECT_TRUE(SInt::compare_values(S(-7), S(-7)) == 0);

  // Bit-width matches and not is-signed.
  EXPECT_TRUE(SInt::compare_values(U(7), U(8)) < 0);
  EXPECT_TRUE(SInt::compare_values(U(8), U(7)) > 0);
  EXPECT_TRUE(SInt::compare_values(U(7), U(7)) == 0);

  // Bit-width matches and mixed signs.
  EXPECT_TRUE(SInt::compare_values(U(7), S(8)) < 0);
  EXPECT_TRUE(SInt::compare_values(U(8), S(7)) > 0);
  EXPECT_TRUE(SInt::compare_values(U(7), S(7)) == 0);
  EXPECT_TRUE(SInt::compare_values(U(8), S(-7)) > 0);

  // Bit-width mismatch and is-signed.
  EXPECT_TRUE(SInt::compare_values(S(7).trunc(32), S(8)) < 0);
  EXPECT_TRUE(SInt::compare_values(S(8).trunc(32), S(7)) > 0);
  EXPECT_TRUE(SInt::compare_values(S(7).trunc(32), S(7)) == 0);
  EXPECT_TRUE(SInt::compare_values(S(-7).trunc(32), S(8)) < 0);
  EXPECT_TRUE(SInt::compare_values(S(8).trunc(32), S(-7)) > 0);
  EXPECT_TRUE(SInt::compare_values(S(-7).trunc(32), S(-7)) == 0);
  EXPECT_TRUE(SInt::compare_values(S(-7).trunc(32), S(-8)) > 0);
  EXPECT_TRUE(SInt::compare_values(S(-8).trunc(32), S(-7)) < 0);
  EXPECT_TRUE(SInt::compare_values(S(-7).trunc(32), S(-7)) == 0);
  EXPECT_TRUE(SInt::compare_values(S(7), S(8).trunc(32)) < 0);
  EXPECT_TRUE(SInt::compare_values(S(8), S(7).trunc(32)) > 0);
  EXPECT_TRUE(SInt::compare_values(S(7), S(7).trunc(32)) == 0);
  EXPECT_TRUE(SInt::compare_values(S(-7), S(8).trunc(32)) < 0);
  EXPECT_TRUE(SInt::compare_values(S(8), S(-7).trunc(32)) > 0);
  EXPECT_TRUE(SInt::compare_values(S(-7), S(-7).trunc(32)) == 0);
  EXPECT_TRUE(SInt::compare_values(S(-7), S(-8).trunc(32)) > 0);
  EXPECT_TRUE(SInt::compare_values(S(-8), S(-7).trunc(32)) < 0);
  EXPECT_TRUE(SInt::compare_values(S(-7), S(-7).trunc(32)) == 0);

  // Bit-width mismatch and not is-signed.
  EXPECT_TRUE(SInt::compare_values(U(7), U(8).trunc(32)) < 0);
  EXPECT_TRUE(SInt::compare_values(U(8), U(7).trunc(32)) > 0);
  EXPECT_TRUE(SInt::compare_values(U(7), U(7).trunc(32)) == 0);
  EXPECT_TRUE(SInt::compare_values(U(7).trunc(32), U(8)) < 0);
  EXPECT_TRUE(SInt::compare_values(U(8).trunc(32), U(7)) > 0);
  EXPECT_TRUE(SInt::compare_values(U(7).trunc(32), U(7)) == 0);

  // Bit-width mismatch and mixed signs.
  EXPECT_TRUE(SInt::compare_values(U(7).trunc(32), S(8)) < 0);
  EXPECT_TRUE(SInt::compare_values(U(8).trunc(32), S(7)) > 0);
  EXPECT_TRUE(SInt::compare_values(U(7).trunc(32), S(7)) == 0);
  EXPECT_TRUE(SInt::compare_values(U(8).trunc(32), S(-7)) > 0);
  EXPECT_TRUE(SInt::compare_values(U(7), S(8).trunc(32)) < 0);
  EXPECT_TRUE(SInt::compare_values(U(8), S(7).trunc(32)) > 0);
  EXPECT_TRUE(SInt::compare_values(U(7), S(7).trunc(32)) == 0);
  EXPECT_TRUE(SInt::compare_values(U(8), S(-7).trunc(32)) > 0);
}

TEST(SIntTest, FromString) {
  EXPECT_EQ(SInt("1").ext_value(), 1);
  std::cout << SInt("-1") << "\n";
  EXPECT_EQ(SInt("-1").ext_value(), -1);
  EXPECT_EQ(SInt("0").ext_value(), 0);
  EXPECT_EQ(SInt("56789").ext_value(), 56789);
  EXPECT_EQ(SInt("-1234").ext_value(), -1234);
}

TEST(SIntTest, FromStringBitWidth) {
  EXPECT_EQ(SInt("0").bits(), 1U);
  EXPECT_EQ(SInt("000").bits(), 1U);
  EXPECT_EQ(SInt("1").bits(), 1U);
  EXPECT_EQ(SInt("2").bits(), 2U);
  EXPECT_EQ(SInt("3").bits(), 2U);
  EXPECT_EQ(SInt("003").bits(), 2U);
  EXPECT_EQ(SInt("15").bits(), 4U);
  EXPECT_EQ(SInt("16").bits(), 5U);
  EXPECT_EQ(SInt("17").bits(), 5U);

  EXPECT_EQ(SInt("-0").bits(), 1U);
  EXPECT_EQ(SInt("-000").bits(), 1U);
  EXPECT_EQ(SInt("-1").bits(), 1U);
  EXPECT_EQ(SInt("-2").bits(), 2U);
  EXPECT_EQ(SInt("-3").bits(), 3U);
  EXPECT_EQ(SInt("-003").bits(), 3U);
  EXPECT_EQ(SInt("-5").bits(), 4U);
  EXPECT_EQ(SInt("-15").bits(), 5U);
  EXPECT_EQ(SInt("-16").bits(), 5U);
  EXPECT_EQ(SInt("-17").bits(), 6U);
}

TEST(FloatTest, is_signaling) {
  Int payload = Int::one_bit_set(4, 2);
  EXPECT_FALSE(
      Float::qNaN(details::FloatBase::IEEEsingle(), false).is_signaling());
  EXPECT_FALSE(
      Float::qNaN(details::FloatBase::IEEEsingle(), true).is_signaling());
  EXPECT_FALSE(Float::qNaN(details::FloatBase::IEEEsingle(), false, &payload)
                   .is_signaling());
  EXPECT_FALSE(Float::qNaN(details::FloatBase::IEEEsingle(), true, &payload)
                   .is_signaling());
  EXPECT_TRUE(
      Float::sNaN(details::FloatBase::IEEEsingle(), false).is_signaling());
  EXPECT_TRUE(
      Float::sNaN(details::FloatBase::IEEEsingle(), true).is_signaling());
  EXPECT_TRUE(Float::sNaN(details::FloatBase::IEEEsingle(), false, &payload)
                  .is_signaling());
  EXPECT_TRUE(Float::sNaN(details::FloatBase::IEEEsingle(), true, &payload)
                  .is_signaling());
}

TEST(FloatTest, next) {

  Float test(details::FloatBase::IEEEquad(),
             details::UninitializedTag::kUninitialized);
  Float expected(details::FloatBase::IEEEquad(),
                 details::UninitializedTag::kUninitialized);

  // 1. Test Special Cases Values.
  //
  // Test all special values for nextUp and nextDown perscribed by IEEE-754R
  // 2008. These are:
  //   1.  +inf
  //   2.  -inf
  //   3.  getLargest()
  //   4.  -getLargest()
  //   5.  smallest()
  //   6.  -smallest()
  //   7.  qNaN
  //   8.  sNaN
  //   9.  +0
  //   10. -0

  // nextUp(+inf) = +inf.
  test = Float::inf(details::FloatBase::IEEEquad(), false);
  expected = Float::inf(details::FloatBase::IEEEquad(), false);

  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_infinity());
  EXPECT_TRUE(!test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(+inf) = -nextUp(-inf) = -(-getLargest()) = getLargest()
  test = Float::inf(details::FloatBase::IEEEquad(), false);
  expected = Float::largest(details::FloatBase::IEEEquad(), false);
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(!test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(-inf) = -getLargest()
  test = Float::inf(details::FloatBase::IEEEquad(), true);
  expected = Float::largest(details::FloatBase::IEEEquad(), true);
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(-inf) = -nextUp(+inf) = -(+inf) = -inf.
  test = Float::inf(details::FloatBase::IEEEquad(), true);
  expected = Float::inf(details::FloatBase::IEEEquad(), true);
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_infinity() && test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(getLargest()) = +inf
  test = Float::largest(details::FloatBase::IEEEquad(), false);
  expected = Float::inf(details::FloatBase::IEEEquad(), false);
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_infinity() && !test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(getLargest()) = -nextUp(-getLargest())
  //                        = -(-getLargest() + inc)
  //                        = getLargest() - inc.
  test = Float::largest(details::FloatBase::IEEEquad(), false);
  expected = Float(details::FloatBase::IEEEquad(),
                   "0x1.fffffffffffffffffffffffffffep+16383");
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(!test.is_infinity() && !test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(-getLargest()) = -getLargest() + inc.
  test = Float::largest(details::FloatBase::IEEEquad(), true);
  expected = Float(details::FloatBase::IEEEquad(),
                   "-0x1.fffffffffffffffffffffffffffep+16383");
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(-getLargest()) = -nextUp(getLargest()) = -(inf) = -inf.
  test = Float::largest(details::FloatBase::IEEEquad(), true);
  expected = Float::inf(details::FloatBase::IEEEquad(), true);
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_infinity() && test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(smallest()) = smallest() + inc.
  test = Float(details::FloatBase::IEEEquad(),
               "0x0.0000000000000000000000000001p-16382");
  expected = Float(details::FloatBase::IEEEquad(),
                   "0x0.0000000000000000000000000002p-16382");
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(smallest()) = -nextUp(-smallest()) = -(-0) = +0.
  test = Float(details::FloatBase::IEEEquad(),
               "0x0.0000000000000000000000000001p-16382");
  expected = Float::zero(details::FloatBase::IEEEquad(), false);
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_pos_zero());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(-smallest()) = -0.
  test = Float(details::FloatBase::IEEEquad(),
               "-0x0.0000000000000000000000000001p-16382");
  expected = Float::zero(details::FloatBase::IEEEquad(), true);
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_neg_zero());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(-smallest()) = -nextUp(smallest()) = -smallest() - inc.
  test = Float(details::FloatBase::IEEEquad(),
               "-0x0.0000000000000000000000000001p-16382");
  expected = Float(details::FloatBase::IEEEquad(),
                   "-0x0.0000000000000000000000000002p-16382");
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(qNaN) = qNaN
  test = Float::qNaN(details::FloatBase::IEEEquad(), false);
  expected = Float::qNaN(details::FloatBase::IEEEquad(), false);
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(qNaN) = qNaN
  test = Float::qNaN(details::FloatBase::IEEEquad(), false);
  expected = Float::qNaN(details::FloatBase::IEEEquad(), false);
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(sNaN) = qNaN
  test = Float::sNaN(details::FloatBase::IEEEquad(), false);
  expected = Float::qNaN(details::FloatBase::IEEEquad(), false);
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kInvalidOp);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(sNaN) = qNaN
  test = Float::sNaN(details::FloatBase::IEEEquad(), false);
  expected = Float::qNaN(details::FloatBase::IEEEquad(), false);
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kInvalidOp);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(+0) = +smallest()
  test = Float::zero(details::FloatBase::IEEEquad(), false);
  expected = Float::smallest(details::FloatBase::IEEEquad(), false);
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(+0) = -nextUp(-0) = -smallest()
  test = Float::zero(details::FloatBase::IEEEquad(), false);
  expected = Float::smallest(details::FloatBase::IEEEquad(), true);
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(-0) = +smallest()
  test = Float::zero(details::FloatBase::IEEEquad(), true);
  expected = Float::smallest(details::FloatBase::IEEEquad(), false);
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(-0) = -nextUp(0) = -smallest()
  test = Float::zero(details::FloatBase::IEEEquad(), true);
  expected = Float::smallest(details::FloatBase::IEEEquad(), true);
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // 2. Binade Boundary Tests.

  // 2a. Test denormal <-> normal binade boundaries.
  //     * nextUp(+Largest Denormal) -> +Smallest Normal.
  //     * nextDown(-Largest Denormal) -> -Smallest Normal.
  //     * nextUp(-Smallest Normal) -> -Largest Denormal.
  //     * nextDown(+Smallest Normal) -> +Largest Denormal.

  // nextUp(+Largest Denormal) -> +Smallest Normal.
  test = Float(details::FloatBase::IEEEquad(),
               "0x0.ffffffffffffffffffffffffffffp-16382");
  expected = Float(details::FloatBase::IEEEquad(),
                   "0x1.0000000000000000000000000000p-16382");
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_FALSE(test.is_denormal());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(-Largest Denormal) -> -Smallest Normal.
  test = Float(details::FloatBase::IEEEquad(),
               "-0x0.ffffffffffffffffffffffffffffp-16382");
  expected = Float(details::FloatBase::IEEEquad(),
                   "-0x1.0000000000000000000000000000p-16382");
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_FALSE(test.is_denormal());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(-Smallest Normal) -> -LargestDenormal.
  test = Float(details::FloatBase::IEEEquad(),
               "-0x1.0000000000000000000000000000p-16382");
  expected = Float(details::FloatBase::IEEEquad(),
                   "-0x0.ffffffffffffffffffffffffffffp-16382");
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_denormal());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(+Smallest Normal) -> +Largest Denormal.
  test = Float(details::FloatBase::IEEEquad(),
               "+0x1.0000000000000000000000000000p-16382");
  expected = Float(details::FloatBase::IEEEquad(),
                   "+0x0.ffffffffffffffffffffffffffffp-16382");
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_denormal());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // 2b. Test normal <-> normal binade boundaries.
  //     * nextUp(-Normal Binade Boundary) -> -Normal Binade Boundary + 1.
  //     * nextDown(+Normal Binade Boundary) -> +Normal Binade Boundary - 1.
  //     * nextUp(+Normal Binade Boundary - 1) -> +Normal Binade Boundary.
  //     * nextDown(-Normal Binade Boundary + 1) -> -Normal Binade Boundary.

  // nextUp(-Normal Binade Boundary) -> -Normal Binade Boundary + 1.
  test = Float(details::FloatBase::IEEEquad(), "-0x1p+1");
  expected = Float(details::FloatBase::IEEEquad(),
                   "-0x1.ffffffffffffffffffffffffffffp+0");
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(+Normal Binade Boundary) -> +Normal Binade Boundary - 1.
  test = Float(details::FloatBase::IEEEquad(), "0x1p+1");
  expected = Float(details::FloatBase::IEEEquad(),
                   "0x1.ffffffffffffffffffffffffffffp+0");
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(+Normal Binade Boundary - 1) -> +Normal Binade Boundary.
  test = Float(details::FloatBase::IEEEquad(),
               "0x1.ffffffffffffffffffffffffffffp+0");
  expected = Float(details::FloatBase::IEEEquad(), "0x1p+1");
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(-Normal Binade Boundary + 1) -> -Normal Binade Boundary.
  test = Float(details::FloatBase::IEEEquad(),
               "-0x1.ffffffffffffffffffffffffffffp+0");
  expected = Float(details::FloatBase::IEEEquad(), "-0x1p+1");
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // 2c. Test using next at binade boundaries with a direction away from the
  // binade boundary. Away from denormal <-> normal boundaries.
  //
  // This is to make sure that even though we are at a binade boundary, since
  // we are rounding away, we do not trigger the binade boundary code. Thus we
  // test:
  //   * nextUp(-Largest Denormal) -> -Largest Denormal + inc.
  //   * nextDown(+Largest Denormal) -> +Largest Denormal - inc.
  //   * nextUp(+Smallest Normal) -> +Smallest Normal + inc.
  //   * nextDown(-Smallest Normal) -> -Smallest Normal - inc.

  // nextUp(-Largest Denormal) -> -Largest Denormal + inc.
  test = Float(details::FloatBase::IEEEquad(),
               "-0x0.ffffffffffffffffffffffffffffp-16382");
  expected = Float(details::FloatBase::IEEEquad(),
                   "-0x0.fffffffffffffffffffffffffffep-16382");
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_denormal());
  EXPECT_TRUE(test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(+Largest Denormal) -> +Largest Denormal - inc.
  test = Float(details::FloatBase::IEEEquad(),
               "0x0.ffffffffffffffffffffffffffffp-16382");
  expected = Float(details::FloatBase::IEEEquad(),
                   "0x0.fffffffffffffffffffffffffffep-16382");
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_denormal());
  EXPECT_TRUE(!test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(+Smallest Normal) -> +Smallest Normal + inc.
  test = Float(details::FloatBase::IEEEquad(),
               "0x1.0000000000000000000000000000p-16382");
  expected = Float(details::FloatBase::IEEEquad(),
                   "0x1.0000000000000000000000000001p-16382");
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(!test.is_denormal());
  EXPECT_TRUE(!test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(-Smallest Normal) -> -Smallest Normal - inc.
  test = Float(details::FloatBase::IEEEquad(),
               "-0x1.0000000000000000000000000000p-16382");
  expected = Float(details::FloatBase::IEEEquad(),
                   "-0x1.0000000000000000000000000001p-16382");
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(!test.is_denormal());
  EXPECT_TRUE(test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // 2d. Test values which cause our exponent to go to min exponent. This
  // is to ensure that guards in the code to check for min exponent
  // trigger properly.
  //     * nextUp(-0x1p-16381) -> -0x1.ffffffffffffffffffffffffffffp-16382
  //     * nextDown(-0x1.ffffffffffffffffffffffffffffp-16382) ->
  //         -0x1p-16381
  //     * nextUp(0x1.ffffffffffffffffffffffffffffp-16382) -> 0x1p-16382
  //     * nextDown(0x1p-16382) -> 0x1.ffffffffffffffffffffffffffffp-16382

  // nextUp(-0x1p-16381) -> -0x1.ffffffffffffffffffffffffffffp-16382
  test = Float(details::FloatBase::IEEEquad(), "-0x1p-16381");
  expected = Float(details::FloatBase::IEEEquad(),
                   "-0x1.ffffffffffffffffffffffffffffp-16382");
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(-0x1.ffffffffffffffffffffffffffffp-16382) ->
  //         -0x1p-16381
  test = Float(details::FloatBase::IEEEquad(),
               "-0x1.ffffffffffffffffffffffffffffp-16382");
  expected = Float(details::FloatBase::IEEEquad(), "-0x1p-16381");
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(0x1.ffffffffffffffffffffffffffffp-16382) -> 0x1p-16381
  test = Float(details::FloatBase::IEEEquad(),
               "0x1.ffffffffffffffffffffffffffffp-16382");
  expected = Float(details::FloatBase::IEEEquad(), "0x1p-16381");
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(0x1p-16381) -> 0x1.ffffffffffffffffffffffffffffp-16382
  test = Float(details::FloatBase::IEEEquad(), "0x1p-16381");
  expected = Float(details::FloatBase::IEEEquad(),
                   "0x1.ffffffffffffffffffffffffffffp-16382");
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.bitwise_equal(expected));

  // 3. Now we test both denormal/normal computation which will not cause us
  // to go across binade boundaries. Specifically we test:
  //   * nextUp(+Denormal) -> +Denormal.
  //   * nextDown(+Denormal) -> +Denormal.
  //   * nextUp(-Denormal) -> -Denormal.
  //   * nextDown(-Denormal) -> -Denormal.
  //   * nextUp(+Normal) -> +Normal.
  //   * nextDown(+Normal) -> +Normal.
  //   * nextUp(-Normal) -> -Normal.
  //   * nextDown(-Normal) -> -Normal.

  // nextUp(+Denormal) -> +Denormal.
  test = Float(details::FloatBase::IEEEquad(),
               "0x0.ffffffffffffffffffffffff000cp-16382");
  expected = Float(details::FloatBase::IEEEquad(),
                   "0x0.ffffffffffffffffffffffff000dp-16382");
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_denormal());
  EXPECT_TRUE(!test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(+Denormal) -> +Denormal.
  test = Float(details::FloatBase::IEEEquad(),
               "0x0.ffffffffffffffffffffffff000cp-16382");
  expected = Float(details::FloatBase::IEEEquad(),
                   "0x0.ffffffffffffffffffffffff000bp-16382");
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_denormal());
  EXPECT_TRUE(!test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(-Denormal) -> -Denormal.
  test = Float(details::FloatBase::IEEEquad(),
               "-0x0.ffffffffffffffffffffffff000cp-16382");
  expected = Float(details::FloatBase::IEEEquad(),
                   "-0x0.ffffffffffffffffffffffff000bp-16382");
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_denormal());
  EXPECT_TRUE(test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(-Denormal) -> -Denormal
  test = Float(details::FloatBase::IEEEquad(),
               "-0x0.ffffffffffffffffffffffff000cp-16382");
  expected = Float(details::FloatBase::IEEEquad(),
                   "-0x0.ffffffffffffffffffffffff000dp-16382");
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(test.is_denormal());
  EXPECT_TRUE(test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(+Normal) -> +Normal.
  test = Float(details::FloatBase::IEEEquad(),
               "0x1.ffffffffffffffffffffffff000cp-16000");
  expected = Float(details::FloatBase::IEEEquad(),
                   "0x1.ffffffffffffffffffffffff000dp-16000");
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(!test.is_denormal());
  EXPECT_TRUE(!test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(+Normal) -> +Normal.
  test = Float(details::FloatBase::IEEEquad(),
               "0x1.ffffffffffffffffffffffff000cp-16000");
  expected = Float(details::FloatBase::IEEEquad(),
                   "0x1.ffffffffffffffffffffffff000bp-16000");
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(!test.is_denormal());
  EXPECT_TRUE(!test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextUp(-Normal) -> -Normal.
  test = Float(details::FloatBase::IEEEquad(),
               "-0x1.ffffffffffffffffffffffff000cp-16000");
  expected = Float(details::FloatBase::IEEEquad(),
                   "-0x1.ffffffffffffffffffffffff000bp-16000");
  EXPECT_EQ(test.next(false), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(!test.is_denormal());
  EXPECT_TRUE(test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));

  // nextDown(-Normal) -> -Normal.
  test = Float(details::FloatBase::IEEEquad(),
               "-0x1.ffffffffffffffffffffffff000cp-16000");
  expected = Float(details::FloatBase::IEEEquad(),
                   "-0x1.ffffffffffffffffffffffff000dp-16000");
  EXPECT_EQ(test.next(true), details::FloatBase::OpStatus::kOK);
  EXPECT_TRUE(!test.is_denormal());
  EXPECT_TRUE(test.is_negative());
  EXPECT_TRUE(test.bitwise_equal(expected));
}

TEST(FloatTest, FMA) {
  details::FloatBase::RoundingMode rdmd =
      details::FloatBase::RoundingMode::kNearestTiesToEven;

  {
    Float f1(14.5F);
    Float f2(-14.5F);
    Float f3(225.0F);
    f1.fused_mul_add(f2, f3,
                     details::FloatBase::RoundingMode::kNearestTiesToEven);
    EXPECT_EQ(14.75F, f1.to_float());
  }

  {
    Float Val2(2.0f);
    Float f1((float)1.17549435e-38F);
    Float f2((float)1.17549435e-38F);
    f1.div(Val2, rdmd);
    f2.div(Val2, rdmd);
    Float f3(12.0f);
    f1.fused_mul_add(f2, f3,
                     details::FloatBase::RoundingMode::kNearestTiesToEven);
    EXPECT_EQ(12.0f, f1.to_float());
  }

  // Test for correct zero sign when answer is exactly zero.
  // fma(1.0, -1.0, 1.0) -> +ve 0.
  {
    Float f1(1.0);
    Float f2(-1.0);
    Float f3(1.0);
    f1.fused_mul_add(f2, f3,
                     details::FloatBase::RoundingMode::kNearestTiesToEven);
    EXPECT_TRUE(!f1.is_negative() && f1.is_zero());
  }

  // Test for correct zero sign when answer is exactly zero and rounding towards
  // negative.
  // fma(1.0, -1.0, 1.0) -> +ve 0.
  {
    Float f1(1.0);
    Float f2(-1.0);
    Float f3(1.0);
    f1.fused_mul_add(f2, f3, details::FloatBase::RoundingMode::kTowardNegative);
    EXPECT_TRUE(f1.is_negative() && f1.is_zero());
  }

  // Test for correct (in this case -ve) sign when adding like signed zeros.
  // Test fma(0.0, -0.0, -0.0) -> -ve 0.
  {
    Float f1(0.0);
    Float f2(-0.0);
    Float f3(-0.0);
    f1.fused_mul_add(f2, f3,
                     details::FloatBase::RoundingMode::kNearestTiesToEven);
    EXPECT_TRUE(f1.is_negative() && f1.is_zero());
  }

  // Test -ve sign preservation when small negative results underflow.
  {
    Float f1(details::FloatBase::IEEEdouble(), "-0x1p-1074");
    Float f2(details::FloatBase::IEEEdouble(), "+0x1p-1074");
    Float f3(0.0);
    f1.fused_mul_add(f2, f3,
                     details::FloatBase::RoundingMode::kNearestTiesToEven);
    EXPECT_TRUE(f1.is_negative() && f1.is_zero());
  }

  // Regression test that failed an assertion.
  {
    Float f1(-8.85242279E-41f);
    Float f2(2.0f);
    Float f3(8.85242279E-41f);
    f1.fused_mul_add(f2, f3,
                     details::FloatBase::RoundingMode::kNearestTiesToEven);
    EXPECT_EQ(-8.85242279E-41f, f1.to_float());
  }

  // Test using only a single instance of Float.
  {
    Float F(1.5);

    F.fused_mul_add(F, F, details::FloatBase::RoundingMode::kNearestTiesToEven);
    EXPECT_EQ(3.75, F.to_double());
  }
}

TEST(FloatTest, MinNum) {
  Float f1(1.0);
  Float f2(2.0);
  Float nan = Float::NaN(details::FloatBase::IEEEdouble());

  EXPECT_EQ(1.0, minnum(f1, f2).to_double());
  EXPECT_EQ(1.0, minnum(f2, f1).to_double());
  EXPECT_EQ(1.0, minnum(f1, nan).to_double());
  EXPECT_EQ(1.0, minnum(nan, f1).to_double());
}

TEST(FloatTest, MaxNum) {
  Float f1(1.0);
  Float f2(2.0);
  Float nan = Float::NaN(details::FloatBase::IEEEdouble());

  EXPECT_EQ(2.0, maxnum(f1, f2).to_double());
  EXPECT_EQ(2.0, maxnum(f2, f1).to_double());
  EXPECT_EQ(1.0, maxnum(f1, nan).to_double());
  EXPECT_EQ(1.0, maxnum(nan, f1).to_double());
}

TEST(FloatTest, Minimum) {
  Float f1(1.0);
  Float f2(2.0);
  Float zp(0.0);
  Float zn(-0.0);
  Float nan = Float::NaN(details::FloatBase::IEEEdouble());

  EXPECT_EQ(1.0, minimum(f1, f2).to_double());
  EXPECT_EQ(1.0, minimum(f2, f1).to_double());
  EXPECT_EQ(-0.0, minimum(zp, zn).to_double());
  EXPECT_EQ(-0.0, minimum(zn, zp).to_double());
  EXPECT_TRUE(std::isnan(minimum(f1, nan).to_double()));
  EXPECT_TRUE(std::isnan(minimum(nan, f1).to_double()));
}

TEST(FloatTest, Maximum) {
  Float f1(1.0);
  Float f2(2.0);
  Float zp(0.0);
  Float zn(-0.0);
  Float nan = Float::NaN(details::FloatBase::IEEEdouble());

  EXPECT_EQ(2.0, maximum(f1, f2).to_double());
  EXPECT_EQ(2.0, maximum(f2, f1).to_double());
  EXPECT_EQ(0.0, maximum(zp, zn).to_double());
  EXPECT_EQ(0.0, maximum(zn, zp).to_double());
  EXPECT_TRUE(std::isnan(maximum(f1, nan).to_double()));
  EXPECT_TRUE(std::isnan(maximum(nan, f1).to_double()));
}

TEST(FloatTest, Denormal) {
  details::FloatBase::RoundingMode rdmd =
      details::FloatBase::RoundingMode::kNearestTiesToEven;

  // Test single precision
  {
    const char* MinNormalStr = "1.17549435082228750797e-38";
    EXPECT_FALSE(
        Float(details::FloatBase::IEEEsingle(), MinNormalStr).is_denormal());
    EXPECT_FALSE(Float(details::FloatBase::IEEEsingle(), 0).is_denormal());

    Float Val2(details::FloatBase::IEEEsingle(), 2);
    Float T(details::FloatBase::IEEEsingle(), MinNormalStr);
    T.div(Val2, rdmd);
    EXPECT_TRUE(T.is_denormal());
  }

  // Test double precision
  {
    const char* MinNormalStr = "2.22507385850720138309e-308";
    EXPECT_FALSE(
        Float(details::FloatBase::IEEEdouble(), MinNormalStr).is_denormal());
    EXPECT_FALSE(Float(details::FloatBase::IEEEdouble(), 0).is_denormal());

    Float Val2(details::FloatBase::IEEEdouble(), 2);
    Float T(details::FloatBase::IEEEdouble(), MinNormalStr);
    T.div(Val2, rdmd);
    EXPECT_TRUE(T.is_denormal());
  }

  // Test quadruple precision
  {
    const char* MinNormalStr = "3.36210314311209350626267781732175260e-4932";
    EXPECT_FALSE(
        Float(details::FloatBase::IEEEquad(), MinNormalStr).is_denormal());
    EXPECT_FALSE(Float(details::FloatBase::IEEEquad(), 0).is_denormal());

    Float Val2(details::FloatBase::IEEEquad(), 2);
    Float T(details::FloatBase::IEEEquad(), MinNormalStr);
    T.div(Val2, rdmd);
    EXPECT_TRUE(T.is_denormal());
  }
}

TEST(FloatTest, IsSmallestNormalized) {
  for (unsigned I = 1;
       I !=
       static_cast<uint8_t>(details::FloatBase::Semantics::kMaxSemantics) + 1;
       ++I) {
    const auto& Semantics = details::FloatBase::Semantics2FloatSemantics(
        static_cast<details::FloatBase::Semantics>(I));

    EXPECT_FALSE(Float::zero(Semantics, false).is_smallest_normalized());
    EXPECT_FALSE(Float::zero(Semantics, true).is_smallest_normalized());

    EXPECT_FALSE(Float::inf(Semantics, false).is_smallest_normalized());
    EXPECT_FALSE(Float::inf(Semantics, true).is_smallest_normalized());

    EXPECT_FALSE(Float::qNaN(Semantics).is_smallest_normalized());
    EXPECT_FALSE(Float::sNaN(Semantics).is_smallest_normalized());

    EXPECT_FALSE(Float::largest(Semantics).is_smallest_normalized());
    EXPECT_FALSE(Float::largest(Semantics, true).is_smallest_normalized());

    EXPECT_FALSE(Float::smallest(Semantics).is_smallest_normalized());
    EXPECT_FALSE(Float::smallest(Semantics, true).is_smallest_normalized());

    EXPECT_FALSE(Float::all_ones(Semantics).is_smallest_normalized());

    Float PosSmallestNormalized = Float::smallest_normalized(Semantics, false);
    Float NegSmallestNormalized = Float::smallest_normalized(Semantics, true);
    EXPECT_TRUE(PosSmallestNormalized.is_smallest_normalized());
    EXPECT_TRUE(NegSmallestNormalized.is_smallest_normalized());

    for (Float* Val : {&PosSmallestNormalized, &NegSmallestNormalized}) {
      bool OldSign = Val->is_negative();

      // Step down, make sure it's still not smallest normalized.
      EXPECT_EQ(details::FloatBase::OpStatus::kOK, Val->next(false));
      EXPECT_EQ(OldSign, Val->is_negative());
      EXPECT_FALSE(Val->is_smallest_normalized());
      EXPECT_EQ(OldSign, Val->is_negative());

      // Step back up should restore it to being smallest normalized.
      EXPECT_EQ(details::FloatBase::OpStatus::kOK, Val->next(true));
      EXPECT_TRUE(Val->is_smallest_normalized());
      EXPECT_EQ(OldSign, Val->is_negative());

      // Step beyond should no longer smallest normalized.
      EXPECT_EQ(details::FloatBase::OpStatus::kOK, Val->next(true));
      EXPECT_FALSE(Val->is_smallest_normalized());
      EXPECT_EQ(OldSign, Val->is_negative());
    }
  }
}

TEST(FloatTest, Zero) {
  EXPECT_EQ(0.0f, Float(0.0f).to_float());
  EXPECT_EQ(-0.0f, Float(-0.0f).to_float());
  EXPECT_TRUE(Float(-0.0f).is_negative());

  EXPECT_EQ(0.0, Float(0.0).to_double());
  EXPECT_EQ(-0.0, Float(-0.0).to_double());
  EXPECT_TRUE(Float(-0.0).is_negative());
}

static double to_double_from_string(lps::basic::StringRef Str) {
  Float f(0.0);
  auto status_or_none =
      f.form(Str, details::FloatBase::RoundingMode::kNearestTiesToEven);
  EXPECT_FALSE(!status_or_none);
  return f.to_double();
}

TEST(FloatTest, DecimalStringsWithoutNullTerminators) {

  EXPECT_EQ(to_double_from_string(lps::basic::StringRef("0.00", 3)), 0.0);
  EXPECT_EQ(to_double_from_string(lps::basic::StringRef("0.01", 3)), 0.0);
  EXPECT_EQ(to_double_from_string(lps::basic::StringRef("0.09", 3)), 0.0);
  EXPECT_EQ(to_double_from_string(lps::basic::StringRef("0.095", 4)), 0.09);
  EXPECT_EQ(to_double_from_string(lps::basic::StringRef("0.00e+3", 7)), 0.00);
  EXPECT_EQ(to_double_from_string(lps::basic::StringRef("0e+3", 4)), 0.00);
}

TEST(FloatTest, fromZeroDecimalString) {
  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0").to_double());
  EXPECT_EQ(+0.0, Float(details::FloatBase::IEEEdouble(), "+0").to_double());
  EXPECT_EQ(-0.0, Float(details::FloatBase::IEEEdouble(), "-0").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0.").to_double());
  EXPECT_EQ(+0.0, Float(details::FloatBase::IEEEdouble(), "+0.").to_double());
  EXPECT_EQ(-0.0, Float(details::FloatBase::IEEEdouble(), "-0.").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), ".0").to_double());
  EXPECT_EQ(+0.0, Float(details::FloatBase::IEEEdouble(), "+.0").to_double());
  EXPECT_EQ(-0.0, Float(details::FloatBase::IEEEdouble(), "-.0").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0.0").to_double());
  EXPECT_EQ(+0.0, Float(details::FloatBase::IEEEdouble(), "+0.0").to_double());
  EXPECT_EQ(-0.0, Float(details::FloatBase::IEEEdouble(), "-0.0").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "00000.").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+00000.").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-00000.").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), ".00000").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+.00000").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-.00000").to_double());

  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "0000.00000").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0000.00000").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0000.00000").to_double());
}

TEST(FloatTest, fromZeroDecimalSingleExponentString) {
  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0e1").to_double());
  EXPECT_EQ(+0.0, Float(details::FloatBase::IEEEdouble(), "+0e1").to_double());
  EXPECT_EQ(-0.0, Float(details::FloatBase::IEEEdouble(), "-0e1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0e+1").to_double());
  EXPECT_EQ(+0.0, Float(details::FloatBase::IEEEdouble(), "+0e+1").to_double());
  EXPECT_EQ(-0.0, Float(details::FloatBase::IEEEdouble(), "-0e+1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0e-1").to_double());
  EXPECT_EQ(+0.0, Float(details::FloatBase::IEEEdouble(), "+0e-1").to_double());
  EXPECT_EQ(-0.0, Float(details::FloatBase::IEEEdouble(), "-0e-1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0.e1").to_double());
  EXPECT_EQ(+0.0, Float(details::FloatBase::IEEEdouble(), "+0.e1").to_double());
  EXPECT_EQ(-0.0, Float(details::FloatBase::IEEEdouble(), "-0.e1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0.e+1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0.e+1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0.e+1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0.e-1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0.e-1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0.e-1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), ".0e1").to_double());
  EXPECT_EQ(+0.0, Float(details::FloatBase::IEEEdouble(), "+.0e1").to_double());
  EXPECT_EQ(-0.0, Float(details::FloatBase::IEEEdouble(), "-.0e1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), ".0e+1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+.0e+1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-.0e+1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), ".0e-1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+.0e-1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-.0e-1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0.0e1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0.0e1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0.0e1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0.0e+1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0.0e+1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0.0e+1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0.0e-1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0.0e-1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0.0e-1").to_double());

  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "000.0000e1").to_double());
  EXPECT_EQ(
      +0.0,
      Float(details::FloatBase::IEEEdouble(), "+000.0000e+1").to_double());
  EXPECT_EQ(
      -0.0,
      Float(details::FloatBase::IEEEdouble(), "-000.0000e+1").to_double());
}

TEST(FloatTest, fromZeroDecimalLargeExponentString) {
  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0e1234").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0e1234").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0e1234").to_double());

  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "0e+1234").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0e+1234").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0e+1234").to_double());

  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "0e-1234").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0e-1234").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0e-1234").to_double());

  EXPECT_EQ(
      0.0,
      Float(details::FloatBase::IEEEdouble(), "000.0000e1234").to_double());
  EXPECT_EQ(
      0.0,
      Float(details::FloatBase::IEEEdouble(), "000.0000e-1234").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(),
                       lps::basic::StringRef("0e1234"
                                             "\0"
                                             "2",
                                             6))
                     .to_double());
}

TEST(FloatTest, fromZeroHexadecimalString) {
  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0x0p1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0x0p1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0x0p1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0x0p+1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0x0p+1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0x0p+1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0x0p-1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0x0p-1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0x0p-1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0x0.p1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0x0.p1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0x0.p1").to_double());

  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "0x0.p+1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0x0.p+1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0x0.p+1").to_double());

  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "0x0.p-1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0x0.p-1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0x0.p-1").to_double());

  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0x.0p1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0x.0p1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0x.0p1").to_double());

  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "0x.0p+1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0x.0p+1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0x.0p+1").to_double());

  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "0x.0p-1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0x.0p-1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0x.0p-1").to_double());

  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "0x0.0p1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0x0.0p1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0x0.0p1").to_double());

  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "0x0.0p+1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0x0.0p+1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0x0.0p+1").to_double());

  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "0x0.0p-1").to_double());
  EXPECT_EQ(+0.0,
            Float(details::FloatBase::IEEEdouble(), "+0x0.0p-1").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0x0.0p-1").to_double());

  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "0x00000.p1").to_double());
  EXPECT_EQ(
      0.0,
      Float(details::FloatBase::IEEEdouble(), "0x0000.00000p1").to_double());
  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "0x.00000p1").to_double());
  EXPECT_EQ(0.0, Float(details::FloatBase::IEEEdouble(), "0x0.p1").to_double());
  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "0x0p1234").to_double());
  EXPECT_EQ(-0.0,
            Float(details::FloatBase::IEEEdouble(), "-0x0p1234").to_double());
  EXPECT_EQ(
      0.0,
      Float(details::FloatBase::IEEEdouble(), "0x00000.p1234").to_double());
  EXPECT_EQ(
      0.0,
      Float(details::FloatBase::IEEEdouble(), "0x0000.00000p1234").to_double());
  EXPECT_EQ(
      0.0,
      Float(details::FloatBase::IEEEdouble(), "0x.00000p1234").to_double());
  EXPECT_EQ(0.0,
            Float(details::FloatBase::IEEEdouble(), "0x0.p1234").to_double());
}

TEST(FloatTest, fromDecimalString) {
  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "1").to_double());
  EXPECT_EQ(2.0, Float(details::FloatBase::IEEEdouble(), "2.").to_double());
  EXPECT_EQ(0.5, Float(details::FloatBase::IEEEdouble(), ".5").to_double());
  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "1.0").to_double());
  EXPECT_EQ(-2.0, Float(details::FloatBase::IEEEdouble(), "-2").to_double());
  EXPECT_EQ(-4.0, Float(details::FloatBase::IEEEdouble(), "-4.").to_double());
  EXPECT_EQ(-0.5, Float(details::FloatBase::IEEEdouble(), "-.5").to_double());
  EXPECT_EQ(-1.5, Float(details::FloatBase::IEEEdouble(), "-1.5").to_double());
  EXPECT_EQ(1.25e12,
            Float(details::FloatBase::IEEEdouble(), "1.25e12").to_double());
  EXPECT_EQ(1.25e+12,
            Float(details::FloatBase::IEEEdouble(), "1.25e+12").to_double());
  EXPECT_EQ(1.25e-12,
            Float(details::FloatBase::IEEEdouble(), "1.25e-12").to_double());
  EXPECT_EQ(1024.0,
            Float(details::FloatBase::IEEEdouble(), "1024.").to_double());
  EXPECT_EQ(1024.05,
            Float(details::FloatBase::IEEEdouble(), "1024.05000").to_double());
  EXPECT_EQ(0.05,
            Float(details::FloatBase::IEEEdouble(), ".05000").to_double());
  EXPECT_EQ(2.0, Float(details::FloatBase::IEEEdouble(), "2.").to_double());
  EXPECT_EQ(2.0e2, Float(details::FloatBase::IEEEdouble(), "2.e2").to_double());
  EXPECT_EQ(2.0e+2,
            Float(details::FloatBase::IEEEdouble(), "2.e+2").to_double());
  EXPECT_EQ(2.0e-2,
            Float(details::FloatBase::IEEEdouble(), "2.e-2").to_double());
  EXPECT_EQ(2.05e2,
            Float(details::FloatBase::IEEEdouble(), "002.05000e2").to_double());
  EXPECT_EQ(
      2.05e+2,
      Float(details::FloatBase::IEEEdouble(), "002.05000e+2").to_double());
  EXPECT_EQ(
      2.05e-2,
      Float(details::FloatBase::IEEEdouble(), "002.05000e-2").to_double());
  EXPECT_EQ(
      2.05e12,
      Float(details::FloatBase::IEEEdouble(), "002.05000e12").to_double());
  EXPECT_EQ(
      2.05e+12,
      Float(details::FloatBase::IEEEdouble(), "002.05000e+12").to_double());
  EXPECT_EQ(
      2.05e-12,
      Float(details::FloatBase::IEEEdouble(), "002.05000e-12").to_double());

  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "1e").to_double());
  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "+1e").to_double());
  EXPECT_EQ(-1.0, Float(details::FloatBase::IEEEdouble(), "-1e").to_double());

  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "1.e").to_double());
  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "+1.e").to_double());
  EXPECT_EQ(-1.0, Float(details::FloatBase::IEEEdouble(), "-1.e").to_double());

  EXPECT_EQ(0.1, Float(details::FloatBase::IEEEdouble(), ".1e").to_double());
  EXPECT_EQ(0.1, Float(details::FloatBase::IEEEdouble(), "+.1e").to_double());
  EXPECT_EQ(-0.1, Float(details::FloatBase::IEEEdouble(), "-.1e").to_double());

  EXPECT_EQ(1.1, Float(details::FloatBase::IEEEdouble(), "1.1e").to_double());
  EXPECT_EQ(1.1, Float(details::FloatBase::IEEEdouble(), "+1.1e").to_double());
  EXPECT_EQ(-1.1, Float(details::FloatBase::IEEEdouble(), "-1.1e").to_double());

  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "1e+").to_double());
  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "1e-").to_double());

  EXPECT_EQ(0.1, Float(details::FloatBase::IEEEdouble(), ".1e").to_double());
  EXPECT_EQ(0.1, Float(details::FloatBase::IEEEdouble(), ".1e+").to_double());
  EXPECT_EQ(0.1, Float(details::FloatBase::IEEEdouble(), ".1e-").to_double());

  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "1.0e").to_double());
  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "1.0e+").to_double());
  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "1.0e-").to_double());

  // These are "carefully selected" to overflow the fast log-base
  // calculations in Float.cpp
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEdouble(), "99e99999").is_infinity());
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEdouble(), "-99e99999").is_infinity());
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEdouble(), "1e-99999").is_pos_zero());
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEdouble(), "-1e-99999").is_neg_zero());

  EXPECT_EQ(2.71828, to_double_from_string("2.71828"));
}

TEST(FloatTest, fromStringSpecials) {
  const details::FloatSemantics& Sem = details::FloatBase::IEEEdouble();
  const unsigned Precision = 53;
  const unsigned PayloadBits = Precision - 2;
  uint64_t PayloadMask = (uint64_t(1) << PayloadBits) - uint64_t(1);

  uint64_t NaNPayloads[] = {
      0,
      1,
      123,
      0xDEADBEEF,
      uint64_t(-2),
      uint64_t(1) << PayloadBits,        // overflow bit
      uint64_t(1) << (PayloadBits - 1),  // signaling bit
      uint64_t(1) << (PayloadBits - 2)   // highest possible bit
  };

  // Convert payload integer to decimal string representation.
  std::string NaNPayloadDecStrings[std::size(NaNPayloads)];
  for (size_t I = 0; I < std::size(NaNPayloads); ++I)
    NaNPayloadDecStrings[I] = lps::basic::str::utostr(NaNPayloads[I]);

  // Convert payload integer to hexadecimal string representation.
  std::string NaNPayloadHexStrings[std::size(NaNPayloads)];
  for (size_t I = 0; I < std::size(NaNPayloads); ++I)
    NaNPayloadHexStrings[I] = "0x" + lps::basic::str::utohexstr(NaNPayloads[I]);

  // Fix payloads to expected result.
  for (uint64_t& Payload : NaNPayloads)
    Payload &= PayloadMask;

  // Signaling NaN must have a non-zero payload. In case a zero payload is
  // requested, a default arbitrary payload is set instead. Save this payload
  // for testing.
  const uint64_t SNaNDefaultPayload =
      Float::sNaN(Sem).bit_cast_to_int().zero_ext_value() & PayloadMask;

  // Negative sign prefix (or none - for positive).
  const char Signs[] = {0, '-'};

  // "Signaling" prefix (or none - for "Quiet").
  const char NaNTypes[] = {0, 's', 'S'};

  const lps::basic::StringRef NaNStrings[] = {"nan", "NaN"};
  for (lps::basic::StringRef NaNStr : NaNStrings)
    for (char TypeChar : NaNTypes) {
      bool Signaling = (TypeChar == 's' || TypeChar == 'S');

      for (size_t J = 0; J < std::size(NaNPayloads); ++J) {
        uint64_t Payload = (Signaling && !NaNPayloads[J]) ? SNaNDefaultPayload
                                                          : NaNPayloads[J];
        std::string& PayloadDec = NaNPayloadDecStrings[J];
        std::string& PayloadHex = NaNPayloadHexStrings[J];

        for (char SignChar : Signs) {
          bool Negative = (SignChar == '-');

          std::string TestStrings[5];
          size_t NumTestStrings = 0;

          std::string Prefix;
          if (SignChar)
            Prefix += SignChar;
          if (TypeChar)
            Prefix += TypeChar;
          Prefix += NaNStr;

          // Test without any paylod.
          if (!Payload)
            TestStrings[NumTestStrings++] = Prefix;

          // Test with the payload as a suffix.
          TestStrings[NumTestStrings++] = Prefix + PayloadDec;
          TestStrings[NumTestStrings++] = Prefix + PayloadHex;

          // Test with the payload inside parentheses.
          TestStrings[NumTestStrings++] = Prefix + '(' + PayloadDec + ')';
          TestStrings[NumTestStrings++] = Prefix + '(' + PayloadHex + ')';

          for (size_t K = 0; K < NumTestStrings; ++K) {
            lps::basic::StringRef TestStr(TestStrings[K]);

            Float F(Sem);
            bool HasError = !F.form(
                TestStr, details::FloatBase::RoundingMode::kNearestTiesToEven);
            EXPECT_FALSE(HasError);
            EXPECT_TRUE(F.isNaN());
            EXPECT_EQ(Signaling, F.is_signaling());
            EXPECT_EQ(Negative, F.is_negative());
            uint64_t PayloadResult =
                F.bit_cast_to_int().zero_ext_value() & PayloadMask;
            EXPECT_EQ(Payload, PayloadResult);
          }
        }
      }
    }

  const lps::basic::StringRef InfStrings[] = {"inf",  "INFINITY",  "+Inf",
                                              "-inf", "-INFINITY", "-Inf"};
  for (lps::basic::StringRef InfStr : InfStrings) {
    bool Negative = InfStr.front() == '-';

    Float F(Sem);
    bool HasError =
        !F.form(InfStr, details::FloatBase::RoundingMode::kNearestTiesToEven);
    EXPECT_FALSE(HasError);
    EXPECT_TRUE(F.is_infinity());
    EXPECT_EQ(Negative, F.is_negative());
    uint64_t PayloadResult = F.bit_cast_to_int().zero_ext_value() & PayloadMask;
    EXPECT_EQ(UINT64_C(0), PayloadResult);
  }
}

static std::string to_string(double d, unsigned Prec, unsigned Pad,
                             bool Tr = true) {
  lps::basic::Vector<100, char> buffer;
  Float f(d);
  f.string(buffer, Prec, Pad, Tr);
  return std::string(buffer.data(), buffer.size());
}

TEST(FloatTest, fromToStringSpecials) {
  auto expects = [](const char* first, const char* second) {
    std::string roundtrip = to_string(to_double_from_string(second), 0, 3);
    EXPECT_STREQ(first, roundtrip);
  };
  expects("+Inf", "+Inf");
  expects("+Inf", "INFINITY");
  expects("+Inf", "inf");
  expects("-Inf", "-Inf");
  expects("-Inf", "-INFINITY");
  expects("-Inf", "-inf");
  expects("NaN", "NaN");
  expects("NaN", "nan");
  expects("NaN", "-NaN");
  expects("NaN", "-nan");
}

TEST(FloatTest, fromHexadecimalString) {
  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "0x1p0").to_double());
  EXPECT_EQ(+1.0,
            Float(details::FloatBase::IEEEdouble(), "+0x1p0").to_double());
  EXPECT_EQ(-1.0,
            Float(details::FloatBase::IEEEdouble(), "-0x1p0").to_double());

  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "0x1p+0").to_double());
  EXPECT_EQ(+1.0,
            Float(details::FloatBase::IEEEdouble(), "+0x1p+0").to_double());
  EXPECT_EQ(-1.0,
            Float(details::FloatBase::IEEEdouble(), "-0x1p+0").to_double());

  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "0x1p-0").to_double());
  EXPECT_EQ(+1.0,
            Float(details::FloatBase::IEEEdouble(), "+0x1p-0").to_double());
  EXPECT_EQ(-1.0,
            Float(details::FloatBase::IEEEdouble(), "-0x1p-0").to_double());

  EXPECT_EQ(2.0, Float(details::FloatBase::IEEEdouble(), "0x1p1").to_double());
  EXPECT_EQ(+2.0,
            Float(details::FloatBase::IEEEdouble(), "+0x1p1").to_double());
  EXPECT_EQ(-2.0,
            Float(details::FloatBase::IEEEdouble(), "-0x1p1").to_double());

  EXPECT_EQ(2.0, Float(details::FloatBase::IEEEdouble(), "0x1p+1").to_double());
  EXPECT_EQ(+2.0,
            Float(details::FloatBase::IEEEdouble(), "+0x1p+1").to_double());
  EXPECT_EQ(-2.0,
            Float(details::FloatBase::IEEEdouble(), "-0x1p+1").to_double());

  EXPECT_EQ(0.5, Float(details::FloatBase::IEEEdouble(), "0x1p-1").to_double());
  EXPECT_EQ(+0.5,
            Float(details::FloatBase::IEEEdouble(), "+0x1p-1").to_double());
  EXPECT_EQ(-0.5,
            Float(details::FloatBase::IEEEdouble(), "-0x1p-1").to_double());

  EXPECT_EQ(3.0,
            Float(details::FloatBase::IEEEdouble(), "0x1.8p1").to_double());
  EXPECT_EQ(+3.0,
            Float(details::FloatBase::IEEEdouble(), "+0x1.8p1").to_double());
  EXPECT_EQ(-3.0,
            Float(details::FloatBase::IEEEdouble(), "-0x1.8p1").to_double());

  EXPECT_EQ(3.0,
            Float(details::FloatBase::IEEEdouble(), "0x1.8p+1").to_double());
  EXPECT_EQ(+3.0,
            Float(details::FloatBase::IEEEdouble(), "+0x1.8p+1").to_double());
  EXPECT_EQ(-3.0,
            Float(details::FloatBase::IEEEdouble(), "-0x1.8p+1").to_double());

  EXPECT_EQ(0.75,
            Float(details::FloatBase::IEEEdouble(), "0x1.8p-1").to_double());
  EXPECT_EQ(+0.75,
            Float(details::FloatBase::IEEEdouble(), "+0x1.8p-1").to_double());
  EXPECT_EQ(-0.75,
            Float(details::FloatBase::IEEEdouble(), "-0x1.8p-1").to_double());

  EXPECT_EQ(
      8192.0,
      Float(details::FloatBase::IEEEdouble(), "0x1000.000p1").to_double());
  EXPECT_EQ(
      +8192.0,
      Float(details::FloatBase::IEEEdouble(), "+0x1000.000p1").to_double());
  EXPECT_EQ(
      -8192.0,
      Float(details::FloatBase::IEEEdouble(), "-0x1000.000p1").to_double());

  EXPECT_EQ(
      8192.0,
      Float(details::FloatBase::IEEEdouble(), "0x1000.000p+1").to_double());
  EXPECT_EQ(
      +8192.0,
      Float(details::FloatBase::IEEEdouble(), "+0x1000.000p+1").to_double());
  EXPECT_EQ(
      -8192.0,
      Float(details::FloatBase::IEEEdouble(), "-0x1000.000p+1").to_double());

  EXPECT_EQ(
      2048.0,
      Float(details::FloatBase::IEEEdouble(), "0x1000.000p-1").to_double());
  EXPECT_EQ(
      +2048.0,
      Float(details::FloatBase::IEEEdouble(), "+0x1000.000p-1").to_double());
  EXPECT_EQ(
      -2048.0,
      Float(details::FloatBase::IEEEdouble(), "-0x1000.000p-1").to_double());

  EXPECT_EQ(8192.0,
            Float(details::FloatBase::IEEEdouble(), "0x1000p1").to_double());
  EXPECT_EQ(+8192.0,
            Float(details::FloatBase::IEEEdouble(), "+0x1000p1").to_double());
  EXPECT_EQ(-8192.0,
            Float(details::FloatBase::IEEEdouble(), "-0x1000p1").to_double());

  EXPECT_EQ(8192.0,
            Float(details::FloatBase::IEEEdouble(), "0x1000p+1").to_double());
  EXPECT_EQ(+8192.0,
            Float(details::FloatBase::IEEEdouble(), "+0x1000p+1").to_double());
  EXPECT_EQ(-8192.0,
            Float(details::FloatBase::IEEEdouble(), "-0x1000p+1").to_double());

  EXPECT_EQ(2048.0,
            Float(details::FloatBase::IEEEdouble(), "0x1000p-1").to_double());
  EXPECT_EQ(+2048.0,
            Float(details::FloatBase::IEEEdouble(), "+0x1000p-1").to_double());
  EXPECT_EQ(-2048.0,
            Float(details::FloatBase::IEEEdouble(), "-0x1000p-1").to_double());

  EXPECT_EQ(16384.0,
            Float(details::FloatBase::IEEEdouble(), "0x10p10").to_double());
  EXPECT_EQ(+16384.0,
            Float(details::FloatBase::IEEEdouble(), "+0x10p10").to_double());
  EXPECT_EQ(-16384.0,
            Float(details::FloatBase::IEEEdouble(), "-0x10p10").to_double());

  EXPECT_EQ(16384.0,
            Float(details::FloatBase::IEEEdouble(), "0x10p+10").to_double());
  EXPECT_EQ(+16384.0,
            Float(details::FloatBase::IEEEdouble(), "+0x10p+10").to_double());
  EXPECT_EQ(-16384.0,
            Float(details::FloatBase::IEEEdouble(), "-0x10p+10").to_double());

  EXPECT_EQ(0.015625,
            Float(details::FloatBase::IEEEdouble(), "0x10p-10").to_double());
  EXPECT_EQ(+0.015625,
            Float(details::FloatBase::IEEEdouble(), "+0x10p-10").to_double());
  EXPECT_EQ(-0.015625,
            Float(details::FloatBase::IEEEdouble(), "-0x10p-10").to_double());

  EXPECT_EQ(1.0625,
            Float(details::FloatBase::IEEEdouble(), "0x1.1p0").to_double());
  EXPECT_EQ(1.0, Float(details::FloatBase::IEEEdouble(), "0x1p0").to_double());

  EXPECT_EQ(to_double_from_string("0x1p-150"),
            to_double_from_string("+0x800000000000000001.p-221"));
  EXPECT_EQ(2251799813685248.5,
            to_double_from_string("0x80000000000004000000.010p-28"));
}

TEST(FloatTest, toString) {
  EXPECT_EQ("10", to_string(10.0, 6, 3));
  EXPECT_EQ("1.0E+1", to_string(10.0, 6, 0));
  EXPECT_EQ("10100", to_string(1.01E+4, 5, 2));
  EXPECT_EQ("1.01E+4", to_string(1.01E+4, 4, 2));
  EXPECT_EQ("1.01E+4", to_string(1.01E+4, 5, 1));
  EXPECT_EQ("0.0101", to_string(1.01E-2, 5, 2));
  EXPECT_EQ("0.0101", to_string(1.01E-2, 4, 2));
  EXPECT_EQ("1.01E-2", to_string(1.01E-2, 5, 1));
  EXPECT_EQ("0.78539816339744828", to_string(0.78539816339744830961, 0, 3));
  EXPECT_EQ("4.9406564584124654E-324",
            to_string(4.9406564584124654e-324, 0, 3));
  EXPECT_EQ("873.18340000000001", to_string(873.1834, 0, 1));
  EXPECT_EQ("8.7318340000000001E+2", to_string(873.1834, 0, 0));
  EXPECT_EQ("1.7976931348623157E+308",
            to_string(1.7976931348623157E+308, 0, 0));
  EXPECT_EQ("10", to_string(10.0, 6, 3, false));
  EXPECT_EQ("1.000000e+01", to_string(10.0, 6, 0, false));
  EXPECT_EQ("10100", to_string(1.01E+4, 5, 2, false));
  EXPECT_EQ("1.0100e+04", to_string(1.01E+4, 4, 2, false));
  EXPECT_EQ("1.01000e+04", to_string(1.01E+4, 5, 1, false));
  EXPECT_EQ("0.0101", to_string(1.01E-2, 5, 2, false));
  EXPECT_EQ("0.0101", to_string(1.01E-2, 4, 2, false));
  EXPECT_EQ("1.01000e-02", to_string(1.01E-2, 5, 1, false));
  EXPECT_EQ("0.78539816339744828",
            to_string(0.78539816339744830961, 0, 3, false));
  EXPECT_EQ("4.94065645841246540e-324",
            to_string(4.9406564584124654e-324, 0, 3, false));
  EXPECT_EQ("873.18340000000001", to_string(873.1834, 0, 1, false));
  EXPECT_EQ("8.73183400000000010e+02", to_string(873.1834, 0, 0, false));
  EXPECT_EQ("1.79769313486231570e+308",
            to_string(1.7976931348623157E+308, 0, 0, false));
}

TEST(FloatTest, toInteger) {
  bool isExact = false;
  SInt result(5, true);

  EXPECT_EQ(details::FloatBase::OpStatus::kOK,
            Float(details::FloatBase::IEEEdouble(), "10")
                .integer(result, details::FloatBase::RoundingMode::kTowardZero,
                         &isExact));
  EXPECT_TRUE(isExact);
  EXPECT_EQ(SInt(Int(5, 10), true), result);

  EXPECT_EQ(details::FloatBase::OpStatus::kInvalidOp,
            Float(details::FloatBase::IEEEdouble(), "-10")
                .integer(result, details::FloatBase::RoundingMode::kTowardZero,
                         &isExact));
  EXPECT_FALSE(isExact);
  EXPECT_EQ(SInt::min_value(5, true), result);

  EXPECT_EQ(details::FloatBase::OpStatus::kInvalidOp,
            Float(details::FloatBase::IEEEdouble(), "32")
                .integer(result, details::FloatBase::RoundingMode::kTowardZero,
                         &isExact));
  EXPECT_FALSE(isExact);
  EXPECT_EQ(SInt::max_value(5, true), result);

  EXPECT_EQ(details::FloatBase::OpStatus::kInexact,
            Float(details::FloatBase::IEEEdouble(), "7.9")
                .integer(result, details::FloatBase::RoundingMode::kTowardZero,
                         &isExact));
  EXPECT_FALSE(isExact);
  EXPECT_EQ(SInt(Int(5, 7), true), result);

  result.is_unsigned(false);
  EXPECT_EQ(details::FloatBase::OpStatus::kOK,
            Float(details::FloatBase::IEEEdouble(), "-10")
                .integer(result, details::FloatBase::RoundingMode::kTowardZero,
                         &isExact));
  EXPECT_TRUE(isExact);
  EXPECT_EQ(SInt(Int(5, -10, true), false), result);

  EXPECT_EQ(details::FloatBase::OpStatus::kInvalidOp,
            Float(details::FloatBase::IEEEdouble(), "-17")
                .integer(result, details::FloatBase::RoundingMode::kTowardZero,
                         &isExact));
  EXPECT_FALSE(isExact);
  EXPECT_EQ(SInt::min_value(5, false), result);

  EXPECT_EQ(details::FloatBase::OpStatus::kInvalidOp,
            Float(details::FloatBase::IEEEdouble(), "16")
                .integer(result, details::FloatBase::RoundingMode::kTowardZero,
                         &isExact));
  EXPECT_FALSE(isExact);
  EXPECT_EQ(SInt::max_value(5, false), result);
}

static Int nanbitsFromInt(const details::FloatSemantics& Sem, bool SNaN,
                          bool Negative, uint64_t payload) {
  Int appayload(64, payload);
  if (SNaN)
    return Float::sNaN(Sem, Negative, &appayload).bit_cast_to_int();
  return Float::qNaN(Sem, Negative, &appayload).bit_cast_to_int();
}

TEST(FloatTest, makeNaN) {
  const struct {
    uint64_t expected;
    const details::FloatSemantics& semantics;
    bool SNaN;
    bool Negative;
    uint64_t payload;
  } tests[] = {
      /*             expected              semantics   SNaN    Neg                payload */
      {0x7fc00000ULL, details::FloatBase::IEEEsingle(), false, false,
       0x00000000ULL},
      {0xffc00000ULL, details::FloatBase::IEEEsingle(), false, true,
       0x00000000ULL},
      {0x7fc0ae72ULL, details::FloatBase::IEEEsingle(), false, false,
       0x0000ae72ULL},
      {0x7fffae72ULL, details::FloatBase::IEEEsingle(), false, false,
       0xffffae72ULL},
      {0x7fdaae72ULL, details::FloatBase::IEEEsingle(), false, false,
       0x00daae72ULL},
      {0x7fa00000ULL, details::FloatBase::IEEEsingle(), true, false,
       0x00000000ULL},
      {0xffa00000ULL, details::FloatBase::IEEEsingle(), true, true,
       0x00000000ULL},
      {0x7f80ae72ULL, details::FloatBase::IEEEsingle(), true, false,
       0x0000ae72ULL},
      {0x7fbfae72ULL, details::FloatBase::IEEEsingle(), true, false,
       0xffffae72ULL},
      {0x7f9aae72ULL, details::FloatBase::IEEEsingle(), true, false,
       0x001aae72ULL},
      {0x7ff8000000000000ULL, details::FloatBase::IEEEdouble(), false, false,
       0x0000000000000000ULL},
      {0xfff8000000000000ULL, details::FloatBase::IEEEdouble(), false, true,
       0x0000000000000000ULL},
      {0x7ff800000000ae72ULL, details::FloatBase::IEEEdouble(), false, false,
       0x000000000000ae72ULL},
      {0x7fffffffffffae72ULL, details::FloatBase::IEEEdouble(), false, false,
       0xffffffffffffae72ULL},
      {0x7ffdaaaaaaaaae72ULL, details::FloatBase::IEEEdouble(), false, false,
       0x000daaaaaaaaae72ULL},
      {0x7ff4000000000000ULL, details::FloatBase::IEEEdouble(), true, false,
       0x0000000000000000ULL},
      {0xfff4000000000000ULL, details::FloatBase::IEEEdouble(), true, true,
       0x0000000000000000ULL},
      {0x7ff000000000ae72ULL, details::FloatBase::IEEEdouble(), true, false,
       0x000000000000ae72ULL},
      {0x7ff7ffffffffae72ULL, details::FloatBase::IEEEdouble(), true, false,
       0xffffffffffffae72ULL},
      {0x7ff1aaaaaaaaae72ULL, details::FloatBase::IEEEdouble(), true, false,
       0x0001aaaaaaaaae72ULL},
  };

  for (const auto& t : tests) {
    EXPECT_EQ(t.expected,
              nanbitsFromInt(t.semantics, t.SNaN, t.Negative, t.payload));
  }
}

TEST(FloatTest, exactInverse) {
  Float inv(0.0f);

  // Trivial operation.
  EXPECT_TRUE(Float(2.0).exact_inverse(&inv));
  EXPECT_TRUE(inv.bitwise_equal(Float(0.5)));
  EXPECT_TRUE(Float(2.0f).exact_inverse(&inv));
  EXPECT_TRUE(inv.bitwise_equal(Float(0.5f)));
  EXPECT_TRUE(Float(details::FloatBase::IEEEquad(), "2.0").exact_inverse(&inv));
  EXPECT_TRUE(inv.bitwise_equal(Float(details::FloatBase::IEEEquad(), "0.5")));
  // FLT_MIN
  EXPECT_TRUE(Float(1.17549435e-38f).exact_inverse(&inv));
  EXPECT_TRUE(inv.bitwise_equal(Float(8.5070592e+37f)));

  // Large float, inverse is a denormal.
  EXPECT_FALSE(Float(1.7014118e38f).exact_inverse(nullptr));
  // Zero
  EXPECT_FALSE(Float(0.0).exact_inverse(nullptr));
  // Denormalized float
  EXPECT_FALSE(Float(1.40129846e-45f).exact_inverse(nullptr));
}

TEST(FloatTest, round2int) {
  Float T(-0.5), S(3.14), R(Float::largest(details::FloatBase::IEEEdouble())),
      P(0.0);

  P = T;
  P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_EQ(-0.0, P.to_double());
  P = T;
  P.round2int(details::FloatBase::RoundingMode::kTowardNegative);
  EXPECT_EQ(-1.0, P.to_double());
  P = T;
  P.round2int(details::FloatBase::RoundingMode::kTowardPositive);
  EXPECT_EQ(-0.0, P.to_double());
  P = T;
  P.round2int(details::FloatBase::RoundingMode::kNearestTiesToEven);
  EXPECT_EQ(-0.0, P.to_double());

  P = S;
  P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_EQ(3.0, P.to_double());
  P = S;
  P.round2int(details::FloatBase::RoundingMode::kTowardNegative);
  EXPECT_EQ(3.0, P.to_double());
  P = S;
  P.round2int(details::FloatBase::RoundingMode::kTowardPositive);
  EXPECT_EQ(4.0, P.to_double());
  P = S;
  P.round2int(details::FloatBase::RoundingMode::kNearestTiesToEven);
  EXPECT_EQ(3.0, P.to_double());

  P = R;
  P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_EQ(R.to_double(), P.to_double());
  P = R;
  P.round2int(details::FloatBase::RoundingMode::kTowardNegative);
  EXPECT_EQ(R.to_double(), P.to_double());
  P = R;
  P.round2int(details::FloatBase::RoundingMode::kTowardPositive);
  EXPECT_EQ(R.to_double(), P.to_double());
  P = R;
  P.round2int(details::FloatBase::RoundingMode::kNearestTiesToEven);
  EXPECT_EQ(R.to_double(), P.to_double());

  P = Float::zero(details::FloatBase::IEEEdouble());
  P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_EQ(0.0, P.to_double());
  P = Float::zero(details::FloatBase::IEEEdouble(), true);
  P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_EQ(-0.0, P.to_double());
  P = Float::NaN(details::FloatBase::IEEEdouble());
  P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_TRUE(std::isnan(P.to_double()));
  P = Float::inf(details::FloatBase::IEEEdouble());
  P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_TRUE(std::isinf(P.to_double()) && P.to_double() > 0.0);
  P = Float::inf(details::FloatBase::IEEEdouble(), true);
  P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_TRUE(std::isinf(P.to_double()) && P.to_double() < 0.0);

  details::FloatBase::OpStatus St;

  P = Float::NaN(details::FloatBase::IEEEdouble());
  St = P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_TRUE(P.isNaN());
  EXPECT_FALSE(P.is_negative());
  EXPECT_EQ(details::FloatBase::OpStatus::kOK, St);

  P = Float::NaN(details::FloatBase::IEEEdouble(), true);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_TRUE(P.isNaN());
  EXPECT_TRUE(P.is_negative());
  EXPECT_EQ(details::FloatBase::OpStatus::kOK, St);

  P = Float::sNaN(details::FloatBase::IEEEdouble());
  St = P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_TRUE(P.isNaN());
  EXPECT_FALSE(P.is_signaling());
  EXPECT_FALSE(P.is_negative());
  EXPECT_EQ(details::FloatBase::OpStatus::kInvalidOp, St);

  P = Float::sNaN(details::FloatBase::IEEEdouble(), true);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_TRUE(P.isNaN());
  EXPECT_FALSE(P.is_signaling());
  EXPECT_TRUE(P.is_negative());
  EXPECT_EQ(details::FloatBase::OpStatus::kInvalidOp, St);

  P = Float::inf(details::FloatBase::IEEEdouble());
  St = P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_TRUE(P.is_infinity());
  EXPECT_FALSE(P.is_negative());
  EXPECT_EQ(details::FloatBase::OpStatus::kOK, St);

  P = Float::inf(details::FloatBase::IEEEdouble(), true);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_TRUE(P.is_infinity());
  EXPECT_TRUE(P.is_negative());
  EXPECT_EQ(details::FloatBase::OpStatus::kOK, St);

  P = Float::zero(details::FloatBase::IEEEdouble(), false);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_TRUE(P.is_zero());
  EXPECT_FALSE(P.is_negative());
  EXPECT_EQ(details::FloatBase::OpStatus::kOK, St);

  P = Float::zero(details::FloatBase::IEEEdouble(), false);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardNegative);
  EXPECT_TRUE(P.is_zero());
  EXPECT_FALSE(P.is_negative());
  EXPECT_EQ(details::FloatBase::OpStatus::kOK, St);

  P = Float::zero(details::FloatBase::IEEEdouble(), true);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_TRUE(P.is_zero());
  EXPECT_TRUE(P.is_negative());
  EXPECT_EQ(details::FloatBase::OpStatus::kOK, St);

  P = Float::zero(details::FloatBase::IEEEdouble(), true);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardNegative);
  EXPECT_TRUE(P.is_zero());
  EXPECT_TRUE(P.is_negative());
  EXPECT_EQ(details::FloatBase::OpStatus::kOK, St);

  P = Float(1E-100);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardNegative);
  EXPECT_TRUE(P.is_zero());
  EXPECT_FALSE(P.is_negative());
  EXPECT_EQ(details::FloatBase::OpStatus::kInexact, St);

  P = Float(1E-100);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardPositive);
  EXPECT_EQ(1.0, P.to_double());
  EXPECT_FALSE(P.is_negative());
  EXPECT_EQ(details::FloatBase::OpStatus::kInexact, St);

  P = Float(-1E-100);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardNegative);
  EXPECT_TRUE(P.is_negative());
  EXPECT_EQ(-1.0, P.to_double());
  EXPECT_EQ(details::FloatBase::OpStatus::kInexact, St);

  P = Float(-1E-100);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardPositive);
  EXPECT_TRUE(P.is_zero());
  EXPECT_TRUE(P.is_negative());
  EXPECT_EQ(details::FloatBase::OpStatus::kInexact, St);

  P = Float(10.0);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_EQ(10.0, P.to_double());
  EXPECT_EQ(details::FloatBase::OpStatus::kOK, St);

  P = Float(10.5);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardZero);
  EXPECT_EQ(10.0, P.to_double());
  EXPECT_EQ(details::FloatBase::OpStatus::kInexact, St);

  P = Float(10.5);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardPositive);
  EXPECT_EQ(11.0, P.to_double());
  EXPECT_EQ(details::FloatBase::OpStatus::kInexact, St);

  P = Float(10.5);
  St = P.round2int(details::FloatBase::RoundingMode::kTowardNegative);
  EXPECT_EQ(10.0, P.to_double());
  EXPECT_EQ(details::FloatBase::OpStatus::kInexact, St);

  P = Float(10.5);
  St = P.round2int(details::FloatBase::RoundingMode::kNearestTiesToAway);
  EXPECT_EQ(11.0, P.to_double());
  EXPECT_EQ(details::FloatBase::OpStatus::kInexact, St);

  P = Float(10.5);
  St = P.round2int(details::FloatBase::RoundingMode::kNearestTiesToEven);
  EXPECT_EQ(10.0, P.to_double());
  EXPECT_EQ(details::FloatBase::OpStatus::kInexact, St);
}

TEST(FloatTest, is_int) {
  Float T(-0.0);
  EXPECT_TRUE(T.is_int());
  T = Float(3.14159);
  EXPECT_FALSE(T.is_int());
  T = Float::NaN(details::FloatBase::IEEEdouble());
  EXPECT_FALSE(T.is_int());
  T = Float::inf(details::FloatBase::IEEEdouble());
  EXPECT_FALSE(T.is_int());
  T = Float::inf(details::FloatBase::IEEEdouble(), true);
  EXPECT_FALSE(T.is_int());
  T = Float::largest(details::FloatBase::IEEEdouble());
  EXPECT_TRUE(T.is_int());
}

TEST(FloatTest, getLargest) {
  EXPECT_EQ(3.402823466e+38f,
            Float::largest(details::FloatBase::IEEEsingle()).to_float());
  EXPECT_EQ(1.7976931348623158e+308,
            Float::largest(details::FloatBase::IEEEdouble()).to_double());
}

TEST(FloatTest, smallest) {
  Float test = Float::smallest(details::FloatBase::IEEEsingle(), false);
  Float expected = Float(details::FloatBase::IEEEsingle(), "0x0.000002p-126");
  EXPECT_FALSE(test.is_negative());
  EXPECT_TRUE(test.is_finite_nonzero());
  EXPECT_TRUE(test.is_denormal());
  EXPECT_TRUE(test.bitwise_equal(expected));

  test = Float::smallest(details::FloatBase::IEEEsingle(), true);
  expected = Float(details::FloatBase::IEEEsingle(), "-0x0.000002p-126");
  EXPECT_TRUE(test.is_negative());
  EXPECT_TRUE(test.is_finite_nonzero());
  EXPECT_TRUE(test.is_denormal());
  EXPECT_TRUE(test.bitwise_equal(expected));

  test = Float::smallest(details::FloatBase::IEEEquad(), false);
  expected = Float(details::FloatBase::IEEEquad(),
                   "0x0.0000000000000000000000000001p-16382");
  EXPECT_FALSE(test.is_negative());
  EXPECT_TRUE(test.is_finite_nonzero());
  EXPECT_TRUE(test.is_denormal());
  EXPECT_TRUE(test.bitwise_equal(expected));

  test = Float::smallest(details::FloatBase::IEEEquad(), true);
  expected = Float(details::FloatBase::IEEEquad(),
                   "-0x0.0000000000000000000000000001p-16382");
  EXPECT_TRUE(test.is_negative());
  EXPECT_TRUE(test.is_finite_nonzero());
  EXPECT_TRUE(test.is_denormal());
  EXPECT_TRUE(test.bitwise_equal(expected));
}

TEST(FloatTest, smallest_normalized) {
  Float test =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), false);
  Float expected = Float(details::FloatBase::IEEEsingle(), "0x1p-126");
  EXPECT_FALSE(test.is_negative());
  EXPECT_TRUE(test.is_finite_nonzero());
  EXPECT_FALSE(test.is_denormal());
  EXPECT_TRUE(test.bitwise_equal(expected));
  EXPECT_TRUE(test.is_smallest_normalized());

  test = Float::smallest_normalized(details::FloatBase::IEEEsingle(), true);
  expected = Float(details::FloatBase::IEEEsingle(), "-0x1p-126");
  EXPECT_TRUE(test.is_negative());
  EXPECT_TRUE(test.is_finite_nonzero());
  EXPECT_FALSE(test.is_denormal());
  EXPECT_TRUE(test.bitwise_equal(expected));
  EXPECT_TRUE(test.is_smallest_normalized());

  test = Float::smallest_normalized(details::FloatBase::IEEEdouble(), false);
  expected = Float(details::FloatBase::IEEEdouble(), "0x1p-1022");
  EXPECT_FALSE(test.is_negative());
  EXPECT_TRUE(test.is_finite_nonzero());
  EXPECT_FALSE(test.is_denormal());
  EXPECT_TRUE(test.bitwise_equal(expected));
  EXPECT_TRUE(test.is_smallest_normalized());

  test = Float::smallest_normalized(details::FloatBase::IEEEdouble(), true);
  expected = Float(details::FloatBase::IEEEdouble(), "-0x1p-1022");
  EXPECT_TRUE(test.is_negative());
  EXPECT_TRUE(test.is_finite_nonzero());
  EXPECT_FALSE(test.is_denormal());
  EXPECT_TRUE(test.bitwise_equal(expected));
  EXPECT_TRUE(test.is_smallest_normalized());

  test = Float::smallest_normalized(details::FloatBase::IEEEquad(), false);
  expected = Float(details::FloatBase::IEEEquad(), "0x1p-16382");
  EXPECT_FALSE(test.is_negative());
  EXPECT_TRUE(test.is_finite_nonzero());
  EXPECT_FALSE(test.is_denormal());
  EXPECT_TRUE(test.bitwise_equal(expected));
  EXPECT_TRUE(test.is_smallest_normalized());

  test = Float::smallest_normalized(details::FloatBase::IEEEquad(), true);
  expected = Float(details::FloatBase::IEEEquad(), "-0x1p-16382");
  EXPECT_TRUE(test.is_negative());
  EXPECT_TRUE(test.is_finite_nonzero());
  EXPECT_FALSE(test.is_denormal());
  EXPECT_TRUE(test.bitwise_equal(expected));
  EXPECT_TRUE(test.is_smallest_normalized());
}

TEST(FloatTest, getZero) {
  struct {
    const details::FloatSemantics* semantics;
    const bool sign;
    const unsigned long long bitPattern[2];
    const unsigned bitPatternLength;
  } const GetZeroTest[] = {
      {&details::FloatBase::IEEEhalf(), false, {0, 0}, 1},
      {&details::FloatBase::IEEEhalf(), true, {0x8000ULL, 0}, 1},
      {&details::FloatBase::IEEEsingle(), false, {0, 0}, 1},
      {&details::FloatBase::IEEEsingle(), true, {0x80000000ULL, 0}, 1},
      {&details::FloatBase::IEEEdouble(), false, {0, 0}, 1},
      {&details::FloatBase::IEEEdouble(), true, {0x8000000000000000ULL, 0}, 1},
      {&details::FloatBase::IEEEquad(), false, {0, 0}, 2},
      {&details::FloatBase::IEEEquad(), true, {0, 0x8000000000000000ULL}, 2},

  };
  const unsigned NumGetZeroTests = 8;
  for (unsigned i = 0; i < NumGetZeroTests; ++i) {
    Float test = Float::zero(*GetZeroTest[i].semantics, GetZeroTest[i].sign);
    const char* pattern = GetZeroTest[i].sign ? "-0x0p+0" : "0x0p+0";
    Float expected = Float(*GetZeroTest[i].semantics, pattern);
    EXPECT_TRUE(test.is_zero());
    EXPECT_TRUE(GetZeroTest[i].sign ? test.is_negative() : !test.is_negative());
    EXPECT_TRUE(test.bitwise_equal(expected));
    for (unsigned j = 0, je = GetZeroTest[i].bitPatternLength; j < je; ++j) {
      EXPECT_EQ(GetZeroTest[i].bitPattern[j],
                test.bit_cast_to_int().raw_data()[j]);
    }
  }
}

TEST(FloatTest, copy_sign) {
  EXPECT_TRUE(
      Float(-42.0).bitwise_equal(Float::copy_sign(Float(42.0), Float(-1.0))));
  EXPECT_TRUE(
      Float(42.0).bitwise_equal(Float::copy_sign(Float(-42.0), Float(1.0))));
  EXPECT_TRUE(
      Float(-42.0).bitwise_equal(Float::copy_sign(Float(-42.0), Float(-1.0))));
  EXPECT_TRUE(
      Float(42.0).bitwise_equal(Float::copy_sign(Float(42.0), Float(1.0))));
}

TEST(FloatTest, convert) {
  bool losesInfo;
  Float test(details::FloatBase::IEEEdouble(), "1.0");
  test.convert(details::FloatBase::IEEEsingle(),
               details::FloatBase::RoundingMode::kNearestTiesToEven,
               &losesInfo);
  EXPECT_EQ(1.0f, test.to_float());
  EXPECT_FALSE(losesInfo);

  test = Float(details::FloatBase::IEEEquad(), "0x1p-53");
  test.add(Float(details::FloatBase::IEEEquad(), "1.0"),
           details::FloatBase::RoundingMode::kNearestTiesToEven);
  test.convert(details::FloatBase::IEEEdouble(),
               details::FloatBase::RoundingMode::kNearestTiesToEven,
               &losesInfo);
  EXPECT_EQ(1.0, test.to_double());
  EXPECT_TRUE(losesInfo);

  // The payload is lost in truncation, but we retain NaN by setting the quiet bit.
  Int payload(52, 1);
  test = Float::sNaN(details::FloatBase::IEEEdouble(), false, &payload);
  auto status = test.convert(
      details::FloatBase::IEEEsingle(),
      details::FloatBase::RoundingMode::kNearestTiesToEven, &losesInfo);
  EXPECT_EQ(0x7fc00000, test.bit_cast_to_int());
  EXPECT_TRUE(losesInfo);
  EXPECT_EQ(status, details::FloatBase::OpStatus::kInvalidOp);

  // The payload is lost in truncation. QNaN remains QNaN.
  test = Float::qNaN(details::FloatBase::IEEEdouble(), false, &payload);
  status = test.convert(details::FloatBase::IEEEsingle(),
                        details::FloatBase::RoundingMode::kNearestTiesToEven,
                        &losesInfo);
  EXPECT_EQ(0x7fc00000, test.bit_cast_to_int());
  EXPECT_TRUE(losesInfo);
  EXPECT_EQ(status, details::FloatBase::OpStatus::kOK);

  // Test that subnormals are handled correctly in double to float conversion
  test = Float(details::FloatBase::IEEEdouble(), "0x0.0000010000000p-1022");
  test.convert(details::FloatBase::IEEEsingle(),
               details::FloatBase::RoundingMode::kNearestTiesToEven,
               &losesInfo);
  EXPECT_EQ(0.0f, test.to_float());
  EXPECT_TRUE(losesInfo);

  test = Float(details::FloatBase::IEEEdouble(), "0x0.0000010000001p-1022");
  test.convert(details::FloatBase::IEEEsingle(),
               details::FloatBase::RoundingMode::kNearestTiesToEven,
               &losesInfo);
  EXPECT_EQ(0.0f, test.to_float());
  EXPECT_TRUE(losesInfo);

  test = Float(details::FloatBase::IEEEdouble(), "-0x0.0000010000001p-1022");
  test.convert(details::FloatBase::IEEEsingle(),
               details::FloatBase::RoundingMode::kNearestTiesToEven,
               &losesInfo);
  EXPECT_EQ(0.0f, test.to_float());
  EXPECT_TRUE(losesInfo);

  test = Float(details::FloatBase::IEEEdouble(), "0x0.0000020000000p-1022");
  test.convert(details::FloatBase::IEEEsingle(),
               details::FloatBase::RoundingMode::kNearestTiesToEven,
               &losesInfo);
  EXPECT_EQ(0.0f, test.to_float());
  EXPECT_TRUE(losesInfo);

  test = Float(details::FloatBase::IEEEdouble(), "0x0.0000020000001p-1022");
  test.convert(details::FloatBase::IEEEsingle(),
               details::FloatBase::RoundingMode::kNearestTiesToEven,
               &losesInfo);
  EXPECT_EQ(0.0f, test.to_float());
  EXPECT_TRUE(losesInfo);

  // Test subnormal conversion to bfloat
  test = Float(details::FloatBase::IEEEsingle(), "0x0.01p-126");
  test.convert(details::FloatBase::BFloat(),
               details::FloatBase::RoundingMode::kNearestTiesToEven,
               &losesInfo);
  EXPECT_EQ(0.0f, test.to_float());
  EXPECT_TRUE(losesInfo);

  test = Float(details::FloatBase::IEEEsingle(), "0x0.02p-126");
  test.convert(details::FloatBase::BFloat(),
               details::FloatBase::RoundingMode::kNearestTiesToEven,
               &losesInfo);
  EXPECT_EQ(0x01, test.bit_cast_to_int());
  EXPECT_FALSE(losesInfo);

  test = Float(details::FloatBase::IEEEsingle(), "0x0.01p-126");
  test.convert(details::FloatBase::BFloat(),
               details::FloatBase::RoundingMode::kNearestTiesToAway,
               &losesInfo);
  EXPECT_EQ(0x01, test.bit_cast_to_int());
  EXPECT_TRUE(losesInfo);
}

TEST(FloatTest, is_negative) {
  Float t(details::FloatBase::IEEEsingle(), "0x1p+0");
  EXPECT_FALSE(t.is_negative());
  t = Float(details::FloatBase::IEEEsingle(), "-0x1p+0");
  EXPECT_TRUE(t.is_negative());

  EXPECT_FALSE(
      Float::inf(details::FloatBase::IEEEsingle(), false).is_negative());
  EXPECT_TRUE(Float::inf(details::FloatBase::IEEEsingle(), true).is_negative());

  EXPECT_FALSE(
      Float::zero(details::FloatBase::IEEEsingle(), false).is_negative());
  EXPECT_TRUE(
      Float::zero(details::FloatBase::IEEEsingle(), true).is_negative());

  EXPECT_FALSE(
      Float::NaN(details::FloatBase::IEEEsingle(), false).is_negative());
  EXPECT_TRUE(Float::NaN(details::FloatBase::IEEEsingle(), true).is_negative());

  EXPECT_FALSE(
      Float::sNaN(details::FloatBase::IEEEsingle(), false).is_negative());
  EXPECT_TRUE(
      Float::sNaN(details::FloatBase::IEEEsingle(), true).is_negative());
}

TEST(FloatTest, is_normal) {
  Float t(details::FloatBase::IEEEsingle(), "0x1p+0");
  EXPECT_TRUE(t.is_normal());

  EXPECT_FALSE(Float::inf(details::FloatBase::IEEEsingle(), false).is_normal());
  EXPECT_FALSE(
      Float::zero(details::FloatBase::IEEEsingle(), false).is_normal());
  EXPECT_FALSE(Float::NaN(details::FloatBase::IEEEsingle(), false).is_normal());
  EXPECT_FALSE(
      Float::sNaN(details::FloatBase::IEEEsingle(), false).is_normal());
  EXPECT_FALSE(Float(details::FloatBase::IEEEsingle(), "0x1p-149").is_normal());
}

TEST(FloatTest, is_finite) {
  Float t(details::FloatBase::IEEEsingle(), "0x1p+0");
  EXPECT_TRUE(t.is_finite());
  EXPECT_FALSE(Float::inf(details::FloatBase::IEEEsingle(), false).is_finite());
  EXPECT_TRUE(Float::zero(details::FloatBase::IEEEsingle(), false).is_finite());
  EXPECT_FALSE(Float::NaN(details::FloatBase::IEEEsingle(), false).is_finite());
  EXPECT_FALSE(
      Float::sNaN(details::FloatBase::IEEEsingle(), false).is_finite());
  EXPECT_TRUE(Float(details::FloatBase::IEEEsingle(), "0x1p-149").is_finite());
}

TEST(FloatTest, is_infinity) {
  Float t(details::FloatBase::IEEEsingle(), "0x1p+0");
  EXPECT_FALSE(t.is_infinity());

  Float PosInf = Float::inf(details::FloatBase::IEEEsingle(), false);
  Float NegInf = Float::inf(details::FloatBase::IEEEsingle(), true);

  EXPECT_TRUE(PosInf.is_infinity());
  EXPECT_TRUE(PosInf.is_pos_infinity());
  EXPECT_FALSE(PosInf.is_neg_infinity());
  EXPECT_TRUE(NegInf.is_infinity());
  EXPECT_FALSE(NegInf.is_pos_infinity());
  EXPECT_TRUE(NegInf.is_neg_infinity());

  EXPECT_FALSE(
      Float::zero(details::FloatBase::IEEEsingle(), false).is_infinity());
  EXPECT_FALSE(
      Float::NaN(details::FloatBase::IEEEsingle(), false).is_infinity());
  EXPECT_FALSE(
      Float::sNaN(details::FloatBase::IEEEsingle(), false).is_infinity());
  EXPECT_FALSE(
      Float(details::FloatBase::IEEEsingle(), "0x1p-149").is_infinity());
}

TEST(FloatTest, isNaN) {
  Float t(details::FloatBase::IEEEsingle(), "0x1p+0");
  EXPECT_FALSE(t.isNaN());
  EXPECT_FALSE(Float::inf(details::FloatBase::IEEEsingle(), false).isNaN());
  EXPECT_FALSE(Float::zero(details::FloatBase::IEEEsingle(), false).isNaN());
  EXPECT_TRUE(Float::NaN(details::FloatBase::IEEEsingle(), false).isNaN());
  EXPECT_TRUE(Float::sNaN(details::FloatBase::IEEEsingle(), false).isNaN());
  EXPECT_FALSE(Float(details::FloatBase::IEEEsingle(), "0x1p-149").isNaN());
}

TEST(FloatTest, is_finite_nonzero) {
  // Test positive/negative normal value.
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEsingle(), "0x1p+0").is_finite_nonzero());
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEsingle(), "-0x1p+0").is_finite_nonzero());

  // Test positive/negative denormal value.
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEsingle(), "0x1p-149").is_finite_nonzero());
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEsingle(), "-0x1p-149").is_finite_nonzero());

  // Test +/- Infinity.
  EXPECT_FALSE(
      Float::inf(details::FloatBase::IEEEsingle(), false).is_finite_nonzero());
  EXPECT_FALSE(
      Float::inf(details::FloatBase::IEEEsingle(), true).is_finite_nonzero());

  // Test +/- Zero.
  EXPECT_FALSE(
      Float::zero(details::FloatBase::IEEEsingle(), false).is_finite_nonzero());
  EXPECT_FALSE(
      Float::zero(details::FloatBase::IEEEsingle(), true).is_finite_nonzero());

  // Test +/- qNaN. +/- dont mean anything with qNaN but paranoia can't hurt in
  // this instance.
  EXPECT_FALSE(
      Float::NaN(details::FloatBase::IEEEsingle(), false).is_finite_nonzero());
  EXPECT_FALSE(
      Float::NaN(details::FloatBase::IEEEsingle(), true).is_finite_nonzero());

  // Test +/- sNaN. +/- dont mean anything with sNaN but paranoia can't hurt in
  // this instance.
  EXPECT_FALSE(
      Float::sNaN(details::FloatBase::IEEEsingle(), false).is_finite_nonzero());
  EXPECT_FALSE(
      Float::sNaN(details::FloatBase::IEEEsingle(), true).is_finite_nonzero());
}

TEST(FloatTest, add) {
  // Test Special Cases against each other and normal values.

  Float PInf = Float::inf(details::FloatBase::IEEEsingle(), false);
  Float MInf = Float::inf(details::FloatBase::IEEEsingle(), true);
  Float PZero = Float::zero(details::FloatBase::IEEEsingle(), false);
  Float MZero = Float::zero(details::FloatBase::IEEEsingle(), true);
  Float QNaN = Float::NaN(details::FloatBase::IEEEsingle(), false);
  Float SNaN = Float(details::FloatBase::IEEEsingle(), "snan123");
  Float PNormalValue = Float(details::FloatBase::IEEEsingle(), "0x1p+0");
  Float MNormalValue = Float(details::FloatBase::IEEEsingle(), "-0x1p+0");
  Float PLargestValue = Float::largest(details::FloatBase::IEEEsingle(), false);
  Float MLargestValue = Float::largest(details::FloatBase::IEEEsingle(), true);
  Float PSmallestValue =
      Float::smallest(details::FloatBase::IEEEsingle(), false);
  Float MSmallestValue =
      Float::smallest(details::FloatBase::IEEEsingle(), true);
  Float PSmallestNormalized =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), false);
  Float MSmallestNormalized =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), true);

  auto OverflowStatus = static_cast<details::FloatBase::OpStatus>(
      static_cast<uint8_t>(details::FloatBase::OpStatus::kOverflow) |
      static_cast<uint8_t>(details::FloatBase::OpStatus::kInexact));

  struct {
    Float x;
    Float y;
    const char* result;
    details::FloatBase::OpStatus status;
    details::FloatBase::FloatCategory category;
  } SpecialCaseTests[] = {
      {PInf, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, PZero, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MZero, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, PNormalValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MNormalValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, PLargestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MLargestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, PSmallestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MSmallestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, PSmallestNormalized, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MSmallestNormalized, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PZero, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MZero, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, PNormalValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MNormalValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PLargestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MLargestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PSmallestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MSmallestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PSmallestNormalized, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MSmallestNormalized, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PZero, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PZero, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PZero, PZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, PNormalValue, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PZero, MNormalValue, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PZero, PLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PZero, MLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PZero, PSmallestValue, "0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PZero, MSmallestValue, "-0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PZero, PSmallestNormalized, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PZero, MSmallestNormalized, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MZero, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MZero, PZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MZero, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, PNormalValue, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, MNormalValue, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, PLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, MLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, PSmallestValue, "0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, MSmallestValue, "-0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, PSmallestNormalized, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, MSmallestNormalized, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {QNaN, PInf, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MInf, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PZero, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MZero, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, SNaN, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PNormalValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MNormalValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PLargestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MLargestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PSmallestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MSmallestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PSmallestNormalized, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MSmallestNormalized, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PInf, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MInf, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PZero, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MZero, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, QNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PNormalValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MNormalValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PLargestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MLargestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PSmallestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MSmallestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PSmallestNormalized, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MSmallestNormalized, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PNormalValue, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PNormalValue, PZero, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MZero, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, PNormalValue, "0x1p+1", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MNormalValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PNormalValue, PLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, PSmallestValue, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MSmallestValue, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, PSmallestNormalized, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MSmallestNormalized, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MNormalValue, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MNormalValue, PZero, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MZero, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MNormalValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MNormalValue, PNormalValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MNormalValue, MNormalValue, "-0x1p+1", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PSmallestValue, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MSmallestValue, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PSmallestNormalized, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MSmallestNormalized, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, PZero, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, MZero, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PLargestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PLargestValue, PNormalValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, MNormalValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, PLargestValue, "inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, MLargestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PLargestValue, PSmallestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, MSmallestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, PSmallestNormalized, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, MSmallestNormalized, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, PZero, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, MZero, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MLargestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MLargestValue, PNormalValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, MNormalValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, PLargestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MLargestValue, MLargestValue, "-inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, PSmallestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, MSmallestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, PSmallestNormalized, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, MSmallestNormalized, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestValue, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestValue, PZero, "0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MZero, "0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestValue, PNormalValue, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MNormalValue, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PSmallestValue, "0x1p-148",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MSmallestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestValue, PSmallestNormalized, "0x1.000002p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MSmallestNormalized, "-0x1.fffffcp-127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestValue, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestValue, PZero, "-0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MZero, "-0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestValue, PNormalValue, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MNormalValue, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PSmallestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestValue, MSmallestValue, "-0x1p-148",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PSmallestNormalized, "0x1.fffffcp-127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MSmallestNormalized, "-0x1.000002p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestNormalized, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestNormalized, PZero, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MZero, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestNormalized, SNaN, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestNormalized, PNormalValue, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MNormalValue, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PSmallestValue, "0x1.000002p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MSmallestValue, "0x1.fffffcp-127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PSmallestNormalized, "0x1p-125",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MSmallestNormalized, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestNormalized, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestNormalized, PZero, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MZero, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestNormalized, SNaN, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestNormalized, PNormalValue, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MNormalValue, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PSmallestValue, "-0x1.fffffcp-127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MSmallestValue, "-0x1.000002p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PSmallestNormalized, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, MSmallestNormalized, "-0x1p-125",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal}};

  for (size_t i = 0; i < std::size(SpecialCaseTests); ++i) {
    Float x(SpecialCaseTests[i].x);
    Float y(SpecialCaseTests[i].y);
    auto status =
        x.add(y, details::FloatBase::RoundingMode::kNearestTiesToEven);

    Float result(details::FloatBase::IEEEsingle(), SpecialCaseTests[i].result);

    EXPECT_TRUE(result.bitwise_equal(x));
    EXPECT_EQ(SpecialCaseTests[i].status, status);
    EXPECT_EQ(SpecialCaseTests[i].category, x.category());
  }
}

TEST(FloatTest, subtract) {
  // Test Special Cases against each other and normal values.

  Float PInf = Float::inf(details::FloatBase::IEEEsingle(), false);
  Float MInf = Float::inf(details::FloatBase::IEEEsingle(), true);
  Float PZero = Float::zero(details::FloatBase::IEEEsingle(), false);
  Float MZero = Float::zero(details::FloatBase::IEEEsingle(), true);
  Float QNaN = Float::NaN(details::FloatBase::IEEEsingle(), false);
  Float SNaN = Float(details::FloatBase::IEEEsingle(), "snan123");
  Float PNormalValue = Float(details::FloatBase::IEEEsingle(), "0x1p+0");
  Float MNormalValue = Float(details::FloatBase::IEEEsingle(), "-0x1p+0");
  Float PLargestValue = Float::largest(details::FloatBase::IEEEsingle(), false);
  Float MLargestValue = Float::largest(details::FloatBase::IEEEsingle(), true);
  Float PSmallestValue =
      Float::smallest(details::FloatBase::IEEEsingle(), false);
  Float MSmallestValue =
      Float::smallest(details::FloatBase::IEEEsingle(), true);
  Float PSmallestNormalized =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), false);
  Float MSmallestNormalized =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), true);

  auto OverflowStatus = static_cast<details::FloatBase::OpStatus>(
      static_cast<uint8_t>(details::FloatBase::OpStatus::kOverflow) |
      static_cast<uint8_t>(details::FloatBase::OpStatus::kInexact));

  struct {
    Float x;
    Float y;
    const char* result;
    details::FloatBase::OpStatus status;
    details::FloatBase::FloatCategory category;
  } SpecialCaseTests[] = {
      {PInf, PInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, PZero, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MZero, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, PNormalValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MNormalValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, PLargestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MLargestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, PSmallestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MSmallestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, PSmallestNormalized, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MSmallestNormalized, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, PZero, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MZero, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, PNormalValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MNormalValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PLargestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MLargestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PSmallestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MSmallestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PSmallestNormalized, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MSmallestNormalized, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PZero, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PZero, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PZero, PZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, PNormalValue, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PZero, MNormalValue, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PZero, PLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PZero, MLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PZero, PSmallestValue, "-0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PZero, MSmallestValue, "0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PZero, PSmallestNormalized, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PZero, MSmallestNormalized, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MZero, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MZero, PZero, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, PNormalValue, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, MNormalValue, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, PLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, MLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, PSmallestValue, "-0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, MSmallestValue, "0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, PSmallestNormalized, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MZero, MSmallestNormalized, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {QNaN, PInf, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MInf, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PZero, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MZero, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, SNaN, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PNormalValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MNormalValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PLargestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MLargestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PSmallestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MSmallestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PSmallestNormalized, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MSmallestNormalized, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PInf, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MInf, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PZero, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MZero, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, QNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PNormalValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MNormalValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PLargestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MLargestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PSmallestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MSmallestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PSmallestNormalized, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MSmallestNormalized, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PNormalValue, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PNormalValue, PZero, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MZero, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, PNormalValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PNormalValue, MNormalValue, "0x1p+1", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, PLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, PSmallestValue, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MSmallestValue, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, PSmallestNormalized, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MSmallestNormalized, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MNormalValue, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MNormalValue, PZero, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MZero, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MNormalValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MNormalValue, PNormalValue, "-0x1p+1", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MNormalValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MNormalValue, PLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PSmallestValue, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MSmallestValue, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PSmallestNormalized, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MSmallestNormalized, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, PZero, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, MZero, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PLargestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PLargestValue, PNormalValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, MNormalValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, PLargestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PLargestValue, MLargestValue, "inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, PSmallestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, MSmallestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, PSmallestNormalized, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, MSmallestNormalized, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, PZero, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, MZero, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MLargestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MLargestValue, PNormalValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, MNormalValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, PLargestValue, "-inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, MLargestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MLargestValue, PSmallestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, MSmallestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, PSmallestNormalized, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, MSmallestNormalized, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestValue, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestValue, PZero, "0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MZero, "0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestValue, PNormalValue, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MNormalValue, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PSmallestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestValue, MSmallestValue, "0x1p-148",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PSmallestNormalized, "-0x1.fffffcp-127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MSmallestNormalized, "0x1.000002p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestValue, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestValue, PZero, "-0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MZero, "-0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestValue, PNormalValue, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MNormalValue, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PSmallestValue, "-0x1p-148",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MSmallestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestValue, PSmallestNormalized, "-0x1.000002p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MSmallestNormalized, "0x1.fffffcp-127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestNormalized, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestNormalized, PZero, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MZero, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestNormalized, SNaN, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestNormalized, PNormalValue, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MNormalValue, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PSmallestValue, "0x1.fffffcp-127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MSmallestValue, "0x1.000002p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PSmallestNormalized, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestNormalized, MSmallestNormalized, "0x1p-125",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestNormalized, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestNormalized, PZero, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MZero, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestNormalized, SNaN, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestNormalized, PNormalValue, "-0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MNormalValue, "0x1p+0",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PSmallestValue, "-0x1.000002p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MSmallestValue, "-0x1.fffffcp-127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PSmallestNormalized, "-0x1p-125",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MSmallestNormalized, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero}};

  for (size_t i = 0; i < std::size(SpecialCaseTests); ++i) {
    Float x(SpecialCaseTests[i].x);
    Float y(SpecialCaseTests[i].y);
    auto status =
        x.sub(y, details::FloatBase::RoundingMode::kNearestTiesToEven);

    Float result(details::FloatBase::IEEEsingle(), SpecialCaseTests[i].result);

    EXPECT_TRUE(result.bitwise_equal(x));
    EXPECT_EQ(SpecialCaseTests[i].status, status);
    EXPECT_EQ(SpecialCaseTests[i].category, x.category());
  }
}

TEST(FloatTest, multiply) {
  // Test Special Cases against each other and normal values.

  Float PInf = Float::inf(details::FloatBase::IEEEsingle(), false);
  Float MInf = Float::inf(details::FloatBase::IEEEsingle(), true);
  Float PZero = Float::zero(details::FloatBase::IEEEsingle(), false);
  Float MZero = Float::zero(details::FloatBase::IEEEsingle(), true);
  Float QNaN = Float::NaN(details::FloatBase::IEEEsingle(), false);
  Float SNaN = Float(details::FloatBase::IEEEsingle(), "snan123");
  Float PNormalValue = Float(details::FloatBase::IEEEsingle(), "0x1p+0");
  Float MNormalValue = Float(details::FloatBase::IEEEsingle(), "-0x1p+0");
  Float PLargestValue = Float::largest(details::FloatBase::IEEEsingle(), false);
  Float MLargestValue = Float::largest(details::FloatBase::IEEEsingle(), true);
  Float PSmallestValue =
      Float::smallest(details::FloatBase::IEEEsingle(), false);
  Float MSmallestValue =
      Float::smallest(details::FloatBase::IEEEsingle(), true);
  Float PSmallestNormalized =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), false);
  Float MSmallestNormalized =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), true);

  Float MaxQuad(details::FloatBase::IEEEquad(),
                "0x1.ffffffffffffffffffffffffffffp+16383");
  Float MinQuad(details::FloatBase::IEEEquad(),
                "0x0.0000000000000000000000000001p-16382");
  Float NMinQuad(details::FloatBase::IEEEquad(),
                 "-0x0.0000000000000000000000000001p-16382");

  auto OverflowStatus = static_cast<details::FloatBase::OpStatus>(
      static_cast<uint8_t>(details::FloatBase::OpStatus::kOverflow) |
      static_cast<uint8_t>(details::FloatBase::OpStatus::kInexact));
  auto UnderflowStatus = static_cast<details::FloatBase::OpStatus>(
      static_cast<uint8_t>(details::FloatBase::OpStatus::kUnderflow) |
      static_cast<uint8_t>(details::FloatBase::OpStatus::kInexact));

  struct {
    Float x;
    Float y;
    const char* result;
    details::FloatBase::OpStatus status;
    details::FloatBase::FloatCategory category;
    details::FloatBase::RoundingMode roundingMode =
        details::FloatBase::RoundingMode::kNearestTiesToEven;
  } SpecialCaseTests[] = {
      {PInf, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, PZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, MZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, PNormalValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MNormalValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, PLargestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MLargestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, PSmallestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MSmallestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, PSmallestNormalized, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MSmallestNormalized, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, MZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, PNormalValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MNormalValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PLargestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MLargestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PSmallestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MSmallestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PSmallestNormalized, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MSmallestNormalized, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PZero, PInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, MInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, PZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MZero, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, PNormalValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MNormalValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, PLargestValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MLargestValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, PSmallestValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MSmallestValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, PSmallestNormalized, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MSmallestNormalized, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, PInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, MInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, PZero, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, PNormalValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MNormalValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, PLargestValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MLargestValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, PSmallestValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MSmallestValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, PSmallestNormalized, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MSmallestNormalized, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {QNaN, PInf, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MInf, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PZero, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MZero, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, SNaN, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PNormalValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MNormalValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PLargestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MLargestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PSmallestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MSmallestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PSmallestNormalized, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MSmallestNormalized, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PInf, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MInf, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PZero, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MZero, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, QNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PNormalValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MNormalValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PLargestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MLargestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PSmallestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MSmallestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PSmallestNormalized, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MSmallestNormalized, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PNormalValue, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PNormalValue, PZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PNormalValue, MZero, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PNormalValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, PNormalValue, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MNormalValue, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, PLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, PSmallestValue, "0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MSmallestValue, "-0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, PSmallestNormalized, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MSmallestNormalized, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MNormalValue, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MNormalValue, PZero, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MNormalValue, MZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MNormalValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MNormalValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MNormalValue, PNormalValue, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MNormalValue, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PLargestValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MLargestValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PSmallestValue, "-0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MSmallestValue, "0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PSmallestNormalized, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MSmallestNormalized, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, PZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PLargestValue, MZero, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PLargestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PLargestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PLargestValue, PNormalValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, MNormalValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, PLargestValue, "inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, MLargestValue, "-inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, PSmallestValue, "0x1.fffffep-22",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, MSmallestValue, "-0x1.fffffep-22",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, PSmallestNormalized, "0x1.fffffep+1",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, MSmallestNormalized, "-0x1.fffffep+1",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, PZero, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MLargestValue, MZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MLargestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MLargestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MLargestValue, PNormalValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, MNormalValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, PLargestValue, "-inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, MLargestValue, "inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, PSmallestValue, "-0x1.fffffep-22",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, MSmallestValue, "0x1.fffffep-22",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, PSmallestNormalized, "-0x1.fffffep+1",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, MSmallestNormalized, "0x1.fffffep+1",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestValue, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestValue, PZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestValue, MZero, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestValue, PNormalValue, "0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MNormalValue, "-0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PLargestValue, "0x1.fffffep-22",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MLargestValue, "-0x1.fffffep-22",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PSmallestValue, "0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestValue, MSmallestValue, "-0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestValue, PSmallestNormalized, "0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestValue, MSmallestNormalized, "-0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestValue, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestValue, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestValue, PZero, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestValue, MZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestValue, PNormalValue, "-0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MNormalValue, "0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PLargestValue, "-0x1.fffffep-22",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MLargestValue, "0x1.fffffep-22",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PSmallestValue, "-0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestValue, MSmallestValue, "0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestValue, PSmallestNormalized, "-0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestValue, MSmallestNormalized, "0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestNormalized, PInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestNormalized, MInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestNormalized, PZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestNormalized, MZero, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestNormalized, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestNormalized, SNaN, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestNormalized, PNormalValue, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MNormalValue, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PLargestValue, "0x1.fffffep+1",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MLargestValue, "-0x1.fffffep+1",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PSmallestValue, "0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestNormalized, MSmallestValue, "-0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestNormalized, PSmallestNormalized, "0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestNormalized, MSmallestNormalized, "-0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, PInf, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestNormalized, MInf, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestNormalized, PZero, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, MZero, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestNormalized, SNaN, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestNormalized, PNormalValue, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MNormalValue, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PLargestValue, "-0x1.fffffep+1",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MLargestValue, "0x1.fffffep+1",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PSmallestValue, "-0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, MSmallestValue, "0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, PSmallestNormalized, "-0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, MSmallestNormalized, "0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},

      {MaxQuad, MinQuad, "0x1.ffffffffffffffffffffffffffffp-111",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kNearestTiesToEven},
      {MaxQuad, MinQuad, "0x1.ffffffffffffffffffffffffffffp-111",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kTowardPositive},
      {MaxQuad, MinQuad, "0x1.ffffffffffffffffffffffffffffp-111",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kTowardNegative},
      {MaxQuad, MinQuad, "0x1.ffffffffffffffffffffffffffffp-111",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kTowardZero},
      {MaxQuad, MinQuad, "0x1.ffffffffffffffffffffffffffffp-111",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kNearestTiesToAway},

      {MaxQuad, NMinQuad, "-0x1.ffffffffffffffffffffffffffffp-111",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kNearestTiesToEven},
      {MaxQuad, NMinQuad, "-0x1.ffffffffffffffffffffffffffffp-111",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kTowardPositive},
      {MaxQuad, NMinQuad, "-0x1.ffffffffffffffffffffffffffffp-111",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kTowardNegative},
      {MaxQuad, NMinQuad, "-0x1.ffffffffffffffffffffffffffffp-111",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kTowardZero},
      {MaxQuad, NMinQuad, "-0x1.ffffffffffffffffffffffffffffp-111",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kNearestTiesToAway},

      {MaxQuad, MaxQuad, "inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity,
       details::FloatBase::RoundingMode::kNearestTiesToEven},
      {MaxQuad, MaxQuad, "inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity,
       details::FloatBase::RoundingMode::kTowardPositive},
      {MaxQuad, MaxQuad, "0x1.ffffffffffffffffffffffffffffp+16383",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kTowardNegative},
      {MaxQuad, MaxQuad, "0x1.ffffffffffffffffffffffffffffp+16383",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kTowardZero},
      {MaxQuad, MaxQuad, "inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity,
       details::FloatBase::RoundingMode::kNearestTiesToAway},

      {MinQuad, MinQuad, "0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kNearestTiesToEven},
      {MinQuad, MinQuad, "0x0.0000000000000000000000000001p-16382",
       UnderflowStatus, details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kTowardPositive},
      {MinQuad, MinQuad, "0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kTowardNegative},
      {MinQuad, MinQuad, "0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kTowardZero},
      {MinQuad, MinQuad, "0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kNearestTiesToAway},

      {MinQuad, NMinQuad, "-0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kNearestTiesToEven},
      {MinQuad, NMinQuad, "-0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kTowardPositive},
      {MinQuad, NMinQuad, "-0x0.0000000000000000000000000001p-16382",
       UnderflowStatus, details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kTowardNegative},
      {MinQuad, NMinQuad, "-0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kTowardZero},
      {MinQuad, NMinQuad, "-0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kNearestTiesToAway},
  };

  for (size_t i = 0; i < std::size(SpecialCaseTests); ++i) {
    Float x(SpecialCaseTests[i].x);
    Float y(SpecialCaseTests[i].y);
    auto status = x.mul(y, SpecialCaseTests[i].roundingMode);

    Float result(x.sem(), SpecialCaseTests[i].result);

    EXPECT_TRUE(result.bitwise_equal(x));
    EXPECT_EQ(SpecialCaseTests[i].status, status);
    EXPECT_EQ(SpecialCaseTests[i].category, x.category());
  }
}

TEST(FloatTest, divide) {
  // Test Special Cases against each other and normal values.

  Float PInf = Float::inf(details::FloatBase::IEEEsingle(), false);
  Float MInf = Float::inf(details::FloatBase::IEEEsingle(), true);
  Float PZero = Float::zero(details::FloatBase::IEEEsingle(), false);
  Float MZero = Float::zero(details::FloatBase::IEEEsingle(), true);
  Float QNaN = Float::NaN(details::FloatBase::IEEEsingle(), false);
  Float SNaN = Float(details::FloatBase::IEEEsingle(), "snan123");
  Float PNormalValue = Float(details::FloatBase::IEEEsingle(), "0x1p+0");
  Float MNormalValue = Float(details::FloatBase::IEEEsingle(), "-0x1p+0");
  Float PLargestValue = Float::largest(details::FloatBase::IEEEsingle(), false);
  Float MLargestValue = Float::largest(details::FloatBase::IEEEsingle(), true);
  Float PSmallestValue =
      Float::smallest(details::FloatBase::IEEEsingle(), false);
  Float MSmallestValue =
      Float::smallest(details::FloatBase::IEEEsingle(), true);
  Float PSmallestNormalized =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), false);
  Float MSmallestNormalized =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), true);

  Float MaxQuad(details::FloatBase::IEEEquad(),
                "0x1.ffffffffffffffffffffffffffffp+16383");
  Float MinQuad(details::FloatBase::IEEEquad(),
                "0x0.0000000000000000000000000001p-16382");
  Float NMinQuad(details::FloatBase::IEEEquad(),
                 "-0x0.0000000000000000000000000001p-16382");
  auto OverflowStatus = static_cast<details::FloatBase::OpStatus>(
      static_cast<uint8_t>(details::FloatBase::OpStatus::kOverflow) |
      static_cast<uint8_t>(details::FloatBase::OpStatus::kInexact));
  auto UnderflowStatus = static_cast<details::FloatBase::OpStatus>(
      static_cast<uint8_t>(details::FloatBase::OpStatus::kUnderflow) |
      static_cast<uint8_t>(details::FloatBase::OpStatus::kInexact));
  struct {
    Float x;
    Float y;
    const char* result;
    details::FloatBase::OpStatus status;
    details::FloatBase::FloatCategory category;
    details::FloatBase::RoundingMode roundingMode =
        details::FloatBase::RoundingMode::kNearestTiesToEven;
  } SpecialCaseTests[] = {
      {PInf, PInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, MInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, PZero, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MZero, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, PNormalValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MNormalValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, PLargestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MLargestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, PSmallestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MSmallestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, PSmallestNormalized, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PInf, MSmallestNormalized, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, MInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, PZero, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MZero, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, PNormalValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MNormalValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PLargestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MLargestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PSmallestValue, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MSmallestValue, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, PSmallestNormalized, "-inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {MInf, MSmallestNormalized, "inf", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kInfinity},
      {PZero, PInf, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MInf, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, PZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, MZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, PNormalValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MNormalValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, PLargestValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MLargestValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, PSmallestValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MSmallestValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, PSmallestNormalized, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MSmallestNormalized, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, PInf, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MInf, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, PZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, MZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, PNormalValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MNormalValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, PLargestValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MLargestValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, PSmallestValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MSmallestValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, PSmallestNormalized, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MSmallestNormalized, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {QNaN, PInf, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MInf, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PZero, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MZero, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, SNaN, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PNormalValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MNormalValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PLargestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MLargestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PSmallestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MSmallestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PSmallestNormalized, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MSmallestNormalized, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PInf, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MInf, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PZero, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MZero, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, QNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PNormalValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MNormalValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PLargestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MLargestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PSmallestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MSmallestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PSmallestNormalized, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MSmallestNormalized, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, PInf, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PNormalValue, MInf, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PNormalValue, PZero, "inf", details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {PNormalValue, MZero, "-inf", details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {PNormalValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, PNormalValue, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MNormalValue, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, PLargestValue, "0x1p-128", UnderflowStatus,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MLargestValue, "-0x1p-128", UnderflowStatus,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, PSmallestValue, "inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {PNormalValue, MSmallestValue, "-inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {PNormalValue, PSmallestNormalized, "0x1p+126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MSmallestNormalized, "-0x1p+126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PInf, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MNormalValue, MInf, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MNormalValue, PZero, "-inf", details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {MNormalValue, MZero, "inf", details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {MNormalValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MNormalValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MNormalValue, PNormalValue, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MNormalValue, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PLargestValue, "-0x1p-128", UnderflowStatus,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MLargestValue, "0x1p-128", UnderflowStatus,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PSmallestValue, "-inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {MNormalValue, MSmallestValue, "inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {MNormalValue, PSmallestNormalized, "-0x1p+126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MSmallestNormalized, "0x1p+126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, PInf, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PLargestValue, MInf, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PLargestValue, PZero, "inf", details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, MZero, "-inf", details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PLargestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PLargestValue, PNormalValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, MNormalValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, PLargestValue, "0x1p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, MLargestValue, "-0x1p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, PSmallestValue, "inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, MSmallestValue, "-inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, PSmallestNormalized, "inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {PLargestValue, MSmallestNormalized, "-inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, PInf, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MLargestValue, MInf, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MLargestValue, PZero, "-inf", details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, MZero, "inf", details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MLargestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MLargestValue, PNormalValue, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, MNormalValue, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, PLargestValue, "-0x1p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, MLargestValue, "0x1p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, PSmallestValue, "-inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, MSmallestValue, "inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, PSmallestNormalized, "-inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {MLargestValue, MSmallestNormalized, "inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestValue, PInf, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestValue, MInf, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestValue, PZero, "inf", details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestValue, MZero, "-inf", details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestValue, PNormalValue, "0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MNormalValue, "-0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PLargestValue, "0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestValue, MLargestValue, "-0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestValue, PSmallestValue, "0x1p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MSmallestValue, "-0x1p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PSmallestNormalized, "0x1p-23",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MSmallestNormalized, "-0x1p-23",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PInf, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestValue, MInf, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestValue, PZero, "-inf", details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestValue, MZero, "inf", details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestValue, PNormalValue, "-0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MNormalValue, "0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PLargestValue, "-0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestValue, MLargestValue, "0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestValue, PSmallestValue, "-0x1p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MSmallestValue, "0x1p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PSmallestNormalized, "-0x1p-23",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MSmallestNormalized, "0x1p-23",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PInf, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestNormalized, MInf, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestNormalized, PZero, "inf",
       details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestNormalized, MZero, "-inf",
       details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {PSmallestNormalized, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestNormalized, SNaN, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestNormalized, PNormalValue, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MNormalValue, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PLargestValue, "0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestNormalized, MLargestValue, "-0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestNormalized, PSmallestValue, "0x1p+23",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MSmallestValue, "-0x1p+23",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PSmallestNormalized, "0x1p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MSmallestNormalized, "-0x1p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PInf, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, MInf, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, PZero, "-inf",
       details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestNormalized, MZero, "inf",
       details::FloatBase::OpStatus::kDivByZero,
       details::FloatBase::FloatCategory::kInfinity},
      {MSmallestNormalized, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestNormalized, SNaN, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestNormalized, PNormalValue, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MNormalValue, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PLargestValue, "-0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, MLargestValue, "0x0p+0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, PSmallestValue, "-0x1p+23",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MSmallestValue, "0x1p+23",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PSmallestNormalized, "-0x1p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MSmallestNormalized, "0x1p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},

      {MaxQuad, NMinQuad, "-inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity,
       details::FloatBase::RoundingMode::kNearestTiesToEven},
      {MaxQuad, NMinQuad, "-0x1.ffffffffffffffffffffffffffffp+16383",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kTowardPositive},
      {MaxQuad, NMinQuad, "-inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity,
       details::FloatBase::RoundingMode::kTowardNegative},
      {MaxQuad, NMinQuad, "-0x1.ffffffffffffffffffffffffffffp+16383",
       details::FloatBase::OpStatus::kInexact,
       details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kTowardZero},
      {MaxQuad, NMinQuad, "-inf", OverflowStatus,
       details::FloatBase::FloatCategory::kInfinity,
       details::FloatBase::RoundingMode::kNearestTiesToAway},

      {MinQuad, MaxQuad, "0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kNearestTiesToEven},
      {MinQuad, MaxQuad, "0x0.0000000000000000000000000001p-16382",
       UnderflowStatus, details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kTowardPositive},
      {MinQuad, MaxQuad, "0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kTowardNegative},
      {MinQuad, MaxQuad, "0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kTowardZero},
      {MinQuad, MaxQuad, "0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kNearestTiesToAway},

      {NMinQuad, MaxQuad, "-0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kNearestTiesToEven},
      {NMinQuad, MaxQuad, "-0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kTowardPositive},
      {NMinQuad, MaxQuad, "-0x0.0000000000000000000000000001p-16382",
       UnderflowStatus, details::FloatBase::FloatCategory::kNormal,
       details::FloatBase::RoundingMode::kTowardNegative},
      {NMinQuad, MaxQuad, "-0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kTowardZero},
      {NMinQuad, MaxQuad, "-0", UnderflowStatus,
       details::FloatBase::FloatCategory::kZero,
       details::FloatBase::RoundingMode::kNearestTiesToAway},
  };

  for (size_t i = 0; i < std::size(SpecialCaseTests); ++i) {
    Float x(SpecialCaseTests[i].x);
    Float y(SpecialCaseTests[i].y);
    auto status = x.div(y, SpecialCaseTests[i].roundingMode);

    Float result(x.sem(), SpecialCaseTests[i].result);

    EXPECT_TRUE(result.bitwise_equal(x));
    EXPECT_EQ(SpecialCaseTests[i].status, status);
    EXPECT_EQ(SpecialCaseTests[i].category, x.category());
  }
}

TEST(FloatTest, operatorOverloads) {
  // This is mostly testing that these operator overloads compile.
  Float One = Float(details::FloatBase::IEEEsingle(), "0x1p+0");
  Float Two = Float(details::FloatBase::IEEEsingle(), "0x2p+0");
  EXPECT_TRUE(Two.bitwise_equal(One + One));
  EXPECT_TRUE(One.bitwise_equal(Two - One));
  EXPECT_TRUE(Two.bitwise_equal(One * Two));
  EXPECT_TRUE(One.bitwise_equal(Two / Two));
}

TEST(FloatTest, Comparisons) {
  enum { MNan, MInf, MBig, MOne, MZer, PZer, POne, PBig, PInf, PNan, NumVals };
  Float Vals[NumVals] = {
      Float::NaN(details::FloatBase::IEEEsingle(), true),
      Float::inf(details::FloatBase::IEEEsingle(), true),
      Float::largest(details::FloatBase::IEEEsingle(), true),
      Float(details::FloatBase::IEEEsingle(), "-0x1p+0"),
      Float::zero(details::FloatBase::IEEEsingle(), true),
      Float::zero(details::FloatBase::IEEEsingle(), false),
      Float(details::FloatBase::IEEEsingle(), "0x1p+0"),
      Float::largest(details::FloatBase::IEEEsingle(), false),
      Float::inf(details::FloatBase::IEEEsingle(), false),
      Float::NaN(details::FloatBase::IEEEsingle(), false),
  };
  using Relation = void (*)(const Float&, const Float&);
  Relation LT = [](const Float& LHS, const Float& RHS) {
    EXPECT_FALSE(LHS == RHS);
    EXPECT_TRUE(LHS != RHS);
    EXPECT_TRUE(LHS < RHS);
    EXPECT_FALSE(LHS > RHS);
    EXPECT_TRUE(LHS <= RHS);
    EXPECT_FALSE(LHS >= RHS);
  };
  Relation EQ = [](const Float& LHS, const Float& RHS) {
    EXPECT_TRUE(LHS == RHS);
    EXPECT_FALSE(LHS != RHS);
    EXPECT_FALSE(LHS < RHS);
    EXPECT_FALSE(LHS > RHS);
    EXPECT_TRUE(LHS <= RHS);
    EXPECT_TRUE(LHS >= RHS);
  };
  Relation GT = [](const Float& LHS, const Float& RHS) {
    EXPECT_FALSE(LHS == RHS);
    EXPECT_TRUE(LHS != RHS);
    EXPECT_FALSE(LHS < RHS);
    EXPECT_TRUE(LHS > RHS);
    EXPECT_FALSE(LHS <= RHS);
    EXPECT_TRUE(LHS >= RHS);
  };
  Relation UN = [](const Float& LHS, const Float& RHS) {
    EXPECT_FALSE(LHS == RHS);
    EXPECT_TRUE(LHS != RHS);
    EXPECT_FALSE(LHS < RHS);
    EXPECT_FALSE(LHS > RHS);
    EXPECT_FALSE(LHS <= RHS);
    EXPECT_FALSE(LHS >= RHS);
  };
  Relation Relations[NumVals][NumVals] = {
      //          -N  -I  -B  -1  -0  +0  +1  +B  +I  +N
      /* MNan */ {UN, UN, UN, UN, UN, UN, UN, UN, UN, UN},
      /* MInf */ {UN, EQ, LT, LT, LT, LT, LT, LT, LT, UN},
      /* MBig */ {UN, GT, EQ, LT, LT, LT, LT, LT, LT, UN},
      /* MOne */ {UN, GT, GT, EQ, LT, LT, LT, LT, LT, UN},
      /* MZer */ {UN, GT, GT, GT, EQ, EQ, LT, LT, LT, UN},
      /* PZer */ {UN, GT, GT, GT, EQ, EQ, LT, LT, LT, UN},
      /* POne */ {UN, GT, GT, GT, GT, GT, EQ, LT, LT, UN},
      /* PBig */ {UN, GT, GT, GT, GT, GT, GT, EQ, LT, UN},
      /* PInf */ {UN, GT, GT, GT, GT, GT, GT, GT, EQ, UN},
      /* PNan */ {UN, UN, UN, UN, UN, UN, UN, UN, UN, UN},
  };
  for (unsigned I = 0; I < NumVals; ++I)
    for (unsigned J = 0; J < NumVals; ++J)
      Relations[I][J](Vals[I], Vals[J]);
}

TEST(FloatTest, ABS) {
  Float PInf = Float::inf(details::FloatBase::IEEEsingle(), false);
  Float MInf = Float::inf(details::FloatBase::IEEEsingle(), true);
  Float PZero = Float::zero(details::FloatBase::IEEEsingle(), false);
  Float MZero = Float::zero(details::FloatBase::IEEEsingle(), true);
  Float PQNaN = Float::NaN(details::FloatBase::IEEEsingle(), false);
  Float MQNaN = Float::NaN(details::FloatBase::IEEEsingle(), true);
  Float PSNaN = Float::sNaN(details::FloatBase::IEEEsingle(), false);
  Float MSNaN = Float::sNaN(details::FloatBase::IEEEsingle(), true);
  Float PNormalValue = Float(details::FloatBase::IEEEsingle(), "0x1p+0");
  Float MNormalValue = Float(details::FloatBase::IEEEsingle(), "-0x1p+0");
  Float PLargestValue = Float::largest(details::FloatBase::IEEEsingle(), false);
  Float MLargestValue = Float::largest(details::FloatBase::IEEEsingle(), true);
  Float PSmallestValue =
      Float::smallest(details::FloatBase::IEEEsingle(), false);
  Float MSmallestValue =
      Float::smallest(details::FloatBase::IEEEsingle(), true);
  Float PSmallestNormalized =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), false);
  Float MSmallestNormalized =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), true);

  EXPECT_TRUE(PInf.bitwise_equal(abs(PInf)));
  EXPECT_TRUE(PInf.bitwise_equal(abs(MInf)));
  EXPECT_TRUE(PZero.bitwise_equal(abs(PZero)));
  EXPECT_TRUE(PZero.bitwise_equal(abs(MZero)));
  EXPECT_TRUE(PQNaN.bitwise_equal(abs(PQNaN)));
  EXPECT_TRUE(PQNaN.bitwise_equal(abs(MQNaN)));
  EXPECT_TRUE(PSNaN.bitwise_equal(abs(PSNaN)));
  EXPECT_TRUE(PSNaN.bitwise_equal(abs(MSNaN)));
  EXPECT_TRUE(PNormalValue.bitwise_equal(abs(PNormalValue)));
  EXPECT_TRUE(PNormalValue.bitwise_equal(abs(MNormalValue)));
  EXPECT_TRUE(PLargestValue.bitwise_equal(abs(PLargestValue)));
  EXPECT_TRUE(PLargestValue.bitwise_equal(abs(MLargestValue)));
  EXPECT_TRUE(PSmallestValue.bitwise_equal(abs(PSmallestValue)));
  EXPECT_TRUE(PSmallestValue.bitwise_equal(abs(MSmallestValue)));
  EXPECT_TRUE(PSmallestNormalized.bitwise_equal(abs(PSmallestNormalized)));
  EXPECT_TRUE(PSmallestNormalized.bitwise_equal(abs(MSmallestNormalized)));
}

TEST(FloatTest, NEG) {
  Float One = Float(details::FloatBase::IEEEsingle(), "1.0");
  Float NegOne = Float(details::FloatBase::IEEEsingle(), "-1.0");
  Float Zero = Float::zero(details::FloatBase::IEEEsingle(), false);
  Float NegZero = Float::zero(details::FloatBase::IEEEsingle(), true);
  Float Inf = Float::inf(details::FloatBase::IEEEsingle(), false);
  Float NegInf = Float::inf(details::FloatBase::IEEEsingle(), true);
  Float QNaN = Float::NaN(details::FloatBase::IEEEsingle(), false);
  Float NegQNaN = Float::NaN(details::FloatBase::IEEEsingle(), true);

  EXPECT_TRUE(NegOne.bitwise_equal(neg(One)));
  EXPECT_TRUE(One.bitwise_equal(neg(NegOne)));
  EXPECT_TRUE(NegZero.bitwise_equal(neg(Zero)));
  EXPECT_TRUE(Zero.bitwise_equal(neg(NegZero)));
  EXPECT_TRUE(NegInf.bitwise_equal(neg(Inf)));
  EXPECT_TRUE(Inf.bitwise_equal(neg(NegInf)));
  EXPECT_TRUE(NegInf.bitwise_equal(neg(Inf)));
  EXPECT_TRUE(Inf.bitwise_equal(neg(NegInf)));
  EXPECT_TRUE(NegQNaN.bitwise_equal(neg(QNaN)));
  EXPECT_TRUE(QNaN.bitwise_equal(neg(NegQNaN)));

  EXPECT_TRUE(NegOne.bitwise_equal(-One));
  EXPECT_TRUE(One.bitwise_equal(-NegOne));
  EXPECT_TRUE(NegZero.bitwise_equal(-Zero));
  EXPECT_TRUE(Zero.bitwise_equal(-NegZero));
  EXPECT_TRUE(NegInf.bitwise_equal(-Inf));
  EXPECT_TRUE(Inf.bitwise_equal(-NegInf));
  EXPECT_TRUE(NegInf.bitwise_equal(-Inf));
  EXPECT_TRUE(Inf.bitwise_equal(-NegInf));
  EXPECT_TRUE(NegQNaN.bitwise_equal(-QNaN));
  EXPECT_TRUE(QNaN.bitwise_equal(-NegQNaN));
}

TEST(FloatTest, ILOGB) {
  EXPECT_EQ(-1074,
            ilogb(Float::smallest(details::FloatBase::IEEEdouble(), false)));
  EXPECT_EQ(-1074,
            ilogb(Float::smallest(details::FloatBase::IEEEdouble(), true)));
  EXPECT_EQ(-1023, ilogb(Float(details::FloatBase::IEEEdouble(),
                               "0x1.ffffffffffffep-1024")));
  EXPECT_EQ(-1023, ilogb(Float(details::FloatBase::IEEEdouble(),
                               "0x1.ffffffffffffep-1023")));
  EXPECT_EQ(-1023, ilogb(Float(details::FloatBase::IEEEdouble(),
                               "-0x1.ffffffffffffep-1023")));
  EXPECT_EQ(-51, ilogb(Float(details::FloatBase::IEEEdouble(), "0x1p-51")));
  EXPECT_EQ(-1023, ilogb(Float(details::FloatBase::IEEEdouble(),
                               "0x1.c60f120d9f87cp-1023")));
  EXPECT_EQ(-2, ilogb(Float(details::FloatBase::IEEEdouble(), "0x0.ffffp-1")));
  EXPECT_EQ(-1023,
            ilogb(Float(details::FloatBase::IEEEdouble(), "0x1.fffep-1023")));
  EXPECT_EQ(1023,
            ilogb(Float::largest(details::FloatBase::IEEEdouble(), false)));
  EXPECT_EQ(1023,
            ilogb(Float::largest(details::FloatBase::IEEEdouble(), true)));

  EXPECT_EQ(0, ilogb(Float(details::FloatBase::IEEEsingle(), "0x1p+0")));
  EXPECT_EQ(0, ilogb(Float(details::FloatBase::IEEEsingle(), "-0x1p+0")));
  EXPECT_EQ(42, ilogb(Float(details::FloatBase::IEEEsingle(), "0x1p+42")));
  EXPECT_EQ(-42, ilogb(Float(details::FloatBase::IEEEsingle(), "0x1p-42")));

  EXPECT_EQ(static_cast<int>(
                static_cast<int>(details::FloatBase::IlogbErrorKinds::kInf)),
            ilogb(Float::inf(details::FloatBase::IEEEsingle(), false)));
  EXPECT_EQ(static_cast<int>(details::FloatBase::IlogbErrorKinds::kInf),
            ilogb(Float::inf(details::FloatBase::IEEEsingle(), true)));
  EXPECT_EQ(static_cast<int>(details::FloatBase::IlogbErrorKinds::kZero),
            ilogb(Float::zero(details::FloatBase::IEEEsingle(), false)));
  EXPECT_EQ(static_cast<int>(details::FloatBase::IlogbErrorKinds::kZero),
            ilogb(Float::zero(details::FloatBase::IEEEsingle(), true)));
  EXPECT_EQ(static_cast<int>(details::FloatBase::IlogbErrorKinds::kNaN),
            ilogb(Float::NaN(details::FloatBase::IEEEsingle(), false)));
  EXPECT_EQ(static_cast<int>(details::FloatBase::IlogbErrorKinds::kNaN),
            ilogb(Float::sNaN(details::FloatBase::IEEEsingle(), false)));

  EXPECT_EQ(127,
            ilogb(Float::largest(details::FloatBase::IEEEsingle(), false)));
  EXPECT_EQ(127, ilogb(Float::largest(details::FloatBase::IEEEsingle(), true)));

  EXPECT_EQ(-149,
            ilogb(Float::smallest(details::FloatBase::IEEEsingle(), false)));
  EXPECT_EQ(-149,
            ilogb(Float::smallest(details::FloatBase::IEEEsingle(), true)));
  EXPECT_EQ(-126, ilogb(Float::smallest_normalized(
                      details::FloatBase::IEEEsingle(), false)));
  EXPECT_EQ(-126, ilogb(Float::smallest_normalized(
                      details::FloatBase::IEEEsingle(), true)));
}

TEST(FloatTest, Scalbn) {

  const details::FloatBase::RoundingMode RM =
      details::FloatBase::RoundingMode::kNearestTiesToEven;
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEsingle(), "0x1p+0")
          .bitwise_equal(scalbn(
              Float(details::FloatBase::IEEEsingle(), "0x1p+0"), 0, RM)));
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEsingle(), "0x1p+42")
          .bitwise_equal(scalbn(
              Float(details::FloatBase::IEEEsingle(), "0x1p+0"), 42, RM)));
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEsingle(), "0x1p-42")
          .bitwise_equal(scalbn(
              Float(details::FloatBase::IEEEsingle(), "0x1p+0"), -42, RM)));

  Float PInf = Float::inf(details::FloatBase::IEEEsingle(), false);
  Float MInf = Float::inf(details::FloatBase::IEEEsingle(), true);
  Float PZero = Float::zero(details::FloatBase::IEEEsingle(), false);
  Float MZero = Float::zero(details::FloatBase::IEEEsingle(), true);
  Float QPNaN = Float::NaN(details::FloatBase::IEEEsingle(), false);
  Float QMNaN = Float::NaN(details::FloatBase::IEEEsingle(), true);
  Float SNaN = Float::sNaN(details::FloatBase::IEEEsingle(), false);

  EXPECT_TRUE(PInf.bitwise_equal(scalbn(PInf, 0, RM)));
  EXPECT_TRUE(MInf.bitwise_equal(scalbn(MInf, 0, RM)));
  EXPECT_TRUE(PZero.bitwise_equal(scalbn(PZero, 0, RM)));
  EXPECT_TRUE(MZero.bitwise_equal(scalbn(MZero, 0, RM)));
  EXPECT_TRUE(QPNaN.bitwise_equal(scalbn(QPNaN, 0, RM)));
  EXPECT_TRUE(QMNaN.bitwise_equal(scalbn(QMNaN, 0, RM)));
  EXPECT_FALSE(scalbn(SNaN, 0, RM).is_signaling());

  Float ScalbnSNaN = scalbn(SNaN, 1, RM);
  EXPECT_TRUE(ScalbnSNaN.isNaN() && !ScalbnSNaN.is_signaling());

  // Make sure highest bit of payload is preserved.
  const Int Payload(64, (UINT64_C(1) << 50) | (UINT64_C(1) << 49) |
                            (UINT64_C(1234) << 32) | 1);

  Float SNaNWithPayload =
      Float::sNaN(details::FloatBase::IEEEdouble(), false, &Payload);
  Float QuietPayload = scalbn(SNaNWithPayload, 1, RM);
  EXPECT_TRUE(QuietPayload.isNaN() && !QuietPayload.is_signaling());
  EXPECT_EQ(Payload, QuietPayload.bit_cast_to_int().lo_bits(51));

  EXPECT_TRUE(PInf.bitwise_equal(
      scalbn(Float(details::FloatBase::IEEEsingle(), "0x1p+0"), 128, RM)));
  EXPECT_TRUE(MInf.bitwise_equal(
      scalbn(Float(details::FloatBase::IEEEsingle(), "-0x1p+0"), 128, RM)));
  EXPECT_TRUE(PInf.bitwise_equal(
      scalbn(Float(details::FloatBase::IEEEsingle(), "0x1p+127"), 1, RM)));
  EXPECT_TRUE(PZero.bitwise_equal(
      scalbn(Float(details::FloatBase::IEEEsingle(), "0x1p-127"), -127, RM)));
  EXPECT_TRUE(MZero.bitwise_equal(
      scalbn(Float(details::FloatBase::IEEEsingle(), "-0x1p-127"), -127, RM)));
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEsingle(), "-0x1p-149")
          .bitwise_equal(scalbn(
              Float(details::FloatBase::IEEEsingle(), "-0x1p-127"), -22, RM)));
  EXPECT_TRUE(PZero.bitwise_equal(
      scalbn(Float(details::FloatBase::IEEEsingle(), "0x1p-126"), -24, RM)));

  Float SmallestF64 = Float::smallest(details::FloatBase::IEEEdouble(), false);
  Float NegSmallestF64 =
      Float::smallest(details::FloatBase::IEEEdouble(), true);

  Float LargestF64 = Float::largest(details::FloatBase::IEEEdouble(), false);
  Float NegLargestF64 = Float::largest(details::FloatBase::IEEEdouble(), true);

  Float SmallestNormalizedF64 =
      Float::smallest_normalized(details::FloatBase::IEEEdouble(), false);
  Float NegSmallestNormalizedF64 =
      Float::smallest_normalized(details::FloatBase::IEEEdouble(), true);

  Float LargestDenormalF64(details::FloatBase::IEEEdouble(),
                           "0x1.ffffffffffffep-1023");
  Float NegLargestDenormalF64(details::FloatBase::IEEEdouble(),
                              "-0x1.ffffffffffffep-1023");

  EXPECT_TRUE(SmallestF64.bitwise_equal(
      scalbn(Float(details::FloatBase::IEEEdouble(), "0x1p-1074"), 0, RM)));
  EXPECT_TRUE(NegSmallestF64.bitwise_equal(
      scalbn(Float(details::FloatBase::IEEEdouble(), "-0x1p-1074"), 0, RM)));

  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1p+1023")
                  .bitwise_equal(scalbn(SmallestF64, 2097, RM)));

  EXPECT_TRUE(scalbn(SmallestF64, -2097, RM).is_pos_zero());
  EXPECT_TRUE(scalbn(SmallestF64, -2098, RM).is_pos_zero());
  EXPECT_TRUE(scalbn(SmallestF64, -2099, RM).is_pos_zero());
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1p+1022")
                  .bitwise_equal(scalbn(SmallestF64, 2096, RM)));
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1p+1023")
                  .bitwise_equal(scalbn(SmallestF64, 2097, RM)));
  EXPECT_TRUE(scalbn(SmallestF64, 2098, RM).is_infinity());
  EXPECT_TRUE(scalbn(SmallestF64, 2099, RM).is_infinity());

  // Test for integer overflows when adding to exponent.
  EXPECT_TRUE(scalbn(SmallestF64, -INT_MAX, RM).is_pos_zero());
  EXPECT_TRUE(scalbn(LargestF64, INT_MAX, RM).is_infinity());

  EXPECT_TRUE(
      LargestDenormalF64.bitwise_equal(scalbn(LargestDenormalF64, 0, RM)));
  EXPECT_TRUE(NegLargestDenormalF64.bitwise_equal(
      scalbn(NegLargestDenormalF64, 0, RM)));

  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1.ffffffffffffep-1022")
                  .bitwise_equal(scalbn(LargestDenormalF64, 1, RM)));
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEdouble(), "-0x1.ffffffffffffep-1021")
          .bitwise_equal(scalbn(NegLargestDenormalF64, 2, RM)));

  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1.ffffffffffffep+1")
                  .bitwise_equal(scalbn(LargestDenormalF64, 1024, RM)));
  EXPECT_TRUE(scalbn(LargestDenormalF64, -1023, RM).is_pos_zero());
  EXPECT_TRUE(scalbn(LargestDenormalF64, -1024, RM).is_pos_zero());
  EXPECT_TRUE(scalbn(LargestDenormalF64, -2048, RM).is_pos_zero());
  EXPECT_TRUE(scalbn(LargestDenormalF64, 2047, RM).is_infinity());
  EXPECT_TRUE(scalbn(LargestDenormalF64, 2098, RM).is_infinity());
  EXPECT_TRUE(scalbn(LargestDenormalF64, 2099, RM).is_infinity());

  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1.ffffffffffffep-2")
                  .bitwise_equal(scalbn(LargestDenormalF64, 1021, RM)));
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1.ffffffffffffep-1")
                  .bitwise_equal(scalbn(LargestDenormalF64, 1022, RM)));
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1.ffffffffffffep+0")
                  .bitwise_equal(scalbn(LargestDenormalF64, 1023, RM)));
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1.ffffffffffffep+1023")
                  .bitwise_equal(scalbn(LargestDenormalF64, 2046, RM)));
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1p+974")
                  .bitwise_equal(scalbn(SmallestF64, 2048, RM)));

  Float RandomDenormalF64(details::FloatBase::IEEEdouble(),
                          "0x1.c60f120d9f87cp+51");
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1.c60f120d9f87cp-972")
                  .bitwise_equal(scalbn(RandomDenormalF64, -1023, RM)));
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1.c60f120d9f87cp-1")
                  .bitwise_equal(scalbn(RandomDenormalF64, -52, RM)));
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1.c60f120d9f87cp-2")
                  .bitwise_equal(scalbn(RandomDenormalF64, -53, RM)));
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1.c60f120d9f87cp+0")
                  .bitwise_equal(scalbn(RandomDenormalF64, -51, RM)));

  EXPECT_TRUE(scalbn(RandomDenormalF64, -2097, RM).is_pos_zero());
  EXPECT_TRUE(scalbn(RandomDenormalF64, -2090, RM).is_pos_zero());

  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "-0x1p-1073")
                  .bitwise_equal(scalbn(NegLargestF64, -2097, RM)));

  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "-0x1p-1024")
                  .bitwise_equal(scalbn(NegLargestF64, -2048, RM)));

  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1p-1073")
                  .bitwise_equal(scalbn(LargestF64, -2097, RM)));

  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1p-1074")
                  .bitwise_equal(scalbn(LargestF64, -2098, RM)));
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "-0x1p-1074")
                  .bitwise_equal(scalbn(NegLargestF64, -2098, RM)));
  EXPECT_TRUE(scalbn(NegLargestF64, -2099, RM).is_neg_zero());
  EXPECT_TRUE(scalbn(LargestF64, 1, RM).is_infinity());

  EXPECT_TRUE(
      Float(details::FloatBase::IEEEdouble(), "0x1p+0")
          .bitwise_equal(scalbn(
              Float(details::FloatBase::IEEEdouble(), "0x1p+52"), -52, RM)));

  EXPECT_TRUE(
      Float(details::FloatBase::IEEEdouble(), "0x1p-103")
          .bitwise_equal(scalbn(
              Float(details::FloatBase::IEEEdouble(), "0x1p-51"), -52, RM)));
}

TEST(FloatTest, FREXP) {
  const details::FloatBase::RoundingMode RM =
      details::FloatBase::RoundingMode::kNearestTiesToEven;

  Float PZero = Float::zero(details::FloatBase::IEEEdouble(), false);
  Float MZero = Float::zero(details::FloatBase::IEEEdouble(), true);
  Float One(1.0);
  Float MOne(-1.0);
  Float Two(2.0);
  Float MTwo(-2.0);

  Float LargestDenormal(details::FloatBase::IEEEdouble(),
                        "0x1.ffffffffffffep-1023");
  Float NegLargestDenormal(details::FloatBase::IEEEdouble(),
                           "-0x1.ffffffffffffep-1023");

  Float Smallest = Float::smallest(details::FloatBase::IEEEdouble(), false);
  Float NegSmallest = Float::smallest(details::FloatBase::IEEEdouble(), true);

  Float Largest = Float::largest(details::FloatBase::IEEEdouble(), false);
  Float NegLargest = Float::largest(details::FloatBase::IEEEdouble(), true);

  Float PInf = Float::inf(details::FloatBase::IEEEdouble(), false);
  Float MInf = Float::inf(details::FloatBase::IEEEdouble(), true);

  Float QPNaN = Float::NaN(details::FloatBase::IEEEdouble(), false);
  Float QMNaN = Float::NaN(details::FloatBase::IEEEdouble(), true);
  Float SNaN = Float::sNaN(details::FloatBase::IEEEdouble(), false);

  // Make sure highest bit of payload is preserved.
  const Int Payload(64, (UINT64_C(1) << 50) | (UINT64_C(1) << 49) |
                            (UINT64_C(1234) << 32) | 1);

  Float SNaNWithPayload =
      Float::sNaN(details::FloatBase::IEEEdouble(), false, &Payload);

  Float SmallestNormalized =
      Float::smallest_normalized(details::FloatBase::IEEEdouble(), false);
  Float NegSmallestNormalized =
      Float::smallest_normalized(details::FloatBase::IEEEdouble(), true);

  int Exp;
  Float Frac(details::FloatBase::IEEEdouble());

  Frac = frexp(PZero, Exp, RM);
  EXPECT_EQ(0, Exp);
  EXPECT_TRUE(Frac.is_pos_zero());

  Frac = frexp(MZero, Exp, RM);
  EXPECT_EQ(0, Exp);
  EXPECT_TRUE(Frac.is_neg_zero());

  Frac = frexp(One, Exp, RM);
  EXPECT_EQ(1, Exp);
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEdouble(), "0x1p-1").bitwise_equal(Frac));

  Frac = frexp(MOne, Exp, RM);
  EXPECT_EQ(1, Exp);
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEdouble(), "-0x1p-1").bitwise_equal(Frac));

  Frac = frexp(LargestDenormal, Exp, RM);
  EXPECT_EQ(-1022, Exp);
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1.ffffffffffffep-1")
                  .bitwise_equal(Frac));

  Frac = frexp(NegLargestDenormal, Exp, RM);
  EXPECT_EQ(-1022, Exp);
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "-0x1.ffffffffffffep-1")
                  .bitwise_equal(Frac));

  Frac = frexp(Smallest, Exp, RM);
  EXPECT_EQ(-1073, Exp);
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEdouble(), "0x1p-1").bitwise_equal(Frac));

  Frac = frexp(NegSmallest, Exp, RM);
  EXPECT_EQ(-1073, Exp);
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEdouble(), "-0x1p-1").bitwise_equal(Frac));

  Frac = frexp(Largest, Exp, RM);
  EXPECT_EQ(1024, Exp);
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1.fffffffffffffp-1")
                  .bitwise_equal(Frac));

  Frac = frexp(NegLargest, Exp, RM);
  EXPECT_EQ(1024, Exp);
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "-0x1.fffffffffffffp-1")
                  .bitwise_equal(Frac));

  Frac = frexp(PInf, Exp, RM);
  EXPECT_EQ(INT_MAX, Exp);
  EXPECT_TRUE(Frac.is_infinity() && !Frac.is_negative());

  Frac = frexp(MInf, Exp, RM);
  EXPECT_EQ(INT_MAX, Exp);
  EXPECT_TRUE(Frac.is_infinity() && Frac.is_negative());

  Frac = frexp(QPNaN, Exp, RM);
  EXPECT_EQ(INT_MIN, Exp);
  EXPECT_TRUE(Frac.isNaN());

  Frac = frexp(QMNaN, Exp, RM);
  EXPECT_EQ(INT_MIN, Exp);
  EXPECT_TRUE(Frac.isNaN());

  Frac = frexp(SNaN, Exp, RM);
  EXPECT_EQ(INT_MIN, Exp);
  EXPECT_TRUE(Frac.isNaN() && !Frac.is_signaling());

  Frac = frexp(SNaNWithPayload, Exp, RM);
  EXPECT_EQ(INT_MIN, Exp);
  EXPECT_TRUE(Frac.isNaN() && !Frac.is_signaling());
  EXPECT_EQ(Payload, Frac.bit_cast_to_int().lo_bits(51));

  Frac = frexp(Float(details::FloatBase::IEEEdouble(), "0x0.ffffp-1"), Exp, RM);
  EXPECT_EQ(-1, Exp);
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1.fffep-1")
                  .bitwise_equal(Frac));

  Frac = frexp(Float(details::FloatBase::IEEEdouble(), "0x1p-51"), Exp, RM);
  EXPECT_EQ(-50, Exp);
  EXPECT_TRUE(
      Float(details::FloatBase::IEEEdouble(), "0x1p-1").bitwise_equal(Frac));

  Frac = frexp(Float(details::FloatBase::IEEEdouble(), "0x1.c60f120d9f87cp+51"),
               Exp, RM);
  EXPECT_EQ(52, Exp);
  EXPECT_TRUE(Float(details::FloatBase::IEEEdouble(), "0x1.c60f120d9f87cp-1")
                  .bitwise_equal(Frac));
}

TEST(FloatTest, mod) {
  {
    Float f1(details::FloatBase::IEEEdouble(), "1.5");
    Float f2(details::FloatBase::IEEEdouble(), "1.0");
    Float expected(details::FloatBase::IEEEdouble(), "0.5");
    EXPECT_EQ(f1.mod(f2), details::FloatBase::OpStatus::kOK);
    EXPECT_TRUE(f1.bitwise_equal(expected));
  }
  {
    Float f1(details::FloatBase::IEEEdouble(), "0.5");
    Float f2(details::FloatBase::IEEEdouble(), "1.0");
    Float expected(details::FloatBase::IEEEdouble(), "0.5");
    EXPECT_EQ(f1.mod(f2), details::FloatBase::OpStatus::kOK);
    EXPECT_TRUE(f1.bitwise_equal(expected));
  }
  {
    Float f1(details::FloatBase::IEEEdouble(), "0x1.3333333333333p-2");  // 0.3
    Float f2(details::FloatBase::IEEEdouble(), "0x1.47ae147ae147bp-7");  // 0.01
    Float expected(details::FloatBase::IEEEdouble(),
                   "0x1.47ae147ae1471p-7");  // 0.009999999999999983
    EXPECT_EQ(f1.mod(f2), details::FloatBase::OpStatus::kOK);
    EXPECT_TRUE(f1.bitwise_equal(expected));
  }
  {
    Float f1(details::FloatBase::IEEEdouble(),
             "0x1p64");  // 1.8446744073709552e19
    Float f2(details::FloatBase::IEEEdouble(), "1.5");
    Float expected(details::FloatBase::IEEEdouble(), "1.0");
    EXPECT_EQ(f1.mod(f2), details::FloatBase::OpStatus::kOK);
    EXPECT_TRUE(f1.bitwise_equal(expected));
  }
  {
    Float f1(details::FloatBase::IEEEdouble(), "0x1p1000");
    Float f2(details::FloatBase::IEEEdouble(), "0x1p-1000");
    Float expected(details::FloatBase::IEEEdouble(), "0.0");
    EXPECT_EQ(f1.mod(f2), details::FloatBase::OpStatus::kOK);
    EXPECT_TRUE(f1.bitwise_equal(expected));
  }
  {
    Float f1(details::FloatBase::IEEEdouble(), "0.0");
    Float f2(details::FloatBase::IEEEdouble(), "1.0");
    Float expected(details::FloatBase::IEEEdouble(), "0.0");
    EXPECT_EQ(f1.mod(f2), details::FloatBase::OpStatus::kOK);
    EXPECT_TRUE(f1.bitwise_equal(expected));
  }
  {
    Float f1(details::FloatBase::IEEEdouble(), "1.0");
    Float f2(details::FloatBase::IEEEdouble(), "0.0");
    EXPECT_EQ(f1.mod(f2), details::FloatBase::OpStatus::kInvalidOp);
    EXPECT_TRUE(f1.isNaN());
  }
  {
    Float f1(details::FloatBase::IEEEdouble(), "0.0");
    Float f2(details::FloatBase::IEEEdouble(), "0.0");
    EXPECT_EQ(f1.mod(f2), details::FloatBase::OpStatus::kInvalidOp);
    EXPECT_TRUE(f1.isNaN());
  }
  {
    Float f1 = Float::inf(details::FloatBase::IEEEdouble(), false);
    Float f2(details::FloatBase::IEEEdouble(), "1.0");
    EXPECT_EQ(f1.mod(f2), details::FloatBase::OpStatus::kInvalidOp);
    EXPECT_TRUE(f1.isNaN());
  }
  {
    Float f1(details::FloatBase::IEEEdouble(), "-4.0");
    Float f2(details::FloatBase::IEEEdouble(), "-2.0");
    Float expected(details::FloatBase::IEEEdouble(), "-0.0");
    EXPECT_EQ(f1.mod(f2), details::FloatBase::OpStatus::kOK);
    EXPECT_TRUE(f1.bitwise_equal(expected));
  }
  {
    Float f1(details::FloatBase::IEEEdouble(), "-4.0");
    Float f2(details::FloatBase::IEEEdouble(), "2.0");
    Float expected(details::FloatBase::IEEEdouble(), "-0.0");
    EXPECT_EQ(f1.mod(f2), details::FloatBase::OpStatus::kOK);
    EXPECT_TRUE(f1.bitwise_equal(expected));
  }
}

TEST(FloatTest, rem) {
  // Test Special Cases against each other and normal values.

  Float PInf = Float::inf(details::FloatBase::IEEEsingle(), false);
  Float MInf = Float::inf(details::FloatBase::IEEEsingle(), true);
  Float PZero = Float::zero(details::FloatBase::IEEEsingle(), false);
  Float MZero = Float::zero(details::FloatBase::IEEEsingle(), true);
  Float QNaN = Float::NaN(details::FloatBase::IEEEsingle(), false);
  Float SNaN = Float(details::FloatBase::IEEEsingle(), "snan123");
  Float PNormalValue = Float(details::FloatBase::IEEEsingle(), "0x1p+0");
  Float MNormalValue = Float(details::FloatBase::IEEEsingle(), "-0x1p+0");
  Float PLargestValue = Float::largest(details::FloatBase::IEEEsingle(), false);
  Float MLargestValue = Float::largest(details::FloatBase::IEEEsingle(), true);
  Float PSmallestValue =
      Float::smallest(details::FloatBase::IEEEsingle(), false);
  Float MSmallestValue =
      Float::smallest(details::FloatBase::IEEEsingle(), true);
  Float PSmallestNormalized =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), false);
  Float MSmallestNormalized =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), true);

  Float PVal1(details::FloatBase::IEEEsingle(), "0x1.fffffep+126");
  Float MVal1(details::FloatBase::IEEEsingle(), "-0x1.fffffep+126");
  Float PVal2(details::FloatBase::IEEEsingle(), "0x1.fffffep-126");
  Float MVal2(details::FloatBase::IEEEsingle(), "-0x1.fffffep-126");
  Float PVal3(details::FloatBase::IEEEsingle(), "0x1p-125");
  Float MVal3(details::FloatBase::IEEEsingle(), "-0x1p-125");
  Float PVal4(details::FloatBase::IEEEsingle(), "0x1p+127");
  Float MVal4(details::FloatBase::IEEEsingle(), "-0x1p+127");
  Float PVal5(details::FloatBase::IEEEsingle(), "1.5");
  Float MVal5(details::FloatBase::IEEEsingle(), "-1.5");
  Float PVal6(details::FloatBase::IEEEsingle(), "1");
  Float MVal6(details::FloatBase::IEEEsingle(), "-1");

  struct {
    Float x;
    Float y;
    const char* result;
    details::FloatBase::OpStatus status;
    details::FloatBase::FloatCategory category;
  } SpecialCaseTests[] = {
      {PInf, PInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, MInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, PZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, MZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, PNormalValue, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, MNormalValue, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, PLargestValue, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, MLargestValue, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, PSmallestValue, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, MSmallestValue, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, PSmallestNormalized, "nan",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PInf, MSmallestNormalized, "nan",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, PInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, MInf, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, PZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, MZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, PNormalValue, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, MNormalValue, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, PLargestValue, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, MLargestValue, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, PSmallestValue, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, MSmallestValue, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, PSmallestNormalized, "nan",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MInf, MSmallestNormalized, "nan",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, PInf, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MInf, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, PZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, MZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PZero, PNormalValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MNormalValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, PLargestValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MLargestValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, PSmallestValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MSmallestValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, PSmallestNormalized, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PZero, MSmallestNormalized, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, PInf, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MInf, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, PZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, MZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MZero, PNormalValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MNormalValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, PLargestValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MLargestValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, PSmallestValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MSmallestValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, PSmallestNormalized, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MZero, MSmallestNormalized, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {QNaN, PInf, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MInf, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PZero, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MZero, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, SNaN, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PNormalValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MNormalValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PLargestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MLargestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PSmallestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MSmallestValue, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, PSmallestNormalized, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {QNaN, MSmallestNormalized, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PInf, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MInf, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PZero, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MZero, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, QNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PNormalValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MNormalValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PLargestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MLargestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PSmallestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MSmallestValue, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, PSmallestNormalized, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {SNaN, MSmallestNormalized, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, PInf, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MInf, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, PZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, MZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PNormalValue, PNormalValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PNormalValue, MNormalValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PNormalValue, PLargestValue, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, MLargestValue, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PNormalValue, PSmallestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PNormalValue, MSmallestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PNormalValue, PSmallestNormalized, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PNormalValue, MSmallestNormalized, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MNormalValue, PInf, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MInf, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MNormalValue, MZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MNormalValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MNormalValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MNormalValue, PNormalValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MNormalValue, MNormalValue, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MNormalValue, PLargestValue, "-0x1p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, MLargestValue, "-0x1p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MNormalValue, PSmallestValue, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MNormalValue, MSmallestValue, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MNormalValue, PSmallestNormalized, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MNormalValue, MSmallestNormalized, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PLargestValue, PInf, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, MInf, "0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PLargestValue, PZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PLargestValue, MZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PLargestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PLargestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PLargestValue, PNormalValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PLargestValue, MNormalValue, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PLargestValue, PLargestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PLargestValue, MLargestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PLargestValue, PSmallestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PLargestValue, MSmallestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PLargestValue, PSmallestNormalized, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PLargestValue, MSmallestNormalized, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MLargestValue, PInf, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, MInf, "-0x1.fffffep+127",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MLargestValue, PZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MLargestValue, MZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MLargestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MLargestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MLargestValue, PNormalValue, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MLargestValue, MNormalValue, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MLargestValue, PLargestValue, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MLargestValue, MLargestValue, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MLargestValue, PSmallestValue, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MLargestValue, MSmallestValue, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MLargestValue, PSmallestNormalized, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MLargestValue, MSmallestNormalized, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestValue, PInf, "0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MInf, "0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestValue, MZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestValue, PNormalValue, "0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MNormalValue, "0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PLargestValue, "0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MLargestValue, "0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, PSmallestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestValue, MSmallestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestValue, PSmallestNormalized, "0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestValue, MSmallestNormalized, "0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PInf, "-0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MInf, "-0x1p-149", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestValue, MZero, "nan", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestValue, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestValue, SNaN, "nan123", details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestValue, PNormalValue, "-0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MNormalValue, "-0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PLargestValue, "-0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MLargestValue, "-0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, PSmallestValue, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestValue, MSmallestValue, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestValue, PSmallestNormalized, "-0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestValue, MSmallestNormalized, "-0x1p-149",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PInf, "0x1p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MInf, "0x1p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PZero, "nan",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestNormalized, MZero, "nan",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestNormalized, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestNormalized, SNaN, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {PSmallestNormalized, PNormalValue, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MNormalValue, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PLargestValue, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, MLargestValue, "0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PSmallestNormalized, PSmallestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestNormalized, MSmallestValue, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestNormalized, PSmallestNormalized, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PSmallestNormalized, MSmallestNormalized, "0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, PInf, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MInf, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PZero, "nan",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestNormalized, MZero, "nan",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestNormalized, QNaN, "nan", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestNormalized, SNaN, "nan123",
       details::FloatBase::OpStatus::kInvalidOp,
       details::FloatBase::FloatCategory::kNaN},
      {MSmallestNormalized, PNormalValue, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MNormalValue, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PLargestValue, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, MLargestValue, "-0x1p-126",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MSmallestNormalized, PSmallestValue, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, MSmallestValue, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, PSmallestNormalized, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MSmallestNormalized, MSmallestNormalized, "-0x0p+0",
       details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},

      {PVal1, PVal1, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal1, MVal1, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal1, PVal2, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal1, MVal2, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal1, PVal3, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal1, MVal3, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal1, PVal4, "-0x1p+103", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal1, MVal4, "-0x1p+103", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal1, PVal5, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal1, MVal5, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal1, PVal6, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal1, MVal6, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal1, PVal1, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal1, MVal1, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal1, PVal2, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal1, MVal2, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal1, PVal3, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal1, MVal3, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal1, PVal4, "0x1p+103", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal1, MVal4, "0x1p+103", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal1, PVal5, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal1, MVal5, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal1, PVal6, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal1, MVal6, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal2, PVal1, "0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal2, MVal1, "0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal2, PVal2, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal2, MVal2, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal2, PVal3, "-0x0.000002p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal2, MVal3, "-0x0.000002p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal2, PVal4, "0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal2, MVal4, "0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal2, PVal5, "0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal2, MVal5, "0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal2, PVal6, "0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal2, MVal6, "0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal2, PVal1, "-0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal2, MVal1, "-0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal2, PVal2, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal2, MVal2, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal2, PVal3, "0x0.000002p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal2, MVal3, "0x0.000002p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal2, PVal4, "-0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal2, MVal4, "-0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal2, PVal5, "-0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal2, MVal5, "-0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal2, PVal6, "-0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal2, MVal6, "-0x1.fffffep-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal3, PVal1, "0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal3, MVal1, "0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal3, PVal2, "0x0.000002p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal3, MVal2, "0x0.000002p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal3, PVal3, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal3, MVal3, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal3, PVal4, "0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal3, MVal4, "0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal3, PVal5, "0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal3, MVal5, "0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal3, PVal6, "0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal3, MVal6, "0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal3, PVal1, "-0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal3, MVal1, "-0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal3, PVal2, "-0x0.000002p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal3, MVal2, "-0x0.000002p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal3, PVal3, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal3, MVal3, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal3, PVal4, "-0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal3, MVal4, "-0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal3, PVal5, "-0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal3, MVal5, "-0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal3, PVal6, "-0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal3, MVal6, "-0x1p-125", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal4, PVal1, "0x1p+103", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal4, MVal1, "0x1p+103", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal4, PVal2, "0x0.002p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal4, MVal2, "0x0.002p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal4, PVal3, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal4, MVal3, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal4, PVal4, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal4, MVal4, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal4, PVal5, "0.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal4, MVal5, "0.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal4, PVal6, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal4, MVal6, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal4, PVal1, "-0x1p+103", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal4, MVal1, "-0x1p+103", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal4, PVal2, "-0x0.002p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal4, MVal2, "-0x0.002p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal4, PVal3, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal4, MVal3, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal4, PVal4, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal4, MVal4, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal4, PVal5, "-0.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal4, MVal5, "-0.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal4, PVal6, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal4, MVal6, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal5, PVal1, "1.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal5, MVal1, "1.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal5, PVal2, "0x0.00006p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal5, MVal2, "0x0.00006p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal5, PVal3, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal5, MVal3, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal5, PVal4, "1.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal5, MVal4, "1.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal5, PVal5, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal5, MVal5, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal5, PVal6, "-0.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal5, MVal6, "-0.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal5, PVal1, "-1.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal5, MVal1, "-1.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal5, PVal2, "-0x0.00006p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal5, MVal2, "-0x0.00006p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal5, PVal3, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal5, MVal3, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal5, PVal4, "-1.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal5, MVal4, "-1.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal5, PVal5, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal5, MVal5, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal5, PVal6, "0.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal5, MVal6, "0.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal6, PVal1, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal6, MVal1, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal6, PVal2, "0x0.00004p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal6, MVal2, "0x0.00004p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal6, PVal3, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal6, MVal3, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal6, PVal4, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal6, MVal4, "0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal6, PVal5, "-0.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal6, MVal5, "-0.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {PVal6, PVal6, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {PVal6, MVal6, "0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal6, PVal1, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal6, MVal1, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal6, PVal2, "-0x0.00004p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal6, MVal2, "-0x0.00004p-126", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal6, PVal3, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal6, MVal3, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal6, PVal4, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal6, MVal4, "-0x1p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal6, PVal5, "0.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal6, MVal5, "0.5", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kNormal},
      {MVal6, PVal6, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
      {MVal6, MVal6, "-0x0p+0", details::FloatBase::OpStatus::kOK,
       details::FloatBase::FloatCategory::kZero},
  };

  for (size_t i = 0; i < std::size(SpecialCaseTests); ++i) {
    Float x(SpecialCaseTests[i].x);
    Float y(SpecialCaseTests[i].y);
    auto status = x.rem(y);

    Float result(x.sem(), SpecialCaseTests[i].result);

    EXPECT_TRUE(result.bitwise_equal(x));
    EXPECT_EQ(SpecialCaseTests[i].status, status);
    EXPECT_EQ(SpecialCaseTests[i].category, x.category());
  }

  {
    Float f1(details::FloatBase::IEEEdouble(), "0x1.3333333333333p-2");  // 0.3
    Float f2(details::FloatBase::IEEEdouble(), "0x1.47ae147ae147bp-7");  // 0.01
    Float expected(details::FloatBase::IEEEdouble(), "-0x1.4p-56");
    EXPECT_EQ(details::FloatBase::OpStatus::kOK, f1.rem(f2));
    EXPECT_TRUE(f1.bitwise_equal(expected));
  }
  {
    Float f1(details::FloatBase::IEEEdouble(),
             "0x1p64");  // 1.8446744073709552e19
    Float f2(details::FloatBase::IEEEdouble(), "1.5");
    Float expected(details::FloatBase::IEEEdouble(), "-0.5");
    EXPECT_EQ(details::FloatBase::OpStatus::kOK, f1.rem(f2));
    EXPECT_TRUE(f1.bitwise_equal(expected));
  }
  {
    Float f1(details::FloatBase::IEEEdouble(), "0x1p1000");
    Float f2(details::FloatBase::IEEEdouble(), "0x1p-1000");
    Float expected(details::FloatBase::IEEEdouble(), "0.0");
    EXPECT_EQ(details::FloatBase::OpStatus::kOK, f1.rem(f2));
    EXPECT_TRUE(f1.bitwise_equal(expected));
  }
  {
    Float f1 = Float::inf(details::FloatBase::IEEEdouble(), false);
    Float f2(details::FloatBase::IEEEdouble(), "1.0");
    EXPECT_EQ(f1.rem(f2), details::FloatBase::OpStatus::kInvalidOp);
    EXPECT_TRUE(f1.isNaN());
  }
  {
    Float f1(details::FloatBase::IEEEdouble(), "-4.0");
    Float f2(details::FloatBase::IEEEdouble(), "-2.0");
    Float expected(details::FloatBase::IEEEdouble(), "-0.0");
    EXPECT_EQ(details::FloatBase::OpStatus::kOK, f1.rem(f2));
    EXPECT_TRUE(f1.bitwise_equal(expected));
  }
  {
    Float f1(details::FloatBase::IEEEdouble(), "-4.0");
    Float f2(details::FloatBase::IEEEdouble(), "2.0");
    Float expected(details::FloatBase::IEEEdouble(), "-0.0");
    EXPECT_EQ(details::FloatBase::OpStatus::kOK, f1.rem(f2));
    EXPECT_TRUE(f1.bitwise_equal(expected));
  }
}

TEST(FloatTest, IEEEdoubleToDouble) {
  Float DPosZero(0.0);
  Float DPosZeroToDouble(DPosZero.to_double());
  EXPECT_TRUE(DPosZeroToDouble.is_pos_zero());
  Float DNegZero(-0.0);
  Float DNegZeroToDouble(DNegZero.to_double());
  EXPECT_TRUE(DNegZeroToDouble.is_neg_zero());

  Float DOne(1.0);
  EXPECT_EQ(1.0, DOne.to_double());
  Float DPosLargest = Float::largest(details::FloatBase::IEEEdouble(), false);
  EXPECT_EQ(std::numeric_limits<double>::max(), DPosLargest.to_double());
  Float DNegLargest = Float::largest(details::FloatBase::IEEEdouble(), true);
  EXPECT_EQ(-std::numeric_limits<double>::max(), DNegLargest.to_double());
  Float DPosSmallest =
      Float::smallest_normalized(details::FloatBase::IEEEdouble(), false);
  EXPECT_EQ(std::numeric_limits<double>::min(), DPosSmallest.to_double());
  Float DNegSmallest =
      Float::smallest_normalized(details::FloatBase::IEEEdouble(), true);
  EXPECT_EQ(-std::numeric_limits<double>::min(), DNegSmallest.to_double());

  Float DSmallestDenorm =
      Float::smallest(details::FloatBase::IEEEdouble(), false);
  EXPECT_EQ(std::numeric_limits<double>::denorm_min(),
            DSmallestDenorm.to_double());
  Float DLargestDenorm(details::FloatBase::IEEEdouble(),
                       "0x0.FFFFFFFFFFFFFp-1022");
  EXPECT_EQ(/*0x0.FFFFFFFFFFFFFp-1022*/ 2.225073858507201e-308,
            DLargestDenorm.to_double());

  Float DPosInf = Float::inf(details::FloatBase::IEEEdouble());
  EXPECT_EQ(std::numeric_limits<double>::infinity(), DPosInf.to_double());
  Float DNegInf = Float::inf(details::FloatBase::IEEEdouble(), true);
  EXPECT_EQ(-std::numeric_limits<double>::infinity(), DNegInf.to_double());
  Float DQNaN = Float::qNaN(details::FloatBase::IEEEdouble());
  EXPECT_TRUE(std::isnan(DQNaN.to_double()));
}

TEST(FloatTest, IEEEsingleToDouble) {
  Float FPosZero(0.0F);
  Float FPosZeroToDouble(FPosZero.to_double());
  EXPECT_TRUE(FPosZeroToDouble.is_pos_zero());
  Float FNegZero(-0.0F);
  Float FNegZeroToDouble(FNegZero.to_double());
  EXPECT_TRUE(FNegZeroToDouble.is_neg_zero());

  Float FOne(1.0F);
  EXPECT_EQ(1.0, FOne.to_double());
  Float FPosLargest = Float::largest(details::FloatBase::IEEEsingle(), false);
  EXPECT_EQ(std::numeric_limits<float>::max(), FPosLargest.to_double());
  Float FNegLargest = Float::largest(details::FloatBase::IEEEsingle(), true);
  EXPECT_EQ(-std::numeric_limits<float>::max(), FNegLargest.to_double());
  Float FPosSmallest =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), false);
  EXPECT_EQ(std::numeric_limits<float>::min(), FPosSmallest.to_double());
  Float FNegSmallest =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), true);
  EXPECT_EQ(-std::numeric_limits<float>::min(), FNegSmallest.to_double());

  Float FSmallestDenorm =
      Float::smallest(details::FloatBase::IEEEsingle(), false);
  EXPECT_EQ(std::numeric_limits<float>::denorm_min(),
            FSmallestDenorm.to_double());
  Float FLargestDenorm(details::FloatBase::IEEEdouble(), "0x0.FFFFFEp-126");
  EXPECT_EQ(/*0x0.FFFFFEp-126*/ 1.1754942106924411e-38,
            FLargestDenorm.to_double());

  Float FPosInf = Float::inf(details::FloatBase::IEEEsingle());
  EXPECT_EQ(std::numeric_limits<double>::infinity(), FPosInf.to_double());
  Float FNegInf = Float::inf(details::FloatBase::IEEEsingle(), true);
  EXPECT_EQ(-std::numeric_limits<double>::infinity(), FNegInf.to_double());
  Float FQNaN = Float::qNaN(details::FloatBase::IEEEsingle());
  EXPECT_TRUE(std::isnan(FQNaN.to_double()));
}

TEST(FloatTest, IEEEhalfToDouble) {
  Float HPosZero = Float::zero(details::FloatBase::IEEEhalf());
  Float HPosZeroToDouble(HPosZero.to_double());
  EXPECT_TRUE(HPosZeroToDouble.is_pos_zero());
  Float HNegZero = Float::zero(details::FloatBase::IEEEhalf(), true);
  Float HNegZeroToDouble(HNegZero.to_double());
  EXPECT_TRUE(HNegZeroToDouble.is_neg_zero());

  Float HOne(details::FloatBase::IEEEhalf(), "1.0");
  EXPECT_EQ(1.0, HOne.to_double());
  Float HPosLargest = Float::largest(details::FloatBase::IEEEhalf(), false);
  EXPECT_EQ(65504.0, HPosLargest.to_double());
  Float HNegLargest = Float::largest(details::FloatBase::IEEEhalf(), true);
  EXPECT_EQ(-65504.0, HNegLargest.to_double());
  Float HPosSmallest =
      Float::smallest_normalized(details::FloatBase::IEEEhalf(), false);
  EXPECT_EQ(/*0x1.p-14*/ 6.103515625e-05, HPosSmallest.to_double());
  Float HNegSmallest =
      Float::smallest_normalized(details::FloatBase::IEEEhalf(), true);
  EXPECT_EQ(/*-0x1.p-14*/ -6.103515625e-05, HNegSmallest.to_double());

  Float HSmallestDenorm =
      Float::smallest(details::FloatBase::IEEEhalf(), false);
  EXPECT_EQ(/*0x1.p-24*/ 5.960464477539063e-08, HSmallestDenorm.to_double());
  Float HLargestDenorm(details::FloatBase::IEEEhalf(), "0x1.FFCp-14");
  EXPECT_EQ(/*0x1.FFCp-14*/ 0.00012201070785522461, HLargestDenorm.to_double());

  Float HPosInf = Float::inf(details::FloatBase::IEEEhalf());
  EXPECT_EQ(std::numeric_limits<double>::infinity(), HPosInf.to_double());
  Float HNegInf = Float::inf(details::FloatBase::IEEEhalf(), true);
  EXPECT_EQ(-std::numeric_limits<double>::infinity(), HNegInf.to_double());
  Float HQNaN = Float::qNaN(details::FloatBase::IEEEhalf());
  EXPECT_TRUE(std::isnan(HQNaN.to_double()));

  Float BPosZero = Float::zero(details::FloatBase::IEEEhalf());
  Float BPosZeroToDouble(BPosZero.to_double());
  EXPECT_TRUE(BPosZeroToDouble.is_pos_zero());
  Float BNegZero = Float::zero(details::FloatBase::IEEEhalf(), true);
  Float BNegZeroToDouble(BNegZero.to_double());
  EXPECT_TRUE(BNegZeroToDouble.is_neg_zero());
}

TEST(FloatTest, BFloatToDouble) {
  Float BOne(details::FloatBase::BFloat(), "1.0");
  EXPECT_EQ(1.0, BOne.to_double());
  Float BPosLargest = Float::largest(details::FloatBase::BFloat(), false);
  EXPECT_EQ(/*0x1.FEp127*/ 3.3895313892515355e+38, BPosLargest.to_double());
  Float BNegLargest = Float::largest(details::FloatBase::BFloat(), true);
  EXPECT_EQ(/*-0x1.FEp127*/ -3.3895313892515355e+38, BNegLargest.to_double());
  Float BPosSmallest =
      Float::smallest_normalized(details::FloatBase::BFloat(), false);
  EXPECT_EQ(/*0x1.p-126*/ 1.1754943508222875e-38, BPosSmallest.to_double());
  Float BNegSmallest =
      Float::smallest_normalized(details::FloatBase::BFloat(), true);
  EXPECT_EQ(/*-0x1.p-126*/ -1.1754943508222875e-38, BNegSmallest.to_double());

  Float BSmallestDenorm = Float::smallest(details::FloatBase::BFloat(), false);
  EXPECT_EQ(/*0x1.p-133*/ 9.183549615799121e-41, BSmallestDenorm.to_double());
  Float BLargestDenorm(details::FloatBase::BFloat(), "0x1.FCp-127");
  EXPECT_EQ(/*0x1.FCp-127*/ 1.1663108012064884e-38, BLargestDenorm.to_double());

  Float BPosInf = Float::inf(details::FloatBase::BFloat());
  EXPECT_EQ(std::numeric_limits<double>::infinity(), BPosInf.to_double());
  Float BNegInf = Float::inf(details::FloatBase::BFloat(), true);
  EXPECT_EQ(-std::numeric_limits<double>::infinity(), BNegInf.to_double());
  Float BQNaN = Float::qNaN(details::FloatBase::BFloat());
  EXPECT_TRUE(std::isnan(BQNaN.to_double()));
}

TEST(FloatTest, IEEEsingleToFloat) {
  Float FPosZero(0.0F);
  Float FPosZeroToFloat(FPosZero.to_float());
  EXPECT_TRUE(FPosZeroToFloat.is_pos_zero());
  Float FNegZero(-0.0F);
  Float FNegZeroToFloat(FNegZero.to_float());
  EXPECT_TRUE(FNegZeroToFloat.is_neg_zero());

  Float FOne(1.0F);
  EXPECT_EQ(1.0F, FOne.to_float());
  Float FPosLargest = Float::largest(details::FloatBase::IEEEsingle(), false);
  EXPECT_EQ(std::numeric_limits<float>::max(), FPosLargest.to_float());
  Float FNegLargest = Float::largest(details::FloatBase::IEEEsingle(), true);
  EXPECT_EQ(-std::numeric_limits<float>::max(), FNegLargest.to_float());
  Float FPosSmallest =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), false);
  EXPECT_EQ(std::numeric_limits<float>::min(), FPosSmallest.to_float());
  Float FNegSmallest =
      Float::smallest_normalized(details::FloatBase::IEEEsingle(), true);
  EXPECT_EQ(-std::numeric_limits<float>::min(), FNegSmallest.to_float());

  Float FSmallestDenorm =
      Float::smallest(details::FloatBase::IEEEsingle(), false);
  EXPECT_EQ(std::numeric_limits<float>::denorm_min(),
            FSmallestDenorm.to_float());
  Float FLargestDenorm(details::FloatBase::IEEEsingle(), "0x1.FFFFFEp-126");
  EXPECT_EQ(/*0x1.FFFFFEp-126*/ 2.3509885615147286e-38F,
            FLargestDenorm.to_float());

  Float FPosInf = Float::inf(details::FloatBase::IEEEsingle());
  EXPECT_EQ(std::numeric_limits<float>::infinity(), FPosInf.to_float());
  Float FNegInf = Float::inf(details::FloatBase::IEEEsingle(), true);
  EXPECT_EQ(-std::numeric_limits<float>::infinity(), FNegInf.to_float());
  Float FQNaN = Float::qNaN(details::FloatBase::IEEEsingle());
  EXPECT_TRUE(std::isnan(FQNaN.to_float()));
}

TEST(FloatTest, IEEEhalfToFloat) {
  Float HPosZero = Float::zero(details::FloatBase::IEEEhalf());
  Float HPosZeroToFloat(HPosZero.to_float());
  EXPECT_TRUE(HPosZeroToFloat.is_pos_zero());
  Float HNegZero = Float::zero(details::FloatBase::IEEEhalf(), true);
  Float HNegZeroToFloat(HNegZero.to_float());
  EXPECT_TRUE(HNegZeroToFloat.is_neg_zero());

  Float HOne(details::FloatBase::IEEEhalf(), "1.0");
  EXPECT_EQ(1.0F, HOne.to_float());
  Float HPosLargest = Float::largest(details::FloatBase::IEEEhalf(), false);
  EXPECT_EQ(/*0x1.FFCp15*/ 65504.0F, HPosLargest.to_float());
  Float HNegLargest = Float::largest(details::FloatBase::IEEEhalf(), true);
  EXPECT_EQ(/*-0x1.FFCp15*/ -65504.0F, HNegLargest.to_float());
  Float HPosSmallest =
      Float::smallest_normalized(details::FloatBase::IEEEhalf(), false);
  EXPECT_EQ(/*0x1.p-14*/ 6.103515625e-05F, HPosSmallest.to_float());
  Float HNegSmallest =
      Float::smallest_normalized(details::FloatBase::IEEEhalf(), true);
  EXPECT_EQ(/*-0x1.p-14*/ -6.103515625e-05F, HNegSmallest.to_float());

  Float HSmallestDenorm =
      Float::smallest(details::FloatBase::IEEEhalf(), false);
  EXPECT_EQ(/*0x1.p-24*/ 5.960464477539063e-08F, HSmallestDenorm.to_float());
  Float HLargestDenorm(details::FloatBase::IEEEhalf(), "0x1.FFCp-14");
  EXPECT_EQ(/*0x1.FFCp-14*/ 0.00012201070785522461F, HLargestDenorm.to_float());

  Float HPosInf = Float::inf(details::FloatBase::IEEEhalf());
  EXPECT_EQ(std::numeric_limits<float>::infinity(), HPosInf.to_float());
  Float HNegInf = Float::inf(details::FloatBase::IEEEhalf(), true);
  EXPECT_EQ(-std::numeric_limits<float>::infinity(), HNegInf.to_float());
  Float HQNaN = Float::qNaN(details::FloatBase::IEEEhalf());
  EXPECT_TRUE(std::isnan(HQNaN.to_float()));
}

TEST(FloatTest, BFloatToFloat) {
  Float BPosZero = Float::zero(details::FloatBase::BFloat());
  Float BPosZeroToDouble(BPosZero.to_float());
  EXPECT_TRUE(BPosZeroToDouble.is_pos_zero());
  Float BNegZero = Float::zero(details::FloatBase::BFloat(), true);
  Float BNegZeroToDouble(BNegZero.to_float());
  EXPECT_TRUE(BNegZeroToDouble.is_neg_zero());

  Float BOne(details::FloatBase::BFloat(), "1.0");
  EXPECT_EQ(1.0F, BOne.to_float());
  Float BPosLargest = Float::largest(details::FloatBase::BFloat(), false);
  EXPECT_EQ(/*0x1.FEp127*/ 3.3895313892515355e+38F, BPosLargest.to_float());
  Float BNegLargest = Float::largest(details::FloatBase::BFloat(), true);
  EXPECT_EQ(/*-0x1.FEp127*/ -3.3895313892515355e+38F, BNegLargest.to_float());
  Float BPosSmallest =
      Float::smallest_normalized(details::FloatBase::BFloat(), false);
  EXPECT_EQ(/*0x1.p-126*/ 1.1754943508222875e-38F, BPosSmallest.to_float());
  Float BNegSmallest =
      Float::smallest_normalized(details::FloatBase::BFloat(), true);
  EXPECT_EQ(/*-0x1.p-126*/ -1.1754943508222875e-38F, BNegSmallest.to_float());

  Float BSmallestDenorm = Float::smallest(details::FloatBase::BFloat(), false);
  EXPECT_EQ(/*0x1.p-133*/ 9.183549615799121e-41F, BSmallestDenorm.to_float());
  Float BLargestDenorm(details::FloatBase::BFloat(), "0x1.FCp-127");
  EXPECT_EQ(/*0x1.FCp-127*/ 1.1663108012064884e-38F, BLargestDenorm.to_float());

  Float BPosInf = Float::inf(details::FloatBase::BFloat());
  EXPECT_EQ(std::numeric_limits<float>::infinity(), BPosInf.to_float());
  Float BNegInf = Float::inf(details::FloatBase::BFloat(), true);
  EXPECT_EQ(-std::numeric_limits<float>::infinity(), BNegInf.to_float());
  Float BQNaN = Float::qNaN(details::FloatBase::BFloat());
  EXPECT_TRUE(std::isnan(BQNaN.to_float()));
}

TEST_MAIN();
