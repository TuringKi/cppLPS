#pragma once

#include <assert.h>
#include <cstdint>

class Location {

 public:
  using Ty = uint32_t;
  Ty offset() const { return id & ~MASK; }
  Location operator+(Ty l) {
    assert(((offset() + l) & MASK) == 0 && "location overflow");
    Location L;
    L.id = id + l;
    return L;
  }
  Ty raw() const { return id; }

 private:
  Ty id;
  const Ty MASK = 1ULL << (8 * sizeof(Ty) - 1);
};

namespace tok {

enum class TokenKind : uint16_t {
#define TOK(X) X,
#include "token/kinds.def"
  NUM_TOKENS
};
};  // namespace tok

struct Token {
 private:
  Location::Ty loc;
  tok::TokenKind Kind;

  uint16_t Flags;
};
