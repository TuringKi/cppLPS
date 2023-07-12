

#define CL(__NAME__) \
  class __NAME__ {   \
    int a = 0;       \
  };

#define A(P0, P1, P2) P0 + P1 + P2

A((1 + 3), (2) + 1, 3 + (2333 - 111) + 111)
A(1, 2, 3)
A(1 + 3, 2 + 1, 3 + 111)
CL(A0)
