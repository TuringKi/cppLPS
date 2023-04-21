#pragma once

#include "lexer.h"

class Parser {

  explicit Parser() {}

 private:
  void translation_unit();
  void declaration_seq();
};
