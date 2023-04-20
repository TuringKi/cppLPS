
#include "parser.h"

// translation_unit:
//  declaration_seq
//  global_module_fragment[opt] module_declaration declaration_seq[opt] private_module_fragment[opt]
void Parser::translation_unit() {

    declaration_seq();

}

// declaration_seq:
//  declaration
//  declaration_seq declaration
void Parser::declaration_seq(){

}
