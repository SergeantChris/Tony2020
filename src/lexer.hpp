#ifndef __LEXER_HPP__
#define __LEXER_HPP__

#include "error.hpp"

int yylex();
void yyerror(const char *msg);

#endif