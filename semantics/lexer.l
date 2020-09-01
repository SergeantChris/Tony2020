%option noyywrap

%{

#include <cstdio>
#include <cstdlib>
#include "ast.hpp"
#include "lexer.hpp"
#include "parser.hpp"

int linecount=1;
int m_comment=0;
//void yyerror (const char* msg);

%}

L [A-Za-z]
D [0-9]
W [ \t\r]
HEX [0-9|A-F|a-f]
ESCAPE \\(n|t|r|0|\\|\'|\"|x{HEX}{HEX})

%x M_COMMENT

%%

"and"		{ return T_and; }
"end"		{ return T_end; }
"list"		{ return T_list; }
"ref"		{ return T_ref; }
"bool"		{ return T_bool; }
"exit"		{ return T_exit; }
"mod"		{ return T_mod; }
"return"	{ return T_return; }
"char"		{ return T_char; }
"false"		{ return T_false; }
"new"		{ return T_new; }
"skip"		{ return T_skip; }
"decl"		{ return T_decl; }
"for"		{ return T_for; }
"nil"		{ return T_nil; }
"tail"		{ return T_tail; }
"def"		{ return T_def; }
"head"		{ return T_head; }
"nil?"		{ return T_nilq; }
"true"		{ return T_true; }
"else"		{ return T_else; }
"or"		{ return T_or; }
"int"		{ return T_int; }
"if"		{ return T_if; }
"not"		{ return T_not; }
"elsif"		{ return T_elsif; }

{L}({L}|{D}|_|\?)* {return T_id;}
{D}+ { return T_constInt; }
\"([^\n\"\'\\]|{ESCAPE})*\" { return T_string; }
\'([^\"\'\\]|{ESCAPE})\' { return T_constChar; }
[+|\-|*|/|#|=|<|>] {return yytext[0];}

"<="				{ return T_le;	}
">="				{ return T_ge;}
"<>"				{ return T_ne;}

[\(|\)|\[|\]|,|;|:]	{ return yytext[0]; }
":="				{ return T_assign; }
%[^\n]*\n			{ linecount++; }

"<*"            { BEGIN(M_COMMENT); m_comment++; }
<M_COMMENT>"*>" { m_comment--; if(m_comment==0) BEGIN(INITIAL); }
<M_COMMENT>"<*"   { m_comment++; }
<M_COMMENT>\n   { /*nothing*/ }
<M_COMMENT>.    { /*nothing*/ }

{W}+ {/*nothing*/}
\n					{ linecount++; }

<<EOF>> { return T_eof; }

. { yyerror("lexer: no such token"); }

%%

void yyerror (const char* msg)
{
  fprintf(stderr, "ERROR, line %d: %s\n", linecount, msg);
  exit(1);
}
