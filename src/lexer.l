%option noyywrap
%option nounput

%{
#include <cstdio>
#include <cstdlib>
#include <string>
#include <sstream>
#include "ast.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "error.hpp"

#define T_eof	0

using namespace std;

extern int linecount;
int comment_nesting = 0;


%}

digit	[0-9]
letter	[a-zA-Z]
W [ \t\r]
HEX [0-9|A-F|a-f]
ESC \\(n|t|r|0|\\|\'|\"|x{HEX}{HEX})

%x SC_COMMENT

%%

"and"     { return T_and; }
"bool"	  { return T_bool; }
"char"	  { return T_char; }
"decl"	  { return T_decl; }
"def"	  { return T_def; }
"else"	  { return T_else; }
"elsif"	  { return T_elsif; }
"end"	  { return T_end; }
"exit"	  { return T_exit; }
"false"	  { return T_false; }
"for"	  { return T_for; }
"head"	  { return T_head; }
"if"	  { return T_if; }
"int"	  { return T_int; }
"list"	  { return T_list; }
"mod"	  { return T_mod; }
"new"	  { return T_new; }
"nil"	  { return T_nil; }
"nil?"	  { return T_nilqm; }
"not"	  { return T_not; }
"or"	  { return T_or; }
"ref"	  { return T_ref; }
"return"  { return T_return; }
"skip"	  { return T_skip; }
"tail"	  { return T_tail; }
"true"	  { return T_true; }



{letter}({letter}|{digit}|_|\?)*	{ yylval.cstr = strdup(yytext); return T_id; }
{digit}+							{ yylval.cint = atoi(yytext); return T_constInt; }
\"([^\n\"\\]|{ESC})*\"				{ string s(yytext);
									  string in = s.substr(1, s.size()-2);
									  yylval.cstr = strdup(in.c_str());
									  return T_string; }
\'([^\'\"\\]|{ESC})\'					{ string s(yytext);
									  string in = s.substr(1, s.size()-2);
									  if(strcmp(in.c_str(), "\0")) {
									  	yylval.cchar = '\0'; // weird case here; does not take \0
									  }
									  else {
									  	yylval.cchar = yytext[1];
									  }
									  return T_constChar; }

[+|\-|*|/|#|=|<|>]   				{ return yytext[0]; }
"<="			 	         		{ return T_le; }
">="		             			{ return T_ge; }
"<>"				         		{ return T_ne; }
[\(|\)|\[|\]|,|;|:]	 				{ return yytext[0]; }
":="				         		{ return T_assign; }

%[^\n]*\n			       			{ linecount++; }

"<*"								{ BEGIN(SC_COMMENT); comment_nesting++; }
<SC_COMMENT>"*>"   					{ comment_nesting--; if(comment_nesting==0) BEGIN(INITIAL); }
<SC_COMMENT>"<*"   					{ comment_nesting++; }
<SC_COMMENT>\n     					{ linecount++; }
<SC_COMMENT>.      					{ /*nothing*/ }

{W}+    							{ /* nothing */ }
\n									{ linecount++; }

.									{ ostringstream formatted;
									  formatted << "Lexical Error: unexpected token: " << strdup(yytext);
									  error(formatted.str()); }
%%

void yyerror(const char *msg) {
	/* printf("Error at Line: %d\n", linecount); */
	/* fprintf(stderr, "%s\n", msg); */
  /* exit(1); */
	error(msg);
}
