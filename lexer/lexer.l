%option noyywrap

%{
#define T_eof	0
#define T_and	1000
#define T_for	1001
#define T_end	1002
#define T_list	1003
#define T_ref	1004
#define T_bool	1005
#define T_exit	1006
#define T_mod	1007
#define	T_return	1008
#define T_false	1009
#define T_new	1010
#define T_skip	1011
#define T_decl	1012
#define T_nil	1013
#define T_tail	1014
#define T_def	1015
#define T_head	1016
#define T_nilq	1017
#define T_true	1018
#define T_else	1019
#define T_or	1020
#define T_int	1021
#define T_if	1022
#define T_not	1023
#define T_elsif	1024
#define T_char 1025
#define T_id 1026
#define T_constInt 1027
#define T_string 1028
#define T_constChar 1029
#define T_le 1030
#define T_ge 1031
#define T_ne 1032
#define T_assign 1033

int linecount=1;
int m_comment=0;
void yyerror (const char msg []);

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

{L}({L}|{D}|_|\?)*          { return T_id; }
{D}+                        { return T_constInt; }
\"([^\n\"\'\\]|{ESCAPE})*\" { return T_string; }
\'([^\"\'\\]|{ESCAPE})\'    { return T_constChar; }
[+|\-|*|/|#|=|<|>]          {return yytext[0];}

"<="				{ return T_le; }
">="				{ return T_ge; }
"<>"				{ return T_ne; }

[\(|\)|\[|\]|,|;|:]	{ return yytext[0]; }
":="				        { return T_assign; }
%[^\n]*\n			      { linecount++; }

"<*"              { BEGIN(M_COMMENT); m_comment++; }
<M_COMMENT>"*>"   { m_comment--; if(m_comment==0) BEGIN(INITIAL); }
<M_COMMENT>"<*"   { m_comment++; }
<M_COMMENT>\n     { /*nothing*/ }
<M_COMMENT>.      { /*nothing*/ }

{W}+        {/*nothing*/}
\n					{ linecount++; }

<<EOF>>     { return T_eof; }

. { yyerror("Lexical Error: no such token"); }

%%

void yyerror (const char msg [])
{
  fprintf(stderr, "ERROR, line %d: %s\n", linecount, msg);
  exit(1);
}

int main ()
{
  int token;
  do {
    token = yylex();
    printf("token=%d, lexeme=\"%s\"\n", token, yytext);
  }
  while (token != T_eof);
  return 0;
}

/*
flex -s -o lexer.c firstLexer.l
cc -o lexer lexer.c */
