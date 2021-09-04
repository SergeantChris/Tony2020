/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_PARSER_HPP_INCLUDED
# define YY_YY_PARSER_HPP_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    T_and = 258,
    T_bool = 259,
    T_char = 260,
    T_decl = 261,
    T_def = 262,
    T_else = 263,
    T_elsif = 264,
    T_end = 265,
    T_exit = 266,
    T_false = 267,
    T_for = 268,
    T_head = 269,
    T_if = 270,
    T_int = 271,
    T_list = 272,
    T_mod = 273,
    T_new = 274,
    T_nil = 275,
    T_nilqm = 276,
    T_not = 277,
    T_or = 278,
    T_ref = 279,
    T_return = 280,
    T_skip = 281,
    T_tail = 282,
    T_true = 283,
    T_le = 284,
    T_ge = 285,
    T_ne = 286,
    T_assign = 287,
    T_id = 288,
    T_string = 289,
    T_constInt = 290,
    T_constChar = 291,
    T_constBool = 292,
    PLUS_SIGN = 293,
    MINUS_SIGN = 294
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 71 "parser.y"

	const char *cstr;			/* identifiers and constant strings*/
	int cint;							/* const integers */
	char cchar;						/* const characters */
	bool cbool;					/* const bools */
	Def* def;
	vector<shared_ptr<Def>>* defl;
	Header* h;
	Formal* frml;
	vector< Formal*>* fl;
	vector<const char*>* idl;
	Type type;
	Stmt* stmt;
	vector<shared_ptr<Stmt>>* stmtl;
	Branch* brn;
	vector<Branch*>* brnl;
	Simple* sim;
	vector<shared_ptr<Simple>>* siml;
	Call* call;
	Expr* expr;
	vector<shared_ptr<Expr>>* exprl;
	Atom* atom;

#line 121 "parser.hpp"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_HPP_INCLUDED  */
