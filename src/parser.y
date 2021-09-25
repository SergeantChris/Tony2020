%{
#include <cstring>
#include "ast.hpp"
#include <vector>
#include <signal.h>
#include <execinfo.h>
#include <stack>
extern FILE* yyin;

#include <iostream>
using namespace std;

#define YYERROR_VERBOSE 1

SymbolTable st;
ValueTable vt;
Library *lib = new Library();
bool retval;

int linecount = 1;
int opt_flag=0, lco_flag=0, ico_flag=0;

%}

%token T_and "and"
%token T_bool	"bool"
%token T_char	"char"
%token T_decl "decl"
%token T_def "def"
%token T_else "else"
%token T_elsif "elsif"
%token T_end "end"
%token T_exit "exit"
%token T_false "false"
%token T_for "for"
%token T_head "head"
%token T_if "if"
%token T_int	"int"
%token T_list "list"
%token T_mod "mod"
%token T_new "new"
%token T_nil "nil"
%token T_nilqm "nil?"
%token T_not "not"
%token T_or "or"
%token T_ref "ref"
%token T_return "return"
%token T_skip "skip"
%token T_tail "tail"
%token T_true "true"

%token T_le		"<="
%token T_ge		">="
%token T_ne		"<>"
%token T_assign	":="

%token<cstr> T_id T_string
%token<cint>	T_constInt
%token<cchar> T_constChar

%left<cstr> "or"
%left<cstr> "and"
%right<cstr> "not"
%nonassoc<cstr> '=' "<>" '<' '>' "<=" ">="
%right<cstr> '#'
%left<cstr> '+' '-'
%left<cstr> '*' '/' "mod"
%right PLUS_SIGN
%right MINUS_SIGN

%union {
	const char *cstr;			/* identifiers and constant strings*/
	int cint;							/* const integers */
	char cchar;						/* const characters */
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
}

%type<def> func_def func_decl var_def
%type<defl> func_list
%type<h> header
%type<frml> formal
%type<fl> formal_list maybe_formal
%type<idl> id_list
%type<type> type
%type<stmt> stmt for_statement
%type<stmtl> stmt_list
%type<brn> if_statement else_statement
%type<brnl> elsif_statement
%type<sim> simple
%type<siml> simple_list
%type<call> call
%type<expr> expr
%type<exprl> expr_list
%type<atom> atom


%%

program:
	func_def  {
		/* cout << "-------------------------------------------------------- AST --------------------------------------------------------" << std::endl;
		cout << *$1 << endl;
    cout << endl << "----------------------------------------------------- SEMANTICS -----------------------------------------------------" << std::endl;
		st.openScope();
		lib->init(); // Initialize all built in functions and procedures
		$1->sem();
		st.closeScope(); */

		/* cout << endl << "------------------------------------------------------- LLVM --------------------------------------------------------" << std::endl; */
		$1->llvm_compile_and_dump(opt_flag);
		delete $1;
		vt.closeScope();
		return 0;
	}
;

func_def:
  "def" header ':' func_list stmt_list "end"  { $$ = new FuncDef($2, $4, $5); }

;

func_list:
  /*nothing*/   			{ $$ = new vector<shared_ptr<Def>>; }
| func_list func_def  { $1->push_back(shared_ptr<Def>($2)); $$ = $1; }
| func_list func_decl	{ $1->push_back(shared_ptr<Def>($2)); $$ = $1; }
| func_list var_def		{ $1->push_back(shared_ptr<Def>($2)); $$ = $1; }
;

header:
  type T_id	'(' maybe_formal ')'	{ $$ = new Header($2, $4, $1); }
| T_id '(' maybe_formal ')'				{ Type t; t.p = TYPE_void; $$ = new Header($1, $3, t); }
;

maybe_formal:
formal_list								{ $$ = $1; }
| /* nothing */						{ $$ = nullptr; }
;

formal_list:
	formal									{ $$ = new vector<Formal*>;
														$$->push_back($1);}
| formal_list ';' formal	{ $1->push_back($3); $$ = $1; }
;

formal:
	"ref" type id_list { $$ = new Formal($2, $3, "ref"); }
| type id_list       { $$ = new Formal($1, $2, "val"); }
;

id_list:
	T_id							{ $$ = new vector<const char*>; $$->push_back($1); }
| id_list ',' T_id	{ $1->push_back($3); $$ = $1; }
;

type:
  "int"								{ $$.p = TYPE_int; }
| "bool"							{ $$.p = TYPE_bool; }
| "char"							{ $$.p = TYPE_char; }
| type '[' ']'				{ $$.c = new Array($1); }
| "list" '[' type ']'	{ $$.c = new List($3); }
;

func_decl:
  "decl" header { $$ = new FuncDecl($2); }
;

var_def:
  type id_list { $$ = new VarDef($1, $2); }
;

stmt_list:
  stmt      { $$ = new vector<shared_ptr<Stmt>>;
              $$->push_back(shared_ptr<Stmt>($1)); }
| stmt_list stmt  { $1->push_back(shared_ptr<Stmt>($2)); $$ = $1; }

stmt:
  simple				{ $$ = $1; }
| "exit"				{ $$ = new Return(); }
| "return" expr	{ $$ = new Return($2); }
| if_statement	{ $$ = $1; }
| for_statement	{ $$ = $1; }
;

if_statement:
	"if" expr	':' stmt_list	elsif_statement else_statement "end"
		{ $$ = new Branch($4, $2, $5, $6);}
;

elsif_statement:
	/* nothing */		{ $$ = new vector<Branch*>; }
| elsif_statement "elsif" expr ':' stmt_list
		{ $1->push_back(new Branch($5, $3)); $$ = $1; }
;

else_statement:
	"else" ':' stmt_list	{ $$ = new Branch($3); }
| /* nothing */					{ $$ = nullptr; }
;

for_statement:
	"for" simple_list ';' expr ';' simple_list ':' stmt_list "end" {
		$$ = new Loop($2, $4, $6, $8);
	}
;

simple_list:
	simple									{ $$ = new vector<shared_ptr<Simple>>;
														$$->push_back(shared_ptr<Simple>($1)); }
| simple_list ',' simple	{ $1->push_back(shared_ptr<Simple>($3)); $$ = $1; }
;

simple:
	"skip"					{ $$ = new NoAction();}
| atom ":=" expr	{ $$ = new Assign($1, $3); }
| call						{ $$ = $1; }
;

call:
	T_id '(' expr_list ')'	{$$ = new Call($1, $3); }
| T_id '(' ')'						{$$ = new Call($1); }
;

expr_list:
	expr								{ $$ = new vector<shared_ptr<Expr>>;
												$$->push_back(shared_ptr<Expr>($1)); }
| expr_list ',' expr 	{ $1->push_back(shared_ptr<Expr>($3)); $$ = $1; }
;

atom:
	T_id								{ $$ = new Id($1); }
| T_string						{ $$ = new String($1); } // TODO: semcheck can we Assign to string atoms
| atom '[' expr ']'		{ $$ = new DirectAcc($1, $3); }
| call								{ $$ = new ReturnValue($1); }
;

expr:
	atom 														{ $$ = $1; }
| T_constInt											{ $$ = new Const($1); }
| T_constChar											{ $$ = new Const($1); }
| '(' expr ')'										{ $$ = $2; }
| '+' expr %prec PLUS_SIGN				{ $$ = new PreOp("+", $2); }
| '-' expr %prec MINUS_SIGN				{ $$ = new PreOp("-", $2); }
| expr '+' expr										{ $$ = new Op($1, "+", $3); }
| expr '-' expr										{ $$ = new Op($1, "-", $3); }
| expr '*' expr										{ $$ = new Op($1, "*", $3); }
| expr '/' expr										{ $$ = new Op($1, "/", $3); }
| expr "mod" expr									{ $$ = new Op($1, "mod", $3); }
| expr '=' expr										{ $$ = new Op($1, "=", $3); }
| expr "<>" expr									{ $$ = new Op($1, "<>", $3); }
| expr '<' expr										{ $$ = new Op($1, "<", $3); }
| expr '>' expr 									{ $$ = new Op($1, ">", $3); }
| expr "<=" expr									{ $$ = new Op($1, "<=", $3); }
| expr ">=" expr									{ $$ = new Op($1, ">=", $3); }
| "not" expr											{ $$ = new PreOp("not", $2); }
| expr "and" expr									{ $$ = new Op($1, "and", $3); }
| expr "or" expr									{ $$ = new Op($1, "or", $3); }
| "true"													{ $$ = new Const(string("true")); }
| "false"													{ $$ = new Const(string("false")); }
| "new" type '[' expr ']'					{ $$ = new MemoryAlloc($2, $4); }
| "nil"														{ $$ = new Const(string("nil")); }
| "nil?" '(' expr ')'							{ $$ = new PreOp("nil?", $3); }
| expr '#' expr										{ $$ = new Op($1, "#", $3); }
| "head" '(' expr ')'							{ $$ = new PreOp("head", $3); }
| "tail" '(' expr ')'							{ $$ = new PreOp("tail", $3); }
;

%%

int main(int argc, char* argv[]) {
	if(argc < 2) {
		cout << "Provide at least 1 argument (tony file)" << endl;
		exit(1);
	}

	for(int i = 1; i < argc-1; i++) {
		if(strcmp(argv[i], "-O")==0 && !opt_flag) {
			cout << "+++	Optimizations sellected" << endl;
			opt_flag=1;
		}
		else if(strcmp(argv[i], "-f")==0 && !lco_flag) {
			cout << "+++	Final Code Output to stdout sellected" << endl;
			lco_flag=1;
		}
		else if(strcmp(argv[i], "-i")==0 && !ico_flag) {
			cout << "+++	Intermediate Code Output to stdout sellected" << endl;
			ico_flag=1;
		}
	}

	FILE* f = fopen(argv[argc-1], "r");
  if(!f) {
    cout << "File not found" << endl;
    return -1;
  }
  yyin = f;
  int result = yyparse();
  if (result == 0) printf("Success.\n");
  return result;
}
