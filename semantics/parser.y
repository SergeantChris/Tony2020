%{

#include "ast.hpp"

extern FILE* yyin;

%}

%token T_eof  "eof"
%token T_and	"and"
%token T_bool	"bool"
%token T_char	"char"
%token T_decl	"decl"
%token T_def	"def"
%token T_else	"else"
%token T_elsif  "elsif"
%token T_end	"end"
%token T_exit	"exit"
%token T_false	"false"
%token T_for	"for"
%token T_head	"head"
%token T_if		"if"
%token T_int	"int"
%token T_list	"list"
%token T_mod	"mod"
%token T_new	"new"
%token T_nil	"nil"
%token T_nilq	"nil?"
%token T_not	"not"
%token T_or		"or"
%token T_ref	"ref"
%token T_return	"return"
%token T_skip	"skip"
%token T_tail	"tail"
%token T_true	"true"
%token T_le		"<="
%token T_ge		">="
%token T_ne		"<>"
%token T_assign	":="

%token<name> T_id T_string
%token<integer>	T_constInt
%token<character> T_constChar

%left<op> "or"
%left<op> "and"
%right<op> "not"
%nonassoc<op> '=' "<>" '<' '>' "<=" ">="
%right<op> '#'
%left<op> '+' '-'
%left<op> '*' '/' "mod"
%right PSIGN
%right MSIGN

%union{
  const char* name;
  int integer;
  char character;
  const char* op;
  bool boolean;
  Def* def;
  vector<shared_ptr<Def>>* defl;
  Header* h;
  Formal* frml;
  vector<Formal>* fl;
  vector<const char*>* idl;
  Type type;
  Stmt* stmt;
  vector<shared_ptr<Stmt>>* stmtl;
  Branch* brn;
  vector<Branch>* brnl;
  Simple* sim;
  vector<shared_ptr<Simple>>* siml;
  Call* call;
  Expr* expr;
  vector<shared_ptr<Expr>>* exprl;
  Atom* atom;
}

%type<def> func_def func_decl var_def
%type<defl> def_list
%type<h> header
%type<frml> formal
%type<fl> formal_list formal_opt
%type<idl> id_list
%type<type> type
%type<stmt> stmt for_clause
%type<stmtl> /*stmt_star*/ stmt_plus
%type<brn> if_clause else_clause
%type<brnl> elsif_clause
%type<sim> simple
%type<siml> simple_list
%type<call> call
%type<expr> expr rval
%type<exprl> expr_list
%type<atom> atom

//%expect 1

%debug

%%

program:
  func_def { /*$1->sem();*/ cout << "AST: " << *$1 << endl; }
;

func_def:
  "def" header ':' def_list stmt_plus "end" { 
    $$ = new FuncDef($2, $4, $5);
  }
;
//make objects derived class
def_list:
  /* nothing */ { $$ = new vector<shared_ptr<Def>>; }
| def_list func_def { $1->push_back(shared_ptr<Def>($2)); $$ = $1; }
| def_list func_decl { $1->push_back(shared_ptr<Def>($2)); $$ = $1; }
| def_list var_def { $1->push_back(shared_ptr<Def>($2)); $$ = $1; }
;

header:
  type T_id	'(' formal_opt ')' { $$ = new Header($2, $4, $1); }
| T_id '(' formal_opt ')' { $$ = new Header($1, $3); }
;

formal_opt:
  formal_list { $$ = $1; }
| /* nothing */ { $$ = nullptr; }
;

formal_list:
  formal { $$ = new vector<Formal>; $$->push_back(*$1); }
| formal_list ';' formal { $1->push_back(*$3); $$ = $1; }
;

formal:
  "ref" type id_list { $$ = new Formal($2, $3, "cbr"); }
| type id_list { $$ = new Formal($1, $2, "cbv"); }
;

id_list: 
  T_id { $$ = new vector<const char*>; $$->push_back($1); }
| id_list ',' T_id { $1->push_back($3); $$ = $1; }
;

type: 
  "int" { $$.p = TYPE_int; }
| "bool" { $$.p = TYPE_bool; }
| "char" { $$.p = TYPE_char; }
| type '[' ']' { new ($$.c) Array($1); }
| "list" '[' type ']' { new ($$.c) List($3); }
;

func_decl: 
  "decl" header { new FuncDecl($2); }
;

var_def: 
  type id_list { new VarDef($1, $2); }
;

stmt_plus:
  stmt { $$ = new vector<shared_ptr<Stmt>>; $$->push_back(shared_ptr<Stmt>($1)); }
| stmt_plus stmt { $1->push_back(shared_ptr<Stmt>($2)); $$ = $1; }
;


//stmt_star:
//  /*nothing*/	{ $$ = new vector<shared_ptr<Stmt>>; }
//| stmt_star stmt { $1->push_back(shared_ptr<Stmt>($2)); $$ = $1; }
//;

stmt:
  simple { $$ = $1; }
| "exit" { $$ = new Return(); }
| "return" expr { $$ = new Return($2); }
| if_clause { $$ = $1; }
| for_clause { $$ = $1; }
;

if_clause: 
  "if" expr	':' stmt_plus	elsif_clause else_clause "end" {
    $$ = new Branch($4, $2, $5, $6);
  }
;

elsif_clause:
  /* nothing */ { $$ = new vector<Branch>; }
| elsif_clause "elsif" expr ':' stmt_plus { 
    $1->push_back(*(new Branch($5, $3))); $$ = $1; 
  }
;

else_clause:
  "else" ':' stmt_plus { $$ = new Branch($3); }
| /* nothing */ { $$ = nullptr; }
;

for_clause:
  "for" simple_list ';' expr ';' simple_list ':' stmt_plus "end" {
    $$ = new Loop($2, $4, $6, $8);
  }
;

simple_list:
  simple { $$ = new vector<shared_ptr<Simple>>; $$->push_back(shared_ptr<Simple>($1)); }
| simple_list ',' simple { $1->push_back(shared_ptr<Simple>($3)); $$ = $1; }
;

simple:
  "skip" { $$ = nullptr; }
| atom ":=" expr { $$ = new Assign($1, $3); } 
/*semcheck: atom is l-value && expr.type=atom.type*/
| call { $$ = $1; }
;

call:
  T_id '(' expr_list ')' { $$ = new Call($1, $3); }
| T_id '(' ')' { $$ = new Call($1); }
;

expr_list: 
  expr { $$ = new vector<shared_ptr<Expr>>; $$->push_back(shared_ptr<Expr>($1)); }
| expr_list ',' expr { $1->push_back(shared_ptr<Expr>($3)); $$ = $1; }
;

expr:
  atom { $$ = $1; }
| rval { $$ = $1; }
;

atom:
  T_id { $$ = new Id($1); }
| T_string { $$ = new String($1); }
| call { $$ = new RetVal($1); } //semcheck if has return type
| atom '[' expr ']' { $$ = new IndexAccess($1, $3); }
;

rval:
	T_constInt { $$ = new Const($1); }
| T_constChar { $$ = new Const($1); }
| '(' expr ')' { $$ = $2; }
| '+' expr %prec PSIGN { $$ = new UnOp($1, $2); }
| '-' expr %prec MSIGN { $$ = new UnOp($1, $2); }
| expr '+' expr { $$ = new BinOp($1, $2, $3); }
| expr '-' expr { $$ = new BinOp($1, $2, $3); }
| expr '*' expr { $$ = new BinOp($1, $2, $3); }
| expr '/' expr { $$ = new BinOp($1, $2, $3); }
| expr "mod" expr { $$ = new BinOp($1, $2, $3); }
| expr '=' expr { $$ = new BinOp($1, $2, $3); }
| expr "<>" expr { $$ = new BinOp($1, $2, $3); }
| expr '<' expr { $$ = new BinOp($1, $2, $3); }
| expr '>' expr { $$ = new BinOp($1, $2, $3); }
| expr "<=" expr { $$ = new BinOp($1, $2, $3); }
| expr ">=" expr { $$ = new BinOp($1, $2, $3); }
| "not" expr { $$ = new UnOp($1, $2); }
| expr "and" expr { $$ = new BinOp($1, $2, $3); }
| expr "or"	expr { $$ = new BinOp($1, $2, $3); }
| "true" { $$ = new Const("true"); }
| "false" { $$ = new Const("false"); }
| "new" type '[' expr ']' { $$ = new MemAlloc($2, $4); }
| expr '#' expr { $$ = new BinOp($1, $2, $3); }
| "nil" { $$ = new Const("nil"); }
| "nil?" '(' expr ')' { $$ = new UnOp("nil?", $3); }
| "head" '(' expr ')' { $$ = new UnOp("head", $3); }
| "tail" '(' expr ')' { $$ = new UnOp("tail", $3); }
;


%%

int main(int argc, char** argv) {
  yydebug = 1;
  if(argc != 2) {
    cout << "1 argument expected" << endl;
    exit(1);
  }
  FILE* f = fopen(argv[1], "r");
  if(!f) {
    cout << "File not found" << endl;
    return -1;
  }
  yyin = f;
  int result = yyparse();
  if (result == 0) printf("Success.\n");
  return result;
}
