#include <iostream>
#include <sstream>
#include <vector>
#include <memory>
#include <string>
#include <string.h>
#include "lexer.hpp"
#include "symbol.hpp"
#include "ast.hpp"

using namespace std;

#define PRE_DEBUG 1


CompositeType::~CompositeType() {}

string CompositeType::getId() {
	return id;
}

Type CompositeType::getType() {
	return type;
}


Array::Array(Type t) {
	id = "array";
	type = t;
}
Array::Array(PrimitiveType p) {
	id = "array";
	type.p = p;
}


List::List(Type t) {
	id = "list";
	type = t;
}
List::List(PrimitiveType p) {
	id = "list";
	type.p = p;
}


bool isPrimitive(Type t) {
	bool is_prim = false;
	switch(t.p) {
		case TYPE_int: is_prim = true; break;
		case TYPE_bool: is_prim = true; break;
		case TYPE_char: is_prim = true; break;
		case TYPE_nil: is_prim = true; break;
		case TYPE_void: is_prim = true; break;
		default: break; // it means it is a CompositeType
	}
	return is_prim;
}

ostream& operator<<(ostream &out, const Type t) {
	switch(t.p) {
		case TYPE_int: out << "int"; return out;
		case TYPE_bool: out << "bool"; return out;
		case TYPE_char: out << "char"; return out;
		case TYPE_nil: out << "nil"; return out;
		case TYPE_void: out << "void"; return out;
		default: break;
	}
	out << (t.c)->getId() << "[" << ((t.c)->getType()) << "]";
	return out;
}

ostream& operator<<(ostream &out, const PrimitiveType p) { // overloading
	switch(p) {
		case TYPE_int: out << "int";
		case TYPE_bool: out << "bool";
		case TYPE_char: out << "char";
		case TYPE_nil: out << "nil";
		case TYPE_void: out << "void";
	}
	return out;
}

bool operator==(const Type &t1, const Type &t2) {
	bool res, comp1 = false, comp2 = false;
	if(t1.p == TYPE_nil || t2.p == TYPE_nil) return true; // nil is for always matching, void is for not
	comp1 = !isPrimitive(t1);
	comp2 = !isPrimitive(t2);
	if(!comp1 && !comp2) {
		switch(t1.p) {
			case TYPE_int: res = (t2.p == TYPE_int ? true : false); break;
			case TYPE_bool: res = (t2.p == TYPE_bool ? true : false); break;
			case TYPE_char: res = (t2.p == TYPE_char ? true : false); break;
			case TYPE_nil: res = (t2.p == TYPE_nil ? true : false); break;
			case TYPE_void: res = (t2.p == TYPE_void ? true : false); break;
		}
		if(res) return true;
	}
	else if(comp1 && comp2) {
		if((t1.c)->getId() == (t2.c)->getId() && ((t1.c)->getType() == (t2.c)->getType())) return true;
	}
	return false;
}


ASTnode::~ASTnode() {}
void ASTnode::sem() {}


ostream& operator<<(ostream &out, const ASTnode &n) {
	n.printNode(out);
	return out;
}


Expr::~Expr() {}

void Expr::typeCheck(Type t) {
	#if PRE_DEBUG
	cout << "-- TYPE CHECK  ("<< type << ", " << t << ")";
	#endif
	if(!(type == t)){
		cout << endl;
		ostringstream formatted;
		formatted << "Type mismatch, expected type: " << t << ", but type: " << type << " was used";
		error(formatted.str());
	}
	#if PRE_DEBUG
	else {
		cout << "  ---> ok" << endl;
	}
	#endif
}

void Expr::typeCheck(PrimitiveType p) { // overloading
	Type t;
	t.p = p;
	typeCheck(t);
}

void Expr::typeCheck(CompositeType* c) { // overloading
	Type t;
	t.c = c;
	typeCheck(t);
}

void Expr::typeCheck(CompositeType* c1, CompositeType* c2) { // overloading for union
	Type t1;
	t1.c = c1;
	Type t2;
	t2.c = c2;
	#if PRE_DEBUG
	cout << "-- TYPE CHECK  ("<< type << ", " << t1 << " OR " << t2 << ")";
	#endif
	if(!(type == t1) && !(type == t2)){
		cout << endl;
		ostringstream formatted;
		formatted << "Type mismatch, expected " << c1->getId() << " or " << c2->getId() << " kind of type, but " << type << " was used";
		error(formatted.str());
	}
	#if PRE_DEBUG
	else {
		cout << "  ---> ok" << endl;
	}
	#endif
}

Type Expr::getType() {
	return type;
}


Const::Const(int i)		 			{ tc.integer = i; tc_act = TC_int; }
Const::Const(char c) 				{ tc.character = c; tc_act = TC_char; }
Const::Const(string v, bool special) { // for boolean types and nil + string literals
	if(special == 1) {
		if(v == "true") {
			tc.boolean = true;
			tc_act = TC_bool;
		}
		else if(v == "false") {
			tc.boolean = false;
			tc_act = TC_bool;
		}
		else if(v == "nil") {
			tc_act = TC_nil;
		}
	}
	else {
		tc.str = strdup(v.c_str());
		tc_act = TC_str;
	}
}
Const::~Const() {}
void Const::printNode(ostream &out) const {
	out << "Const(";
	switch(tc_act) {
		case(TC_int): out << tc.integer << ")"; break;
		case(TC_char): out << tc.character << ")"; break;
		case(TC_str): out << tc.str << ")"; break;
		case(TC_bool): out << (tc.boolean ? "true" : "false") << ")"; break;
		case(TC_nil): out << "nil_list" << ")"; break;
	}
}
void Const::sem() {
		switch(tc_act) {
			case(TC_int): type.p = TYPE_int; break;
			case(TC_char): type.p = TYPE_char; break;
			case(TC_str): type.c = new Array(TYPE_char); break;
			case(TC_bool): type.p = TYPE_bool; break;
			case(TC_nil): type.c = new List(TYPE_nil); break;
		}
}


PreOp::PreOp(const char* o, Expr* e): op(o), expr(e) {}
PreOp::~PreOp() { delete expr; }
void PreOp::printNode(ostream &out) const {
	out << "PreOp(" << op << ", " << *expr << ")";
}
void PreOp::sem() {
	expr->sem();
	if(op == "+" || op == "-") {
		expr->typeCheck(TYPE_int);
		type.p = TYPE_int;
	}
	else if(op == "not") {
		expr->typeCheck(TYPE_bool);
		type.p = TYPE_bool;
	}
	else if(op == "nil?") {
		expr->typeCheck(new List(TYPE_nil));
		type.p = TYPE_bool;
	}
	else if(op == "head") {
		expr->typeCheck(new List(TYPE_nil));
		type = ((expr->getType()).c)->getType();
	}
	else if(op == "tail") {
		expr->typeCheck(new List(TYPE_nil));
		type = expr->getType();
	}
}


Op::Op(Expr* e1, const char* o, Expr* e2): op(o), expr1(e1), expr2(e2) {}
Op::~Op() { delete expr1; delete expr2; }
void Op::printNode(ostream &out) const {
	out << "Op(" << *expr1 << ", " << op << ", " << *expr2 << ")";
}
void Op::sem() {
	expr1->sem();
	expr2->sem();
	if(op == "and" || op == "or") {
		expr1->typeCheck(TYPE_bool);
		expr2->typeCheck(TYPE_bool);
		type.p = TYPE_bool;
	}
	else if(op == "#") { // expr1->t, expr2->list[t]
		expr2->typeCheck(new List(expr1->getType()));
		type.c = new List(expr1->getType()); // because expr2->getType might still be null list
	}
	else if(op == "=" || op == "<>" || op == "<" || op == ">" || op == "<=" || op == ">=") {
		expr2->typeCheck(expr1->getType());
		type.p = TYPE_bool;
	}
	else if(op == "+" || op == "-" || op == "*" || op == "/" || op == "mod") {
		expr1->typeCheck(TYPE_int);
		expr2->typeCheck(TYPE_int);
		type.p = TYPE_int;
	}
}


MemoryAlloc::MemoryAlloc(Type t, Expr* e): new_type(t), expr(e) {}
MemoryAlloc::~MemoryAlloc() { delete expr; }
void MemoryAlloc::printNode(ostream &out) const {
	out << "MemoryAlloc(" << type << ", " << *expr << ")";
}
void MemoryAlloc::sem() {
	expr->sem();
	expr->typeCheck(TYPE_int);

	Type final_type;
	final_type.c = new Array(new_type);
	type = final_type;
}


Atom::~Atom() {}


Id::Id(const char* i): id(i) {}
Id::~Id() { delete id; }
void Id::printNode(ostream &out) const {
	out << "Id(" << id << ")";
}
const char* Id::getId() {
	return id;
}
void Id::sem() {
	cout << "Searching for: " << id << " ..." << endl;
	SymbolEntry *e = st.lookup(string(id));
	cout << "Found it with offset: " << e->offset << " and type: " << e->type << endl;
	type = e->type;
}


String::String(string s): Const(s, 0) {}
String::~String() {}


DirectAcc::DirectAcc(Atom* a, Expr* e): atom(a), expr(e) {}
DirectAcc::~DirectAcc() { delete atom; delete expr; }
void DirectAcc::printNode(ostream &out) const {
	out << "DirectAcc(" << *atom << ", " << *expr << ")";
}
void DirectAcc::sem() {
	expr->sem();
	atom->sem();
	atom->typeCheck(new Array(TYPE_nil), new List(TYPE_nil));
	expr->typeCheck(TYPE_int);
	type = ((atom->getType()).c)->getType();
}


Stmt::~Stmt() {}


Simple::~Simple() {}

NoAction::NoAction() {}
NoAction::~NoAction() {}
void NoAction::printNode(ostream &out) const {
	out << "no_action";
}
void NoAction::sem() {}


Assign::Assign(Atom* a, Expr* e): atom(a), expr(e) {}
Assign::~Assign() { delete atom; delete expr; }
void Assign::printNode(ostream &out) const {
	out << "Assign(" << *atom << ", " << *expr << ")";
}
void Assign::sem() {
	expr->sem();
	atom->sem();
	expr->typeCheck(atom->getType());

	// acceptable: atom is id[index], where id is array/list of same type as expr
	// if i dont check that id is defined in this scope, will blow up in the code generation stage
	// (check is either call return id defined in this scope OR call return id was arg passed by ref)

	// acceptable: atom is string[expr] because of papy comment http://moodle.softlab.ntua.gr/mod/forum/discuss.php?d=9839
	// -> means that "test"[0]='e' will blow up in the code generation stage
}


Call::Call(const char* i, vector<shared_ptr<Expr>>* e): id(i), exprList(e) {}
Call::~Call() { delete id; delete exprList; }
void Call::printNode(ostream &out) const {
	out << "Call(" << id;
	if(exprList != nullptr) {
		for(shared_ptr<Expr> e: *exprList) {
			out << ", ";
			out << *e;
		}
	}
	out << ")";
}
void Call::sem() {
	// checks if the function is defined
	cout << "Looking up for definition of the function ..." << endl;
	SymbolEntry *func = st.lookup(string(id), "func_def");
	vector<Formal*>* params = func->params;
	type = func->type;

	if(exprList != nullptr) {
		if(params != nullptr) {
			if(params->size() == exprList->size()) {
				// checks if the function's arguments are the same type as the exprList (one by one)
				int i = 0;
				for(shared_ptr<Expr> e: *exprList) {
					e->sem();
					e->typeCheck(params->at(i)->getType());
					i++;
				}
			}
			else {
				ostringstream formatted;
				formatted << "Function takes " << params->size() << " arguments, " << exprList->size() << " were given";
				error(formatted.str());
			}
		}
		else {
			ostringstream formatted;
			formatted << "Function takes no arguments, " << exprList->size() << " were given";
			error(formatted.str());
		}
	}
}
Type Call::getType() {
	return type;
}


ReturnValue::ReturnValue(Call* c): call(c) {}
ReturnValue::~ReturnValue() { delete call; }
void ReturnValue::printNode(ostream &out) const {
	out << "ReturnValue(" << *call << ")";
}
void ReturnValue::sem() {
	// CAN I SOMEHOW GET FUNC RETURN ID AS WELL? see assign comment
	call->sem();
	type = call->getType();
}


Return::Return(Expr* v): ret_val(v) {}
Return::~Return() { delete ret_val; }
void Return::printNode(ostream &out) const {
	out << "Return(";
	if(ret_val != nullptr) out << *ret_val;
	out << ")";
}
void Return::sem() {
	if(ret_val != nullptr) {
		ret_val->sem();
		cout << "Checking return type ..." << endl;
		ret_val->typeCheck(st.getReturnType());
	}
	else {
		cout << "Checking return type ..." << endl;
		Type rv_type;
		rv_type.p = TYPE_void;
		Type r_type = st.getReturnType();
		if(!(r_type == rv_type)) {
			ostringstream formatted;
			formatted << "Function of return type " << r_type << " must return a value";
			error(formatted.str());
		}
	}
}


Branch::Branch(vector<shared_ptr<Stmt>>* ct, Expr* c, vector<Branch*>* eif, Branch* e): cond_true(ct), condition(c), elsif_branches(eif), else_branch(e) {}
Branch::~Branch() { delete cond_true; delete condition; delete elsif_branches; delete else_branch; }
void Branch::printNode(ostream &out) const {
	out << "Branch(";
	bool first = true;
	for(shared_ptr<Stmt> s: *cond_true) {
		if(!first) out << ", ";
		first = false;
		out << *s;
	}
	out << ", " << *condition;
	if(elsif_branches != nullptr) {
		for(Branch *b: *elsif_branches) {
			out << ", ";
			out << *b;
		}
	}
	if(else_branch != nullptr) out << ", " << *else_branch;
	out << ")";
}
void Branch::sem() {
	condition->sem();
	condition->typeCheck(TYPE_bool);
	for(shared_ptr<Stmt> s: *cond_true) s->sem();
	if(elsif_branches != nullptr) {
		for(Branch *b: *elsif_branches) b->sem();
	}
	if(else_branch != nullptr) else_branch->sem();
}


Loop::Loop(vector<shared_ptr<Simple>>* i,	Expr* c, vector<shared_ptr<Simple>>* s,	vector<shared_ptr<Stmt>>* ct): inits(i), condition(c), steps(s), cond_true(ct) {}
Loop::~Loop() { delete inits; delete condition; delete steps; delete cond_true; }
void Loop::printNode(ostream &out) const {
	out << "Loop(";
	bool first = true;
	for(shared_ptr<Simple> s: *inits) {
		if(!first) out << ", ";
		first = false;
		out << *s;
	}
	out << ", " << *condition;
	for(shared_ptr<Simple> s: *steps) {
		out << ", ";
		out << *s;
	}
	for(shared_ptr<Stmt> s: *cond_true) {
		out << ", ";
		out << *s;
	}
	out << ")";
}
void Loop::sem() {
	condition->sem();
	condition->typeCheck(TYPE_bool);
	for(shared_ptr<Simple> s: *inits) 	s->sem();
	for(shared_ptr<Simple> s: *steps) 	s->sem();
	for(shared_ptr<Stmt> s: *cond_true) s->sem();
}


Formal::Formal(Type t, vector<const char*>* i, string cb): type(t), idl(i) {
		if(cb == "val") call_by_reference = false;
		else if(cb == "ref") call_by_reference = true;  //could be enum
	}
Formal::~Formal() { delete idl; }
void Formal::printNode(ostream &out) const {
	out << "Formal(" << type << ", ";
	if(call_by_reference == true) out << "call by reference" << ", ";
	bool first = true;
	for(const char* i: *idl) {
		if(!first) out << ", ";
		first = false;
		out << i;
	}
	out << ")";
}
void Formal::sem() {
	for(const char* i: *idl) {
		// saves each var into the SymbolTable in the scope of the function
		st.insert(string(i), type);
	}
}
vector<Formal*>* Formal::getOpenedFormal() {
	vector<Formal*>* opened = new vector<Formal*>;
	string ref = (call_by_reference ? "ref" : "val");
	for(const char* id: *idl) {
		vector<const char*>* idl_self = new vector<const char*>;
		idl_self->push_back(id);
		opened->push_back(new Formal(type, idl_self, ref));
	}
	return opened;
}
Type Formal::getType() {
	return type;
}
vector<const char*>* Formal::getIds() {
	return idl;
}
bool Formal::getCb() {
	return call_by_reference;
}


Header::Header(const char* i, vector< Formal*>* f, Type t): id(i), fl(f) {
	type = t;
}
Header::~Header() { delete id; delete fl; }
void Header::printNode(ostream &out) const {
	out << "Header(" << id;
	if(type.p != TYPE_void) out << ", " << type;
	if(fl != nullptr) {
		for(Formal *f: *fl) {
			out << ", ";
			out << *f;
		}
	}
	out << ")";
}
void Header::sem(bool func) {
	if (!st.EmptyScopes()){
		cout << "Looking up for declaration of the function ... " << endl;
		SymbolEntry *e = st.lookup(id, "func_decl");
		vector<Formal*>* params;
		if(fl != nullptr) {
			params = new vector<Formal*>;
			for(Formal *f: *fl) {
				vector<Formal*>* subformals;
				subformals = f->getOpenedFormal();
				for(Formal *f: *subformals) {
					params->push_back(f);
				}
			}
		}
		else params = nullptr;
		if (e == nullptr) {
			if(func) st.insert(string(id), type, "func_def", params);
			else st.insert(string(id), type, "func_decl", params);
		}
		else {
			string def = e->from;
			if ((def == "func_def") && func) error("Duplicate function definition");
			if ((def == "func_decl") && !func) error("Duplicate function declaration");
			else {
				if (!(e->type == type)) error("Mismatch in function definition");
				vector<Formal*>* decl_params = e->params;
				if(decl_params != nullptr) {
					int i=0;
					for(Formal* f_def: *params) {
						Formal* f_decl = decl_params->at(i);
						ostringstream formatted;
						if(!(f_def->getType() == f_decl->getType())) {
							formatted << "Mismatch in arg type at position " << i;
							error(formatted.str());
						}
						if(strcmp(f_def->getIds()->at(0), f_decl->getIds()->at(0))) {
							formatted << "Mismatch in arg id at position " << i;
							error(formatted.str());
						}
						if(f_def->getCb() != f_decl->getCb()) {
							formatted << "Mismatch in arg call by method at position " << i;
							error(formatted.str());
						}
						i++;
					} // checks if types, names, and callbys are the same as decl
				}
			} 
		}
	}
	st.openScope();
	cout << "+++ Opening new scope!" << endl;

	if((fl != nullptr) & func) {
		for(Formal *f: *fl) {
				f->sem();
		}
	}
}


Def::~Def() {}


FuncDef::FuncDef(Header* h, vector<shared_ptr<Def>>* d, vector<shared_ptr<Stmt>>* s):
	hd(h), defl(d), stmtl(s) {}
FuncDef::~FuncDef() { delete hd; delete defl; delete stmtl; }
void FuncDef::printNode(ostream &out) const {
	out << "FuncDef(" << *hd << ", ";
	bool first = true;
	for(shared_ptr<Def> d: *defl) {
		if(!first) out << ", ";
		first = false;
		out << *d;
	}
	for(shared_ptr<Stmt> s: *stmtl) {
		if(!first) out << ", ";
		first = false;
		out << *s;
	}
	out << ")";
}
void FuncDef::sem() {
	hd->sem();
	for(shared_ptr<Def> d: *defl) d->sem();
	for(shared_ptr<Stmt> s: *stmtl) s->sem();
	size = st.getSizeOfCurrentScope();
	cout << "--- Closing scope!" << endl;
  st.closeScope();
}


FuncDecl::FuncDecl(Header* h): hd(h) {}
FuncDecl::~FuncDecl() { delete hd; }
void FuncDecl::printNode(ostream &out) const {
	out << "FuncDecl(" << *hd << ")";
}
void FuncDecl::sem() {
	hd->sem(false);
	cout << "--- Closing scope!" << endl;
	st.closeScope();
}


VarDef::VarDef(Type t, vector<const char*>* i): type(t), idl(i) {}
VarDef::~VarDef() { delete idl; }
void VarDef::printNode(ostream &out) const {
	out << "VarDef(";
	out << type << ", ";
	bool first = true;
	for(const char* i: *idl) {
		if(!first) out << ", ";
		first = false;
		out << i;
	}
	out << ")";
}
void VarDef::sem() {
	for(const char* i: *idl){
		st.insert(string(i), type);
	}
}


Library::Library() {}
void Library::init() {
	vector<Formal*>* params;
	vector<const char*>* id_list;

	Type return_type;
	Type variable_type;
	Type varh_t;

	bool built_in = true;

	// insert(name, returnType, "func_decl", vector<Formal*>* params)

	// void puti(int n)
 	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("n");
	variable_type.p = TYPE_int;
	return_type.p = TYPE_void;

	params->push_back(new Formal(variable_type, id_list, "val"));
	st.insert("puti", return_type, "func_decl", params, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	st.insert("n", variable_type);
	st.closeScope(); cout << "--- Closing scope!" << endl;

	// void putb(bool b)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("b");
	variable_type.p = TYPE_bool;
	return_type.p = TYPE_void;

	params->push_back(new Formal(variable_type, id_list, "val"));
	st.insert("putb", return_type, "func_decl", params, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	st.insert("b", variable_type);
	st.closeScope(); cout << "--- Closing scope!" << endl;

	// void putc(char c)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("c");
	variable_type.p = TYPE_char;
	return_type.p = TYPE_void;

	params->push_back(new Formal(variable_type, id_list, "val"));
	st.insert("putc", return_type, "func_decl", params, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	st.insert("c", variable_type);
	st.closeScope(); cout << "--- Closing scope!" << endl;

	// void puts(char[] s)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("s");
	varh_t.p = TYPE_char; variable_type.c = new Array(varh_t);
	return_type.p = TYPE_void;

	params->push_back(new Formal(variable_type, id_list, "val"));
	st.insert("puts", return_type, "func_decl", params, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	st.insert("s", variable_type);
	st.closeScope(); cout << "--- Closing scope!" << endl;

	// int geti()
	return_type.p = TYPE_int;
	st.insert("geti", return_type, "func_decl", nullptr, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	st.closeScope(); cout << "--- Closing scope!" << endl;

	// bool getb()
	return_type.p = TYPE_bool;
	st.insert("getb", return_type, "func_decl", nullptr, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	st.closeScope(); cout << "--- Closing scope!" << endl;

	// char getc()
	return_type.p = TYPE_char;
	st.insert("getc", return_type, "func_decl", nullptr, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	st.closeScope(); cout << "--- Closing scope!" << endl;

	// void gets(int n, char[] s)	// n: max character to read from input (including /0), s: the array of chars where you should put the resault
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("n");
	variable_type.p = TYPE_int;

	params->push_back(new Formal(variable_type, id_list, "val"));

	id_list = new vector<const char*>;
	id_list->push_back("s");
	varh_t.p = TYPE_char; variable_type.c = new Array(varh_t);

	params->push_back(new Formal(variable_type, id_list, "ref")); // TODO: not sure if ref is true

	return_type.p = TYPE_void;
	st.insert("gets", return_type, "func_decl", params, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	variable_type.p = TYPE_int;
	st.insert("n", variable_type);
	varh_t.p = TYPE_char;
	variable_type.c = new Array(varh_t);
	st.insert("s", variable_type);
	st.closeScope(); cout << "--- Closing scope!" << endl;

	// int abs(int n)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("n");
	variable_type.p = TYPE_int;

	params->push_back(new Formal(variable_type, id_list, "val"));
	return_type.p = TYPE_int;
	st.insert("abs", return_type, "func_decl", params, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	st.insert("n", variable_type);
	st.closeScope(); cout << "--- Closing scope!" << endl;

	// int ord(char c)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("c");
	variable_type.p = TYPE_char;

	params->push_back(new Formal(variable_type, id_list, "val"));
	return_type.p = TYPE_int;
	st.insert("ord", return_type, "func_decl", params, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	st.insert("c", variable_type);
	st.closeScope(); cout << "--- Closing scope!" << endl;

	// char chr(int n)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("n");
	variable_type.p = TYPE_int;

	params->push_back(new Formal(variable_type, id_list, "val"));
	return_type.p = TYPE_char;
	st.insert("chr", return_type, "func_decl", params, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	st.insert("n", variable_type);
	st.closeScope(); cout << "--- Closing scope!" << endl;

	// int strlen(char[] s)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("s");
	varh_t.p = TYPE_char; variable_type.c = new Array(varh_t);

	params->push_back(new Formal(variable_type, id_list, "val"));
	return_type.p = TYPE_int;
	st.insert("strlen", return_type, "func_decl", params, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	st.insert("s", variable_type);
	st.closeScope(); cout << "--- Closing scope!" << endl;

	// int strcmp(char[] s1, s2)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("s1");
	id_list->push_back("s2");
	varh_t.p = TYPE_char; variable_type.c = new Array(varh_t);

	params->push_back(new Formal(variable_type, id_list, "val"));
	return_type.p = TYPE_int;
	st.insert("strcmp", return_type, "func_decl", params, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	st.insert("s1", variable_type);
	st.insert("s2", variable_type);
	st.closeScope(); cout << "--- Closing scope!" << endl;

	// void strcpy(char[] trg, src)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("trg");
	id_list->push_back("src");
	varh_t.p = TYPE_char; variable_type.c = new Array(varh_t);

	params->push_back(new Formal(variable_type, id_list, "ref")); // TODO: not sure about ref
	return_type.p = TYPE_void;
	st.insert("strcpy", return_type, "func_decl", params, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	st.insert("trg", variable_type);
	st.insert("src", variable_type);
	st.closeScope(); cout << "--- Closing scope!" << endl;

	// void strcat(char[] trg, src)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("trg");
	id_list->push_back("src");
	varh_t.p = TYPE_char; variable_type.c = new Array(varh_t);

	params->push_back(new Formal(variable_type, id_list, "ref")); // TODO: not sure about ref
	return_type.p = TYPE_void;
	st.insert("strcat", return_type, "func_decl", params, built_in);

	st.openScope(); cout << "+++ Opening new scope!" << endl;
	st.insert("trg", variable_type);
	st.insert("src", variable_type);
	st.closeScope(); cout << "--- Closing scope!" << endl;

}
