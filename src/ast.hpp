#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <sstream>
#include <vector>
#include <memory>
#include <string>
#include "lexer.hpp"
#include "symbol.hpp"

using namespace std;

#define PRE_DEBUG 1


class CompositeType { //abstract class
public:
	virtual ~CompositeType() {}
	string getId() {
		return id;
	}
	Type getType() {
		return type;
	}
protected:
	string id;
	Type type;
};

class Array: public CompositeType {
public:
	Array(Type t) {
		id = "array";
		type = t;
	}
	Array(PrimitiveType p) {
		id = "array";
		type.p = p;
	}
};

class List: public CompositeType {
public:
	List(Type t) {
		id = "list";
		type = t;
	}
	List(PrimitiveType p) {
		id = "list";
		type.p = p;
	}
};

inline bool isPrimitive(Type t) {
	bool is_prim = false;
	switch(t.p) {
		case TYPE_int: is_prim = true; break;
		case TYPE_bool: is_prim = true; break;
		case TYPE_char: is_prim = true; break;
		case TYPE_null: is_prim = true; break;
		default: break; // it means it is a CompositeType
	}
	return is_prim;
}

inline ostream& operator<<(ostream &out, const Type t) {
	switch(t.p) {
		case TYPE_int: out << "int"; return out;
		case TYPE_bool: out << "bool"; return out;
		case TYPE_char: out << "char"; return out;
		case TYPE_null: out << "null"; return out;
		default: break;
	}
	out << (t.c)->getId() << "[" << ((t.c)->getType()) << "]";
	return out;
}

inline ostream& operator<<(ostream &out, const PrimitiveType p) { // overloading
	switch(p) {
		case TYPE_int: out << "int"; return out;
		case TYPE_bool: out << "bool"; return out;
		case TYPE_char: out << "char"; return out;
		case TYPE_null: out << "null"; return out;
	}
}

inline bool operator==(const Type &t1, const Type &t2) {
	bool res, comp1 = false, comp2 = false;
	if(t1.p == TYPE_null || t2.p == TYPE_null) return true;
	comp1 = !isPrimitive(t1);
	comp2 = !isPrimitive(t2);
	if(!comp1 && !comp2) {
		switch(t1.p) {
			case TYPE_int: res = (t2.p == TYPE_int ? true : false); break;
			case TYPE_bool: res = (t2.p == TYPE_bool ? true : false); break;
			case TYPE_char: res = (t2.p == TYPE_char ? true : false); break;
			case TYPE_null: res = (t2.p == TYPE_null ? true : false); break;
		}
		if(res) return true;
	}
	else if(comp1 && comp2) {
		if((t1.c)->getId() == (t2.c)->getId() && ((t1.c)->getType() == (t2.c)->getType())) return true;
	}
	return false;
}

class ASTnode { //abstract class
public:
	virtual ~ASTnode() {}
	virtual void printNode(ostream &out) const = 0;
	virtual void sem() {}
};

inline ostream& operator<<(ostream &out, const ASTnode &n) {
	n.printNode(out);
	return out;
}

class Expr: public ASTnode { //abstract class
public:
	virtual ~Expr() {}
	// TODO: there is a problem here...it never reaches the override function inside const when the caller is not const i.e. Op

	void typeCheck(Type t) {
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

	void typeCheck(PrimitiveType p) { // overloading
		Type t;
		t.p = p;
		typeCheck(t);
	}

	void typeCheck(CompositeType* c) { // overloading
		Type t;
		t.c = c;
		typeCheck(t);
	}

	void typeCheck(CompositeType* c1, CompositeType* c2) { // overloading for union
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
			formatted << "Type " << type << "unsubscriptable";
			error(formatted.str());
		}
		#if PRE_DEBUG
		else {
			cout << "  ---> ok" << endl;
		}
		#endif
	}
	
	Type getType() {
		return type;
	}

protected:
  Type type;
};

class Const: public virtual Expr {
public:
	Const(int i)		 			{ tc.integer = i; tc_act = TC_int; }
	Const(char c) 				{ tc.character = c; tc_act = TC_char; }
	Const(string v, bool special=1) { // for boolean types and nil + string literals
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
	~Const() {}
	virtual void printNode(ostream &out) const override {
		out << "Const(";
		switch(tc_act) {
			case(TC_int): out << tc.integer << ")"; break;
			case(TC_char): out << tc.character << ")"; break;
			case(TC_str): out << tc.str << ")"; break;
			case(TC_bool): out << (tc.boolean ? "true" : "false") << ")"; break;
			case(TC_nil): out << "nil_list" << ")"; break;
		}
	}
	virtual void sem() override {
			switch(tc_act) {
				case(TC_int): type.p = TYPE_int; break;
				case(TC_char): type.p = TYPE_char; break;
				case(TC_str): type.c = new Array(TYPE_char); break;
				case(TC_bool): type.p = TYPE_bool; break;
				case(TC_nil): type.c = new List(TYPE_null); break;
			}
	}
private:
	union TC {
		int integer;
		char character;
		const char* str;
		bool boolean;
	};
	TC tc;
	enum TC_active { TC_int, TC_char, TC_str, TC_bool, TC_nil };
	TC_active tc_act;
};

class PreOp: public Expr {
public:
	PreOp(const char* o, Expr* e): op(o), expr(e) {}
	~PreOp() { delete expr; }
	virtual void printNode(ostream &out) const override {
		out << "PreOp(" << op << ", " << *expr << ")";
	}
	virtual void sem() override {
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
			expr->typeCheck(new List(TYPE_null));
			type.p = TYPE_bool;
		}
		else if(op == "head" || op == "tail" ) {
			expr->typeCheck(new List(TYPE_null));
			type = ((expr->getType()).c)->getType();
		}
	}
private:
	string op;
	Expr* expr;
};

class Op: public Expr {
public:
	Op(Expr* e1, const char* o, Expr* e2): op(o), expr1(e1), expr2(e2) {}
	~Op() { delete expr1; delete expr2; }
	virtual void printNode(ostream &out) const override {
		out << "Op(" << *expr1 << ", " << op << ", " << *expr2 << ")";
	}
	virtual void sem() override {
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
private:
	string op;
	Expr* expr1;
	Expr* expr2;
};

class MemoryAlloc: public Expr {
public:
	MemoryAlloc(Type t, Expr* e): new_type(t), expr(e) {}
	~MemoryAlloc() { delete expr; }
	virtual void printNode(ostream &out) const override {
		out << "MemoryAlloc(" << type << ", " << *expr << ")";
	}
	virtual void sem() {
		expr->sem();
		expr->typeCheck(TYPE_int);

		Type final_type;
		final_type.c = new Array(new_type);
		type = final_type;
	}
private:
	Type new_type;
	Expr* expr;
};

class Atom: virtual public Expr { //abstract class
public:
	virtual ~Atom() {}
	virtual const char* getId() {return "";}
};

class Id: public Atom {
public:
	Id(const char* i): id(i) {}
	~Id() { delete id; }
	virtual void printNode(ostream &out) const override {
		out << "Id(" << id << ")";
	}
	const char* getId() override {
		return id;
	}
	virtual void sem() override {
		cout << "Searching for: " << id << " ... ";
		SymbolEntry *e = st.lookup(string(id));
		if(e != nullptr) {
			cout << "Found it with offset: " << e->offset << " and type: " << e->type << endl;
			type = e->type;
		}
		else {
			Type t;
			t.p = TYPE_null;
			type = t;
		}
	}
private:
	const char* id;
};

class String: public Atom, public Const {
public:
	String(string s): Const(s, 0) {}
	~String() {}
};

class DirectAcc: public Atom {
public:
	DirectAcc(Atom* a, Expr* e): atom(a), expr(e) {}
	~DirectAcc() { delete atom; delete expr; }
	virtual void printNode(ostream &out) const override {
		out << "DirectAcc(" << *atom << ", " << *expr << ")";
	}
	virtual void sem() {
		expr->sem();
		atom->sem();
		atom->typeCheck(new Array(TYPE_null), new List(TYPE_null));
		expr->typeCheck(TYPE_int);
		type = ((atom->getType()).c)->getType();
	}
private:
	Atom* atom;
	Expr* expr;
};

class Stmt: public ASTnode { //abstract class
public:
	virtual ~Stmt() {}
};

class Simple: public Stmt { //abstract class
public:
	virtual ~Simple() {}
};

class NoAction: public Simple {
public:
	NoAction() {}
	~NoAction() {}
	virtual void printNode(ostream &out) const override {
		out << "no_action";
	}
	virtual void sem() {}
};

class Assign: public Simple {
public:
	Assign(Atom* a, Expr* e): atom(a), expr(e) {}
	~Assign() { delete atom; delete expr; }
	virtual void printNode(ostream &out) const override {
		out << "Assign(" << *atom << ", " << *expr << ")";
	}
	virtual void sem() {
		expr->sem();
		atom->sem();
		expr->typeCheck(atom->getType());

		// acceptable: atom is id[index], where id is array/list of same type as expr
		// if i dont check that id is defined in this scope, will blow up in the code generation stage
		// (check is either call return id defined in this scope OR call return id was arg passed by ref)

		// acceptable: atom is string[expr] because of papy comment http://moodle.softlab.ntua.gr/mod/forum/discuss.php?d=9839
		// -> means that "test"[0]='e' will blow up in the code generation stage
	}
private:
	Atom* atom;
	Expr* expr;
};

class Call: public Simple {
public:
	Call(const char* i, Type t, vector<shared_ptr<Expr>>* e = nullptr): id(i), exprList(e), type(t){}
	~Call() { delete id; delete exprList; }
	virtual void printNode(ostream &out) const override {
		out << "Call(" << id;
		if(exprList != nullptr) {
			for(shared_ptr<Expr> e: *exprList) {
				out << ", ";
				out << *e;
			}
		}
		out << ")";
	}
	virtual void sem() {
		// check if the function is defined
		SymbolEntry *func = st.lookup(string(id), "func_def");
		vector<Type> params = func->params;
		type = func->type;

		if(exprList != nullptr) {
			// we have to check if the function's arguments are the same type as the exprList (one by one)
			int i = 0;
			for(shared_ptr<Expr> e: *exprList) {
				e->sem();
				e->typeCheck(params.at(i));
				i++;
			}
		}
	}
	Type getType() {
		return type;
	}

private:
	const char* id;
	vector<shared_ptr<Expr>>* exprList;
	Type type;
};

class ReturnValue: public Atom {
public:
	ReturnValue(Call* c): call(c) {}
	~ReturnValue() { delete call; }
	virtual void printNode(ostream &out) const override {
		out << "ReturnValue(" << *call << ")";
	}
	virtual void sem() {
		// CAN I SOMEHOW GET FUNC RETURN ID AS WELL? see assign comment
		call->sem();
		if(call->getType().p == TYPE_null) {
			ostringstream formatted;
			formatted << *call << " does not return a type";
			error(formatted.str());
		}
		type = call->getType();
	}

private:
	Call* call;
};

class Return: public Stmt {
public:
	Return(Expr* v = nullptr): ret_val(v) {}
	~Return() { delete ret_val; }
	virtual void printNode(ostream &out) const override {
		out << "Return(";
		if(ret_val != nullptr) out << *ret_val;
		out << ")";
	}
	virtual void sem() {
		// cout << "INSIDE SEM for Return" << endl;
		// we have to typecheck to see if the return type is the same as the type of the function
		if(ret_val != nullptr) {
			ret_val->sem();
			// cout << ret_val->getType() << endl;
			cout << "Checking return type ..." << endl;
			ret_val->typeCheck(st.getReturnType());
		}
	}
private:
	Expr* ret_val;
};

class Branch: public Stmt {
public:
	Branch(vector<shared_ptr<Stmt>>* ct,
		Expr* c = new Const("true"),
		vector<Branch*>* eif = nullptr,
		Branch* e = nullptr): cond_true(ct), condition(c), elsif_branches(eif), else_branch(e) {}
	~Branch() { delete cond_true; delete condition; delete elsif_branches; delete else_branch; }
	virtual void printNode(ostream &out) const override {
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
	virtual void sem() override {
		// cout << "INSIDE SEM for Branch" << endl;
		condition->sem();
		condition->typeCheck(TYPE_bool);
		for(shared_ptr<Stmt> s: *cond_true) s->sem();
		if(elsif_branches != nullptr) {
			for(Branch *b: *elsif_branches) b->sem();
		}
		if(else_branch != nullptr) else_branch->sem();
	}
private:
	vector<shared_ptr<Stmt>>* cond_true;
	Expr* condition;
	vector<Branch*>* elsif_branches;
	Branch* else_branch;
};

class Loop: public Stmt {
public:
	Loop(vector<shared_ptr<Simple>>* i,
		Expr* c,
		vector<shared_ptr<Simple>>* s,
		vector<shared_ptr<Stmt>>* ct): inits(i), condition(c), steps(s), cond_true(ct) {}
	~Loop() { delete inits; delete condition; delete steps; delete cond_true; }
	virtual void printNode(ostream &out) const override {
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

	virtual void sem() override {
		// cout << "INSIDE SEM for Loop" << endl;
		condition->sem();
		condition->typeCheck(TYPE_bool);
		for(shared_ptr<Simple> s: *inits) 	s->sem();
		for(shared_ptr<Simple> s: *steps) 	s->sem();
		for(shared_ptr<Stmt> s: *cond_true) s->sem();
	}

private:
	vector<shared_ptr<Simple>>* inits;
	Expr* condition;
	vector<shared_ptr<Simple>>* steps;
	vector<shared_ptr<Stmt>>* cond_true;
};

class Formal: public ASTnode {
public:
	Formal(Type t, vector<const char*>* i, string cb): type(t), idl(i) {
		if(cb == "val") call_by_reference = false;
		else if(cb == "ref") call_by_reference = true;  //could be enum
	}
	~Formal() { delete idl; }
	virtual void printNode(ostream &out) const override {
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
	virtual void sem() {
		// IF BY REFERENCE THEN POINT TO OUTER SCOPE VAR!!!
		for(const char* i: *idl) {
			// save each var into the SymbolTable in the scope of the function
			st.insert(string(i), type);
		}
	}
	pair<Type, int> getType() {
		return make_pair(type, idl->size());
	}
private:
	Type type;
	vector<const char*>* idl;
	bool call_by_reference;
};

class Header: public ASTnode {
public:
	Header(const char* i, vector< Formal*>* f, Type t): id(i), fl(f) {
		type = t;
	}
	Header(const char* i, vector< Formal*>* f): id(i), fl(f) {}
	~Header() { delete id; delete fl; }
	virtual void printNode(ostream &out) const override {
		out << "Header(" << id;
		if(type.p != TYPE_null) out << ", " << type;
		if(fl != nullptr) {
			for(Formal *f: *fl) {
				out << ", ";
				out << *f;
			}
		}
		out << ")";
	}
	virtual void sem(bool func = true) {
		// cout << "INSIDE SEM for Header" << endl;
		if (!st.EmptyScopes()){
			cout << "Looking up for declaration of the function... ";
			SymbolEntry *e = st.lookup(id, "func_decl");
			vector<Type> params;
			if(fl != nullptr) {
				for(Formal *f: *fl) {
					pair<Type, int> pair_type = f->getType();
					params.insert(params.end(), pair_type.second, pair_type.first);
				}
			}
			if (e == nullptr) {
				if(func) st.insert(string(id), type, "func_def", params);
				else st.insert(string(id), type, "func_decl", params);
			}
			else {
				string def = e->from;
				if (def == "func_def") error("Duplicate function definition");
				else {
					if (!((e->type == type) & (e->params == params))) error("Mismatch in function definition");
				} // check if the parameters and the type are the same
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
private:
	const char* id;
	vector< Formal*>* fl;
	Type type;
};

class Def: public ASTnode { //abstract class
public:
	virtual ~Def() {}
};

class FuncDef: public Def {
public:
	FuncDef(Header* h, vector<shared_ptr<Def>>* d, vector<shared_ptr<Stmt>>* s):
		hd(h), defl(d), stmtl(s) {}
	~FuncDef() { delete hd; delete defl; delete stmtl; }
	virtual void printNode(ostream &out) const override {
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
	virtual void sem() {
		// cout << "INSIDE SEM for FuncDef" << endl;
		hd->sem();
		for(shared_ptr<Def> d: *defl) d->sem();
		for(shared_ptr<Stmt> s: *stmtl) s->sem();
		size = st.getSizeOfCurrentScope();
		cout << "--- Closing scope!" << endl;
    st.closeScope();
	}
private:
	Header* hd;
	vector<shared_ptr<Def>>* defl;
	vector<shared_ptr<Stmt>>* stmtl;
	int size;
};

class FuncDecl: public Def {
public:
	FuncDecl(Header* h): hd(h) {}
	~FuncDecl() { delete hd; }
	virtual void printNode(ostream &out) const override {
		out << "FuncDecl(" << *hd << ")";
	}
	virtual void sem() {
		// cout << "INSIDE SEM for FuncDecl" << endl;
		hd->sem(false);
		cout << "--- Closing scope!" << endl;
		st.closeScope();
		// TODO: we need to check for duplicate declarations
		// and also if the same header has been defined berfore with FuncDef
		// and somehow check for the scopes
	}
private:
	Header* hd;
};

class VarDef: public Def {
public:
	VarDef(Type t, vector<const char*>* i): type(t), idl(i) {}
	~VarDef() { delete idl; }
	virtual void printNode(ostream &out) const override {
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
	virtual void sem() {
		#if PRE_DEBUG
		// cout << "INSIDE SEM for VarDef" << endl;
		#endif
		for(const char* i: *idl){
			st.insert(string(i), type);
			// st.insert("a", type);
			// cout << *st.lookup("a");
		}
	}
private:
	Type type;
	vector<const char*>* idl;
};

#endif
