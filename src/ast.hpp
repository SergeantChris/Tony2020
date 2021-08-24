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
};

class List: public CompositeType {
public:
	List(Type t) {
		id = "list";
		type = t;
	}
};

inline PrimitiveType getPrimType(Type t) {
	switch(t.p) {
		case TYPE_int: return TYPE_int;
		case TYPE_bool: return TYPE_bool;
		case TYPE_char: return TYPE_char;
		case TYPE_str: return TYPE_str;
		case TYPE_null: return TYPE_null;
		default: break; // it means it is a CompositeType
	}
	return getPrimType((t.c)->getType());
}
inline bool isPrimitive(Type t) {
	bool prim_fault = false;
	switch(t.p) {
		case TYPE_int: prim_fault = true; break;
		case TYPE_bool: prim_fault = true; break;
		case TYPE_char: prim_fault = true; break;
		case TYPE_str: prim_fault = true; break;
		case TYPE_null: prim_fault = true; break;
		default: break; // it means it is a CompositeType
	}
	return prim_fault;
}
inline ostream& operator<<(ostream &out, const PrimitiveType p) {
	switch(p) {
		case TYPE_int: out << "int"; return out;
		case TYPE_bool: out << "bool"; return out;
		case TYPE_char: out << "char"; return out;
		case TYPE_str: out << "str"; return out;
		case TYPE_null: out << "null"; return out;
		default: out << "PrimitiveType"; break;
	}
	return out;
}

inline ostream& operator<<(ostream &out, const Type t) {
	switch(t.p) {
		case TYPE_int: out << "int"; return out;
		case TYPE_bool: out << "bool"; return out;
		case TYPE_char: out << "char"; return out;
		case TYPE_str: out << "str"; return out;
		case TYPE_null: out << "null"; return out;
		default: break;
	}
	out << (t.c)->getId() << "[" << ((t.c)->getType()) << "]";
	return out;
}
inline bool operator==(const Type &t1, const Type &t2) {
	bool res1, res2, comp1 = false, comp2 = false;
	// TODO: not totally sure if correct...check if TYPE_null is also used in any type checking between 2 Type objects (probably not)
	if(t1.p == TYPE_null || t2.p == TYPE_null) return true;
	switch(t1.p) {
		case TYPE_int: res1 = (t2.p == TYPE_int ? true : false); break;
		case TYPE_bool: res1 = (t2.p == TYPE_bool ? true : false); break;
		case TYPE_char: res1 = (t2.p == TYPE_char ? true : false); break;
		case TYPE_str: res1 = (t2.p == TYPE_str ? true : false); break;
		case TYPE_null: res1 = (t2.p == TYPE_null ? true : false); break;
		default: comp1 = true; break; // it means it is a CompositeType
	}
	switch(t2.p) {
		case TYPE_int: res2 = (t1.p == TYPE_int ? true : false); break;
		case TYPE_bool: res2 = (t1.p == TYPE_bool ? true : false); break;
		case TYPE_char: res2 = (t1.p == TYPE_char ? true : false); break;
		case TYPE_str: res2 = (t1.p == TYPE_str ? true : false); break;
		case TYPE_null: res2 = (t1.p == TYPE_null ? true : false); break;
		default: comp2 = true; break; // it means it is a CompositeType
	}
	if(res1 && res2) return true;
	if(comp1 && comp2)
		if((t1.c)->getId() == (t2.c)->getId() && ((t1.c)->getType() == (t2.c)->getType())) return true;
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
	void primTypeCheck(PrimitiveType t) {
		#if PRE_DEBUG
		cout << "-- TYPE CHECK  (" << t << ")";
		#endif
		if(type.p != t) {
			cout << endl;
			ostringstream formatted;
			formatted << "Type mismatch, expected type: " << t << ", but type: " << type << " was used";
			error(formatted.str());
			// error("Type mismatch, expected type: %s but type: %s was used", "this", "that");
			// cout << "Type mismatch, expected type: " << t << ", but type: " << type << " was used" << endl;
		}
		#if PRE_DEBUG
		else {
			cout << "  ---> ok" << endl;
		}
		#endif
	}
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
	void firstLayerCompositeTypeCheck(string s) {
		#if PRE_DEBUG
		cout << "-- First Layer Composite TYPE CHECK (" << s << ")";
		#endif
		if(isPrimitive(type)) {
			cout << endl;
			ostringstream formatted;
			formatted << "Type mismatch, expected outer type to be: " << s << ", but the given type is Primitive of type: " << type;
			error(formatted.str());
		}
		else if(s == "@" && !((type.c)->getId() == "array" || (type.c)->getId() == "list" || (type.c)->getId() == "string")) {
			cout << endl;
			ostringstream formatted;
			formatted << "Type mismatch, expected type: array or list or string, but type: " << type.c->getId() << " was used";
			error(formatted.str());
		}
		else if((type.c)->getId() != s) {
			cout << endl;
			ostringstream formatted;
			formatted << "Type mismatch, expected type: " << s << ", but type: " << type.c->getId() << " was used";
			error(formatted.str());
		}
		#if PRE_DEBUG
		else {
			cout << "  ---> ok" << endl;
		}
		#endif
	}
	void nestedCompositeyTpeCheck(Type t, string s) {
		#if PRE_DEBUG
		cout << "-- nested Composite TYPE CHECK (" << type << ", " << t << ")";
		#endif
		if(isPrimitive(type)) {
			cout << endl;
			ostringstream formatted;
			formatted << "Type mismatch, expected outer type to be: " << s << " , but the given type is Primitive of type: " << type;			
			error(formatted.str());
		}
		else if(!((type.c)->getType() == t)) {
			cout << endl;
			ostringstream formatted;
			formatted << "Type mismatch, expected type: " << (type.c)->getType() << ", but type: " << t << " was used";			
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
	Type getNestedType() {
		if(isPrimitive(type)) {
			ostringstream formatted;
			formatted << "Type mismatch, trying to acces nested type, but it doesent exist... type is Primitive: " << type << endl;
			formatted << "Returning the type as is...";
			error(formatted.str());
			return type; // why is this here, error exits...
		}
		else return (type.c)->getType();
	}
protected:
  Type type;
};

class Const: public virtual Expr {
public:
	Const(int i)		 			{ tc.integer = i; tc_act = TC_int; }
	Const(char c) 				{ tc.character = c; tc_act = TC_char; }
	// Const(const char* s)  { tc.str = s; tc_act = TC_str; }
	Const(string v) { // for boolean types and nil + string literals
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
		else {
			tc.str = v.c_str();
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
			case(TC_nil): out << "List[...nil]" << ")"; break;
		}
	}
	virtual void sem() override {
			// cout << "INSIDE SEM for Const" << endl;
			switch(tc_act) {
				case(TC_int): type.p = TYPE_int; break;
				case(TC_char): type.p = TYPE_char; break;
				case(TC_str): type.p = TYPE_str; break;
				case(TC_bool): type.p = TYPE_bool; break;
				case(TC_nil): Type t; t.p = TYPE_null; type.c = new List(t); break;
			}
	}
private:
	union TC {
		int integer;
		char character;
		const char* str; //cannot be string
		bool boolean;
		// std::vector<int> list; ; //weird list thing for nil TODO: dont know if correct but gives error
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
		// cout << "INSIDE SEM for PreOp" << endl;
		expr->sem();
			if(op == "+" || op == "-") {
				expr->primTypeCheck(TYPE_int);
				type.p = TYPE_int;
			}
			else if(op == "not") {
				expr->primTypeCheck(TYPE_bool);
				type.p = TYPE_bool;
			}
			else if(op == "nil?") {
				expr->firstLayerCompositeTypeCheck("list");
				type.p = TYPE_bool;
			}
			else if(op == "head" || op == "tail" ) {
				expr->firstLayerCompositeTypeCheck("list");
				type = expr->getNestedType();
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
		// cout << "INSIDE SEM for Op" << endl;
		expr1->sem();
		expr2->sem();
		if(op == "and" || op == "or") {
			expr1->primTypeCheck(TYPE_bool);
			expr2->primTypeCheck(TYPE_bool);
			type.p = TYPE_bool;
		}
		else if(op == "#") { // expr1->t, expr2->list[t]
			expr2->firstLayerCompositeTypeCheck("list");
			expr2->nestedCompositeyTpeCheck(expr1->getType(), "list");
			type = expr2->getType();
		}
		else if(op == "=" || op == "<>" || op == "<" || op == ">" || op == "<=" || op == ">=") {
			Type expr1_type = expr1->getType();
			expr2->primTypeCheck(expr1_type.p);
			type.p = TYPE_bool;
		}
		else if(op == "+" || op == "-" || op == "*" || op == "/" || op == "mod") {
			expr1->primTypeCheck(TYPE_int);
			expr2->primTypeCheck(TYPE_int);
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
		// cout << "INSIDE SEM for MemoryAlloc" << endl;
		expr->sem();
		expr->primTypeCheck(TYPE_int);

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
		// cout << "INSIDE SEM for Id" << endl;
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
	String(const char* s): Const(s) {}
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
		// cout << "INSIDE SEM for DirectAcc" << endl;
		expr->sem();
		atom->sem();
		// atom->firstLayerCompositeTypeCheck("array");
		// TODO: '@' refers to (array or list or string)
		atom->firstLayerCompositeTypeCheck("@");
		expr->primTypeCheck(TYPE_int);
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

class Assign: public Simple {
public:
	Assign(Atom* a, Expr* e): atom(a), expr(e) {}
	~Assign() { delete atom; delete expr; }
	virtual void printNode(ostream &out) const override {
		out << "Assign(" << *atom << ", " << *expr << ")";
	}
	virtual void sem() {
		// cout << "INSIDE SEM for Assign" << endl;

		expr->sem();
		atom->sem();
		expr->typeCheck(atom->getType());
		// TODO: we also have to check what atom is...
		// for example it cant be string or call??
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
		out << "Call(" << id << ", ";
		bool first = true;
		for(shared_ptr<Expr> e: *exprList) {
			if(!first) out << ", ";
			first = false;
			out << *e;
		}
		out << ")";
	}
	virtual void sem() {
		// cout << "INSIDE SEM for Call" << endl;
		// we have to check if the function's arguments are the same type as the exprList (one by one)
		// so we have to look for the ids in the st
		// and also for the funcion itself (return type...)... not sure if true??

		// check if the function is defined
		SymbolEntry *func = st.lookup(string(id), "func_def");
		vector<Type> params = func->params;
		type = func->type;

		int i = 0;
		for(shared_ptr<Expr> e: *exprList) {
			e->sem();
			e->typeCheck(params.at(i));
			i++;
		}
	}
	Type getType(){
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
		// cout << "INSIDE SEM for Return Value" << endl;
		// TODO: initially we need to check if the function has return type
		call->sem();
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
		condition->primTypeCheck(TYPE_bool);
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
		condition->primTypeCheck(TYPE_bool);
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
		// cout << "INSIDE SEM for Formal" << endl;
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
			for(Formal *f: *fl) {
				pair<Type, int> pair_type = f->getType();
				params.insert(params.end(), pair_type.second, pair_type.first);
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
