#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
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
	// virtual int getIntVal() {return -990;}
	// virtual bool isMemoryAlloc() {return false;}
	// virtual int getExprIntVal() {return -999;}
	void primTypeCheck(PrimitiveType t) {
		#if PRE_DEBUG
		cout << "-- TYPE CHECK  (" << t << ")";
		#endif
		if(type.p != t) {
			cout << endl;
			error("");
			cout << "Type mismatch, expected type: " << t << ", but type: " << type << " was used" << endl;
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
			error("");
			cout << "Type mismatch, expected type: " << t << ", but type: " << type << " was used" << endl;
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
			error("");
			cout << "Type mismatch, expected outer type to be: " << s << ", but the given type is Primitive of type: " << type << endl;
		}
		else if((type.c)->getId() != s) {
			cout << endl;
			error("");
			cout << "Type mismatch, expected type: " << s << ", but type: " << type.c->getId() << " was used" << endl;
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
			error("");
			cout << "Type mismatch, expected outer type to be: " << s << " , but the given type is Primitive of type: " << type << endl;
		}
		else if(!((type.c)->getType() == t)) {
			cout << endl;
			error("");
			cout << "Type mismatch, expected type: " << (type.c)->getType() << ", but type: " << t << " was used" << endl;
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
			error("");
			cout << "Type mismatch, trying to acces nested type, but it doesent exist... type is Primitive: " << type << endl;
			cout << "Returning the type as is..." << endl;
			return type;
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
	Const(const char* s)  { tc.str = s; tc_act = TC_str; }
	Const(string v) { // for boolean types and nil
		if(v == "true") {
			tc.boolean = true;
			tc_act = TC_bool;
		}
		else if(v == "false") {
			tc.boolean = false;
			tc_act = TC_bool;
		}
		else if(v == "nil"){
			tc_act = TC_nil;
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
	// virtual int getIntVal() override {
	// 	if(tc_act == TC_int) return tc.integer;
	// 	return -999;
	// }
	virtual void sem() override {
			cout << "INSIDE SEM for Const" << endl;
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
		cout << "INSIDE SEM for PreOp" << endl;
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
		cout << "INSIDE SEM for Op" << endl;
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
	// virtual bool isMemoryAlloc() override {
	// 	return true;
	// }
	// virtual int getExprIntVal() override {
	// 	expr->primTypeCheck(TYPE_int);
	// 	return expr->getIntVal();
	// }
	virtual void sem() {
		cout << "INSIDE SEM for MemoryAlloc" << endl;
		expr->sem();
		expr->primTypeCheck(TYPE_int);
		// cout << "-- VAL CHECK  (" << expr->getIntVal() << ")";
		// if(expr->getIntVal() < 0) {
		// 	cout << endl;
		// 	error("Trying to create new array with size: %d", expr->getIntVal());
		// }
		// cout << "  ---> ok" << endl;
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
	// virtual void setArraySize(int size) {}
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
	// void setArraySize(int size) override {
	// 	Type t = my_st.lookup(id);
	// 	if(isPrimitive(t)) {
	// 		error("Trying to set array size to Primitive");
	// 	}
	// 	else {
	// 		my_st.setSize(id, size);
	// 	}
	// }
	virtual void sem() override {
		cout << "INSIDE SEM for Id" << endl;
		cout << "Searching for: " << id << " ... ";
		SymbolEntry *e = st.lookup(id);
		if(e != nullptr) {
			cout << "Found it with offset: " << e->offset << " and type: " << e->type << endl;
			type = e->type;
		}
		else {
			Type t;
			t.p = TYPE_null;
			type = t;
		}
		// Type t = my_st.lookup(id);
		// if(t.p != TYPE_null) {
		// 		cout << "Found it with type: " << t << endl;
		// }
		// 	type = t;
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
		cout << "INSIDE SEM for DirectAcc" << endl;
		expr->sem();
		atom->sem();
		atom->firstLayerCompositeTypeCheck("array");
		expr->primTypeCheck(TYPE_int);
		// cout << "-- VAL CHECK  (" << expr->getIntVal() << ")";
		// int array_size = my_st.getSize(atom->getId());
		// if(expr->getIntVal() < 0) {
		// 	error("Index is negative, cant access element of array: %s", atom->getId());
		// }
		// else if(expr->getIntVal() >= array_size) {
		// 	cout << endl;
		// 	error("Trying to access element: %d of array %s.\nHowever array has only size: %d", expr->getIntVal(), atom->getId(), array_size);
		// }
		// else {
		// 	cout << "  ---> ok" << endl;
		// }
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
		cout << "INSIDE SEM for Assign" << endl;
		expr->sem();
		// TODO: if the expr is MemoryAlloc we have to pass the array_size to atom
		atom->sem();
		expr->typeCheck(atom->getType());
		// if(expr->isMemoryAlloc()) {
		// 	atom->setArraySize(expr->getExprIntVal());
		// }
	}
private:
	Atom* atom;
	Expr* expr;
};

class Call: public Simple {
public:
	Call(const char* i, vector<shared_ptr<Expr>>* e = nullptr): id(i), exprList(e) {}
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
		cout << "INSIDE SEM for Call" << endl;
		// we have to check if the function's arguments are the same type as the exprList (one by one)
		// so we have to look for the ids in the st
		// and also for the funcion itself (return type...)... not sure if true??
	}
private:
	const char* id;
	vector<shared_ptr<Expr>>* exprList;
};

class ReturnValue: public Atom {
public:
	ReturnValue(Call* c): call(c) {}
	~ReturnValue() { delete call; }
	virtual void printNode(ostream &out) const override {
		out << "ReturnValue(" << *call << ")";
	}
	virtual void sem() {
		cout << "INSIDE SEM for Return Value" << endl;
		// TODO: initially we need to check if the function has return type
		call->sem();
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
		cout << "INSIDE SEM for Return" << endl;
		// dont know what to do yet
		if(ret_val != nullptr) {
			ret_val->sem();
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
		cout << "INSIDE SEM for Branch" << endl;
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
		cout << "INSIDE SEM for Loop" << endl;
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
		cout << "INSIDE SEM for Formal" << endl;
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
	virtual void sem() {
		cout << "INSIDE SEM for Header" << endl;

		// TODO: somewhere we need to check the header type and return type consistency
		if(type.p != TYPE_null) {

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
		// TODO: we need to do something with the Header
		// and we also need to open/close scopes
		cout << "INSIDE SEM for FuncDef" << endl;
		hd->sem();
		st.openScope();
		cout << "+++ Opening new scope!" << endl;
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
		cout << "INSIDE SEM for FuncDecl" << endl;

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
		cout << "INSIDE SEM for VarDef" << endl;
		#endif
		for(const char* i: *idl) st.insert(i, type);
	}
private:
	Type type;
	vector<const char*>* idl;
};

#endif
