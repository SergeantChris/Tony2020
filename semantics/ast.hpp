#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <vector>
#include <string>
#include "lexer.hpp"

using namespace std;

enum PrimitiveType { TYPE_int, TYPE_bool, TYPE_char };

class CompositeType { //abstract class
public:
	virtual ~CompositeType() {}
protected:
	string id;
	Type type;
};

class Array: public CompositeType {
public:
	Array(Type t): id("array"), type(t) {}
};

class List: public CompositeType {
public:
	List(Type t): id("list"), type(t) {}
};

union Type {
	PrimitiveType p;
	CompositeType* c;
	~Type() {}
};

inline ostream& operator<<(ostream &out, Type t) {
	switch(t) {
		case TYPE_int: out << "int"; break;
		case TYPE_bool: out << "bool"; break;
		case TYPE_char: out << "char"; break;
		switch((t.c)->id) {
			case "array": out << "array of" << ((t.c)->type); break;
			case "list": out << "list of" << ((t.c)->type); break;
		}
	}
	return out;
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
private:
	Header* hd;
	vector<shared_ptr<Def>>* defl;
	vector<shared_ptr<Stmt>>* stmtl;
};

class FuncDecl: public Def {
public:
	FuncDecl(Header* h): hd(h) {}
	~FuncDecl() { delete hd; }
	virtual void printNode(ostream &out) const override {
		out << "FuncDecl(" << *hd << ")";
	}
private:
	Header* hd;
};

class VarDef: public Def {
public:
	VarDef(Type t, vector<const char*>* i): type(t), idl(i) {}
	~VarDef() { delete idl; }
	virtual void printNode(ostream &out) const override {
		out << "VarDef(" << type << ", ";
		bool first = true;
		for(const char* i: *idl) {
			if(!first) out << ", ";
			first = false;
			out << i;
		}
		out << ")";
	}
private:
	Type type;
	vector<const char*>* idl;
};

class Header: public ASTnode {
public:
	Header(const char* i, vector<Formal>* f, Type t = nullptr): id(i), fl(f), type(t) {}
	~Header() { delete fl; }
	virtual void printNode(ostream &out) const override {
		out << "Header(" << id << ", ";
		if(type != nullptr) out << type << ", ";
		bool first = true;
		for(Formal f: *fl) {
			if(!first) out << ", ";
			first = false;
			out << f;
		}
		out << ")";
	}
private:
	const char* id;
	vector<Formal>* fl;
	Type type;
};

class Formal: public ASTnode {
public:
	Formal(Type t, vector<const char*>* i, string cb): type(t), idl(i) {
		switch(cb) {
			case "cbv": call_by_reference = false; break;
			case "cbr": call_by_reference = true; break; 
		}
	}
	~Formal() { delete idl; }
	virtual void printNode(ostream &out) const override {
		out << "Formal(" << type << ", ";
		if(call_by_reference == true) out << "cbr" << ", ";
		bool first = true;
		for(const char* i: *idl) {
			if(!first) out << ", ";
			first = false;
			out << i;
		}
		out << ")";
	}
private:
	Type type;
	vector<const char*>* idl;
	bool call_by_reference;
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
		out << "Assign(" << *atom << *expr << ")";
	}
private:
	Atom* atom;
	Expr* expr;
};

class Call: public Simple {
public:
	Call(const char* i, vector<shared_ptr<Expr>>* e = nullptr): id(i), exprl(e) {}
	~Call() { delete exprl; }
	virtual void printNode(ostream &out) const override {
		out << "Call(" << id << ", ";
		bool first = true;
		for(shared_ptr<Expr> e: *exprl) {
			if(!first) out << ", ";
			first = false;
			out << *e;
		}
		out << ")";
	}
private:
	const char* id;
	vector<shared_ptr<Expr>>* exprl; 
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
private:
	Expr* ret_val;
};

class Branch: public Stmt {
public:
	Branch(vector<shared_ptr<Stmt>>* ct, 
		Expr* c = new Const("true"), 
		vector<shared_ptr<Stmt>>* eif = nullptr,
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
		out << ", " << condition;
		if(elsif_branches != nullptr) {
			for(shared_ptr<Stmt> s: *elsif_branches) {
				out << ", ";
				out << *s;
			}
		}
		if(else_branch != nullptr) out << ", " << *else_branch;
		out << ")";
	}
private:
	vector<shared_ptr<Stmt>>* cond_true;
	Expr* condition;
	vector<shared_ptr<Stmt>>* elsif_branches;
	Branch* else_branch;
};

class Loop: public Stmt {
public:
	Loop(vector<shared_ptr<Stmt>>* i, 
		Expr* c, 
		vector<shared_ptr<Stmt>>* s,
		vector<shared_ptr<Stmt>>* ct): inits(i), condition(c), steps(s), cond_true(ct) {}
	~Loop() { delete inits; delete condition; delete steps; delete cond_true; }
	virtual void printNode(ostream &out) const override {
		out << "Loop(";
		bool first = true;
		for(shared_ptr<Stmt> s: *inits) {
			if(!first) out << ", ";
			first = false;
			out << *s;
		}
		out << ", " << condition;
		for(shared_ptr<Stmt> s: *steps) {
			out << ", ";
			out << *s;
		}
		for(shared_ptr<Stmt> s: *cond_true) {
			out << ", ";
			out << *s;
		}
		out << ")";
	}
private:
	vector<shared_ptr<Stmt>>* inits;
	Expr* condition;
	vector<shared_ptr<Stmt>>* steps;
	vector<shared_ptr<Stmt>>* cond_true;
};

class Expr: public ASTnode { //abstract class
public:
	virtual ~Expr() {}
	/*
	void type_check(Type t) {
		sem();
		if (type != t) yyerror("Type mismatch");
	}
	*/
protected:
  Type type; //must go to symbol table
};

class Const: virtual public Expr {
public:
	Const(int i): integer(i) {}
	Const(char c): character(c) {}
	Const(const char* s): str(s) {}
	Const(string v) {
		switch(v) {
			case "true": boolean = true; break; 
			case "false": boolean = false; break;
			case "nil": //no idea //must check l-val for type?
		}
	} 
	~Const() {}
private:
	Union {
		int integer;
		char character;
		const char* str;
		bool boolean;
		//weird list thing for nil
	};
};

class UnOp: public Expr {
public:
	UnOp(const char* o, Expr* e): op(o), expr(e) {}
	~UnOp() { delete expr; }
private:
	const char* op; //info will be used in sem / evaluation
	Expr* expr;
};

class BinOp: public Expr {
public:
	BinOp(Expr* e1, const char* o, Expr* e2): op(o), expr1(e1), expr2(e2) {}
	~BinOp() { delete expr1; delete expr2; }
private:
	const char* op; //info will be used in sem / evaluation
	Expr* expr1;
	Expr* expr2;
};

class MemAlloc: public Expr {
public:
	MemAlloc(Type t, Expr* e): type(t), expr(e) {}
	~MemAlloc() { delete expr; }
private:
	Type type;
	Expr* expr;
};

class Atom: virtual public Expr { //abstract class
public:
	virtual ~Atom() {}
};

class Id: public Atom {
public:
	Id(const char* i): id(i) {}
	~Id() {}
private:
	const char* id;
};

class String: public Atom, public Const {
public:
	String(const char* s): Const(s) {}
	~String() {}
};

class RetVal: public Atom {
public:
	RetVal(Call* c): call(c) {}
	~RetVal() { delete call; }
private:
	Call* call;
};

class IndexAccess: public Atom {
public:
	IndexAccess(Atom* a, Expr* e): atom(a), expr(e) {}
	~IndexAccess() { delete atom; delete expr; }
private:
	Atom* atom;
	Expr* expr;
};
