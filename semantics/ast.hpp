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
	VarDef(Type t, vector<string>* i): type(t), idl(i) {}
	~VarDef() { delete idl; }
	virtual void printNode(ostream &out) const override {
		out << "VarDef(" << type << ", ";
		bool first = true;
		for(string i: *idl) {
			if(!first) out << ", ";
			first = false;
			out << i;
		}
	}
private:
	Type type;
	vector<string>* idl;
};

class Header: public ASTnode {
public:
	Header(string i, vector<Formal>* f, Type t = nullptr): id(i), fl(f), type(t) {}
	~Header() { delete fl; }
	virtual void printNode(ostream &out) const override {}
private:
	string id;
	vector<Formal>* fl;
	Type type;
};

class Formal: public ASTnode {
public:
	Formal(Type t, vector<string>* i, string cb): type(t), idl(i) {
		switch(cb) {
			case "cbv": call_by_reference = false; break;
			case "cbr": call_by_reference = true; break; 
		}
	}
	~Formal() { delete idl; }
	virtual void printNode(ostream &out) const override {}
private:
	Type type;
	vector<string>* idl;
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
	virtual void printNode(ostream &out) const override {}
private:
	Atom* atom;
	Expr* expr;
};

class Call: public Simple {
public:
	Call(string i, vector<shared_ptr<Expr>>* e = nullptr): id(i), exprl(e) {}
	~Call() { delete exprl; }
	virtual void printNode(ostream &out) const override {}
private:
	string id;
	vector<shared_ptr<Expr>>* exprl; 
};

class Return: public Stmt {
public:
	Return(Expr* v = nullptr): ret_val(v) {}
	~Return() { delete ret_val; }
	virtual void printNode(ostream &out) const override {}
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
	virtual void printNode(ostream &out) const override {}
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
	virtual void printNode(ostream &out) const override {}
private:
	vector<shared_ptr<Stmt>>* inits;
	Expr* condition;
	vector<shared_ptr<Stmt>>* steps;
	vector<shared_ptr<Stmt>>* cond_true;
};

class Expr: public ASTnode { //abstract class
public:
	/*
	void type_check(Type t) {
		sem();
		if (type != t) yyerror("Type mismatch");
	}
	*/
protected:
  Type type;
};

class Atom: public Expr { //abstract class

};

class Id: public Atom {

};

class String: public Atom {

};

class RetVal: public Atom {

};

class IndexAccess: public Atom {

};

class Rval: public Expr { //abstract class

};

class Const: public Rval {

}

class UnOp: public Rval {

};

class BinOp: public Rval {

};

class MemAlloc: public Rval {

};
