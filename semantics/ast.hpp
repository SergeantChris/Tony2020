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

};

class Formal: public ASTnode {

};

class Stmt: public ASTnode { //abstract class

};

class Simple: public Stmt { //abstract class

};

class Assign: public Simple {

};

class Call: public Simple {

};

class Return: public Stmt {

};

class Branch: public Stmt {

};

class Loop: public Stmt {

};

class Expr: public ASTnode { //abstract class
public:
  void type_check(Type t) {
    sem();
    if (type != t) yyerror("Type mismatch");
  }
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
