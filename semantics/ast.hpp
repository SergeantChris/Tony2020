#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <list>

using namespace std;

//yyerror?

enum PrimitiveType { TYPE_int, TYPE_bool, TYPE_char };

class CompositeType { //abstract class

};

class Array: public CompositeType {

};

class List: public CompositeType {

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
			case array: out << "array of" << ((t.c)->type); break;
			case list: out << "list of" << ((t.c)->type); break;
		}
	}
	return out;
}

class ASTnode { //abstract class
public:
	virtual ~ASTnode() {}
	virtual void printNode(ostream &out) const = 0;
};

inline ostream& operator<<(ostream &out, const ASTnode &n) {
	n.printNode(out);
	return out;
}

class Def: public ASTnode { //abstract class
public:

};

class FuncDef: public Def {
public:
	

};

class FuncDecl: public Def {

};

class VarDef: public Def {

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
