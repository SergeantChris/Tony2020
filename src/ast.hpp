#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <sstream>
#include <vector>
#include <memory>
#include <string>
#include "lexer.hpp"
#include "symbol.hpp"
#include "type.hpp"

using namespace std;

#define PRE_DEBUG 1


class CompositeType { //abstract class
public:
	virtual ~CompositeType();
	string getId();
	Type getType();
protected:
	string id;
	Type type;
};

class Array: public CompositeType {
public:
	Array(Type t);
	Array(PrimitiveType p);
};

class List: public CompositeType {
public:
	List(Type t);
	List(PrimitiveType p);
};

bool isPrimitive(Type t);

ostream& operator<<(ostream &out, const Type t);

ostream& operator<<(ostream &out, const PrimitiveType p);

bool operator==(const Type &t1, const Type &t2);

class ASTnode { //abstract class
public:
	virtual ~ASTnode();
	virtual void printNode(ostream &out) const = 0;
	virtual void sem();
};

ostream& operator<<(ostream &out, const ASTnode &n);

class Expr: public ASTnode { //abstract class
public:
	virtual ~Expr();

	void typeCheck(Type t);
	void typeCheck(PrimitiveType p);
	void typeCheck(CompositeType* c);
	void typeCheck(CompositeType* c1, CompositeType* c2);
	Type getType();

protected:
  Type type;
};

class Const: public virtual Expr {
public:
	Const(int i);
	Const(char c);
	Const(string v, bool special=1);
	~Const();
	virtual void printNode(ostream &out) const override;
	virtual void sem() override;
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
	PreOp(const char* o, Expr* e);
	~PreOp();
	virtual void printNode(ostream &out) const override;
	virtual void sem() override;
private:
	string op;
	Expr* expr;
};

class Op: public Expr {
public:
	Op(Expr* e1, const char* o, Expr* e2);
	~Op();
	virtual void printNode(ostream &out) const override;
	virtual void sem() override;
private:
	string op;
	Expr* expr1;
	Expr* expr2;
};

class MemoryAlloc: public Expr {
public:
	MemoryAlloc(Type t, Expr* e);
	~MemoryAlloc();
	virtual void printNode(ostream &out) const override;
	virtual void sem();
private:
	Type new_type;
	Expr* expr;
};

class Atom: virtual public Expr { //abstract class
public:
	virtual ~Atom();
};

class Id: public Atom {
public:
	Id(const char* i);
	~Id();
	virtual void printNode(ostream &out) const override;
	virtual void sem() override;
	const char* getId();
private:
	const char* id;
};

class String: public Atom, public Const {
public:
	String(string s);
	~String();
};

class DirectAcc: public Atom {
public:
	DirectAcc(Atom* a, Expr* e);
	~DirectAcc();
	virtual void printNode(ostream &out) const override;
	virtual void sem();
private:
	Atom* atom;
	Expr* expr;
};

class Stmt: public ASTnode { //abstract class
public:
	virtual ~Stmt();
};

class Simple: public Stmt { //abstract class
public:
	virtual ~Simple();
};

class NoAction: public Simple {
public:
	NoAction();
	~NoAction();
	virtual void printNode(ostream &out) const override;
	virtual void sem();
};

class Assign: public Simple {
public:
	Assign(Atom* a, Expr* e);
	~Assign();
	virtual void printNode(ostream &out) const override;
	virtual void sem();

private:
	Atom* atom;
	Expr* expr;
};

class Call: public Simple {
public:
	Call(const char* i, vector<shared_ptr<Expr>>* e = nullptr);
	~Call();
	virtual void printNode(ostream &out) const override;
	virtual void sem();
	Type getType();

private:
	const char* id;
	vector<shared_ptr<Expr>>* exprList;
	Type type;
};

class ReturnValue: public Atom {
public:
	ReturnValue(Call* c);
	~ReturnValue();
	virtual void printNode(ostream &out) const override;
	virtual void sem();

private:
	Call* call;
};

class Return: public Stmt {
public:
	Return(Expr* v = nullptr);
	~Return();
	virtual void printNode(ostream &out) const override;
	virtual void sem();
private:
	Expr* ret_val;
};

class Branch: public Stmt {
public:
	Branch(vector<shared_ptr<Stmt>>* ct,
		Expr* c = new Const("true"),
		vector<Branch*>* eif = nullptr,
		Branch* e = nullptr);
	~Branch();
	virtual void printNode(ostream &out) const override;
	virtual void sem() override;
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
		vector<shared_ptr<Stmt>>* ct);
	~Loop();
	virtual void printNode(ostream &out) const override;
	virtual void sem() override;

private:
	vector<shared_ptr<Simple>>* inits;
	Expr* condition;
	vector<shared_ptr<Simple>>* steps;
	vector<shared_ptr<Stmt>>* cond_true;
};

class Formal: public ASTnode {
public:
	Formal(Type t, vector<const char*>* i, string cb);
	~Formal();
	virtual void printNode(ostream &out) const override;
	virtual void sem();
	vector<Formal*>* getOpenedFormal();
	Type getType();
	vector<const char*>* getIds();
	bool getCb();
private:
	Type type;
	vector<const char*>* idl;
	bool call_by_reference;
};

class Header: public ASTnode {
public:
	Header(const char* i, vector< Formal*>* f, Type t);
	~Header();
	virtual void printNode(ostream &out) const override;
	virtual void sem(bool func = true);
private:
	const char* id;
	vector< Formal*>* fl;
	Type type;
};

class Def: public ASTnode { //abstract class
public:
	virtual ~Def();
};

class FuncDef: public Def {
public:
	FuncDef(Header* h, vector<shared_ptr<Def>>* d, vector<shared_ptr<Stmt>>* s);
	~FuncDef();
	virtual void printNode(ostream &out) const override;
	virtual void sem();
private:
	Header* hd;
	vector<shared_ptr<Def>>* defl;
	vector<shared_ptr<Stmt>>* stmtl;
	int size;
};

class FuncDecl: public Def {
public:
	FuncDecl(Header* h);
	~FuncDecl();
	virtual void printNode(ostream &out) const override;
	virtual void sem();
private:
	Header* hd;
};

class VarDef: public Def {
public:
	VarDef(Type t, vector<const char*>* i);
	~VarDef();
	virtual void printNode(ostream &out) const override;
	virtual void sem();
private:
	Type type;
	vector<const char*>* idl;
};


class Library {
public:
	Library();
	void init();
};

#endif
