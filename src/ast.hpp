#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <sstream>
#include <vector>
#include <memory>
#include <string>
#include <string.h>
#include "lexer.hpp"
#include "symbol.hpp"
#include "type.hpp"


#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/IR/DerivedTypes.h>
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

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
	virtual llvm::Value* compile() const = 0;
	void llvm_compile_and_dump(bool optimize=true);

protected:
  static llvm::LLVMContext TheContext;
  static llvm::IRBuilder<> Builder;
  static unique_ptr<llvm::Module> TheModule;
  static unique_ptr<llvm::legacy::FunctionPassManager> TheFPM;

	static llvm::Function *Puti;
  static llvm::Function *Putb;
  static llvm::Function *Putc;
  static llvm::Function *Puts;

  static llvm::Function *Geti;
  static llvm::Function *Getb;
	static llvm::Function *Getc;
  static llvm::Function *Gets;

	static llvm::Function *Abs;
	static llvm::Function *Ord;
	static llvm::Function *Chr;

	static llvm::Function *Strlen;
	static llvm::Function *Strcmp;
	static llvm::Function *Strcpy;
	static llvm::Function *Strcat;

	static llvm::Type *i1;
  static llvm::Type *i8;
  static llvm::Type *i32;
  static llvm::Type *i64;

  static llvm::ConstantInt* c32(int n) {
    return llvm::ConstantInt::get(TheContext, llvm::APInt(32, n, true));
  }
	static llvm::ConstantInt* c64(int n) {
    return llvm::ConstantInt::get(TheContext, llvm::APInt(64, n, true));
  }
  static llvm::ConstantInt* c8(char c) {
    return llvm::ConstantInt::get(TheContext, llvm::APInt(8, c, true));
  }
	static llvm::ConstantInt* c1(bool b) {
    return llvm::ConstantInt::get(TheContext, llvm::APInt(1, b, true));
  }
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

	virtual llvm::Value* compile_check_call(bool call = false, string func_name = "", int index = 0) const {return nullptr;}
	virtual llvm::Value* compile_alloc() const {return nullptr;}
	virtual llvm::AllocaInst* compile_alloc_mem(string name ) const {return nullptr;}
	virtual const char* getId() {return "_noid";}

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
	virtual llvm::Value* compile() const override;
	virtual llvm::Value* compile_check_call(bool call, string func_name, int index) const override;

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
	virtual llvm::Value* compile() const override;
	virtual llvm::Value* compile_check_call(bool call, string func_name, int index) const override;
	const char* getId() override;

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
	virtual llvm::Value* compile() const override;
	virtual llvm::Value* compile_check_call(bool call, string func_name, int index) const override;
	const char* getId() override;

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
	virtual llvm::Value* compile() const override;
	virtual llvm::AllocaInst* compile_alloc_mem(string name = "tmparray") const override;
	virtual llvm::Value* compile_check_call(bool call, string func_name, int index) const override;
	llvm::Type* defineArrayType(Type t) const;

private:
	Type new_type;
	Expr* expr;
};

class Atom: virtual public Expr { //abstract class
public:
	virtual ~Atom();
	virtual const char* getId();
};

class Id: public Atom {
public:
	Id(const char* i);
	~Id();
	virtual void printNode(ostream &out) const override;
	const char* getId() override;
	virtual void sem() override;
	virtual llvm::Value* compile() const override;
	virtual llvm::Value* compile_check_call(bool call = false, string func_name = "", int index = 0) const override;
	virtual llvm::Value* compile_alloc() const override;

private:
	const char* id;
};

class String: public Atom, public Const {
public:
	String(string s);
	~String();
	virtual llvm::Value* compile_alloc() const override;
};

class DirectAcc: public Atom {
public:
	DirectAcc(Atom* a, Expr* e);
	~DirectAcc();
	virtual void printNode(ostream &out) const override;
	virtual void sem();
	virtual llvm::Value* compile() const override;
	virtual llvm::Value* compile_alloc() const override;
	virtual llvm::Value* compile_check_call(bool call, string func_name, int index) const override;
	virtual const char* getId() override;

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
	virtual llvm::Value* compile() const override;
};

class Assign: public Simple {
public:
	Assign(Atom* a, Expr* e);
	~Assign();
	virtual void printNode(ostream &out) const override;
	virtual void sem();
	virtual llvm::Value* compile() const override;
	
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
	virtual llvm::Value* compile() const override;
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
	virtual llvm::Value* compile_check_call(bool call, string func_name, int index) const override;
	virtual llvm::Value* compile() const override;
	virtual llvm::Value* compile_alloc() const override;

private:
	Call* call;
};

class Return: public Stmt {
public:
	Return(Expr* v = nullptr);
	~Return();
	virtual void printNode(ostream &out) const override;
	virtual void sem();
	virtual llvm::Value* compile() const override;

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
	virtual llvm::Value* compile() const override;
	void setAfterBB(llvm::BasicBlock *AfterBB);

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
	virtual llvm::Value* compile() const override;

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
	virtual llvm::Value* compile() const override;
	vector<Formal*>* getOpenedFormal();
	Type getType();
	int getCountofIds();
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
	Header(const char* i, vector< Formal*>* f);
	~Header();
	virtual void printNode(ostream &out) const override;
	virtual void sem(bool func = true);
	virtual llvm::Value* compile() const override;
	string getId();
	bool isVoid();
	Type getType();
	vector< Formal*>* getFormals();

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
	virtual llvm::Value* compile() const override;

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
	virtual llvm::Value* compile() const override;
	llvm::Type* convertType(Type type) const;

private:
	Header* hd;
};

class VarDef: public Def {
public:
	VarDef(Type t, vector<const char*>* i);
	~VarDef();
	virtual void printNode(ostream &out) const override;
	virtual void sem();
	virtual llvm::Value* compile() const override;
	llvm::Type* defineListType(Type t) const;

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
