#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <vector>
#include <memory>
#include <string>
#include "lexer.hpp"
#include "symbol.hpp"

#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/IR/DerivedTypes.h>

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
	virtual llvm::Value* compile() const = 0;
	void llvm_compile_and_dump(bool optimize=true) {  // later used for compiler optimization flag
    // Initialize
    TheModule = llvm::make_unique<llvm::Module>("tony program", TheContext);
		// TheModule is an LLVM construct that contains functions and global variables
    TheFPM = llvm::make_unique<llvm::legacy::FunctionPassManager>(TheModule.get());
    if(optimize) {
			TheFPM->add(llvm::createPromoteMemoryToRegisterPass());
      TheFPM->add(llvm::createInstructionCombiningPass());
      TheFPM->add(llvm::createReassociatePass());
      TheFPM->add(llvm::createGVNPass());
      TheFPM->add(llvm::createCFGSimplificationPass());
    }

    TheFPM->doInitialization();
/*
 * ------------------------------------------- Initialize Types -------------------------------------------
*/
		i1 = llvm::IntegerType::get(TheContext, 1);
    i8 = llvm::IntegerType::get(TheContext, 8);
    i32 = llvm::IntegerType::get(TheContext, 32);
    i64 = llvm::IntegerType::get(TheContext, 64);
		// i32ptr = llvm::PointerType::getUnqual(i32);

/*
 * ------------------------------------- Initialize Library Functions -------------------------------------
*/
		// void puti(int n)
    llvm::FunctionType *puti_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i32}, false);
    Puti = llvm::Function::Create(puti_type, llvm::Function::ExternalLinkage, "puti", TheModule.get());

		// void putb(bool b)
    llvm::FunctionType *putb_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i1}, false);
    Putb = llvm::Function::Create(putb_type, llvm::Function::ExternalLinkage, "putb", TheModule.get());

		// void putc(char c)
    llvm::FunctionType *putc_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i8}, false);
    Putc = llvm::Function::Create(putc_type, llvm::Function::ExternalLinkage, "putc", TheModule.get());

		// void puts(char[] s)
    llvm::FunctionType *puts_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {llvm::PointerType::get(i8, 0)}, false);
    Puts = llvm::Function::Create(puts_type, llvm::Function::ExternalLinkage, "puts", TheModule.get());

		// int geti()
    llvm::FunctionType *geti_type = llvm::FunctionType::get(i32, {}, false);
    Geti = llvm::Function::Create(geti_type, llvm::Function::ExternalLinkage, "geti", TheModule.get());

		// bool getb()
    llvm::FunctionType *getb_type = llvm::FunctionType::get(i1, {}, false);
    Getb = llvm::Function::Create(getb_type, llvm::Function::ExternalLinkage, "getb", TheModule.get());

		// char getc()
    llvm::FunctionType *getc_type = llvm::FunctionType::get(i8, {}, false);
    Getc = llvm::Function::Create(getc_type, llvm::Function::ExternalLinkage, "getc", TheModule.get());

		// void gets(int n, char[] s)
    llvm::FunctionType *gets_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i32, llvm::PointerType::get(i8, 0)}, false);
    Gets = llvm::Function::Create(gets_type, llvm::Function::ExternalLinkage, "gets", TheModule.get());

		// int abs(int n)
		llvm::FunctionType *abs_type = llvm::FunctionType::get(i32, {i32}, false);
    Abs = llvm::Function::Create(abs_type, llvm::Function::ExternalLinkage, "abs", TheModule.get());

		// int ord(char c)
		llvm::FunctionType *ord_type = llvm::FunctionType::get(i32, {i8}, false);
		Ord = llvm::Function::Create(ord_type, llvm::Function::ExternalLinkage, "ord", TheModule.get());

		// char chr(int n)
		llvm::FunctionType *chr_type = llvm::FunctionType::get(i8, {i32}, false);
		Chr = llvm::Function::Create(chr_type, llvm::Function::ExternalLinkage, "chr", TheModule.get());

		// int strlen(char[] s)
		llvm::FunctionType *strlen_type = llvm::FunctionType::get(i32, {llvm::PointerType::get(i8, 0)}, false);
		Strlen = llvm::Function::Create(strlen_type, llvm::Function::ExternalLinkage, "strlen", TheModule.get());

		// int strcmp(char[] s1, s2)
		llvm::FunctionType *strcmp_type = llvm::FunctionType::get(i32, {llvm::PointerType::get(i8, 0), llvm::PointerType::get(i8, 0)}, false);
		Strcmp = llvm::Function::Create(strcmp_type, llvm::Function::ExternalLinkage, "strcmp", TheModule.get());

		// void strcpy(char[] trg, src)
		llvm::FunctionType *strcpy_type = llvm::FunctionType::get(i32, {llvm::PointerType::get(i8, 0), llvm::PointerType::get(i8, 0)}, false);
		Strcpy = llvm::Function::Create(strcpy_type, llvm::Function::ExternalLinkage, "strcpy", TheModule.get());

		// void strcat(char[] trg, src)
		llvm::FunctionType *strcat_type = llvm::FunctionType::get(i32, {llvm::PointerType::get(i8, 0), llvm::PointerType::get(i8, 0)}, false);
		Strcat = llvm::Function::Create(strcat_type, llvm::Function::ExternalLinkage, "strcat", TheModule.get());

/*
 * --------------------------------- Define and start the main Function ---------------------------------
*/
		// maybe define the main after reading the arguments or change the name
    llvm::FunctionType *main_type = llvm::FunctionType::get(i32, {}, false);
    llvm::Function *main = llvm::Function::Create(main_type, llvm::Function::ExternalLinkage,
                                									"main", TheModule.get());
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "program_entry", main);
    Builder.SetInsertPoint(BB);

    // Emit the program code
		retval = false;
    compile();
		Builder.GetInsertBlock()->eraseFromParent();
    // Verify the IR
    bool bad = llvm::verifyModule(*TheModule, &llvm::errs());
    // if(bad) {
    //   std::cerr << "the IR is BAD!" << std::endl; // make it with color so call error function
    //   std::exit(1);
    // }
    // Optimize
    TheFPM->run(*main);

    // print out IR
    TheModule->print(llvm::outs(), nullptr);
  }
protected:
  static llvm::LLVMContext TheContext;
  static llvm::IRBuilder<> Builder;
  static unique_ptr<llvm::Module> TheModule;
  static unique_ptr<llvm::legacy::FunctionPassManager> TheFPM;
	// static map<string, llvm::Value *> NamedValues;
	// static map<string, llvm::Function *> NamedFuncs;
	// NamedValues: keeps track of which values are defined in the current scope
	// and what their LLVM representation is
	// the only values that can be in the NamedValues map are function arguments.

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
			cout << "Type mismatch, expected type: " << t << ", but type: " << type << " was used" << endl;
			error("");
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
		else if(s == "@" && !((type.c)->getId() == "array" || (type.c)->getId() == "list" || (type.c)->getId() == "string")) {
			cout << endl;
			error("");
			cout << "Type mismatch, expected type: array or list or string, but type: " << type.c->getId() << " was used" << endl;
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
	virtual const char* getId() {}
	Type getNestedType() {
		if(isPrimitive(type)) {
			error("");
			cout << "Type mismatch, trying to acces nested type, but it doesent exist... type is Primitive: " << type << endl;
			cout << "Returning the type as is..." << endl;
			return type;
		}
		else return (type.c)->getType();
	}
	virtual llvm::Value* compile_check_call(bool call = false, string func_name = "", int index = 0) const {}
	virtual llvm::Value* compile_alloc() const {}
	virtual llvm::AllocaInst* compile_alloc_mem(string name ) const {}

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
	virtual llvm::Value* compile() const override {
		switch(tc_act) {
			case(TC_int): return c32(tc.integer);
			case(TC_char): return c8(tc.character);
			case(TC_bool): return c1(tc.boolean);
			// not only i32 but whatever type the list is
			case(TC_nil): return llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(i32));
			case(TC_str): return nullptr;		// TODO: do smth
		}
    return nullptr;
	}
	virtual llvm::Value* compile_check_call(bool call, string func_name, int index) const override{
		return compile();
	}
private:
	union TC {
		int integer;
		char character;
		const char* str; //cannot be string
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
	virtual llvm::Value* compile() const override {
		llvm::Value *val = expr->compile();
		if (op == "+") return val;
		else if (op == "-") return Builder.CreateNeg(val, "negsign");
		else if (op == "not") return Builder.CreateNot(val, "nottmp");
		else if (op == "nil?") {
			llvm::Type *list_type = (val->getType())->getPointerElementType();
			return Builder.CreateICmpEQ(val, llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(list_type)), "nil?");
		}
		else if (op == "head"){
			string lname = expr->getId();
			if (lname == "_ithasnoid") lname = val->getName();
			// string lname = expr->getId();
			string sname = lname + "_size";
			ValueEntry *e = vt.lookup(sname);
			int lsize = e->lsize;
		  llvm::Value *retval = Builder.CreateInBoundsGEP((val->getType())->getPointerElementType(), val,  c32(lsize - 1), "head");// return the first value of the List
			return Builder.CreateLoad(retval, "headval");
		 }
		else if(op == "tail") {
			string lname = expr->getId();
			string sname = lname + "_size";
			ValueEntry *e = vt.lookup(sname);
			int lsize = e->lsize;
			lsize--;
			string valname = val->getName();
			vt.insert(valname + "_size", lsize);
			return val; //return the last value of the list
		}
		else return nullptr;
	}
	virtual llvm::Value* compile_check_call(bool call, string func_name, int index) const override{
		return compile();
	}
	const char* getId() override {
		return "_ithasnoid";
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
	virtual llvm::Value* compile() const override {
		llvm::Value *l = expr1->compile();
    llvm::Value *r = expr2->compile();

    if (op == "+") return Builder.CreateAdd(l, r, "addtmp");
    else if (op == "-") return Builder.CreateSub(l, r, "subtmp");
    else if (op == "*") return Builder.CreateMul(l, r, "multmp");
    else if (op == "/") return Builder.CreateSDiv(l, r, "divtmp");
    else if (op == "mod") return Builder.CreateSRem(l, r, "modtmp");
		else if (op == "and") return Builder.CreateAnd(l, r, "andtmp");
		else if (op == "or") return Builder.CreateOr(l, r, "ortmp");
		else if (op == "#"){
			llvm::Type *seed_type = l->getType();
			// l is the seed and r is the list
			if (r == llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(i32))){
				//if r is nil => lhs has one elem and size 1
				llvm::AllocaInst *retalloc = Builder.CreateAlloca(llvm::PointerType::getUnqual(seed_type), nullptr, "tmplistalloc");
				retalloc->setAlignment(8);
				llvm::AllocaInst *initvalue = Builder.CreateAlloca(seed_type, nullptr, "initvalue");
				retalloc->setAlignment(4);
				Builder.CreateStore(initvalue, retalloc);
				llvm::Value *retval = Builder.CreateLoad(retalloc, "tmplistval");
				llvm::Value *elem = Builder.CreateInBoundsGEP(seed_type, retval,  c32(0), "lelalloc");
				Builder.CreateStore(l, elem);
				// size = 1
				string sname = (string)retval->getName() + "_size";
				vt.insert(sname, 1);
				return retval;
			}
			string lname = expr2->getId();
			if (lname == "_ithasnoid") lname = r->getName();
			string sname = lname + "_size";
			ValueEntry *e = vt.lookup(sname);
			int lsize = e->lsize;
			llvm::Value *v = Builder.CreateInBoundsGEP(seed_type, r,  c32(lsize), "lelalloc");
			Builder.CreateStore(l, v);
			// not always!
			string sname_load = (string)r->getName() + "_size";
			vt.insert(sname_load, lsize+1);
			return r;
		}
		else if (op == "=") return Builder.CreateICmpEQ(l, r, "eqtmp");
		else if (op == "<>") return Builder.CreateICmpNE(l, r, "netmp");
		else if (op == "<") return Builder.CreateICmpSLT(l, r, "slttmp");
		else if (op == ">") return Builder.CreateICmpSGT(l, r, "sgttmp");
		else if (op == "<=") return Builder.CreateICmpSLE(l, r, "sletmp");
		else if (op == ">=") return Builder.CreateICmpSGE(l, r, "sgemp");
    else return nullptr;
	}
	virtual llvm::Value* compile_check_call(bool call, string func_name, int index) const override{
		return compile();
	}
	const char* getId() override {
		return "_ithasnoid";
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
	virtual llvm::Value* compile() const override {
		return compile_alloc_mem();
	}
	llvm::Type* defineArrayType(Type t) const{
		switch (t.p) {
			case TYPE_int: return llvm::Type::getInt32PtrTy(TheContext);
			case TYPE_bool: return llvm::Type::getInt1PtrTy(TheContext);
			case TYPE_char: return llvm::Type::getInt8PtrTy(TheContext);
			default: break;
		}
		if (t.c->getId() == "array" ){
			return llvm::PointerType::getUnqual(defineArrayType(t.c->getType()));
		}
		return llvm::PointerType::getUnqual(defineArrayType(t.c->getType()));
	}
	virtual llvm::AllocaInst* compile_alloc_mem(string name = "tmparray") const override {
		//type = array(new_type)
		llvm::Value *v = expr->compile(); // size of array
		llvm::ConstantInt* ci = llvm::dyn_cast<llvm::ConstantInt>(v);
		uint64_t size = ci->llvm::ConstantInt::getZExtValue();
		llvm::Type *array_type = defineArrayType(new_type);

		llvm::ArrayType *array = llvm::ArrayType::get(array_type->getPointerElementType(), size);
		llvm::AllocaInst *ArrayAlloc = Builder.CreateAlloca(array, nullptr, name);
		// ArrayAlloc = Builder.CreateAlloca(array_type, nullptr, name);
		ArrayAlloc->setAlignment(8);
		return ArrayAlloc;
	}
	virtual llvm::Value* compile_check_call(bool call, string func_name, int index) const override{
		return compile();
	}
private:
	Type new_type;
	Expr* expr;
};

class Atom: virtual public Expr { //abstract class
public:
	virtual ~Atom() {}
	virtual const char* getId() override {}
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
	virtual llvm::Value* compile() const override {
		return compile_check_call();
	}

	virtual llvm::Value* compile_check_call(bool call = false, string func_name = "", int index = 0) const override{
		string var  = id;
		llvm::Value *v;
		if (call){
			// cout << "call" << var << endl;
			llvm::Function *func_value = TheModule->getFunction(func_name);
			ValueEntry *e = vt.lookup(func_value->getName());
			map<string, llvm::Value*> refs = e->refs;
			// define which params are by ref
			int i = 0;
			for (auto &Arg : func_value->args()){
				string name = Arg.getName();
				if (refs.count(name) & (i == index)){
					ValueEntry *parentry = vt.lookup(var);
					return parentry->alloc;
				}
				i++;
			}
		}
		ValueEntry *e = vt.lookup(var);
		v = e->val;
		if (e->alloc){
			// cout << "yes alloc " << var << " " << e->call << endl;
			llvm::Value *ret =  Builder.CreateLoad(v, var);
			if (e->call == "ref"){
				// cout << var << " " << e->call << endl;
				return Builder.CreateLoad(ret, var);
			}
			return ret;
		}
		else {
			// cout << "no alloc " << var << " " << e->call << endl;
			if (e->call == "ref"){
				// cout << var << " " << e->call << endl;
				return Builder.CreateLoad(v, var);
			}
			return v;
		}
	}
	virtual llvm::Value* compile_alloc() const override {
		string var = string(id);
		ValueEntry *e = vt.lookup(var);
		if (!e) return nullptr;
    return e->alloc;
	}
private:
	const char* id;
};

class String: public Atom, public Const {
public:
	String(const char* s): Const(s) {}
	~String() {}
	virtual llvm::Value* compile_alloc() const override {
		return nullptr;
	}
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
		// TODO: '@' refers to (array or list or string) because these composite types can be indexed and we can access their elements
		atom->firstLayerCompositeTypeCheck("@");
		expr->primTypeCheck(TYPE_int);
	}
	virtual llvm::Value* compile() const override {
		llvm::Value *v = Builder.CreateLoad(compile_alloc(), "elemval");
		return v;
	}
	virtual llvm::Value* compile_alloc() const override {
		llvm::Value *vexpr = expr->compile();
		llvm::Value *vatom = atom->compile_alloc();
		// llvm::Value *elem = Builder.CreateLoad(vatom, "arrayelem");
		// llvm::Value *v = Builder.CreateInBoundsGEP((elem->getType())->getPointerElementType(), elem,  vexpr, "elemalloc");
		llvm::Value *v = nullptr;
		if (((vatom->getType())->getPointerElementType())->isArrayTy()){
			  v = Builder.CreateInBoundsGEP(vatom, {c32(0), vexpr}, "elemalloc");
		}
		if (((vatom->getType())->getPointerElementType())->isPointerTy()){
			 llvm::Value *elem = Builder.CreateLoad(vatom, "arrayelem");
			 v = Builder.CreateInBoundsGEP((elem->getType())->getPointerElementType(), elem,  vexpr, "elemalloc");
		}

		return v;
	}
	virtual llvm::Value* compile_check_call(bool call, string func_name, int index) const override{
		return compile();
	}
	virtual const char* getId() override{
		return atom->getId();
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

	virtual llvm::Value* compile() const override {
		bool ref = 0;
		string name = string(atom->getId());
		ValueEntry *e = vt.lookup(name);
		if (!e) {
			// etc i = new int[2]
			// no store only memory allocation
			llvm::AllocaInst *ral = expr->compile_alloc_mem(name);
			vt.insert(name, ral, "no", Tarray);
			llvm::Value *v = ral;
			vt.insert(name, v);
			return nullptr;
		}

		llvm::Value *rhs = expr->compile();
		// lhs works for id but not for direct access yet.
		llvm::Value *lhs = atom->compile_alloc();
		if (!lhs) {
			llvm::AllocaInst *al;
			// is a parameter!
			// if is call by value we just need to allocate memory and store (locally)
			if (e->call == "val"){
				al = Builder.CreateAlloca(rhs->getType(), nullptr, name);
				al->setAlignment(4);
				vt.insert(name, al);
				lhs = al;
			}
			else if (e->call == "ref"){
				//if call by ref we need PointerType to rhs->getType() and setAlignment(8)
				al = Builder.CreateAlloca(llvm::PointerType::getUnqual(rhs->getType()), nullptr, name);
				al->setAlignment(8);
				vt.insert(name, al);
				lhs = al;
				Builder.CreateStore(e->val, lhs);
				llvm::Value *ptr = Builder.CreateLoad(lhs, name);
				Builder.CreateStore(rhs, ptr);
				ref = 1;
			}
		}
		if (rhs == llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(i32)) &&
					e->type == Tlist)
		{
			// assign nil to list
			llvm::Type *list_type = (lhs->getType())->getPointerElementType()->getPointerElementType();
			rhs = llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(list_type));
		}
		else if (e->type == Tlist){
			string rname = expr->getId();
			if (rname == "_ithasnoid") rname = rhs->getName();
			// string rname = rhs->getName();
			// cout << rname << endl;
			ValueEntry *rse = vt.lookup(rname + "_size");
			int rsize = rse->lsize;
			vt.insert(name + "_size", rsize);
		}

		if ((rhs->getType())->isArrayTy()){
			// right side is a defined array
			string rnme = expr->getId();
			ValueEntry *re = vt.lookup(rnme);
			llvm::Type* eltype = (rhs->getType())->getArrayElementType();
			rhs = Builder.CreateBitCast(re->alloc, llvm::PointerType::getUnqual(eltype), "ptr");
		}
		else if ((rhs->getType())->isPointerTy()
			&& ((rhs->getType())->getPointerElementType())->isArrayTy()) {
				// right side is a new memory allocation
				// cout << "its an ptr!" << endl;
				llvm::Type* eltype = ((rhs->getType())->getPointerElementType())->getArrayElementType();
				rhs = Builder.CreateBitCast(rhs, llvm::PointerType::getUnqual(eltype), "ptr");
		}
		if (!ref) Builder.CreateStore(rhs, lhs);

		if (e->type != Tarray) vt.insert(name, lhs);
		if (e->call == "ref"){
			string func_name = Builder.GetInsertBlock()->getParent()->getName();
			ValueEntry *fe = vt.lookup(func_name);
			map<string, llvm::Value*> refs = fe->refs;
			refs[name] = lhs;
		}
		return nullptr;
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
		if (exprList)
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

		// TODO: check size of the arguments!

		int i = 0;
		for(shared_ptr<Expr> e: *exprList) {
			e->sem();
			e->typeCheck(params.at(i));
			i++;
		}
	}
	virtual llvm::Value* compile() const override {
		// Look up the name in the global module table.
		string func_name = string(id);
		llvm::Function *func_value = TheModule->getFunction(func_name);

		int i = 0;
		vector<llvm::Value *> argsv = {};
		if (exprList)
			for(shared_ptr<Expr> e: *exprList) {
				llvm::Value *v = e->compile_check_call(true, func_name, i);
				argsv.push_back(v);
				i++;
			}
		if (func_value->getReturnType() == llvm::Type::getVoidTy(TheContext))
			return Builder.CreateCall(func_value, argsv);
		llvm::Value *ret = Builder.CreateCall(func_value, argsv, "calltmp");
		return ret;
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
	virtual llvm::Value* compile_check_call(bool call, string func_name, int index) const override {
		return compile();
	}
	virtual llvm::Value* compile() const override {
		llvm::Value *ret =  call->compile();
		return ret;

	}
	virtual llvm::Value* compile_alloc() const override {
		return nullptr;
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
			cout << "Checking return type ..." << endl;
			ret_val->typeCheck(st.getReturnType());
		}
	}
	virtual llvm::Value* compile() const override {
		retval = true;
		if (llvm::Value *RetVal = ret_val->compile()) {
			// Finish off the function.
				Builder.CreateRet(RetVal);
			return RetVal;
		}
		else return Builder.CreateRetVoid();
	}
private:
	Expr* ret_val;
};

class Branch: public Stmt {
public:
	Branch(vector<shared_ptr<Stmt>>* ct,
		Expr* c = new Const(string("true")),
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
		// cout << condition->getType() << endl;
		condition->primTypeCheck(TYPE_bool);
		for(shared_ptr<Stmt> s: *cond_true) s->sem();
		if(!((*elsif_branches).empty())) {
			for(Branch *b: *elsif_branches) b->sem();
		}
		if(else_branch != nullptr) else_branch->sem();
	}
	virtual llvm::Value* compile() const override {
		llvm::Value *v = condition->compile();
		llvm::Value *cond = Builder.CreateICmpNE(v, c1(0), "if_cond");
		llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
		llvm::BasicBlock *ThenBB =
      llvm::BasicBlock::Create(TheContext, "then", TheFunction);
		vector<llvm::BasicBlock*> IfElseVec;
		if(elsif_branches != nullptr && !(elsif_branches->empty()))
			for(Branch *b: *elsif_branches)
				IfElseVec.push_back(llvm::BasicBlock::Create(TheContext, "ifelse", TheFunction));
    else IfElseVec.push_back(llvm::BasicBlock::Create(TheContext, "ifelse", TheFunction));
		llvm::BasicBlock *ElseBB =
      llvm::BasicBlock::Create(TheContext, "else", TheFunction);
		llvm::BasicBlock *AfterBB =
      llvm::BasicBlock::Create(TheContext, "endif", TheFunction);

		llvm::BasicBlock *IfElseBB;
		IfElseBB = IfElseVec.back();
		IfElseVec.pop_back();
		if(elsif_branches != nullptr){
			Builder.CreateCondBr(cond, ThenBB, IfElseBB);
		}
		else {
			IfElseBB->eraseFromParent();
			ElseBB->eraseFromParent();
			Builder.CreateCondBr(cond, ThenBB, AfterBB);
		}

		Builder.SetInsertPoint(ThenBB);
		for(shared_ptr<Stmt> s: *cond_true) s->compile();
		if (!retval) Builder.CreateBr(AfterBB);
		else retval = false;

		if(elsif_branches != nullptr) {
			Builder.SetInsertPoint(IfElseBB);
			for(Branch *b: *elsif_branches) {
				b->compile();
				if(!(IfElseVec.empty())){
					IfElseBB = IfElseVec.back();
					IfElseVec.pop_back();
					Builder.CreateBr(IfElseBB);
					// else retval = false;
					Builder.SetInsertPoint(IfElseBB);
				}
				else {
					Builder.CreateBr(ElseBB);
					// else retval = false;
					Builder.SetInsertPoint(ElseBB);
				}
			}
			if (elsif_branches->empty()) Builder.CreateBr(ElseBB);
		  Builder.SetInsertPoint(ElseBB);
			if(else_branch != nullptr) else_branch->compile();
			Builder.CreateBr(AfterBB);
		}
    Builder.SetInsertPoint(AfterBB);
		// TODO: problem when there is no other body in the afterBB afterwards
		// retval = false;
		return nullptr;
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
	virtual llvm::Value* compile() const override {
		for(shared_ptr<Simple> s: *inits) 	s->compile();
		llvm::Value *cond = condition->compile();
		llvm::BasicBlock *PrevBB = Builder.GetInsertBlock();
    llvm::Function *TheFunction = PrevBB->getParent();

    llvm::BasicBlock *LoopBB =
      llvm::BasicBlock::Create(TheContext, "loop", TheFunction);
    llvm::BasicBlock *BodyBB =
      llvm::BasicBlock::Create(TheContext, "body", TheFunction);
    llvm::BasicBlock *AfterBB =
    llvm::BasicBlock::Create(TheContext, "endfor", TheFunction);

    Builder.CreateBr(LoopBB);
    Builder.SetInsertPoint(LoopBB);
		llvm::PHINode *phi_iter = Builder.CreatePHI(i1, 2, "iter");
    phi_iter->addIncoming(cond, PrevBB);
		// llvm::Value *loop_cond =
    //   Builder.CreateICmpSGT(phi_iter, c1(0), "loop_cond");
		Builder.CreateCondBr(phi_iter, BodyBB, AfterBB);
    Builder.SetInsertPoint(BodyBB);
		for(shared_ptr<Stmt> s: *cond_true) 	s->compile();
		for(shared_ptr<Simple> s: *steps) s->compile();
		llvm::Value *remaining = condition->compile();
		phi_iter->addIncoming(remaining, Builder.GetInsertBlock());
    Builder.CreateBr(LoopBB);
    Builder.SetInsertPoint(AfterBB);
		return nullptr;
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
	virtual llvm::Value* compile() const override {
		// TODO: call by ref how?
		for(const char* i: *idl) {
			string name = string(i);
			llvm::AllocaInst *a = nullptr;
			if (call_by_reference){
				vt.insert(name, a, "ref");
			}
			else vt.insert(name, a, "val");
		}
		return nullptr;
	}
	pair<Type, int> getType() {
		return make_pair(type, idl->size());
	}
	bool getTypeOfCall() {
		return call_by_reference;
	}
	vector<const char*>* getIds(){
		return idl;
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
			vector<Type> params = {};
			if (fl)
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
				else
					if (!((e->type == type) & (e->params == params))) error("Mismatch in function definition");
			}	// check if the parameters and the type are the same
		}
		// TODO: no need to open (and close) scope when you do a func_decl (func==false)
		st.openScope();
		cout << "+++ Opening new scope!" << endl;

		if((fl != nullptr) & func)
			for(Formal *f: *fl)
 				f->sem();
	}
	virtual llvm::Value* compile() const override {
		return compile(nullptr);
	}
	virtual llvm::Value* compile(llvm::BasicBlock* EndFunc) const {
		bool main = false;
		string func_name = string(id);
		if (func_name == "main") func_name = "jackthecutestdoggo";
		if(vt.EmptyScopes()){
			vt.openScope();
			vt.insert("puti",Puti);
			vt.insert("putb",Putb);
			vt.insert("putc",Putc);
			vt.insert("puts",Puts);
			vt.insert("geti",Geti);
			vt.insert("getb",Getb);
			vt.insert("getc",Getc);
			vt.insert("gets",Gets);
			vt.insert("abs",Abs);
			vt.insert("ord",Ord);
			vt.insert("chr",Chr);
			vt.insert("strlen",Strlen);
			vt.insert("strcmp",Strcmp);
			vt.insert("strcpy",Strcpy);
			vt.insert("strcat",Strcat);
			main = true;
		}
		llvm::Type *func_type;
		if (type.p == TYPE_int) func_type = i32;
		else if (type.p == TYPE_bool) func_type = i1;
		else if (type.p == TYPE_char) func_type = i8;
		else func_type = llvm::Type::getVoidTy(TheContext);
		vector<llvm::Type*> params = {};
		if (fl)
			for(Formal *f: *fl) {
				pair<Type, int> pair_type = f->getType();
				llvm::Type *llvm_pair_type;
				if (pair_type.first.p == TYPE_int) llvm_pair_type = i32;
				else if (pair_type.first.p == TYPE_bool) llvm_pair_type = i1;
				else if (pair_type.first.p == TYPE_char) llvm_pair_type = i8;
				// array and list type?
				if (f->getTypeOfCall()) llvm_pair_type = llvm::PointerType::getUnqual(llvm_pair_type);
				params.insert(params.end(), pair_type.second, llvm_pair_type);
			}
		llvm::FunctionType *FT =
    	llvm::FunctionType::get(func_type, params, false);
		llvm::Function *F =
		  llvm::Function::Create(FT, llvm::Function::ExternalLinkage, func_name, TheModule.get());
		if (main){
			 Builder.CreateCall(F, {});
			 Builder.CreateRet(c32(0));
		}
		// Set names for all arguments.
		vector<string> Args = {};
		map<string, llvm::Value*> refs = {};
		if (fl)
			for(Formal *f: *fl) {
				bool call_by_reference = f->getTypeOfCall();
				vector<const char*>* idl = f->getIds();
				for(const char* i: *idl) {
					string name = string(i);
					Args.push_back(name);
					if (call_by_reference) refs[name] = nullptr;
				}
			}
		unsigned Idx = 0;
		for (auto &Arg : F->args())
		  Arg.setName(Args[Idx++]);
		vt.insert(func_name, F, refs);
		vt.openScope();
		if (fl)
			for(Formal *f: *fl) {
				f->compile();
			}
		llvm::BasicBlock *CurrentBB = Builder.GetInsertBlock();
		if (!main & CurrentBB->empty()) CurrentBB->eraseFromParent();
		else if (!main) Builder.CreateBr(EndFunc);
		return nullptr;
	}
	string getId() {
		return string(id);
	}
	void setId(const char* name){
		id = name;
	}
	bool isVoid() {
		return type.p == TYPE_null;
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
	virtual llvm::Value* compile() const override {
		// First, check for an existing function from a previous 'extern' declaration.
		string func_name = hd->getId();
		if (func_name == "main") func_name = "jackthecutestdoggo";
		llvm::Function *ParentFunc = Builder.GetInsertBlock()->getParent();
		llvm::BasicBlock *EndFunc =
      llvm::BasicBlock::Create(TheContext, "continue", ParentFunc);
		llvm::Function *TheFunction = TheModule->getFunction(func_name);
		// if the function has not be declared do it!
		if (!TheFunction){
		  hd->compile(EndFunc);
			ValueEntry *e = vt.lookup(func_name);
			TheFunction = e->func;
		}
		// Create a new basic block to start insertion into.
		llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
		Builder.SetInsertPoint(BB);
		// Record the function arguments in the NamedValues map.
		for (auto &Arg : TheFunction->args())
			vt.insert(Arg.getName(), &Arg);
		for(shared_ptr<Def> d: *defl) d->compile();
		for(shared_ptr<Stmt> s: *stmtl) s->compile();
		if (hd->isVoid()) Builder.CreateRetVoid();
		// Validate the generated code, checking for consistency.
  	verifyFunction(*TheFunction);
		vt.insert(func_name, TheFunction); //update
		vt.closeScope();
		Builder.SetInsertPoint(EndFunc);
		return nullptr;
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
	virtual llvm::Value* compile() const override {
		hd->compile();
		return nullptr;
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
		}
	}
	llvm::Type* defineListType(Type t) const {
		switch (t.p) {
			case TYPE_int: return i32;
			case TYPE_bool: return i1;
			case TYPE_char: return i8;
			default: break;
		}
		// else is array or list
		return llvm::PointerType::getUnqual(defineListType(t.c->getType()));

	}
	virtual llvm::Value* compile() const override {
		bool isList = 0;
		llvm::Type *vtype;
		if (type.p == TYPE_int)
			vtype = i32;
		else if (type.p == TYPE_bool)
			vtype = i1;
		else if (type.p == TYPE_char)
			 vtype = i8;
		else if (type.c->getId() == "array") {
			return nullptr;
		}
		else if (type.c->getId() == "list") {
			isList = 1;
			vtype = llvm::PointerType::getUnqual(defineListType(type.c->getType()));
		}
		// else continue;
		for(const char* i: *idl){
			string name = string(i);
			llvm::AllocaInst *IdAlloc = Builder.CreateAlloca(vtype, nullptr, name);
			if (isList){
				IdAlloc->setAlignment(8);
				vt.insert(name, IdAlloc, "no", Tlist);
				// keep the size of the list in variable!
				string sname = name + "_size";
				// llvm::AllocaInst *SizeAlloc = Builder.CreateAlloca(i32, nullptr, sname);
				// SizeAlloc->setAlignment(4);
				vt.insert(sname, 0);
			}
			else {
				IdAlloc->setAlignment(4);
				vt.insert(name, IdAlloc);
			}
		}

		return nullptr;
	}
private:
	Type type;
	vector<const char*>* idl;
};


class Library {
public:
  Library(){};
  void init() {
    // Formal *formal;
    // Formal_list *formal_list;
    // Id_list *id_list;

		Type ret_t;
		Type var_t;
		Type varh_t;
		vector<Type> params;
		// vector<const char*>* id_list;

		// insert(name, returnType, "func_decl", vector<Type> params)
		// st.openScope(); cout << "+++ Opening new scope!" << endl;
		// void puti(int n)
		var_t.p = TYPE_int;
		params.push_back(var_t);
		ret_t.p = TYPE_null;
		st.insert("puti", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		st.insert("n", var_t);
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

		// void putb(bool b)
		var_t.p = TYPE_bool;
		params.push_back(var_t);
		ret_t.p = TYPE_null;
		st.insert("putb", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		st.insert("b", var_t);
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

		// void putc(char c)
		var_t.p = TYPE_char;
		params.push_back(var_t);
		ret_t.p = TYPE_null;
		st.insert("putc", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		st.insert("c", var_t);
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

		// void puts(char[] s)
		varh_t.p = TYPE_char;
		var_t.c = new Array(varh_t);
		params.push_back(var_t);
		ret_t.p = TYPE_null;
		st.insert("puts", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		st.insert("s", var_t);
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

		// int geti()
		ret_t.p = TYPE_int;
		st.insert("geti", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

		// bool getb()
		ret_t.p = TYPE_bool;
		st.insert("getb", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

		// char getc()
		ret_t.p = TYPE_char;
		st.insert("getc", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

		// void gets(int n, char[] s)	// n: max character to read from input (including /0), s: the array of chars where you should put the resault
		var_t.p = TYPE_int;
		params.push_back(var_t);
		varh_t.p = TYPE_char;
		var_t.c = new Array(varh_t);
		params.push_back(var_t);
		ret_t.p = TYPE_null;
		st.insert("gets", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		var_t.p = TYPE_int;
		st.insert("n", var_t);
		varh_t.p = TYPE_char;
		var_t.c = new Array(varh_t);
		st.insert("s", var_t);
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

		// int abs(int n)
		var_t.p = TYPE_int;
		params.push_back(var_t);
		ret_t.p = TYPE_int;
		st.insert("abs", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		st.insert("n", var_t);
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

		// int ord(char c)
		var_t.p = TYPE_char;
		params.push_back(var_t);
		ret_t.p = TYPE_int;
		st.insert("ord", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		st.insert("c", var_t);
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

		// char chr(int n)
		var_t.p = TYPE_int;
		params.push_back(var_t);
		ret_t.p = TYPE_char;
		st.insert("chr", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		st.insert("n", var_t);
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

		// int strlen(char[] s)
		varh_t.p = TYPE_char;
		var_t.c = new Array(varh_t);
		params.push_back(var_t);
		ret_t.p = TYPE_int;
		st.insert("strlen", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		st.insert("s", var_t);
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

		// int strcmp(char[] s1, s2)
		varh_t.p = TYPE_char;
		var_t.c = new Array(varh_t);
		params.push_back(var_t);
		params.push_back(var_t);
		ret_t.p = TYPE_int;
		st.insert("strcmp", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		st.insert("s1", var_t);
		st.insert("s2", var_t);
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

		// void strcpy(char[] trg, src)
		varh_t.p = TYPE_char;
		var_t.c = new Array(varh_t);
		params.push_back(var_t);
		params.push_back(var_t);
		ret_t.p = TYPE_int;
		st.insert("strcpy", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		st.insert("trg", var_t);
		st.insert("src", var_t);
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

		// void strcat(char[] trg, src)
		varh_t.p = TYPE_char;
		var_t.c = new Array(varh_t);
		params.push_back(var_t);
		params.push_back(var_t);
		ret_t.p = TYPE_int;
		st.insert("strcat", ret_t, "func_decl", params);

		st.openScope(); cout << "+++ Opening new scope!" << endl;
		st.insert("trg", var_t);
		st.insert("src", var_t);
		st.closeScope(); cout << "--- Closing scope!" << endl;
		params.clear();

/*	// The same but with the use of formal instead of vector<Type>

    //procedure readString (size : integer; var s : array of char);
    formal_list = new Formal_list();
    id_list = new Id_list();
    id_list->append_idString("s");

    formal = new Formal(id_list, new Integer(), false);
    formal_list->append_formal(formal);
    id_list = new Id_list();
    id_list->append_idString("size");
    formal = new Formal(id_list, new Array(new Char()), true);
    formal_list->append_formal(formal);
    st.insertProcedureLib("readString", new ProcedureType(), formal_list);

*/
  }
};


#endif
