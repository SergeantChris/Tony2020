#ifndef __AST_HPP__
#define __AST_HPP__

#pragma once

#include <iostream>
#include <vector>

#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>

using namespace llvm;

class AST {
public:
  virtual ~AST() {}
  virtual void printOn(std::ostream &out) const = 0;
  virtual Value* compile() const = 0;

  void llvm_compile_and_dump(bool optimize=true) {  // later used for compiler optimization flag
    // Initialize
    TheModule = make_unique<Module>("tony program", TheContext);
    TheFPM = make_unique<legacy::FunctionPassManager>(TheModule.get());
    if(optimize) {
      TheFPM->add(createPromoteMemoryToRegisterPass());
      TheFPM->add(createInstructionCombiningPass());
      TheFPM->add(createReassociatePass());
      TheFPM->add(createGVNPass());
      TheFPM->add(createCFGSimplificationPass());
    }

    TheFPM->doInitialization();

    // Initialize Types
    i8 = IntegerType::get(TheContext, 8);
    i32 = IntegerType::get(TheContext, 32);
    i64 = IntegerType::get(TheContext, 64);

    // Initialize Global Variavles
    ArrayType *vars_type = ArrayType::get(i32, 26);
    TheVars = new GlobalVariable(
      *TheModule, vars_type, false, GlobalVariable::PrivateLinkage,
      ConstantAggregateZero::get(vars_type), "vars");
    TheVars->setAlignment(16);
    ArrayType *nl_type = ArrayType::get(i8, 2);
    TheNL = new GlobalVariable(
      *TheModule, nl_type, true, GlobalVariable::PrivateLinkage,
      ConstantArray::get(nl_type, {c8('\n'), c8('\0')}), "nl");
    TheNL->setAlignment(1);

    // Initialize Library Functions
    FunctionType *writeInteger_type =
      FunctionType::get(Type::getVoidTy(TheContext), {i64}, false);
    TheWriteInteger =
    Function::Create(writeInteger_type, Function::ExternalLinkage,
    "writeInteger", TheModule.get());
    FunctionType *writeString_type =
      FunctionType::get(Type::getVoidTy(TheContext),
                        {PointerType::get(i8, 0)}, false);
    TheWriteString =
    Function::Create(writeString_type, Function::ExternalLinkage,
    "writeString", TheModule.get());

    // Define and start the main Function
    FunctionType *main_type = FunctionType::get(i32, {}, false);
    Function *main = Function::Create(main_type, Function::ExternalLinkage,
                                "main", TheModule.get());
    BasicBlock *BB = BasicBlock::Create(TheContext, "entry", main);
    Builder.SetInsertPoint(BB);

    // Emit the program code
    compile();
    Builder.CreateRet(c32(0));

    // Verify the IR
    bool bad = verifyModule(*TheModule, &errs());
    if(bad) {
      std::cerr << "the IR is BAD!" << std::endl; // make it with color so call error function
      TheModule->print(outs(), nullptr);
      std::exit(1);
    }
    // Optimize
    TheFPM->run(*main);

    // print
    TheModule->print(outs(), nullptr);

  }
protected:
  static LLVMContext TheContext;
  static IRBuilder<> Builder;
  static std::unique_ptr<Module> TheModule;
  static std::unique_ptr<legacy::FunctionPassManager> TheFPM;

  static GlobalVariable *TheVars;
  static GlobalVariable *TheNL;

  static Function *TheWriteInteger;
  static Function *TheWriteString;

  static Type *i8;
  static Type *i32;
  static Type *i64;

  static ConstantInt* c32(int n) {
    return ConstantInt::get(TheContext, APInt(32, n, true));
  }
  static ConstantInt* c8(char c) {
    return ConstantInt::get(TheContext, APInt(8, c, true));
  }
};

inline std::ostream& operator<< (std::ostream &out, const AST &t) {
  t.printOn(out);
  return out;
}

class Expr: public AST {};
class Stmt: public AST {};

/*	------------------------------ Id ---------------------------------------------- */
class Id: public Expr {
public:
  Id(char v): var(v) {}
  virtual void printOn(std::ostream &out) const override {
    out << "Id(" << var << ")";
  }
  virtual Value* compile() const override {
    char name[] = {var, '_', 'p', 't', 'r', '\0'};
    Value *v = Builder.CreateGEP(TheVars, {c32(0), c32(var - 'a')}, name);
    name[1] = '\0';
    return Builder.CreateLoad(v, name);
  }
private:
  char var;
};

/*	------------------------------ ConstInt ---------------------------------------------- */
class ConstInt: public Expr {
public:
  ConstInt(int n): num(n) {}
  virtual void printOn(std::ostream &out) const override {
    out << "Const(" << num << ")";
  }
  virtual Value* compile() const override {
    return c32(num);
  }
private:
  int num;
};

/*	------------------------------ ConstChar ---------------------------------------------- */
class ConstChar: public Expr {
public:
  ConstChar(char *c): ch(c) {}
  virtual void printOn(std::ostream &out) const override {
    out << "Const(" << ch << ")";
  }
  virtual Value* compile() const override {
    return c8(*ch);	// TODO: check
  }
private:
  char *ch;
};

/*	------------------------------ Oper --------------------------------------------- */
class Oper: public Expr {
public:
  Oper(Expr *l, char o, std::string o2, Expr *r): left(l), op(o), op2(o2), right(r) {}
  ~Oper() { delete left; delete right; }
  virtual void printOn(std::ostream &out) const override {
    out << op << "(" << *left << ", " << *right << ")";
  }
  virtual Value* compile() const override {
    Value *l = left->compile();
    Value *r = right->compile();
		if(op!='@') {			// single character operators
	    switch (op) {
	      case '+': return Builder.CreateAdd(l, r, "addtmp");
	      case '-': return Builder.CreateSub(l, r, "subtmp");
	      case '*': return Builder.CreateMul(l, r, "multmp");
	      case '/': return Builder.CreateSDiv(l, r, "divtmp");
	      case '=': return Builder.CreateSRem(l, r, "modtmp");
	      case '<': return Builder.CreateSRem(l, r, "modtmp");
	      case '>': return Builder.CreateSRem(l, r, "modtmp");
	    }
		}
		else if(op2!="@") {		// 2character / word operators
			if(op2=="<>") {

			}
			else if(op2=="<=") {

			}
			else if(op2==">=") {

			}
			else if(op2=="and") {

			}
			else if(op2=="or") {

			}
			else if(op2=="mod") {
				return Builder.CreateSRem(l, r, "modtmp");
			}
			else if(op2=="not") {

			}
		}
    return nullptr;
  }
private:
  Expr *left;
  char op;
	std::string op2;
	// char* op2;
  Expr *right;
};

/*	---------------------------- Let ------------------------------------------------ */

class Let: public Stmt {
public:
  Let(char v, Expr *e): var(v), expr(e) {}
  ~Let() { delete expr; }
  virtual void printOn(std::ostream &out) const override {
    out << "Let(" << var << " = " << *expr << ")";
  }
  virtual Value* compile() const override {
    char name[] = {var, '_', 'p', 't', 'r', '\0'};
    Value *lhs = Builder.CreateGEP(TheVars, {c32(0), c32(var - 'a')}, name);
    Value *rhs = expr->compile();
    Builder.CreateStore(rhs, lhs);
    return nullptr;
  }
private:
  char var;
  Expr *expr;
};

/*	------------------------------ Print ---------------------------------------------- */
class Print: public Stmt {
public:
  Print(Expr *e): expr(e) {}
  ~Print() { delete expr; }
  virtual void printOn(std::ostream &out) const override {
    out << "Print(" << *expr << ")";
  }
  virtual Value* compile() const override {
    Value *n = expr->compile();
    Value *n64 = Builder.CreateSExt(n, i64, "ext");
    Builder.CreateCall(TheWriteInteger, {n64});
    Value *nl = Builder.CreateGEP(TheNL, {c32(0), c32(0)}, "nl");
    Builder.CreateCall(TheWriteString, {nl});
    return nullptr;

  }
private:
  Expr *expr;
};

/*	----------------------------- If ----------------------------------------------- */
class If: public Stmt {
public:
  If(Expr *c, Stmt *s1, Stmt *s2 = nullptr, Stmt *s3 = nullptr):
    cond(c), stmt1(s1), stmt2(s2), stmt3(s3) {}
  ~If() { delete cond; delete stmt1; delete stmt2; delete stmt3;}
  virtual void printOn(std::ostream &out) const override {
    out << "If(" << *cond << ", " << *stmt1;
    if (stmt2 != nullptr) out << ", " << *stmt2;
    out << ")";
  }
	// virtual void run() const override {
	// 	if (cond->eval())
	// 		stmt1->run();
	// 	else if (stmt2 != nullptr)
	// 		stmt2->run();
	// }
  virtual Value* compile() const override {
    Value *v = cond->compile();
    Value *cond = Builder.CreateICmpNE(v, c32(0), "if_cond");
    // we need this in order to determine in whitch basic block we are at the moment
    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    // bellow we just create the onther 3 basic blocks (but we don t enter in any)
    BasicBlock *ThenBB = BasicBlock::Create(TheContext, "then", TheFunction);
    BasicBlock *ElseBB = BasicBlock::Create(TheContext, "else", TheFunction);
    BasicBlock *AfterBB = BasicBlock::Create(TheContext, "endif", TheFunction);

    // at this point we exit the previous basic block and we enter ThenBB
    // note that we firs have to insert a branch instruction in our assembly in order
    // to make the jump at runtime. Afterwards we move to the next basic block
    Builder.CreateCondBr(cond, ThenBB, ElseBB);
    Builder.SetInsertPoint(ThenBB);
    stmt1->compile();
    // also here we move to the next basic block
    Builder.CreateBr(AfterBB);
    Builder.SetInsertPoint(ElseBB);
    // if we have an else condition we compile it and then we move to the final
    // basic block. If there isnt any else, we just have 2 consecutive branches
    // that they dont do anything useful. An optimization pass will eliminate
    // them afterwards
    if (stmt2 != nullptr)
      stmt2->compile();
    // and then to the last block
    Builder.CreateBr(AfterBB);
    Builder.SetInsertPoint(AfterBB);
    return nullptr;
  }
private:
  Expr *cond;
  Stmt *stmt1;
  Stmt *stmt2;
	Stmt *stmt3;
};

/*	------------------------------ Elif ---------------------------------------------- */
class Elif: public Stmt {
public:
	Elif(Expr *c, Stmt *s1):
		cond(c), stmt1(s1) {}
	~Elif() { delete cond; delete stmt1;}

private:
	Expr *cond;
	Stmt *stmt1;
};

/*	----------------------------------- For --------------------------------------- */
class For: public Stmt {
public:
  For(Expr *e, Stmt *s): expr(e), stmt(s) {}
  ~For() { delete expr; delete stmt; }
  virtual void printOn(std::ostream &out) const override {
    out << "For(" << *expr << ", " << *stmt << ")";
  }
  virtual Value* compile() const override {
    Value *n = expr->compile();
    BasicBlock *PrevBB = Builder.GetInsertBlock();
    Function *TheFunction = PrevBB->getParent();
    BasicBlock *LoopBB = BasicBlock::Create(TheContext, "loop", TheFunction);
    BasicBlock *BodyBB = BasicBlock::Create(TheContext, "body", TheFunction);
    BasicBlock *AfterBB = BasicBlock::Create(TheContext, "end_loop", TheFunction);

    Builder.CreateBr(LoopBB);
    Builder.SetInsertPoint(LoopBB);
    // here we create the phi node
    PHINode *phi_iter = Builder.CreatePHI(i32, 2, "iter");
    phi_iter->addIncoming(n, PrevBB);
    Value *loop_cond = Builder.CreateICmpSGT(phi_iter, c32(0), "loop_cond");
    Builder.CreateCondBr(loop_cond, BodyBB, AfterBB);
    Builder.SetInsertPoint(BodyBB);
    Value *remaining = Builder.CreateSub(phi_iter, c32(1), "remaining");
    stmt->compile();
    // here we dont just add the Body block as phi's incoming, because the stmt->compile()
    // may have added many other new blocks. So by using the GetInsertBlock, we ensure
    // that we pass the last block that was added (which in the best case may be the BodyBB)
    // but in the worst case it may be a body block of a nested for, or a then block of a
    // nested if
    phi_iter->addIncoming(remaining, Builder.GetInsertBlock());
    Builder.CreateBr(LoopBB);
    Builder.SetInsertPoint(AfterBB);
    return nullptr;

  }
private:
  Expr *expr;
  Stmt *stmt;
};

/*	------------------------------ Func ---------------------------------------------- */
class Func: public AST {

};

/*	------------------------------ Block ---------------------------------------------- */
class Block: public Stmt {
public:
  Block(): func_list(), stmt_list(), size(0) {}
  ~Block() {
    for(Stmt *s : stmt_list) delete s;
    for(Func *d : func_list) delete d;
  }
  void append_func(Func *d) { func_list.push_back(d); }
  void append_stmt(Stmt *s) { stmt_list.push_back(s); }
  void merge(Block *b) {
    stmt_list = b->stmt_list;
    b->stmt_list.clear();
    delete b;
  }
  virtual void printOn(std::ostream &out) const override {
    out << "Block(";
    bool first = true;
    for(Func *d : func_list) {
      if(!first) out << ", ";
      first = false;
      out << *d;
    }
    for(Stmt *s : stmt_list) {
      if(!first) out << ", ";
      first = false;
      out << *s;
    }
    out << ")";
  }
  // virtual void run() const override {
  //   for(int i=0; i<size; i++) rt_stack.push_back(0);
  //   for(Stmt *s : stmt_list) s->run();
  //   for(int i=0; i<size; i++) rt_stack.pop_back();
  // }
	virtual Value* compile() const override {
    for (Stmt *s : stmt_list)
      s->compile();
    return nullptr;
  }
  // virtual void sem() override {
  //   st.openScope();
  //   for(func *d : func_list)  d->sem();
  //   for(Stmt *s : stmt_list)  s->sem();
  //   size = st.getSizeOfCurrentScope();
  //   st.closeScope();
  // }
private:
  std::vector<Func *> func_list;
  std::vector<Stmt *> stmt_list;
  int size;
};
#endif
