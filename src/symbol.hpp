#ifndef __SYMBOL_HPP__
#define __SYMBOL_HPP__

#include "error.hpp"
#include <vector>
#include <unordered_map>
#include <map>
#include <sstream>
#include "ast.hpp"
#include "type.hpp"
#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/DerivedTypes.h>

using namespace std;

class Formal;

struct SymbolEntry {
  Type type;
  int offset;
	string from; // "var" or "func_def" of "func_decl"
	vector<Formal*>* params; // vector with the parameters - in func_decl
  SymbolEntry();
  SymbolEntry(Type t, int ofs, string fr = "var", vector<Formal*>* v = nullptr); // inits for var
  // ~SymbolEntry();
};

ostream& operator<<(ostream &out, const SymbolEntry e);

class Scope {
public:
  Scope(int ofs = -1);
  SymbolEntry* lookup(string c, string def);
  void insert(int line_no, string c, Type t, string def, vector<Formal*>* v);
  int getSize() const;
  int getOffset() const;
	Type getLastFuncType() const;
private:
	unordered_map<string, SymbolEntry> locals;
  unordered_map<string, SymbolEntry> funcs;
  SymbolEntry last_func;
  int offset;
  int size;
};

class SymbolTable {
public:
  void openScope();
  void closeScope();
  SymbolEntry* lookup(int line_no, string c, string def = "var");
  void insert(int line_no, string c, Type t, string def = "var", vector<Formal*>* v = nullptr, bool built_in = false);
  int getSizeOfCurrentScope() const;
	Type getReturnType() const;
	bool EmptyScopes() const;
private:
  vector<Scope> scopes;
};

extern SymbolTable st;

// llvm value table below
struct ValueEntry {
	llvm::Value *val;
	llvm::Function *func;
	int lsize; //size of a particular list
	llvm::AllocaInst *alloc;
  int offset;
	string call; // call by ref or val or not a parameter (default = no)
	generalType type;
	map<string, llvm::Value*> refs; // call by ref parameters
  ValueEntry() {}
	// ValueEntry(int s, int ofs) : lsize(s), offset(ofs) {}
	ValueEntry(int s, llvm::AllocaInst* a, int ofs) : lsize(s), alloc(a), offset(ofs) {}
  ValueEntry(llvm::Value *v, int ofs) : val(v), offset(ofs) {}
	ValueEntry(llvm::Function *f, int ofs, map<string, llvm::Value*> r) : func(f), offset(ofs), refs(r) {}
	ValueEntry(llvm::AllocaInst *a, int ofs, string c, generalType t) :
	 alloc(a), offset(ofs), call(c), type(t) {
		val = nullptr; func = nullptr;
	}
};

class CompileScope {
public:
  CompileScope(int ofs = -1);
  ValueEntry * lookup(string c);
  void insert(string c, llvm::Value *v);
	void insert(string c, int s, llvm::AllocaInst *a);
	void insert(string c, llvm::Function *f, map<string, llvm::Value*> refs);
	void insert(string c, llvm::AllocaInst *a, string call, generalType type);
	map<string, ValueEntry> getMap() const;
  int getSize() const;
  int getOffset() const;
private:
	map<string, ValueEntry> defined;
  int offset;
  int size;
	vector<int> v;
};

class ValueTable {
public:
  void openScope();
  void closeScope();
  ValueEntry * lookup(string c, bool glob = false);
  void insert(string c, llvm::Value *v);
	void insert(string c, int lsize, bool call_list = false, llvm::AllocaInst *a = nullptr);
	void insert(string c, llvm::Function *f, map<string, llvm::Value*> refs = {});
	void insert(string c, llvm::AllocaInst *a, string call = "no",
              generalType type = Tnull, bool call_list = false);
  int getSizeOfCurrentScope() const;
	bool EmptyScopes() const;
	map<string, llvm::Type*> getGlobal() const;
private:
  vector<CompileScope> scopes;
};

extern ValueTable vt;
extern bool retval;

#endif
