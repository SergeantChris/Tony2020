#ifndef __SYMBOL_HPP__
#define __SYMBOL_HPP__

#include "error.hpp"
#include <vector>
#include <unordered_map>
#include <sstream>
#include "ast.hpp"

using namespace std;


enum PrimitiveType { TYPE_int, TYPE_bool, TYPE_char, TYPE_nil, TYPE_void};

class CompositeType;

union Type {
	PrimitiveType p;
	CompositeType* c;
	//~Type() {}
	//i dont think ill ever put a primtype in a comptype variable
	//if i want to keep the destructor i'll have to make type a pointer
	//in bison union
};

class Formal;

struct SymbolEntry {
  Type type;
  int offset;
	string from; // "var" or "func_def" of "func_decl"
	vector<Formal*>* params; // vector with the the parameters - in func_decl
  SymbolEntry(); // was this supposed to be destructor
  SymbolEntry(Type t, int ofs, string fr = "var", vector<Formal*>* v = nullptr);
};

inline ostream& operator<<(ostream &out, const Type t); //fwd declaration?

inline ostream& operator<<(ostream &out, const SymbolEntry e);

class Scope {
public:
  Scope(int ofs = -1);
  SymbolEntry * lookup(string c, string def);
  void insert(string c, Type t, string def, vector<Formal*>* v);
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
  SymbolEntry * lookup(string c, string def = "var");
  void insert(string c, Type t, string def = "var", vector<Formal*>* v = nullptr);
  int getSizeOfCurrentScope() const;
	Type getReturnType() const;
	bool EmptyScopes() const;
private:
  std::vector<Scope> scopes;
};

extern SymbolTable st;

#endif
