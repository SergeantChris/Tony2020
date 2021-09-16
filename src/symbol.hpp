#ifndef __SYMBOL_HPP__
#define __SYMBOL_HPP__

#include "error.hpp"
#include <vector>
#include <unordered_map>
#include <sstream>
#include "ast.hpp"
#include "type.hpp"

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
  SymbolEntry* lookup(string c, string def = "var");
  void insert(string c, Type t, string def = "var", vector<Formal*>* v = nullptr, bool built_in = false);
  int getSizeOfCurrentScope() const;
	Type getReturnType() const;
	bool EmptyScopes() const;
private:
  vector<Scope> scopes;
};

extern SymbolTable st;

#endif
