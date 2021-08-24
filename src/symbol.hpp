#ifndef __SYMBOL_HPP__
#define __SYMBOL_HPP__

#include "error.hpp"
#include <vector>
#include <map>
#include "ast.hpp"
#include <llvm/IR/Value.h>

using namespace std;

enum PrimitiveType { TYPE_int, TYPE_bool, TYPE_char, TYPE_str, TYPE_null};

class CompositeType;

union Type {
	PrimitiveType p;
	CompositeType* c;
	//~Type() {}
	//i dont think ill ever put a primtype in a comptype variable
	//if i want to keep the destructor i'll have to make type a pointer
	//in bison union
};

struct SymbolEntry {
  Type type;
  int offset;
	llvm::Value *v;
	string from; // "var" or "func_def" of "func_decl"
	vector<Type> params; // vector with the types of the parameters - in func_decl
  SymbolEntry() {}
  SymbolEntry(Type t, int ofs, string fr = "var", vector<Type> v = vector<Type>()) : type(t), offset(ofs), from(fr), params(v) {}
	SymbolEntry(int ofs, llvm::Value *v, string fr = "var") : offset(ofs), v(v), from(fr){}
};

inline ostream& operator<<(ostream &out, const Type t);

inline ostream& operator<<(ostream &out, const SymbolEntry e) {
	out << "-/-/ SymbolEntry /-/- " << endl << " offset: " << e.offset << endl << " type: " <<  e.type << endl;
	return out;
}

class Scope {
public:
  Scope(int ofs = -1) : locals(), offset(ofs), size(0) {}

  SymbolEntry * lookup(string c, string def) {
		if (def == "var"){
			if (locals.find(c) == locals.end()) {
				return nullptr;
			}
	    return &locals[c];
		}
		else {
			if (funcs.find(c) == funcs.end()) {
				return nullptr;
			}
	    return &funcs[c];
		}
  }

  void insert(string c, Type t, string def, vector<Type> v) {
		if (def == "var"){
			if (locals.find(c) != locals.end()) error("Duplicate variable: %s", c);
			if (funcs.find(c) != funcs.end()) error("Duplicate id: %s", c);
			cout << "Inserting Var: " << c << " into locals" << endl;
			locals[c] = SymbolEntry(t, offset++);
			// cout << locals.find(c)->second;
			// cout << " id: " << c << endl;
	    ++size;
			// cout << " Size: " << size << endl;
		}
		else {
			if (funcs.find(c) != funcs.end()) error("Duplicate function name: %s", c);
			if (locals.find(c) != locals.end()) error("Duplicate name: %s found in a function and a variable", c);
			cout << "Inserting Fun: " << c << " into funcs" << endl;
			funcs[c] = SymbolEntry(t, offset++, def, v);
			// cout << locals.find(c)->second;
			// cout << " id: " << c << endl;
			++size;
			// cout << " Size: " << size << endl;
		}
  }
  int getSize() const { return size; }
  int getOffset() const { return offset; }
	Type getLastFuncType() const {
		return (funcs.rbegin()->second).type;
	}
private:
	map<string, SymbolEntry> locals;
  map<string, SymbolEntry> funcs;
  int offset;
  int size;
};

class SymbolTable {
public:
  void openScope() {
    int ofs = scopes.empty() ? 0 : scopes.back().getOffset();
    scopes.push_back(Scope(ofs));
  }
  void closeScope() {
    scopes.pop_back();
  }
  SymbolEntry * lookup(string c, string def = "var") {
    for (auto i = scopes.rbegin(); i != scopes.rend(); ++i) {
			cout << "scope with size: " << i->getSize() << " and offset: " << i->getOffset() << endl;
			SymbolEntry *e = i->lookup(c, def);
      if (e != nullptr) return e;
    }
    if (def != "func_decl") error("Variable %s not found", c);
    return nullptr;
  }
  void insert(string c, Type t, string def = "var", vector<Type> v = vector<Type>()) {
    scopes.back().insert(c, t, def, v);
  }
  int getSizeOfCurrentScope() const {
    return scopes.back().getSize();
  }
	Type getReturnType() const {
		return (scopes.rbegin()+1)->getLastFuncType();
	}
	bool EmptyScopes() const{
		return scopes.empty();
	}
private:
  std::vector<Scope> scopes;
};

extern SymbolTable st;

#endif
