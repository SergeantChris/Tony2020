#ifndef __SYMBOL_HPP__
#define __SYMBOL_HPP__

#include "error.hpp"
#include <vector>
#include <map>
#include "ast.hpp"
#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/DerivedTypes.h>

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
	string from; // "var" or "func_def" of "func_decl"
	vector<Type> params; // vector with the types of the parameters - in func_decl
  SymbolEntry() {}
  SymbolEntry(Type t, int ofs, string fr = "var", vector<Type> v = vector<Type>()) : type(t), offset(ofs), from(fr), params(v) {}
};

inline ostream& operator<<(ostream &out, const Type t);

inline ostream& operator<<(ostream &out, const SymbolEntry e) {
	out << "-/-/ SymbolEntry /-/- " << endl << " offset: " << e.offset << endl << " type: " <<  e.type << endl;
	return out;
}

class Scope {
public:
  Scope(int ofs = -1) : locals(), funcs(), offset(ofs), size(0) {}

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

enum generalType { Tint, Tbool, Tchar, Tarray, Tlist, Tnull};

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
  CompileScope(int ofs = -1) : defined(), offset(ofs), size(0) {}
  ValueEntry * lookup(string c) {
			if (defined.find(c) == defined.end())
				return nullptr;
	    return &defined[c];
  }
  void insert(string c, llvm::Value *v) {
			defined[c].val = v;
	}
	void insert(string c, int s, llvm::AllocaInst *a) {
		if (defined.find(c) != defined.end()) defined[c].lsize = s;
		else {
			defined[c] = ValueEntry(s, a, offset++);
	    ++size;
		}
	}
	void insert(string c, llvm::Function *f, map<string, llvm::Value*> refs) {
		if (defined.find(c) != defined.end())
			defined[c].func = f;
		else {
			defined[c] = ValueEntry(f, offset++, refs);
	    ++size;
		}
  }
	void insert(string c, llvm::AllocaInst *a, string call, generalType type) {
		if (defined.find(c) != defined.end())
			defined[c].alloc = a;
		else{
			defined[c] = ValueEntry(a, offset++, call, type);
	    ++size;
		}
  }
	map<string, ValueEntry> getMap() const { return defined; }
  int getSize() const { return size; }
  int getOffset() const { return offset; }
private:
	map<string, ValueEntry> defined;
  int offset;
  int size;
	std::vector<int> v;
};

class ValueTable {
public:
  void openScope() {
    int ofs = scopes.empty() ? 0 : scopes.back().getOffset();
    scopes.push_back(CompileScope(ofs));
  }
  void closeScope() {
    scopes.pop_back();
  }
  ValueEntry * lookup(string c, bool glob = false) {
    for (auto i = scopes.rbegin(); i != scopes.rend(); ++i) {
			ValueEntry *e = i->lookup(c);
			if (glob && i == scopes.rbegin()) continue;
      if (e != nullptr) return e;
    }
    return nullptr;
  }
  void insert(string c, llvm::Value *v) {
		for (auto i = scopes.rbegin(); i != scopes.rend(); ++i) {
			ValueEntry *e = i->lookup(c);
      if (e != nullptr) i->insert(c, v);
    }
  }
	void insert(string c, int lsize, bool call_list = false, llvm::AllocaInst *a = nullptr) {
		if (call_list) (scopes.rbegin()[1]).insert(c, lsize, a);
    else scopes.back().insert(c, lsize, a);
  }
	void insert(string c, llvm::Function *f, map<string, llvm::Value*> refs = {}) {
    scopes.back().insert(c, f, refs);
  }
	void insert(string c, llvm::AllocaInst *a, string call = "no",
	 						generalType type = Tnull, bool call_list = false) {
		if (call_list) (scopes.rbegin()[1]).insert(c, a, call, type);
    scopes.back().insert(c, a, call, type);
		// cout << c << " inswrting aloc" << endl;
  }
  int getSizeOfCurrentScope() const {
    return scopes.back().getSize();
  }
	bool EmptyScopes() const{
		return scopes.empty();
	}
	map<string, llvm::Type*> getGlobal() const {
		// return global variables for a specific Functions
		map<string, llvm::Type*> params = {};
		map<string, ValueEntry>::iterator it;
		for (auto i = scopes.rbegin(); i != scopes.rend(); ++i) {
			map<string, ValueEntry> defined = i->getMap();
			for (it = defined.begin(); it != defined.end(); it++) {
				ValueEntry sec = it->second;
				if (sec.alloc != nullptr && sec.func == nullptr){
					string name = it->first;
	    		// cout << name << endl;
					if (params.find(name) == params.end()) params[name] = sec.alloc->getType();
				}
			}
		}
		return params;
	}
private:
  std::vector<CompileScope> scopes;
};

extern ValueTable vt;
extern bool retval;
#endif
