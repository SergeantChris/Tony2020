#ifndef __SYMBOL_HPP__
#define __SYMBOL_HPP__

#include "error.hpp"
#include <vector>
#include <map>
#include "ast.hpp"
using namespace std;
//NOTE TO SELF AFTER EXETASTIKH: I HAVE ISSUE WITH COMPOSITE CONSTRUCTORS,
//BUT I NEED TO FIX STACKTRACE TO SEE WHAT IT IS
//I THINK I NEED TO ADD A FLAG IN COMPILATION

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
  SymbolEntry() {}
  SymbolEntry(Type t, int ofs) : type(t), offset(ofs) {}
};
struct MySymbolEntry {
	const char* id;
	Type type;
	int size; // for arrays
  MySymbolEntry() {}
  MySymbolEntry(const char* i, Type t, int sz = 0) : id(i), type(t), size(sz) {}
};

inline ostream& operator<<(ostream &out, const SymbolEntry e) {
	out << "-/-/ SymbolEntry /-/- " << endl << " offset: " << e.offset << endl << " type: " << /*e.type <<*/ endl;
	return out;
}

class Scope {
public:
  Scope(int ofs = -1) : locals(), offset(ofs), size(0) {}
  SymbolEntry * lookup(const char* c) {
    if (locals.find(c) == locals.end()) {
			cout << locals[c];
			return nullptr;
		}
    return &locals[c];
  }
  void insert(const char* c, Type t) {
    if (locals.find(c) != locals.end()) error("Duplicate variable: %s", c);
		cout << "Inserting Var: " << c << " into locals" << endl;
		locals[c] = SymbolEntry(t, offset++);
		cout << locals[c];
		cout << " id: " << c << endl;
    ++size;
		cout << "Size: " << size << endl;
  }
  int getSize() const { return size; }
  int getOffset() const { return offset; }
private:
	map<const char*, SymbolEntry> locals;
  map<const char*, SymbolEntry> funcs;
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
  SymbolEntry * lookup(const char* c) {
    // for (auto i = scopes.rbegin(); i != scopes.rend(); ++i) {
		// 	cout << "scope with size: " << i->getSize() << " and offset: " << i->getOffset() << endl;
		// 	SymbolEntry *e = i->lookup(c);
    //   if (e != nullptr) return e;
    // }
		for(int i = scopes.size() - 1; i >= 0; i--) {
			cout << "scope with size: " << scopes[i].getSize() << " and offset: " << scopes[i].getOffset() << endl;
			SymbolEntry *e = scopes[i].lookup(c);
      if (e != nullptr) return e;
		}
    error("Variable %s not found", c);
    return nullptr;
  }
  void insert(const char* c, Type t) {
    scopes.back().insert(c, t);
  }
  int getSizeOfCurrentScope() const {
    return scopes.back().getSize();
  }
private:
  std::vector<Scope> scopes;
};


class MySymbolTable {
public:
  Type lookup(const char* c) {
		for(int i = vars.size() - 1; i >= 0; i--) {
			if(strcmp(vars[i].id, c) == 0) return vars[i].type;
		}
    error("Variable %s not found", c);
		Type t;
		t.p = TYPE_null;
    return t;
  }
	// int getSize(const char* c) {
	// 	for(int i = vars.size() - 1; i >= 0; i--) {
	// 		if(strcmp(vars[i].id, c) == 0) return vars[i].size;
	// 	}
	// 	error("Variable %s not found", c);
	// 	return -998;
	// }
	// void setSize(const char* c, int sz) {
	// 	for(int i = vars.size() - 1; i >= 0; i--) {
	// 		if(strcmp(vars[i].id, c) == 0) {
	// 			vars[i].size = sz;
	// 			return;
	// 		}
	// 	}
	// 	error("Variable %s not found", c);
	// }
  void insert(const char* c, Type t, int array_size = 0) {
		vars.push_back(MySymbolEntry(c, t, array_size));
  }

private:
  vector<MySymbolEntry> vars;
};

extern SymbolTable st;
extern MySymbolTable my_st;

#endif
