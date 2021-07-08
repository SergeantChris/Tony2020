#ifndef __SYMBOL_HPP__
#define __SYMBOL_HPP__

#include "error.hpp"
#include <vector>
#include <map>
#include "ast.hpp"

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
  SymbolEntry() {}
  SymbolEntry(Type t, int ofs) : type(t), offset(ofs) {}
};

inline ostream& operator<<(ostream &out, const SymbolEntry e) {
	out << "-/-/ SymbolEntry /-/- " << endl << " offset: " << e.offset << endl << " type: " <<  e.type.p << endl;
	return out;
}

class Scope {
public:
  Scope(int ofs = -1) : locals(), offset(ofs), size(0) {}

  SymbolEntry * lookup(string c) {
    if (locals.find(c) == locals.end()) {
			cout << "Did not found it " << endl;
			return nullptr;
		}

    return &locals[c];
  }

  void insert(string c, Type t) {
    if (locals.find(c) != locals.end()) error("Duplicate variable: %s", c);
		cout << "Inserting Var: " << c << " into locals" << endl;
		locals[c] = SymbolEntry(t, offset++);
		cout << locals.find(c)->second;
		cout << " id: " << c << endl;
    ++size;
		cout << " Size: " << size << endl;
  }
  int getSize() const { return size; }
  int getOffset() const { return offset; }
private:
	map<string, SymbolEntry> locals;
  map<string, SymbolEntry> funcs;
  int offset;
  int size;
};

class SymbolTable {
	//papaspyrou
public:
  void openScope() {
    int ofs = scopes.empty() ? 0 : scopes.back().getOffset();
    scopes.push_back(Scope(ofs));
  }
  void closeScope() {
    scopes.pop_back();
  }
  SymbolEntry * lookup(string c) {
		// papaspyrou
    for (auto i = scopes.rbegin(); i != scopes.rend(); ++i) {
			cout << "scope with size: " << i->getSize() << " and offset: " << i->getOffset() << endl;
			SymbolEntry *e = i->lookup(c);
      if (e != nullptr) return e;
    }
    error("Variable %s not found", c);
    return nullptr;
  }
  void insert(string c, Type t) {
    scopes.back().insert(c, t);
		// SymbolEntry *e = scopes[0].lookup(c);
		// cout << *e << endl;
  }
  int getSizeOfCurrentScope() const {
    return scopes.back().getSize();
  }
private:
  std::vector<Scope> scopes;
};

extern SymbolTable st;

#endif
