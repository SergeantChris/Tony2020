#include "error.hpp"
#include <vector>
#include <unordered_map>
#include <sstream>
#include "ast.hpp"
#include "symbol.hpp"

using namespace std;


SymbolEntry::SymbolEntry() {}

SymbolEntry::SymbolEntry(Type t, int ofs, string fr = "var", vector<Formal*>* v = nullptr): type(t), offset(ofs), from(fr), params(v) {} // inits for var


inline ostream& operator<<(ostream &out, const SymbolEntry e) {
	out << "-/-/ SymbolEntry /-/- " << endl << " offset: " << e.offset << endl << " type: " <<  e.type << endl;
	return out;
}


Scope::Scope(int ofs = -1): locals(), offset(ofs), size(0) {}

SymbolEntry* Scope::lookup(string c, string def) {
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
void Scope::insert(string c, Type t, string def, vector<Formal*>* v) {
	if (def == "var"){
		if (locals.find(c) != locals.end()) error("Duplicate variable: %s", c);
		if (funcs.find(c) != funcs.end()) error("Duplicate id: %s", c);
		cout << "Inserting Var: " << c << " into locals" << endl;
		locals[c] = SymbolEntry(t, offset++);
    ++size;
	}
	else {
		if (funcs.find(c) != funcs.end()) error("Duplicate function name: %s", c);
		if (locals.find(c) != locals.end()) error("Duplicate id: %s", c);
		cout << "Inserting Fun: " << c << " into funcs" << endl;
		funcs[c] = SymbolEntry(t, offset++, def, v);
		last_func = funcs[c];
		++size;
	}
}
int Scope::getSize() const { return size; }
int Scope::getOffset() const { return offset; }
Type Scope::getLastFuncType() const {
	return last_func.type;
}


void SymbolTable::openScope() {
  int ofs = scopes.empty() ? 0 : scopes.back().getOffset();
  scopes.push_back(Scope(ofs));
}
void SymbolTable::closeScope() {
  scopes.pop_back();
}
SymbolEntry* SymbolTable::lookup(string c, string def = "var") {
  for (auto i = scopes.rbegin(); i != scopes.rend(); ++i) {
		cout << "scope with size: " << i->getSize() << " and offset: " << i->getOffset() << endl;
		SymbolEntry *e = i->lookup(c, def);
    if (e != nullptr) return e;
  }
  if (def != "func_decl") {
  	ostringstream formatted;
  	formatted << "Entry " << c << " not found";
  	error(formatted.str());
  	// error("Entry %s not found", c);
  }
  return nullptr;
}
void SymbolTable::insert(string c, Type t, string def = "var", vector<Formal*>* v = nullptr) {
  scopes.back().insert(c, t, def, v);
}
int SymbolTable::getSizeOfCurrentScope() const {
  return scopes.back().getSize();
}
Type SymbolTable::getReturnType() const {
	return (scopes.rbegin()+1)->getLastFuncType();
}

bool SymbolTable::EmptyScopes() const{
	return scopes.empty();
}
