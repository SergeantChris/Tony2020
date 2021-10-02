#include "symbol.hpp"
#include "type.hpp"

using namespace std;


SymbolEntry::SymbolEntry() {}

SymbolEntry::SymbolEntry(Type t, int ofs, string fr, vector<Formal*>* v): type(t), offset(ofs), from(fr), params(v) {}

// SymbolEntry::~SymbolEntry() { del_entries(params); delete params; } // would fix memory leak but leads to error free(): invalid pointer


ostream& operator<<(ostream &out, const SymbolEntry e) {
	out << "-/-/ SymbolEntry /-/- " << endl << " offset: " << e.offset << endl << " type: " <<  e.type << endl;
	return out;
}

//----------------------------------------- SEMANTICS ------------------------------------------------

Scope::Scope(int ofs): locals(), offset(ofs), size(0) {}

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
	ostringstream formatted;
	if (def == "var"){
		if (locals.find(c) != locals.end()) {
			formatted << "Duplicate variable: " << c;
			error(formatted.str());
		}
		if (funcs.find(c) != funcs.end()) {
			formatted << "Duplicate id (this name is already used by a function): " << c;
			error(formatted.str());
		}
		#if PRE_DEBUG
			cout << "Inserting Var: " << c << " into locals" << endl;
		#endif
		locals[c] = SymbolEntry(t, offset++);
    ++size;
	}
	else {
		if (funcs.find(c) != funcs.end()) {
			if(!(def == "func_def") || !(funcs[c].from == "func_decl")) {
				formatted << "Duplicate function name: " << c;
				error(formatted.str());
			}
		}
		if (locals.find(c) != locals.end()) {
			formatted << "Duplicate id (this name is already used by a variable): " << c;
			error(formatted.str());
		}
		#if PRE_DEBUG
			cout << "Inserting Fun: " << c << " into funcs" << endl;
		#endif
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
SymbolEntry* SymbolTable::lookup(string c, string def) {
  for (auto i = scopes.rbegin(); i != scopes.rend(); ++i) {
		#if PRE_DEBUG
			cout << "scope with size: " << i->getSize() << " and offset: " << i->getOffset() << endl;
		#endif
		SymbolEntry *e = i->lookup(c, def);
    if (e != nullptr) return e;
  }
  if (def != "func_decl") {
  	ostringstream formatted;
  	formatted << "Entry " << c << " not found";
  	error(formatted.str());
  }
  return nullptr;
}
void SymbolTable::insert(string c, Type t, string def, vector<Formal*>* v, bool built_in) {
	if(def!="var" && !built_in && scopes.size()==1) {
		Type main_type;
		main_type.p = TYPE_void;
		if(!(t == main_type) || v!=nullptr) {
			ostringstream formatted;
			formatted << "Program's main function " << c << " must take no arguments and return no value";
			error(formatted.str());
		}
	}
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

//----------------------------------------- COMPILE ------------------------------------------------

CompileScope::CompileScope(int ofs) : defined(), offset(ofs), size(0) {}

ValueEntry* CompileScope::lookup(string c) {
		if (defined.find(c) == defined.end())
			return nullptr;
		return &defined[c];
}

void CompileScope::insert(string c, llvm::Value *v) {
		defined[c].val = v;
}

void CompileScope::insert(string c, int s, llvm::AllocaInst *a) {
	if (defined.find(c) != defined.end()) defined[c].lsize = s;
	else {
		defined[c] = ValueEntry(s, a, offset++);
		++size;
	}
}

void CompileScope::insert(string c, llvm::Function *f, map<string, llvm::Value*> refs) {
	if (defined.find(c) != defined.end())
		defined[c].func = f;
	else {
		defined[c] = ValueEntry(f, offset++, refs);
		++size;
	}
}

void CompileScope::insert(string c, llvm::AllocaInst *a, string call, generalType type) {
	if (defined.find(c) != defined.end())
		defined[c].alloc = a;
	else{
		defined[c] = ValueEntry(a, offset++, call, type);
		++size;
	}
}

map<string, ValueEntry> CompileScope::getMap() const { return defined; }

int CompileScope::getSize() const { return size; }

int CompileScope::getOffset() const { return offset; }


void ValueTable::openScope() {
	int ofs = scopes.empty() ? 0 : scopes.back().getOffset();
	scopes.push_back(CompileScope(ofs));
}

void ValueTable::closeScope() {
	scopes.pop_back();
}

ValueEntry* ValueTable::lookup(string c, bool glob) {
	for (auto i = scopes.rbegin(); i != scopes.rend(); ++i) {
		ValueEntry *e = i->lookup(c);
		if (glob && i == scopes.rbegin()) continue;
		if (e != nullptr) return e;
	}
	return nullptr;
}

void ValueTable::insert(string c, llvm::Value *v) {
	for (auto i = scopes.rbegin(); i != scopes.rend(); ++i) {
		ValueEntry *e = i->lookup(c);
		if (e != nullptr) i->insert(c, v);
	}
}

void ValueTable::insert(string c, int lsize, bool call_list, llvm::AllocaInst *a) {
	if (call_list) (scopes.rbegin()[1]).insert(c, lsize, a);
	else scopes.back().insert(c, lsize, a);
}

void ValueTable::insert(string c, llvm::Function *f, map<string, llvm::Value*> refs) {
	scopes.back().insert(c, f, refs);
}

void ValueTable::insert(string c, llvm::AllocaInst *a, string call, generalType type, bool call_list) {
	if (call_list) (scopes.rbegin()[1]).insert(c, a, call, type);
	scopes.back().insert(c, a, call, type);
}

int ValueTable::getSizeOfCurrentScope() const {
	return scopes.back().getSize();
}

bool ValueTable::EmptyScopes() const {
	return scopes.empty();
}

map<string, llvm::Type*> ValueTable::getGlobal() const {
	// return global variables for a specific Functions
	map<string, llvm::Type*> params = {};
	map<string, ValueEntry>::iterator it;
	for (auto i = scopes.rbegin(); i != scopes.rend(); ++i) {
		map<string, ValueEntry> defined = i->getMap();
		for (it = defined.begin(); it != defined.end(); it++) {
			ValueEntry sec = it->second;
			if (sec.alloc != nullptr && sec.func == nullptr){
				string name = it->first;
				if (params.find(name) == params.end()) params[name] = sec.alloc->getType();
			}
		}
	}
	return params;
}
