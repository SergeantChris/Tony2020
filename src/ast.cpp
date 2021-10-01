#include "ast.hpp"

using namespace std;


string CompositeType::getId() {
	return id;
}

Type CompositeType::getType() {
	return type;
}


Array::Array(Type t) {
	id = "array";
	type = t;
}
Array::Array(PrimitiveType p) {
	id = "array";
	type.p = p;
}


List::List(Type t) {
	id = "list";
	type = t;
}
List::List(PrimitiveType p) {
	id = "list";
	type.p = p;
}


bool isPrimitive(Type t) {
	bool is_prim = false;
	switch(t.p) {
		case TYPE_int: is_prim = true; break;
		case TYPE_bool: is_prim = true; break;
		case TYPE_char: is_prim = true; break;
		case TYPE_nil: is_prim = true; break;
		case TYPE_void: is_prim = true; break;
		default: break; // it means it is a CompositeType
	}
	return is_prim;
}

ostream& operator<<(ostream &out, const Type t) {
	switch(t.p) {
		case TYPE_int: out << "int"; return out;
		case TYPE_bool: out << "bool"; return out;
		case TYPE_char: out << "char"; return out;
		case TYPE_nil: out << "nil"; return out;
		case TYPE_void: out << "void"; return out;
		default: break;
	}
	out << (t.c)->getId() << "[" << ((t.c)->getType()) << "]";
	return out;
}

ostream& operator<<(ostream &out, const PrimitiveType p) { // overloading
	switch(p) {
		case TYPE_int: out << "int";
		case TYPE_bool: out << "bool";
		case TYPE_char: out << "char";
		case TYPE_nil: out << "nil";
		case TYPE_void: out << "void";
	}
	return out;
}

bool operator==(const Type &t1, const Type &t2) {
	bool res, comp1 = false, comp2 = false;
	if(t1.p == TYPE_nil || t2.p == TYPE_nil) return true; // nil is for always matching, void is for not
	comp1 = !isPrimitive(t1);
	comp2 = !isPrimitive(t2);
	if(!comp1 && !comp2) {
		switch(t1.p) {
			case TYPE_int: res = (t2.p == TYPE_int ? true : false); break;
			case TYPE_bool: res = (t2.p == TYPE_bool ? true : false); break;
			case TYPE_char: res = (t2.p == TYPE_char ? true : false); break;
			case TYPE_nil: res = (t2.p == TYPE_nil ? true : false); break;
			case TYPE_void: res = (t2.p == TYPE_void ? true : false); break;
		}
		if(res) return true;
	}
	else if(comp1 && comp2) {
		if((t1.c)->getId() == (t2.c)->getId() && ((t1.c)->getType() == (t2.c)->getType())) return true;
	}
	return false;
}

template <typename T>
void del_entries(vector<T>* vec) {
	if(vec != nullptr) {
		for(T p: *vec) {
			delete p;
		}
		vec->clear();
	}
}


ASTnode::~ASTnode() {}

void ASTnode::sem() {}

llvm::LLVMContext ASTnode::TheContext;
llvm::IRBuilder<> ASTnode::Builder(TheContext);
std::unique_ptr<llvm::Module> ASTnode::TheModule;
std::unique_ptr<llvm::legacy::FunctionPassManager> ASTnode::TheFPM;

llvm::Function *ASTnode::Puti;
llvm::Function *ASTnode::Putb;
llvm::Function *ASTnode::Putc;
llvm::Function *ASTnode::Puts;

llvm::Function *ASTnode::Geti;
llvm::Function *ASTnode::Getb;
llvm::Function *ASTnode::Getc;
llvm::Function *ASTnode::Gets;

llvm::Function *ASTnode::Abs;
llvm::Function *ASTnode::Ord;
llvm::Function *ASTnode::Chr;

llvm::Function *ASTnode::Strlen;
llvm::Function *ASTnode::Strcmp;
llvm::Function *ASTnode::Strcpy;
llvm::Function *ASTnode::Strcat;

llvm::Type *ASTnode::i1;
llvm::Type *ASTnode::i8;
llvm::Type *ASTnode::i32;
llvm::Type *ASTnode::i64;

void ASTnode::llvm_compile_and_dump(bool optimize) {
	// Initialize
	TheModule = llvm::make_unique<llvm::Module>("tony program", TheContext);
	// TheModule is an LLVM construct that contains functions and global variables
	TheFPM = llvm::make_unique<llvm::legacy::FunctionPassManager>(TheModule.get());
	if(optimize) {
		TheFPM->add(llvm::createPromoteMemoryToRegisterPass());
		TheFPM->add(llvm::createInstructionCombiningPass());
		TheFPM->add(llvm::createReassociatePass());
		TheFPM->add(llvm::createGVNPass());
		TheFPM->add(llvm::createCFGSimplificationPass());
		// TheFPM->add(llvm::MergeBlockIntoPredecessor());
	}

	TheFPM->doInitialization();
/*
* ------------------------------------------- Initialize Types -------------------------------------------
*/
	i1 = llvm::IntegerType::get(TheContext, 1);
	i8 = llvm::IntegerType::get(TheContext, 8);
	i32 = llvm::IntegerType::get(TheContext, 32);
	i64 = llvm::IntegerType::get(TheContext, 64);

/*
* ------------------------------------- Initialize Library Functions -------------------------------------
*/
	// void puti(int n)
	llvm::FunctionType *puti_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), i32, false);
	Puti = llvm::Function::Create(puti_type, llvm::Function::ExternalLinkage, "puti", TheModule.get());

	// void putb(bool b)
	llvm::FunctionType *putb_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), i1, false);
	Putb = llvm::Function::Create(putb_type, llvm::Function::ExternalLinkage, "putb", TheModule.get());

	// void putc(char c)
	llvm::FunctionType *putc_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), i8, false);
	Putc = llvm::Function::Create(putc_type, llvm::Function::ExternalLinkage, "putc", TheModule.get());

	// void puts(char[] s)
	llvm::FunctionType *puts_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), llvm::PointerType::get(i8, 0), false);
	Puts = llvm::Function::Create(puts_type, llvm::Function::ExternalLinkage, "puts", TheModule.get());

	// int geti()
	llvm::FunctionType *geti_type = llvm::FunctionType::get(i32, {}, false);
	Geti = llvm::Function::Create(geti_type, llvm::Function::ExternalLinkage, "geti", TheModule.get());

	// bool getb()
	llvm::FunctionType *getb_type = llvm::FunctionType::get(i1, {}, false);
	Getb = llvm::Function::Create(getb_type, llvm::Function::ExternalLinkage, "getb", TheModule.get());

	// char getc()
	llvm::FunctionType *getc_type = llvm::FunctionType::get(i8, {}, false);
	Getc = llvm::Function::Create(getc_type, llvm::Function::ExternalLinkage, "getc", TheModule.get());

	// void gets(int n, char[] s)
	llvm::FunctionType *gets_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i32, llvm::PointerType::get(i8, 0)}, false);
	Gets = llvm::Function::Create(gets_type, llvm::Function::ExternalLinkage, "gets", TheModule.get());

	// int abs(int n)
	llvm::FunctionType *abs_type = llvm::FunctionType::get(i32, {i32}, false);
	Abs = llvm::Function::Create(abs_type, llvm::Function::ExternalLinkage, "abs", TheModule.get());

	// int ord(char c)
	llvm::FunctionType *ord_type = llvm::FunctionType::get(i32, {i8}, false);
	Ord = llvm::Function::Create(ord_type, llvm::Function::ExternalLinkage, "ord", TheModule.get());

	// char chr(int n)
	llvm::FunctionType *chr_type = llvm::FunctionType::get(i8, {i32}, false);
	Chr = llvm::Function::Create(chr_type, llvm::Function::ExternalLinkage, "chr", TheModule.get());

	// int strlen(char[] s)
	llvm::FunctionType *strlen_type = llvm::FunctionType::get(i32, {llvm::PointerType::get(i8, 0)}, false);
	Strlen = llvm::Function::Create(strlen_type, llvm::Function::ExternalLinkage, "strlen", TheModule.get());

	// int strcmp(char[] s1, s2)
	llvm::FunctionType *strcmp_type = llvm::FunctionType::get(i32, {llvm::PointerType::get(i8, 0), llvm::PointerType::get(i8, 0)}, false);
	Strcmp = llvm::Function::Create(strcmp_type, llvm::Function::ExternalLinkage, "strcmp", TheModule.get());

	// void strcpy(char[] trg, src)
	llvm::FunctionType *strcpy_type = llvm::FunctionType::get(i32, {llvm::PointerType::get(i8, 0), llvm::PointerType::get(i8, 0)}, false);
	Strcpy = llvm::Function::Create(strcpy_type, llvm::Function::ExternalLinkage, "strcpy", TheModule.get());

	// void strcat(char[] trg, src)
	llvm::FunctionType *strcat_type = llvm::FunctionType::get(i32, {llvm::PointerType::get(i8, 0), llvm::PointerType::get(i8, 0)}, false);
	Strcat = llvm::Function::Create(strcat_type, llvm::Function::ExternalLinkage, "strcat", TheModule.get());

/*
* --------------------------------- Define and start the main Function ---------------------------------
*/
	llvm::FunctionType *main_type = llvm::FunctionType::get(i32, {}, false);
	llvm::Function *main = llvm::Function::Create(main_type, llvm::Function::ExternalLinkage,
																								"main", TheModule.get());
	llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "program_entry", main);
	Builder.SetInsertPoint(BB);

	// Emit the program code
	retval = false;
	compile();
	// Builder.GetInsertBlock()->eraseFromParent();
	// Verify the IR
	// bool bad = llvm::verifyModule(*TheModule, &llvm::errs());
	llvm::verifyModule(*TheModule, &llvm::errs());
	// if(bad) {
	//   std::cerr << "the IR is BAD!" << std::endl; // make it with color so call error function
	//   std::exit(1);
	// }
	// Optimize
	TheFPM->run(*main);

	// print out IR
	TheModule->print(llvm::outs(), nullptr);
}


ostream& operator<<(ostream &out, const ASTnode &n) {
	n.printNode(out);
	return out;
}


Expr::~Expr() {}
void Expr::typeCheck(Type t) {
	#if PRE_DEBUG
	cout << "-- TYPE CHECK  ("<< type << ", " << t << ")";
	#endif
	if(!(type == t)){
		cout << endl;
		ostringstream formatted;
		formatted << "Type mismatch, expected type: " << t << ", but type: " << type << " was used";
		error(formatted.str());
	}
	#if PRE_DEBUG
	else {
		cout << "  ---> ok" << endl;
	}
	#endif
}
void Expr::typeCheck(PrimitiveType p) { // overloading
	Type t;
	t.p = p;
	typeCheck(t);
}
void Expr::typeCheck(CompositeType* c) { // overloading
	Type t;
	t.c = c;
	typeCheck(t);
}
void Expr::typeCheck(CompositeType* c1, CompositeType* c2) { // overloading for union
	Type t1;
	t1.c = c1;
	Type t2;
	t2.c = c2;
	#if PRE_DEBUG
	cout << "-- TYPE CHECK  ("<< type << ", " << t1 << " OR " << t2 << ")";
	#endif
	if(!(type == t1) && !(type == t2)){
		cout << endl;
		ostringstream formatted;
		formatted << "Type mismatch, expected " << c1->getId() << " or " << c2->getId() << " kind of type, but " << type << " was used";
		error(formatted.str());
	}
	#if PRE_DEBUG
	else {
		cout << "  ---> ok" << endl;
	}
	#endif
}
Type Expr::getType() {
	return type;
}


Const::Const(int i)		 			{ tc.integer = i; tc_act = TC_int; }
Const::Const(char c) 				{ tc.character = c; tc_act = TC_char; }
Const::Const(string v, bool special) { // for boolean types and nil + string literals
	if(special == 1) {
		if(v == "true") {
			tc.boolean = true;
			tc_act = TC_bool;
		}
		else if(v == "false") {
			tc.boolean = false;
			tc_act = TC_bool;
		}
		else if(v == "nil") {
			tc_act = TC_nil;
		}
	}
	else {
		tc.str = strdup(v.c_str()); // must be c string so that it is null terminated as required by tony
		tc_act = TC_str;
	}
}
Const::~Const() {}
void Const::printNode(ostream &out) const {
	out << "Const(";
	switch(tc_act) {
		case(TC_int): out << tc.integer << ")"; break;
		case(TC_char): out << tc.character << ")"; break;
		case(TC_str): out << tc.str << ")"; break;
		case(TC_bool): out << (tc.boolean ? "true" : "false") << ")"; break;
		case(TC_nil): out << "nil_list" << ")"; break;
	}
}
void Const::sem() {
		switch(tc_act) {
			case(TC_int): type.p = TYPE_int; break;
			case(TC_char): type.p = TYPE_char; break;
			case(TC_str): type.c = new Array(TYPE_char); break;
			case(TC_bool): type.p = TYPE_bool; break;
			case(TC_nil): type.c = new List(TYPE_nil); break;
		}
}
llvm::Value* Const::compile() const {
	switch(tc_act) {
		case(TC_int): return c32(tc.integer);
		case(TC_char): return c8(tc.character);
		case(TC_bool): return c1(tc.boolean);
		// not only i32 but whatever type the list is
		case(TC_nil): return llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(i32));
		case(TC_str): return nullptr;		// TODO: do smth
	}
	return nullptr;
}
llvm::Value* Const::compile_check_call(bool call, string func_name, int index) const {
	return compile();
}


PreOp::PreOp(const char* o, Expr* e): op(o), expr(e) {}
PreOp::~PreOp() { delete expr; }
void PreOp::printNode(ostream &out) const {
	out << "PreOp(" << op << ", " << *expr << ")";
}
void PreOp::sem() {
	expr->sem();
	if(op == "+" || op == "-") {
		expr->typeCheck(TYPE_int);
		type.p = TYPE_int;
	}
	else if(op == "not") {
		expr->typeCheck(TYPE_bool);
		type.p = TYPE_bool;
	}
	else if(op == "nil?") {
		expr->typeCheck(new List(TYPE_nil));
		type.p = TYPE_bool;
	}
	else if(op == "head") {
		expr->typeCheck(new List(TYPE_nil));
		type = ((expr->getType()).c)->getType();
	}
	else if(op == "tail") {
		expr->typeCheck(new List(TYPE_nil));
		type = expr->getType();
	}
}
llvm::Value* PreOp::compile() const {
	llvm::Value *val = expr->compile();
	if (op == "+") return val;
	else if (op == "-") return Builder.CreateNeg(val, "negsign");
	else if (op == "not") return Builder.CreateNot(val, "nottmp");
	else if (op == "nil?") {
		llvm::Type *list_type = (val->getType())->getPointerElementType();
		return Builder.CreateICmpEQ(val, llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(list_type)), "nil?");
	}
	else if (op == "head"){
		string lname = expr->getId();
		if (lname == "_ithasnoid") lname = val->getName();
		// string lname = expr->getId();
		string sname = lname + "_size";
		ValueEntry *e = vt.lookup(sname);
		int lsize = e->lsize;
		llvm::Value *retval = Builder.CreateInBoundsGEP((val->getType())->getPointerElementType(), val,  c32(lsize - 1), "head");// return the first value of the List
		return Builder.CreateLoad(retval, "headval");
	 }
	else if(op == "tail") {
		string lname = expr->getId();
		string sname = lname + "_size";
		ValueEntry *e = vt.lookup(sname);
		int lsize = e->lsize;
		lsize--;
		string valname = val->getName();
		vt.insert(valname + "_size", lsize);
		return val; //return the last value of the list
	}
	else return nullptr;
}
llvm::Value* PreOp::compile_check_call(bool call, string func_name, int index) const {
	return compile();
}
const char* PreOp::getId()  {
	return "_ithasnoid";
}


Op::Op(Expr* e1, const char* o, Expr* e2): op(o), expr1(e1), expr2(e2) {}
Op::~Op() { delete expr1; delete expr2; }
void Op::printNode(ostream &out) const {
	out << "Op(" << *expr1 << ", " << op << ", " << *expr2 << ")";
}
void Op::sem() {
	expr1->sem();
	expr2->sem();
	if(op == "and" || op == "or") {
		expr1->typeCheck(TYPE_bool);
		expr2->typeCheck(TYPE_bool);
		type.p = TYPE_bool;
	}
	else if(op == "#") { // expr1->t, expr2->list[t]
		expr2->typeCheck(new List(expr1->getType()));
		type.c = new List(expr1->getType()); // because expr2->getType might still be null list
	}
	else if(op == "=" || op == "<>" || op == "<" || op == ">" || op == "<=" || op == ">=") {
		Type type_1 = expr1->getType();
		expr2->typeCheck(type_1);
		if(!isPrimitive(type_1)) {
			ostringstream formatted;
			formatted << "Can only compare primitive type operands, type is " << type_1;
			error(formatted.str());
		}
		type.p = TYPE_bool;
	}
	else if(op == "+" || op == "-" || op == "*" || op == "/" || op == "mod") {
		expr1->typeCheck(TYPE_int);
		expr2->typeCheck(TYPE_int);
		type.p = TYPE_int;
	}
}
llvm::Value* Op::compile() const {
	llvm::Value *l = expr1->compile();
	llvm::Value *r = expr2->compile();

	if (op == "+") return Builder.CreateAdd(l, r, "addtmp");
	else if (op == "-") return Builder.CreateSub(l, r, "subtmp");
	else if (op == "*") return Builder.CreateMul(l, r, "multmp");
	else if (op == "/") return Builder.CreateSDiv(l, r, "divtmp");
	else if (op == "mod") return Builder.CreateSRem(l, r, "modtmp");
	else if (op == "and") return Builder.CreateAnd(l, r, "andtmp");
	else if (op == "or") return Builder.CreateOr(l, r, "ortmp");
	else if (op == "#"){
		llvm::Type *seed_type = l->getType();
		// l is the seed and r is the list
		if (r == llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(i32))){
			//if r is nil => lhs has one elem and size 1
			llvm::AllocaInst *retalloc = Builder.CreateAlloca(llvm::PointerType::getUnqual(seed_type), nullptr, "tmplistalloc");
			retalloc->setAlignment(8);
			llvm::AllocaInst *initvalue = Builder.CreateAlloca(seed_type, nullptr, "initvalue");
			retalloc->setAlignment(4);
			Builder.CreateStore(initvalue, retalloc);
			llvm::Value *retval = Builder.CreateLoad(retalloc, "tmplistval");
			llvm::Value *elem = Builder.CreateInBoundsGEP(seed_type, retval,  c32(0), "lelalloc");
			Builder.CreateStore(l, elem);
			// size = 1
			string sname = (string)retval->getName() + "_size";
			vt.insert(sname, 1);
			return retval;
		}
		string lname = expr2->getId();
		if (lname == "_ithasnoid") lname = r->getName();
		string sname = lname + "_size";
		ValueEntry *e = vt.lookup(sname);
		int lsize = e->lsize;
		llvm::Value *v = Builder.CreateInBoundsGEP(seed_type, r,  c32(lsize), "lelalloc");
		Builder.CreateStore(l, v);
		// not always!
		string sname_load = (string)r->getName() + "_size";
		vt.insert(sname_load, lsize+1);
		return r;
	}
	else if (op == "=") return Builder.CreateICmpEQ(l, r, "eqtmp");
	else if (op == "<>") return Builder.CreateICmpNE(l, r, "netmp");
	else if (op == "<") return Builder.CreateICmpSLT(l, r, "slttmp");
	else if (op == ">") return Builder.CreateICmpSGT(l, r, "sgttmp");
	else if (op == "<=") return Builder.CreateICmpSLE(l, r, "sletmp");
	else if (op == ">=") return Builder.CreateICmpSGE(l, r, "sgemp");
	else return nullptr;
}
llvm::Value* Op::compile_check_call(bool call, string func_name, int index) const {
	return compile();
}
const char* Op::getId() {
	return "_ithasnoid";
}


MemoryAlloc::MemoryAlloc(Type t, Expr* e): new_type(t), expr(e) {}
MemoryAlloc::~MemoryAlloc() { delete expr; }
void MemoryAlloc::printNode(ostream &out) const {
	out << "MemoryAlloc(" << type << ", " << *expr << ")";
}
void MemoryAlloc::sem() {
	expr->sem();
	expr->typeCheck(TYPE_int);

	Type final_type;
	final_type.c = new Array(new_type);
	type = final_type;
}
llvm::Value* MemoryAlloc::compile() const {
	return compile_alloc_mem();
}
llvm::AllocaInst* MemoryAlloc::compile_alloc_mem(string name) const {
	//type = array(new_type)
	llvm::Value *v = expr->compile(); // size of array
	llvm::ConstantInt* ci = llvm::dyn_cast<llvm::ConstantInt>(v);
	uint64_t size = ci->llvm::ConstantInt::getZExtValue();
	llvm::Type *array_type = defineArrayType(new_type);

	llvm::ArrayType *array = llvm::ArrayType::get(array_type->getPointerElementType(), size);
	llvm::AllocaInst *ArrayAlloc = Builder.CreateAlloca(array, nullptr, name);
	// ArrayAlloc = Builder.CreateAlloca(array_type, nullptr, name);
	ArrayAlloc->setAlignment(8);
	return ArrayAlloc;
}
llvm::Value* MemoryAlloc::compile_check_call(bool call, string func_name, int index) const {
	return compile();
}
llvm::Type* MemoryAlloc::defineArrayType(Type t) const{
	switch (t.p) {
		case TYPE_int: return llvm::Type::getInt32PtrTy(TheContext);
		case TYPE_bool: return llvm::Type::getInt1PtrTy(TheContext);
		case TYPE_char: return llvm::Type::getInt8PtrTy(TheContext);
		default: break;
	}
	if (t.c->getId() == "array" ){
		return llvm::PointerType::getUnqual(defineArrayType(t.c->getType()));
	}
	return llvm::PointerType::getUnqual(defineArrayType(t.c->getType()));
}


Atom::~Atom() {}


Id::Id(const char* i): id(i) {}
Id::~Id() { delete id; }
void Id::printNode(ostream &out) const {
	out << "Id(" << id << ")";
}
const char* Id::getId() {
	return id;
}
void Id::sem() {
	#if PRE_DEBUG
		cout << "Searching for: " << id << " ... ";
	#endif
	SymbolEntry *e = st.lookup(string(id));
	#if PRE_DEBUG
		cout << "Found it with offset: " << e->offset << " and type: " << e->type << endl;
	#endif
	type = e->type;
}
bool Id::isLVal() const { return true;}
llvm::Value* Id::compile() const {
	return compile_check_call();
}
llvm::Value* Id::compile_check_call(bool call, string func_name, int index) const {
	string var  = id;
	llvm::Value *v;
	ValueEntry *e = vt.lookup(var);
	v = e->val;
	if (call){
		llvm::Function *func_value = TheModule->getFunction(func_name);
		ValueEntry *e = vt.lookup(func_value->getName());
		map<string, llvm::Value*> refs = e->refs;
		// define which params are by ref
		int i = 0;
		for (auto &Arg : func_value->args()){
			string name = Arg.getName();
			if (refs.count(name) & (i == index)){
				ValueEntry *parentry = vt.lookup(var);
				return parentry->alloc;
			}
			i++;
		}
	}
	if (e->alloc){
		llvm::Value *ret =  Builder.CreateLoad(v, var);
		if (e->call == "ref" || e->call == "glob"){
			return Builder.CreateLoad(ret, var);
		}
		return ret;
	}
	else {
		if (e->call == "ref" || e->call == "glob"){
			return Builder.CreateLoad(v, var);
		}
		return v;
	}
}
llvm::Value* Id::compile_alloc() const {
	string var = string(id);
	ValueEntry *e = vt.lookup(var);
	if (!e) return nullptr;
	return e->alloc;
}


String::String(string s): Const(s, 0) {}
String::~String() {}
bool String::isLVal() const { return false;}
llvm::Value* String::compile_alloc() const {
	return nullptr;
}


DirectAcc::DirectAcc(Atom* a, Expr* e): atom(a), expr(e) {}
DirectAcc::~DirectAcc() { delete atom; delete expr; }
void DirectAcc::printNode(ostream &out) const {
	out << "DirectAcc(" << *atom << ", " << *expr << ")";
}
void DirectAcc::sem() {
	expr->sem();
	atom->sem();
	atom->typeCheck(new Array(TYPE_nil), new List(TYPE_nil));
	expr->typeCheck(TYPE_int);
	type = ((atom->getType()).c)->getType();
}
bool DirectAcc::isLVal() const { //complicated one
	
	Type not_mutable;
	not_mutable.c = new List(TYPE_nil);

	if(dynamic_cast<Id*>(atom) != nullptr) {
		SymbolEntry *e = st.lookup(dynamic_cast<Id*>(atom)->getId());
		Type id_type = e->type;
		if(id_type == not_mutable) return false;
		else return true;
	}
	else if(dynamic_cast<String*>(atom) != nullptr) {
		return false;
	}
	else if(dynamic_cast<ReturnValue*>(atom) != nullptr) {
		return true;
	}
	else {
		Type inner_access_type = atom->getType();
		return (!(inner_access_type == not_mutable) && atom->isLVal()); // inner access is not list, outer access is lval
	}
}
llvm::Value* DirectAcc::compile() const {
	llvm::Value *v = Builder.CreateLoad(compile_alloc(), "elemval");
	return v;
}
llvm::Value* DirectAcc::compile_alloc() const {
	llvm::Value *vexpr = expr->compile();
	llvm::Value *vatom = atom->compile_alloc();
	// llvm::Value *elem = Builder.CreateLoad(vatom, "arrayelem");
	// llvm::Value *v = Builder.CreateInBoundsGEP((elem->getType())->getPointerElementType(), elem,  vexpr, "elemalloc");
	llvm::Value *v = nullptr;
	if (((vatom->getType())->getPointerElementType())->isArrayTy()){
			v = Builder.CreateInBoundsGEP(vatom, {c32(0), vexpr}, "elemalloc");
	}
	if (((vatom->getType())->getPointerElementType())->isPointerTy()){
		 llvm::Value *elem = Builder.CreateLoad(vatom, "arrayelem");
		 v = Builder.CreateInBoundsGEP((elem->getType())->getPointerElementType(), elem,  vexpr, "elemalloc");
	}
	return v;
}
llvm::Value* DirectAcc::compile_check_call(bool call, string func_name, int index) const {
	return compile();
}
const char* DirectAcc::getId() {
	return atom->getId();
}


Stmt::~Stmt() {}


Simple::~Simple() {}


NoAction::NoAction() {}
NoAction::~NoAction() {}
void NoAction::printNode(ostream &out) const {
	out << "NoAction()";
}
void NoAction::sem() {}
llvm::Value* NoAction::compile() const {
	return nullptr;
}


Assign::Assign(Atom* a, Expr* e): atom(a), expr(e) {}
Assign::~Assign() { delete atom; delete expr; }
void Assign::printNode(ostream &out) const {
	out << "Assign(" << *atom << ", " << *expr << ")";
}
void Assign::sem() {
	expr->sem();
	atom->sem();
	expr->typeCheck(atom->getType());
	if(!atom->isLVal()) {
		ostringstream formatted;
		formatted << "Cannot assign to " << *atom << ", not an l-value";
		error(formatted.str());
	}
}
llvm::Value* Assign::compile() const {
	bool ref = 0;
	string name = string(atom->getId());
	ValueEntry *e = vt.lookup(name);
	if (!e) {
		// etc i = new int[2]
		// no store only memory allocation
		llvm::AllocaInst *ral = expr->compile_alloc_mem(name);
		vt.insert(name, ral, "no", Tarray);
		llvm::Value *v = ral;
		vt.insert(name, v);
		return nullptr;
	}

	llvm::Value *rhs = expr->compile();
	// lhs works for id but not for direct access yet.
	llvm::Value *lhs = atom->compile_alloc();
	if (!lhs) {
		llvm::AllocaInst *al;
		// is a parameter!
		// if is call by value we just need to allocate memory and store (locally)
		string calltype = e->call;
		if (calltype == "val"){
			al = Builder.CreateAlloca(rhs->getType(), nullptr, name);
			al->setAlignment(4);
			vt.insert(name, al);
			lhs = al;
		}
		else if (calltype == "ref" || calltype == "glob"){
			//if call by ref we need PointerType to rhs->getType() and setAlignment(8)
			al = Builder.CreateAlloca(llvm::PointerType::getUnqual(rhs->getType()), nullptr, name);
			al->setAlignment(8);
			vt.insert(name, al);
			lhs = al;
			Builder.CreateStore(e->val, lhs);
			llvm::Value *ptr = Builder.CreateLoad(lhs, name);
			// if (calltype == "glob") lhs = ptr;
			Builder.CreateStore(rhs, ptr);
			ref = 1;
		}
	}
	if (rhs == llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(i32)) &&
				e->type == Tlist)
	{
		// assign nil to list
		llvm::Type *list_type = (lhs->getType())->getPointerElementType()->getPointerElementType();
		rhs = llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(list_type));
	}
	else if (e->type == Tlist){
		string rname = expr->getId();
		if (rname == "_ithasnoid") rname = rhs->getName();
		// string rname = rhs->getName();
		// cout << rname << endl;
		ValueEntry *rse = vt.lookup(rname + "_size");
		int rsize = rse->lsize;
		vt.insert(name + "_size", rsize);
	}

	if ((rhs->getType())->isArrayTy()){
		// right side is a defined array
		string rnme = expr->getId();
		ValueEntry *re = vt.lookup(rnme);
		llvm::Type* eltype = (rhs->getType())->getArrayElementType();
		rhs = Builder.CreateBitCast(re->alloc, llvm::PointerType::getUnqual(eltype), "ptr");
	}
	else if ((rhs->getType())->isPointerTy()
		&& ((rhs->getType())->getPointerElementType())->isArrayTy()) {
			// right side is a new memory allocation
			llvm::Type* eltype = ((rhs->getType())->getPointerElementType())->getArrayElementType();
			rhs = Builder.CreateBitCast(rhs, llvm::PointerType::getUnqual(eltype), "ptr");
	}
	if (!ref) Builder.CreateStore(rhs, lhs);

	if (e->type != Tarray) vt.insert(name, lhs);
	if (e->call == "ref" || e->call == "glob"){
		string func_name = Builder.GetInsertBlock()->getParent()->getName();
		ValueEntry *fe = vt.lookup(func_name);
		map<string, llvm::Value*> refs = fe->refs;
		refs[name] = lhs;
	}
	return nullptr;
}


Call::Call(const char* i, vector<shared_ptr<Expr>>* e): id(i), exprList(e) {}
Call::~Call() { delete id; delete exprList; }
void Call::printNode(ostream &out) const {
	out << "Call(" << id;
	if(exprList != nullptr) {
		for(shared_ptr<Expr> e: *exprList) {
			out << ", ";
			out << *e;
		}
	}
	out << ")";
}
void Call::sem() {
	// checks if the function is defined
	cout << "Looking up for definition of the function ..." << endl;
	SymbolEntry *func = st.lookup(string(id), "func_def");
	vector<Formal*>* params = func->params;
	type = func->type;

	if(exprList != nullptr) {
		if(params != nullptr) {
			// check the number of arguments in the call
			if(params->size() == exprList->size()) {
				// checks if the function's arguments are the same type as the exprList (one by one)
				int i = 0;
				for(shared_ptr<Expr> e: *exprList) {
					e->sem();
					e->typeCheck(params->at(i)->getType());
					if(params->at(i)->getCb() == true) {
						if((dynamic_cast<Atom*>(e.get()) == nullptr) || !(dynamic_cast<Atom*>(e.get())->isLVal())) {
							error("Actual parameter has to be l-value when passing by reference");
						}
					}
					i++;
				}
			}
			else {
				ostringstream formatted;
				formatted << "Function takes " << params->size() << " arguments, but" << exprList->size() << " arguments were given";
				error(formatted.str());
			}
		}
		else {
			ostringstream formatted;
			formatted << "Function takes no arguments, but " << exprList->size() << " arguments were given";
			error(formatted.str());
		}
	}
}
Type Call::getType() {
	return type;
}
llvm::Value* Call::compile() const {
	// Look up the name in the global module table.
	string func_name = string(id);
	llvm::Function *func_value = TheModule->getFunction(func_name);

	int i = 0;
	vector<llvm::Value *> argsv = {};
	if (exprList)
		for(shared_ptr<Expr> e: *exprList) {
			llvm::Value *v = e->compile_check_call(true, func_name, i);
			argsv.push_back(v);
			i++;
		}

	unsigned idx = 0;
	unsigned size = argsv.size();
	for (auto &Arg : func_value->args())
		if (idx++ >= size){
			string argname = Arg.getName();
			ValueEntry *e = vt.lookup(argname);
			llvm::Value *argval = e->alloc;
			if (argval == nullptr){
				e = vt.lookup(argname, true);
				argval = e->alloc;
				llvm::AllocaInst *al2 = Builder.CreateAlloca(argval->getType(), nullptr, argname);
				al2->setAlignment(8);
				Builder.CreateStore(argval, al2);
				llvm::Value *v = Builder.CreateLoad(al2, argname);
				argsv.push_back(v);
			}
			else{
				argsv.push_back(e->alloc);
			}
		}
	if (func_value->getReturnType() == llvm::Type::getVoidTy(TheContext))
		return Builder.CreateCall(func_value, argsv);
	return Builder.CreateCall(func_value, argsv, "calltmp");
}


ReturnValue::ReturnValue(Call* c): call(c) {}
ReturnValue::~ReturnValue() { delete call; }
void ReturnValue::printNode(ostream &out) const {
	out << "ReturnValue(" << *call << ")";
}
void ReturnValue::sem() {
	// TODO: CAN I SOMEHOW GET FUNC RETURN ID AS WELL? see assign comment
	call->sem();
	type = call->getType();
}
bool ReturnValue::isLVal() const { return false;}
llvm::Value* ReturnValue::compile_check_call(bool call, string func_name, int index) const {
	return compile();
}
llvm::Value* ReturnValue::compile() const {
	llvm::Value *ret =  call->compile();
	return ret;

}
llvm::Value* ReturnValue::compile_alloc() const {
	return nullptr;
}


Return::Return(Expr* v): ret_val(v) {}
Return::~Return() { delete ret_val; }
void Return::printNode(ostream &out) const {
	out << "Return(";
	if(ret_val != nullptr) out << *ret_val;
	out << ")";
}
void Return::sem() {
	if(ret_val != nullptr) {
		ret_val->sem();
		#if PRE_DEBUG
			cout << "Checking return type ..." << endl;
		#endif
		ret_val->typeCheck(st.getReturnType());
	}
	else {
		cout << "Checking return type ..." << endl;
		Type rv_type;
		rv_type.p = TYPE_void;
		Type r_type = st.getReturnType();
		if(!(r_type == rv_type)) {
			ostringstream formatted;
			formatted << "Function of return type " << r_type << " must return a value";
			error(formatted.str());
		}
	}
}
llvm::Value* Return::compile() const {
	retval = true;
	if (llvm::Value *RetVal = ret_val->compile()) {
		// Finish off the function.
		Builder.CreateRet(RetVal);
		return RetVal;
	}
	else return Builder.CreateRetVoid();
}


Branch::Branch(vector<shared_ptr<Stmt>>* ct, Expr* c, vector<Branch*>* eif, Branch* e): cond_true(ct), condition(c), elsif_branches(eif), else_branch(e) {}
Branch::~Branch() { delete cond_true; delete condition; del_entries(elsif_branches); delete elsif_branches; delete else_branch; }
void Branch::printNode(ostream &out) const {
	out << "Branch(";
	bool first = true;
	for(shared_ptr<Stmt> s: *cond_true) {
		if(!first) out << ", ";
		first = false;
		out << *s;
	}
	out << ", " << *condition;
	if(elsif_branches != nullptr) {
		for(Branch* b: *elsif_branches) {
			out << ", ";
			out << *b;
		}
	}
	if(else_branch != nullptr) out << ", " << *else_branch;
	out << ")";
}
void Branch::sem() {
	condition->sem();
	condition->typeCheck(TYPE_bool);
	for(shared_ptr<Stmt> s: *cond_true) s->sem();
	if(elsif_branches != nullptr) {
		for(Branch* b: *elsif_branches) b->sem();
	}
	if(else_branch != nullptr) else_branch->sem();
}
llvm::Value* Branch::compile() const {

	llvm::Value *v = condition->compile();
	llvm::Value *cond = Builder.CreateICmpNE(v, c1(0), "if_cond");
	llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
	llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(TheContext, "then", TheFunction);

	vector<llvm::BasicBlock*> IfElseVec;
	if(elsif_branches != nullptr && !(elsif_branches->empty()))
		for (size_t i = 0; i < (*elsif_branches).size(); i++)
			IfElseVec.push_back(llvm::BasicBlock::Create(TheContext, "ifelse", TheFunction));
	else IfElseVec.push_back(llvm::BasicBlock::Create(TheContext, "ifelse", TheFunction));
	llvm::BasicBlock *ElseBB =
		llvm::BasicBlock::Create(TheContext, "else", TheFunction);
	llvm::BasicBlock *AfterBB =
		llvm::BasicBlock::Create(TheContext, "endif", TheFunction);

	llvm::BasicBlock *IfElseBB;
	IfElseBB = IfElseVec.back();
	IfElseVec.pop_back();
	if(elsif_branches != nullptr){
		Builder.CreateCondBr(cond, ThenBB, IfElseBB);
	}
	else {
		IfElseBB->eraseFromParent();
		ElseBB->eraseFromParent();
		Builder.CreateCondBr(cond, ThenBB, AfterBB);
	}

	Builder.SetInsertPoint(ThenBB);
	for(shared_ptr<Stmt> s: *cond_true) s->compile();
	if (!retval) Builder.CreateBr(AfterBB);
	else retval = false;

	if(elsif_branches != nullptr) {
		Builder.SetInsertPoint(IfElseBB);
		for(Branch *b: *elsif_branches) {
			b->compile();
			if(!(IfElseVec.empty())){
				IfElseBB = IfElseVec.back();
				IfElseVec.pop_back();
				Builder.CreateBr(IfElseBB);
				// else retval = false;
				Builder.SetInsertPoint(IfElseBB);
			}
			else {
				Builder.CreateBr(ElseBB);
				// else retval = false;
				Builder.SetInsertPoint(ElseBB);
			}
		}
		if (elsif_branches->empty()) Builder.CreateBr(ElseBB);
		Builder.SetInsertPoint(ElseBB);
		if(else_branch != nullptr) else_branch->compile();
		Builder.CreateBr(AfterBB);
	}
	Builder.SetInsertPoint(AfterBB);
	// TODO: problem when there is no other body in the afterBB afterwards
	// retval = false;
	return AfterBB;
}
void Branch::setAfterBB(llvm::BasicBlock *AfterBB){

}


Loop::Loop(vector<shared_ptr<Simple>>* i,	Expr* c, vector<shared_ptr<Simple>>* s,	vector<shared_ptr<Stmt>>* ct): inits(i), condition(c), steps(s), cond_true(ct) {}
Loop::~Loop() { delete inits; delete condition; delete steps; delete cond_true; }
void Loop::printNode(ostream &out) const {
	out << "Loop(";
	bool first = true;
	for(shared_ptr<Simple> s: *inits) {
		if(!first) out << ", ";
		first = false;
		out << *s;
	}
	out << ", " << *condition;
	for(shared_ptr<Simple> s: *steps) {
		out << ", ";
		out << *s;
	}
	for(shared_ptr<Stmt> s: *cond_true) {
		out << ", ";
		out << *s;
	}
	out << ")";
}
void Loop::sem() {
	condition->sem();
	condition->typeCheck(TYPE_bool);
	for(shared_ptr<Simple> s: *inits) 	s->sem();
	for(shared_ptr<Simple> s: *steps) 	s->sem();
	for(shared_ptr<Stmt> s: *cond_true) s->sem();
}
llvm::Value* Loop::compile() const {
	for(shared_ptr<Simple> s: *inits) 	s->compile();
	llvm::Value *cond = condition->compile();
	llvm::BasicBlock *PrevBB = Builder.GetInsertBlock();
	llvm::Function *TheFunction = PrevBB->getParent();

	llvm::BasicBlock *LoopBB =
		llvm::BasicBlock::Create(TheContext, "loop", TheFunction);
	llvm::BasicBlock *BodyBB =
		llvm::BasicBlock::Create(TheContext, "body", TheFunction);
	llvm::BasicBlock *AfterBB =
	llvm::BasicBlock::Create(TheContext, "endfor", TheFunction);

	Builder.CreateBr(LoopBB);
	Builder.SetInsertPoint(LoopBB);
	llvm::PHINode *phi_iter = Builder.CreatePHI(i1, 2, "iter");
	phi_iter->addIncoming(cond, PrevBB);
	// llvm::Value *loop_cond =
	//   Builder.CreateICmpSGT(phi_iter, c1(0), "loop_cond");
	Builder.CreateCondBr(phi_iter, BodyBB, AfterBB);
	Builder.SetInsertPoint(BodyBB);
	for(shared_ptr<Stmt> s: *cond_true) 	s->compile();
	for(shared_ptr<Simple> s: *steps) s->compile();
	llvm::Value *remaining = condition->compile();
	phi_iter->addIncoming(remaining, Builder.GetInsertBlock());
	Builder.CreateBr(LoopBB);
	Builder.SetInsertPoint(AfterBB);
	return nullptr;
}


Formal::Formal(Type t, vector<const char*>* i, string cb): type(t), idl(i) {
	if(cb == "val") call_by_reference = false;
	else if(cb == "ref") call_by_reference = true;
}
Formal::~Formal() { delete idl; }
void Formal::printNode(ostream &out) const {
	out << "Formal(" << type << ", ";
	if(call_by_reference == true) out << "call by reference" << ", ";
	bool first = true;
	for(const char* i: *idl) {
		if(!first) out << ", ";
		first = false;
		out << i;
	}
	out << ")";
}
void Formal::sem() {
	for(const char* i: *idl) {
		// saves each var into the SymbolTable in the scope of the function
		st.insert(string(i), type);
	}
}
vector<Formal*>* Formal::getOpenedFormal() {
	vector<Formal*>* opened = new vector<Formal*>;
	string ref = (call_by_reference ? "ref" : "val");
	for(const char* id: *idl) {
		vector<const char*>* idl_self = new vector<const char*>;
		idl_self->push_back(id);
		opened->push_back(new Formal(type, idl_self, ref));
	}
	return opened;
}
Type Formal::getType() {
	return type;
}
vector<const char*>* Formal::getIds() {
	return idl;
}
bool Formal::getCb() {
	return call_by_reference;
}
llvm::Value* Formal::compile() const {

	for(const char* i: *idl) {
		string name = string(i);
		llvm::AllocaInst *a = nullptr;
		if (call_by_reference){
			vt.insert(name, a, "ref");
		}
		else vt.insert(name, a, "val");
	}
	return nullptr;
}
int Formal::getCountofIds() {
	return idl->size();
}


Header::Header(const char* i, vector< Formal*>* f, Type t): id(i), fl(f) {
	type = t;
}
Header::~Header() { delete id; del_entries(fl); delete fl; }
void Header::printNode(ostream &out) const {
	out << "Header(" << id;
	if(type.p != TYPE_void) out << ", " << type;
	if(fl != nullptr) {
		for(Formal* f: *fl) {
			out << ", ";
			out << *f;
		}
	}
	out << ")";
}
void Header::sem(bool func) {
	if (!st.EmptyScopes()){
		#if PRE_DEBUG
			cout << "Looking up for declaration of the function ... " << endl;
		#endif
		SymbolEntry *e = st.lookup(id, "func_decl");
		vector<Formal*>* params;
		if(fl != nullptr) {
			params = new vector<Formal*>;
			for(Formal* f: *fl) {
				vector<Formal*>* subformals;
				subformals = f->getOpenedFormal();
				for(Formal* f: *subformals) {
					params->push_back(f);
				}
				delete subformals;
			}
		}
		else params = nullptr;
		if (e == nullptr) {
			if(func) st.insert(string(id), type, "func_def", params);
			else st.insert(string(id), type, "func_decl", params);
		}
		else {
			string def = e->from;
			if ((def == "func_def") && func) error("Duplicate function definition");
			else if (!func) error("Duplicate function declaration");
			else {
				if (!(e->type == type)) error("Mismatch in function definition: %s", id);
				vector<Formal*>* decl_params = e->params;
				if(decl_params != nullptr) {
					int i=0;
					for(Formal* f_def: *params) {
						Formal* f_decl = decl_params->at(i);
						ostringstream formatted;
						if(!(f_def->getType() == f_decl->getType())) {
							formatted << "Mismatch in arg type at position " << i << " of function: " << id;
							error(formatted.str());
						}
						if(strcmp(f_def->getIds()->at(0), f_decl->getIds()->at(0))) {
							formatted << "Mismatch in arg id at position " << i << " of function: " << id;
							error(formatted.str());
						}
						if(f_def->getCb() != f_decl->getCb()) {
							formatted << "Mismatch in arg \'call by\' method at position " << i << " of function: " << id;
							error(formatted.str());
						}
						i++;
					} // checks if types, names, and callbys are the same as decl
				}
				st.insert(string(id), type, "func_def", params);
			}
		}
	}
	st.openScope();
	#if PRE_DEBUG
		cout << "+++ Opening new scope!" << endl;
	#endif
	if((fl != nullptr) & func) {
		for(Formal* f: *fl) {
				f->sem();
		}
	}
}
llvm::Value* Header::compile() const {
	bool main = false;
	string func_name = string(id);
	if (func_name == "main") func_name = "jackthecutestdoggo";
	if(vt.EmptyScopes()){
		vt.openScope();
		vt.insert("puti",Puti);
		vt.insert("putb",Putb);
		vt.insert("putc",Putc);
		vt.insert("puts",Puts);
		vt.insert("geti",Geti);
		vt.insert("getb",Getb);
		vt.insert("getc",Getc);
		vt.insert("gets",Gets);
		vt.insert("abs",Abs);
		vt.insert("ord",Ord);
		vt.insert("chr",Chr);
		vt.insert("strlen",Strlen);
		vt.insert("strcmp",Strcmp);
		vt.insert("strcpy",Strcpy);
		vt.insert("strcat",Strcat);
		main = true;
	}
	llvm::Type *func_type;
	if (type.p == TYPE_int) func_type = i32;
	else if (type.p == TYPE_bool) func_type = i1;
	else if (type.p == TYPE_char) func_type = i8;
	else func_type = llvm::Type::getVoidTy(TheContext);

	// adding parameters' type in Function Type
	vector<llvm::Type*> params = {};
	// Set names for all arguments.
	vector<string> Args = {};
	map<string, llvm::Value*> refs = {};
	if (fl)
		for(Formal *f: *fl) {
			Type formal_type = f->getType();
			llvm::Type *llvm_type;
			if (formal_type.p == TYPE_int) llvm_type = i32;
			else if (formal_type.p == TYPE_bool) llvm_type = i1;
			else if (formal_type.p == TYPE_char) llvm_type = i8;
			// array and list type?
			if (f->getCb()) llvm_type = llvm::PointerType::getUnqual(llvm_type);
			params.insert(params.end(), f->getCountofIds(), llvm_type);
			bool call_by_reference = f->getCb();
			vector<const char*>* idl = f->getIds();
			for(const char* i: *idl) {
				string name = string(i);
				Args.push_back(name);
				if (call_by_reference) refs[name] = nullptr;
			}
		}
	// adding global variables
	// cout << "------" << func_name << endl;
	map<string, llvm::Type*> globalParams = vt.getGlobal();
	map<string, llvm::Type*>::iterator it;
	for (it = globalParams.begin(); it != globalParams.end(); it++)
		if (find(Args.begin(), Args.end(), it->first) == Args.end()){
			params.push_back(it->second);
			Args.push_back(it->first);
			refs[it->first] = nullptr;
		}
	// define function!
	llvm::FunctionType *FT =
		llvm::FunctionType::get(func_type, params, false);
	llvm::Function *F =
		llvm::Function::Create(FT, llvm::Function::ExternalLinkage, func_name, TheModule.get());
	if (main){
		 Builder.CreateCall(F, {});
		 Builder.CreateRet(c32(0));
	}
	unsigned Idx = 0;
	for (auto &Arg : F->args())
		Arg.setName(Args[Idx++]);
	vt.insert(func_name, F, refs);
	vt.openScope();
	if (fl)
		for(Formal *f: *fl) {
			f->compile();
		}

	// llvm::BasicBlock *CurrentBB = Builder.GetInsertBlock();
	// cout << (string)CurrentBB->getName() << endl;
	// if (!main & CurrentBB->empty()){ cout << "empty" << endl; CurrentBB->eraseFromParent(); }
	// if (!main){ Builder.CreateBr(EndFunc);}
	return nullptr;
}
string Header::getId() {
	return string(id);
}
bool Header::isVoid() {
	return type.p == TYPE_void;
}
Type Header::getType() {
	return type;
}
vector<Formal*>* Header::getFormals() {
	return fl;
}


Def::~Def() {}


FuncDef::FuncDef(Header* h, vector<shared_ptr<Def>>* d, vector<shared_ptr<Stmt>>* s):
	hd(h), defl(d), stmtl(s) {}
FuncDef::~FuncDef() { delete hd; delete defl; delete stmtl; }
void FuncDef::printNode(ostream &out) const {
	out << "FuncDef(" << *hd << ", ";
	bool first = true;
	for(shared_ptr<Def> d: *defl) {
		if(!first) out << ", ";
		first = false;
		out << *d;
	}
	for(shared_ptr<Stmt> s: *stmtl) {
		if(!first) out << ", ";
		first = false;
		out << *s;
	}
	out << ")";
}
void FuncDef::sem() {
	hd->sem();
	for(shared_ptr<Def> d: *defl) d->sem();
	for(shared_ptr<Stmt> s: *stmtl) s->sem();
	size = st.getSizeOfCurrentScope();
	#if PRE_DEBUG
		cout << "--- Closing scope!" << endl;
	#endif
  st.closeScope();
}
llvm::Value* FuncDef::compile() const {
	// First, check for an existing function from a previous 'extern' declaration.
	string func_name = hd->getId();
	if (func_name == "main") func_name = "jackthecutestdoggo";
	llvm::Function *ParentFunc = Builder.GetInsertBlock()->getParent();
	// llvm::BasicBlock *EndFunc =
	//   llvm::BasicBlock::Create(TheContext, "continue", ParentFunc);
	llvm::BasicBlock &ParentEntry = ParentFunc->getEntryBlock();
	llvm::BasicBlock *ParentEntry1 = &ParentEntry;
	// llvm::Function *TheFunction = TheModule->getFunction(func_name);
	// // if the function has not be declared do it!
	// if (!TheFunction){
	//   hd->compile(EndFunc);
	// 	ValueEntry *e = vt.lookup(func_name);
	// 	TheFunction = e->func;
	// }
	hd->compile();
	ValueEntry *e = vt.lookup(func_name);
	llvm::Function *TheFunction = e->func;
	// Create a new basic block to start insertion into.
	llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
	Builder.SetInsertPoint(BB);

	for (auto &Arg : TheFunction->args()){
		string argname = Arg.getName();
		ValueEntry *e = vt.lookup(argname);
		if (e) vt.insert(argname, nullptr, "glob");
		vt.insert(argname, &Arg);
	}
	for(shared_ptr<Def> d: *defl) d->compile();
	for(shared_ptr<Stmt> s: *stmtl) s->compile();
	if (Builder.GetInsertBlock()->empty()){
		// if return stmt are in Branch
		llvm::Type *func_type = TheFunction->getReturnType();
		if (func_type == i1) Builder.CreateRet(c1(0));
		else if (func_type == i8) Builder.CreateRet(c8(0));
		else if (func_type == i32) Builder.CreateRet(c32(0));
		else if (func_type->isVoidTy()) Builder.CreateRetVoid();
		else if (func_type->isArrayTy()) Builder.CreateRet(llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(func_type->getArrayElementType())));
		else if (func_type->isPointerTy()) Builder.CreateRet(llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(func_type->getPointerElementType())));
	}
	if (hd->isVoid()) Builder.CreateRetVoid();
	// Validate the generated code, checking for consistency.
	verifyFunction(*TheFunction);
	// vt.insert(func_name, TheFunction); //update
	vt.closeScope();
	Builder.SetInsertPoint(ParentEntry1);
	return nullptr;
}


FuncDecl::FuncDecl(Header* h): hd(h) {}
FuncDecl::~FuncDecl() { delete hd; }
void FuncDecl::printNode(ostream &out) const {
	out << "FuncDecl(" << *hd << ")";
}
void FuncDecl::sem() {
	hd->sem(false);
	#if PRE_DEBUG
		cout << "--- Closing scope!" << endl;
	#endif
	st.closeScope();
}
llvm::Value* FuncDecl::compile() const {
	// get details from header
	string func_name = hd->getId();
	Type type = hd->getType();
	vector<Formal*>* fl = hd->getFormals();

	// convert type to llvm Type
	llvm::Type *func_type = convertType(type);
	vector<llvm::Type*> params = {};
	if (fl)
		for(Formal *f: *fl) {
			llvm::Type *llvm_pair_type = convertType(f->getType());
			if (f->getCb()) llvm_pair_type = llvm::PointerType::getUnqual(llvm_pair_type);
			params.insert(params.end(), f->getCountofIds(), llvm_pair_type);
		}
	llvm::FunctionType *FT =
		llvm::FunctionType::get(func_type, params, false);
	llvm::Function::Create(FT, llvm::Function::ExternalLinkage, func_name, TheModule.get());
	return nullptr;
}
llvm::Type* FuncDecl::convertType(Type type) const{
	if (type.p == TYPE_int) return i32;
	else if (type.p == TYPE_bool) return i1;
	else if (type.p == TYPE_char) return i8;
	// TODO: check this not sure!
	else if (!type.p) return llvm::PointerType::getUnqual(convertType(type.c->getType()));
	return llvm::Type::getVoidTy(TheContext);
}


VarDef::VarDef(Type t, vector<const char*>* i): type(t), idl(i) {}
VarDef::~VarDef() { delete idl; }
void VarDef::printNode(ostream &out) const {
	out << "VarDef(";
	out << type << ", ";
	bool first = true;
	for(const char* i: *idl) {
		if(!first) out << ", ";
		first = false;
		out << i;
	}
	out << ")";
}
void VarDef::sem() {
	for(const char* i: *idl){
		st.insert(string(i), type);
	}
}
llvm::Value* VarDef::compile() const {
	bool isList = 0;
	llvm::Type *vtype;
	if (type.p == TYPE_int)
		vtype = i32;
	else if (type.p == TYPE_bool)
		vtype = i1;
	else if (type.p == TYPE_char)
		 vtype = i8;
	else if (type.c->getId() == "array") {
		return nullptr;
	}
	else if (type.c->getId() == "list") {
		isList = 1;
		vtype = llvm::PointerType::getUnqual(defineListType(type.c->getType()));
	}
	// else continue;
	for(const char* i: *idl){
		string name = string(i);
		llvm::AllocaInst *IdAlloc = Builder.CreateAlloca(vtype, nullptr, name);
		if (isList){
			IdAlloc->setAlignment(8);
			vt.insert(name, IdAlloc, "no", Tlist);
			// keep the size of the list in variable!
			string sname = name + "_size";
			vt.insert(sname, 0);
		}
		else {
			IdAlloc->setAlignment(4);
			vt.insert(name, IdAlloc);
		}
	}
	return nullptr;
}
llvm::Type* VarDef::defineListType(Type t) const {
	switch (t.p) {
		case TYPE_int: return i32;
		case TYPE_bool: return i1;
		case TYPE_char: return i8;
		default: break;
	}
	// else is array or list
	return llvm::PointerType::getUnqual(defineListType(t.c->getType()));
}


Library::Library() {}
void Library::init() {
	vector<Formal*>* params;
	vector<const char*>* id_list;

	Type return_type;
	Type variable_type;
	Type varh_t;

	bool built_in = true;
	
	// void puti(int n)
 	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("n");
	variable_type.p = TYPE_int;
	return_type.p = TYPE_void;

	params->push_back(new Formal(variable_type, id_list, "val"));
	st.insert("puti", return_type, "func_decl", params, built_in);

	st.openScope();
	st.insert("n", variable_type);
	st.closeScope();

	// void putb(bool b)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("b");
	variable_type.p = TYPE_bool;
	return_type.p = TYPE_void;

	params->push_back(new Formal(variable_type, id_list, "val"));
	st.insert("putb", return_type, "func_decl", params, built_in);

	st.openScope();
	st.insert("b", variable_type);
	st.closeScope();

	// void putc(char c)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("c");
	variable_type.p = TYPE_char;
	return_type.p = TYPE_void;

	params->push_back(new Formal(variable_type, id_list, "val"));
	st.insert("putc", return_type, "func_decl", params, built_in);

	st.openScope();
	st.insert("c", variable_type);
	st.closeScope();

	// void puts(char[] s)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("s");
	varh_t.p = TYPE_char; variable_type.c = new Array(varh_t);
	return_type.p = TYPE_void;

	params->push_back(new Formal(variable_type, id_list, "val"));
	st.insert("puts", return_type, "func_decl", params, built_in);

	st.openScope();
	st.insert("s", variable_type);
	st.closeScope();

	// int geti()
	return_type.p = TYPE_int;
	st.insert("geti", return_type, "func_decl", nullptr, built_in);

	st.openScope();
	st.closeScope();

	// bool getb()
	return_type.p = TYPE_bool;
	st.insert("getb", return_type, "func_decl", nullptr, built_in);

	st.openScope();
	st.closeScope();

	// char getc()
	return_type.p = TYPE_char;
	st.insert("getc", return_type, "func_decl", nullptr, built_in);

	st.openScope();
	st.closeScope();

	// void gets(int n, char[] s)	// n: max character to read from input (including /0), s: the array of chars where you should put the resault
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("n");
	variable_type.p = TYPE_int;

	params->push_back(new Formal(variable_type, id_list, "val"));

	id_list = new vector<const char*>;
	id_list->push_back("s");
	varh_t.p = TYPE_char; variable_type.c = new Array(varh_t);

	params->push_back(new Formal(variable_type, id_list, "ref")); // TODO: not sure if ref is true

	return_type.p = TYPE_void;
	st.insert("gets", return_type, "func_decl", params, built_in);

	st.openScope();
	variable_type.p = TYPE_int;
	st.insert("n", variable_type);
	varh_t.p = TYPE_char;
	variable_type.c = new Array(varh_t);
	st.insert("s", variable_type);
	st.closeScope();

	// int abs(int n)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("n");
	variable_type.p = TYPE_int;

	params->push_back(new Formal(variable_type, id_list, "val"));
	return_type.p = TYPE_int;
	st.insert("abs", return_type, "func_decl", params, built_in);

	st.openScope();
	st.insert("n", variable_type);
	st.closeScope();

	// int ord(char c)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("c");
	variable_type.p = TYPE_char;

	params->push_back(new Formal(variable_type, id_list, "val"));
	return_type.p = TYPE_int;
	st.insert("ord", return_type, "func_decl", params, built_in);

	st.openScope();
	st.insert("c", variable_type);
	st.closeScope();

	// char chr(int n)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("n");
	variable_type.p = TYPE_int;

	params->push_back(new Formal(variable_type, id_list, "val"));
	return_type.p = TYPE_char;
	st.insert("chr", return_type, "func_decl", params, built_in);

	st.openScope();
	st.insert("n", variable_type);
	st.closeScope();

	// int strlen(char[] s)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("s");
	varh_t.p = TYPE_char; variable_type.c = new Array(varh_t);

	params->push_back(new Formal(variable_type, id_list, "val"));
	return_type.p = TYPE_int;
	st.insert("strlen", return_type, "func_decl", params, built_in);

	st.openScope();
	st.insert("s", variable_type);
	st.closeScope();

	// int strcmp(char[] s1, s2)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("s1");
	id_list->push_back("s2");
	varh_t.p = TYPE_char; variable_type.c = new Array(varh_t);

	params->push_back(new Formal(variable_type, id_list, "val"));
	return_type.p = TYPE_int;
	st.insert("strcmp", return_type, "func_decl", params, built_in);

	st.openScope();
	st.insert("s1", variable_type);
	st.insert("s2", variable_type);
	st.closeScope();

	// void strcpy(char[] trg, src)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("trg");
	id_list->push_back("src");
	varh_t.p = TYPE_char; variable_type.c = new Array(varh_t);

	params->push_back(new Formal(variable_type, id_list, "ref")); // TODO: not sure about ref
	return_type.p = TYPE_void;
	st.insert("strcpy", return_type, "func_decl", params, built_in);

	st.openScope();
	st.insert("trg", variable_type);
	st.insert("src", variable_type);
	st.closeScope();

	// void strcat(char[] trg, src)
	params = new vector<Formal*>;
	id_list = new vector<const char*>;
	id_list->push_back("trg");
	id_list->push_back("src");
	varh_t.p = TYPE_char; variable_type.c = new Array(varh_t);

	params->push_back(new Formal(variable_type, id_list, "ref")); // TODO: not sure about ref
	return_type.p = TYPE_void;
	st.insert("strcat", return_type, "func_decl", params, built_in);

	st.openScope();
	st.insert("trg", variable_type);
	st.insert("src", variable_type);
	st.closeScope();
}
