#include "ast.hpp"

llvm::LLVMContext ASTnode::TheContext;
llvm::IRBuilder<> ASTnode::Builder(TheContext);
std::unique_ptr<llvm::Module> ASTnode::TheModule;
std::unique_ptr<llvm::legacy::FunctionPassManager> ASTnode::TheFPM;


// GlobalVariable *ASTnode::TheVars;
// GlobalVariable *ASTnode::TheNL;
//
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
