#include "ast.hpp"

llvm::LLVMContext ASTnode::TheContext;
llvm::IRBuilder<> ASTnode::Builder(TheContext);
std::unique_ptr<llvm::Module> ASTnode::TheModule;
std::unique_ptr<llvm::legacy::FunctionPassManager> ASTnode::TheFPM;


// GlobalVariable *ASTnode::TheVars;
// GlobalVariable *ASTnode::TheNL;
//
// Function *ASTnode::TheWriteInteger;
// Function *ASTnode::TheWriteString;

llvm::Type *ASTnode::i1;
llvm::Type *ASTnode::i8;
llvm::Type *ASTnode::i32;
llvm::Type *ASTnode::i64;
