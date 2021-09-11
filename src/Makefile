.PHONY: clean distclean default

LLVMCONFIG=llvm-config-9

CXX=c++
CXXFLAGS=-Wall -std=c++11 -g `$(LLVMCONFIG) --cxxflags`
LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all`

default: tony

lexer.cpp: lexer.l
	flex -s -o lexer.cpp lexer.l

error.o: error.cpp error.hpp

lexer.o: lexer.cpp lexer.hpp parser.hpp ast.hpp symbol.hpp

parser.hpp parser.cpp: parser.y
	bison -dv -o parser.cpp parser.y

parser.o: parser.cpp lexer.hpp ast.hpp symbol.hpp

tony: lexer.o parser.o error.o ast.o
	$(CXX) $(CXXFLAGS) -o tony $^ $(LDFLAGS)

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o

distclean: clean
	$(RM) tony a.ll a.s a.out
