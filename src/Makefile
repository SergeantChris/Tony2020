.PHONY: clean distclean default

LLVMCONFIG=llvm-config-9

CXX=c++
CXXFLAGS=-Wall -std=c++11 -g `$(LLVMCONFIG) --cxxflags`
LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all`

default: clean tony

lexer.cpp: lexer.l
	flex -s -o lexer.cpp lexer.l

parser.hpp parser.cpp: parser.y
	bison -dv -o parser.cpp parser.y

CFILES = lexer.cpp parser.cpp error.cpp ast.cpp symbol.cpp

tony: lexer.cpp parser.cpp
	$(CXX) $(CXXFLAGS) -o tony $(CFILES) $(LDFLAGS)

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o *.gch *.out *.ll *.s

distclean: clean
	$(RM) tony
