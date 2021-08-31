.PHONY: clean distclean default

CXX=c++
CXXFLAGS=-std=c++11 -Wall 

default: tony

lexer.cpp: lexer.l
	flex -s -o lexer.cpp lexer.l

#error.o: error.cpp error.hpp

#ast.o: ast.cpp ast.hpp symbol.hpp

#symbols.o: symbols.cpp symbols.hpp

#lexer.o: lexer.cpp lexer.hpp parser.hpp ast.hpp symbol.hpp

parser.hpp parser.cpp: parser.y
	bison -dv -o parser.cpp parser.y

#parser.o: parser.cpp lexer.hpp ast.hpp symbol.hpp

#tony: lexer.o parser.o error.o ast.o #symbols.o #$(CXX) $(CXXFLAGS) -o tony $^

CFILES = lexer.cpp parser.cpp error.cpp ast.cpp symbol.cpp

tony: lexer.cpp parser.cpp
	$(CXX) $(CXXFLAGS) -o tony $(CFILES)

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o *.gch

distclean: clean
	$(RM) tony
