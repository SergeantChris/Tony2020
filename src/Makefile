.PHONY: clean distclean default

CXX=c++
CXXFLAGS=-Wall -std=c++11

default: tony

lexer.cpp: lexer.l
	flex -s -o lexer.cpp lexer.l

error.o: error.cpp error.hpp

lexer.o: lexer.cpp lexer.hpp parser.hpp ast.hpp symbol.hpp

parser.hpp parser.cpp: parser.y
	bison -dv -o parser.cpp parser.y

parser.o: parser.cpp lexer.hpp ast.hpp symbol.hpp

tony: lexer.o parser.o error.o
	$(CXX) $(CXXFLAGS) -o tony $^

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o

distclean: clean
	$(RM) tony
