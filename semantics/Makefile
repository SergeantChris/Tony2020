.PHONY: clean distclean default

CXX=c++
CXXFLAGS=-Wall -std=c++11

default: parser

lexer.cpp: lexer.l
	flex -s -o lexer.cpp lexer.l

#lexer.o: lexer.cpp lexer.hpp parser.hpp ast.hpp

parser.hpp parser.cpp: parser.y
	bison -dv -o parser.cpp parser.y

#parser.o: parser.cpp lexer.hpp ast.hpp

#parser: lexer.o parser.o
	#$(CXX) (CXXFLAGS) -o parser lexer.o parser.o

parser: parser.cpp
	$(CXX) $(CXXFLAGS) -o parser parser.cpp

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o

distclean: clean
	$(RM) parser
