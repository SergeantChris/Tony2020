#ifndef __TYPE_HPP__
#define __TYPE_HPP__

#include "ast.hpp"

enum PrimitiveType { TYPE_int, TYPE_bool, TYPE_char, TYPE_nil, TYPE_void};

class CompositeType;

union Type {
	PrimitiveType p;
	CompositeType* c;
	//~Type() {}
	//i dont think ill ever put a primtype in a comptype variable
	//if i want to keep the destructor i'll have to make type a pointer
	//in bison union
};

#endif