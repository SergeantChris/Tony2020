#ifndef __TYPE_HPP__
#define __TYPE_HPP__

#include "ast.hpp"

enum PrimitiveType { TYPE_int, TYPE_bool, TYPE_char, TYPE_nil, TYPE_void};
enum generalType { Tint, Tbool, Tchar, Tarray, Tlist, Tnull};

class CompositeType;

union Type {
	PrimitiveType p;
	CompositeType* c;
	// memory leak: 
	// whenever some_type.c = new Sth() goes out of scope ...
	// destructor that deletes c leads to segfault because of
	// https://stackoverflow.com/questions/32663008/using-delete-on-any-member-in-a-union-of-pointers
};

#endif
