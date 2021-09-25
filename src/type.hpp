#ifndef __TYPE_HPP__
#define __TYPE_HPP__

#include "ast.hpp"

enum PrimitiveType { TYPE_int, TYPE_bool, TYPE_char, TYPE_nil, TYPE_void};
enum generalType { Tint, Tbool, Tchar, Tarray, Tlist, Tnull};

class CompositeType;

union Type {
	PrimitiveType p;
	CompositeType* c;
};

#endif
