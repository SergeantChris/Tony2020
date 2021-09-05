#!/bin/sh

libname="tonylib.a"

if [ "$1" != "" ]; then
    echo "Compiling $1"
    /src/tony  $1 > output.ll
    if [ $? -eq 0 ]; then
      echo OK
    else
      echo FAIL
      /src/tony < $1
      exit 1
    fi
    llc output.ll -o output.s
    clang -o a.out output.s "$libname"
    echo "Executing output"
    ./a.out
else
  echo "No Input"
fi
