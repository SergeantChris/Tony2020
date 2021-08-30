#!/bin/sh

libname="tony.lib"

if [ "$1" != "" ]; then
    echo "Compiling $1"
    /src/tony < $1 > output.ll
    if [ $? -eq 0 ]; then
      echo OK
    else
      echo FAIL
      /src/tony < $1
      exit 1
    fi
    llc output.ll -o output.s
    clang output.s "$libname" -o output.out
    echo "Executing output"
    ./output.out
else
  echo "No Input"
fi
