#!/bin/bash

./tony $1 > a.ll
llc-9 a.ll -o a.s
clang -o a.out a.s
