#!/bin/sh

_RED_='\033[1;31m'
_GREEN_='\033[1;32m'
_YELLOW_='\033[1;33m'
_BLUE_='\033[1;34m'
_PURPLE_='\033[1;35m'
_CYAN_='\033[1;36m'
_NC_='\033[0m' # No Color

Optimize=0
FinalCodeOut=0
InterCodeOut=0

libname="tonylib.a"

if [ "$1" != "" ]; then
		for i in "$@"; do
				if [ "$i" = "-O" ]; then
					Optimize=1
				elif [ "$i" = "-f" ]; then
					FinalCodeOut=1
				elif [ "$i" = "-i" ]; then
					InterCodeOut=1
				fi
		done
		echo "\n${_PURPLE_}-------------------- COMPILING -------------------${_NC_}"

    echo -n  "$1 ...  "
		if [ ${Optimize} = 1 ]; then
			./tony -O $1 > output.ll
		else
			./tony $1 > output.ll
		fi
    if [ $? -eq 0 ]; then
      echo "${_GREEN_}OK${_NC_}"
    else
      echo "${_RED_}FAIL${_NC_}"
      ./tony $1
      exit 1
    fi

    llc output.ll -o output.s
    clang -o a.out output.s "$libname"
		echo "${_PURPLE_}---------------------------------------------------${_NC_}"

		if [ ${InterCodeOut} = 1 ]; then
			echo "\n${_YELLOW_}---------------- INTERMEDIATE CODE ----------------${_NC_}"
			cat output.ll
			echo "${_YELLOW_}---------------------------------------------------${_NC_}"
		fi
		if [ ${FinalCodeOut} = 1 ]; then
			echo "\n${_CYAN_}-------------------- FINAL CODE -------------------${_NC_}"
			cat output.s
			echo "${_CYAN_}---------------------------------------------------${_NC_}"
		fi
		echo "\n${_PURPLE_}----------------- EXECUTING OUTPUT ----------------${_NC_}"
    ./a.out
		echo "\n${_PURPLE_}---------------------------------------------------${_NC_}"

else
  echo "${_RED_}No Input${_NC_}"
fi
