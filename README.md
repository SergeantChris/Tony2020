# Tony2020
A compiler for the programming language Tony (http://courses.softlab.ntua.gr/compilers/2020a/tony2020.pdf), written in C++, for the purposes of the ECE NTUA Compilers course.

# How to use this compiler
* Navigate to `/src` directory
* Run `make`
* Run `./do.sh [name].tony` with the name of your tony source code
* The available compiler flags are:
	* `-O`: optimize intermediate code (LLVM)
	* `-i`: print intermediate code in `stdout`
	* `-f`: print final code (assembly) in `stdout`
* The program will be compiled (if flags where selected, the respective output will be printed in `stdout`) and the final code will be executed.

# !! Known issues !!
* Arrays and Lists as function arguments
* Lists as function return type
