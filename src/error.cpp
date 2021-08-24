#include <iostream>
#include "error.hpp"


 // requires at least C++11
void error(const std::string sFormat, ...) {    // check what happens when we have multiple error...maybe print them all and the with a flag just dont run the program

    const char * const zcFormat = sFormat.c_str();

    // initialize use of the variable argument array
    va_list vaArgs;
    va_start(vaArgs, sFormat);

    // reliably acquire the size from a copy of
    // the variable argument array
    // and a functionally reliable call
    // to mock the formatting
    va_list vaCopy;
    va_copy(vaCopy, vaArgs);
    const int iLen = std::vsnprintf(NULL, 0, zcFormat, vaCopy);
    va_end(vaCopy);

    // return a formatted string without
    // risking memory mismanagement
    // and without assuming any compiler
    // or platform specific behavior

    std::vector<char> zc(iLen + 1);
    std::vsnprintf(zc.data(), zc.size(), zcFormat, vaArgs);
    va_end(vaArgs);
    std::cout << std::endl << BOLD("line:") << linecount << ": ";
    std::cout << BOLD(FRED("error: "));
    std::cout << std::string(zc.data(), zc.size()) << std::endl;
    exit(1);
}
