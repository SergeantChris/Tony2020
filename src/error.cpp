#include <iostream>
#include <string.h>
#include "error.hpp"

using namespace std;


void error(const string formatted, int line_no) {
    cout << endl << BOLD("line ") << line_no << ": ";
    cout << BOLD(FRED("error: ")) << formatted << endl;
    exit(1);
}
