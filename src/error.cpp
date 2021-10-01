#include <iostream>
#include <string.h>
#include "error.hpp"

using namespace std;


// check what happens when we have multiple error...maybe print them all and the with a flag just dont run the program
void error(const string formatted) {
    cout << endl << BOLD("line ") << linecount << ": ";
    cout << BOLD(FRED("error: ")) << formatted << endl;
    exit(1);
}
