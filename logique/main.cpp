#include <iostream>
#include <string>

#include "naif.h"
#include "input.h"

using namespace std;

#include "prop.h"

#define vardef(A, S) freevar A; A.name = S; A.value = U;

int main() {
	Prop *gamma = read_dimacs();
	cout << gamma->to_string() << endl;
	cout << (SAT(gamma) ? "satisfiable" : "non satisfiable") << endl;
}
