#include <iostream>
#include <string>

#include "naif.h"

using namespace std;

#include "prop.h"

#define vardef(A, S) freevar A; A.name = S; A.value = U;

int main() {
	vardef(a, "a")
	vardef(b, "b")

	a.value = T;
	b.value = F;

	Prop *phi = Impl(new And({new Constant(U), Var(&a), new Constant(F)}), Var(&b));
	cout << phi->to_string() << " : " << phi->eval() << endl;

	cout << phi->get_vars().size() << " variables"  << endl;
	cout << SAT(phi) << endl;

	Prop *psi = (new Or(vector<Prop*>({Non(Var(&a)), Var(&a)})));
	cout << SAT(psi) << endl;
}
