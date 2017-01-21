#include "dpll.h"

bool dpll(Prop* phi) {
	if(!phi->is_cnf())
		throw "DPLL must be applied to a CNF formula\n";

	vector<freevar*> variables = phi->get_vars();
	for(freevar *var : variables)
		var->value = U;

	dpll_step((And*) phi);
}

bool dpll_step(And* phi) {
	// Checks step 1 : empty clause
	if(phi->eval() == F)
		return false;

	// Checks step 2 : clauses with 1 litteral
	for(Or* clause : phi->get_clauses()) {
		vector<Prop*> litterals = phi->get_vars();
	}
}
