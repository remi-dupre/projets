#include "naif.h"

bool next_step(vector<freevar*> &config) {
	unsigned i;
	for(i=0 ; i < config.size() && config[i]->value == T ; i++)
		config[i]->value = F;

	if(i < config.size()) {
		config[i]->value = T;
		return true;
	}
	else
		return false;
}

bool SAT(Prop* phi) {
	vector<freevar*> variables = phi->get_vars();

	for(freevar *var : variables)
		var->value = F;

	do {
		if(phi->eval() == T)
			return true;
	} while(next_step(variables));
	return false;
}
