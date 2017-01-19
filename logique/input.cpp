#include "input.h"

Prop* read_dimacs() {
	char linetype = 0;
	string type, comment;

	do {
		cin >> linetype;
		if(linetype == 'c')
			getline(cin, comment);
	} while(linetype == 'c');

	if(linetype == 'p') {
		cin >> type;
		if(type == "cnf") {
			int nb_vars, nb_clauses;
			cin >> nb_vars >> nb_clauses;

			vector<freevar*> vars(nb_vars);
			for(int i=0 ; i < nb_vars ; i++) {
				vars[i] = new freevar();
				vars[i]->value = U;
				vars[i]->name = string("P") + to_string(i);
			}

			vector<Prop*> clauses(nb_clauses);
			for(int i=0 ; i < nb_clauses ; i++) {
				int lit;
				vector<Prop*> clause_i;

				cin >> lit;
				while(lit != 0) {
					if(lit > 0)
						clause_i.push_back(new Variable(vars[lit-1]));
					else
						clause_i.push_back(new Neg(new Variable(vars[-1-lit])));
					cin >> lit;
				}
				clauses[i] = new Or(clause_i);
			}
			return new And(clauses);
		}
		else
			cout << "(err) unknown type " << type << endl;
	}
}
