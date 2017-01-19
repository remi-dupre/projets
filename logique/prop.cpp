#include "prop.h"

/* ********** Constants ********** */

Constant::Constant(tril val) : m_val(val) {}

string Constant::to_string() const {
	return tril_repr[m_val];
}

tril Constant::eval() const {
	return m_val;
}

vector<freevar*> Constant::get_vars() const {
	return vector<freevar*>();
}

/* ********** Variable ********** */

Variable::Variable(freevar *var) : m_var(var) {}

string Variable::to_string() const {
	return m_var->name;
}

tril Variable::eval() const {
	return m_var->value;
}

vector<freevar*> Variable::get_vars() const {
	return vector<freevar*>({m_var});
}

/* ********** Negation ********** */

Neg::Neg(Prop *expr) : m_expr(expr) {}

string Neg::to_string() const {
	return "-" + m_expr->to_string();
}

tril Neg::eval() const {
	switch(m_expr->eval()) {
		case T :
			return F;
			break;
		case F :
			return T;
			break;
		case U :
			return U;
			break;
	}
}

vector<freevar*> Neg::get_vars() const {
	return m_expr->get_vars();
}

/* ********** And ********** */

And::And(Prop *e1, Prop *e2) : m_expr(vector<Prop*>({e1, e2})) {}

And::And(const vector<Prop*> &expr) : m_expr(expr) {
	m_expr.shrink_to_fit();
}

string And::to_string() const {
	string ret("(");
	for(Prop *e : m_expr)
		ret += e->to_string() + "^";
	ret[ret.length()-1] = ')';
	return ret;
}

tril And::eval() const {
	tril val = T;
	for(Prop *e : m_expr) {
		switch(e->eval()) {
			case F :
				return F;
				break;
			case U :
				val = U;
				break;
		}
	}
	return val;
}

vector<freevar*> And::get_vars() const {
	vector<freevar*> ret;
	for(Prop *e : m_expr) {
		vector<freevar*> evars = e->get_vars();
		for(freevar *var : evars)
			if( find(begin(ret), end(ret), var) == end(ret) )
				ret.push_back(var);
	}
	ret.shrink_to_fit();
	return ret;
}

/* ********** Or ********** */

Or::Or(Prop *e1, Prop *e2) : m_expr(vector<Prop*>({e1, e2})) {}

Or::Or(const vector<Prop*> &expr) : m_expr(expr) {
	m_expr.shrink_to_fit();
}

string Or::to_string() const {
	string ret("(");
	for(Prop *e : m_expr)
		ret += e->to_string() + "+";
	ret[ret.length()-1] = ')';
	return ret;
}

tril Or::eval() const {
	tril val = F;
	for(Prop *e : m_expr) {
		switch(e->eval()) {
			case T :
				return T;
				break;
			case U :
				val = U;
				break;
		}
	}
	return val;
}

vector<freevar*> Or::get_vars() const {
	vector<freevar*> ret;
	for(Prop *e : m_expr) {
		vector<freevar*> evars = e->get_vars();
		for(freevar *var : evars)
			if( find(begin(ret), end(ret), var) == end(ret) )
				ret.push_back(var);
	}
	ret.shrink_to_fit();
	return ret;
}
