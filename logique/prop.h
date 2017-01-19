#include <string>
#include <vector>
#include <algorithm>

using namespace std;

#ifndef prop_h
#define prop_h

enum tril {F, T, U}; // false, true, unknown
#define tril_repr vector<string>({"F", "T", "U"})

typedef struct freevar {
	string name;
	tril value;
} freevar;


#define Non(A) new Neg(A)
#define Var(A) new Variable(A)
#define Et(A, B) new And(A, B)
#define Ou(A, B) Non(Et(Non(A), Non(B)))
#define Impl(A, B) Non(Et(A, Non(B)))

/* ********** Proposition ********** */

class Prop {
	public :
		virtual tril eval() const = 0;
		virtual string to_string() const = 0;
		virtual vector<freevar*> get_vars() const = 0;
};

/* ********** Constants ********** */

class Constant : public Prop {
	public :
		Constant(tril);
		
		string to_string() const;
		tril eval() const;
		vector<freevar*> get_vars() const;

	private :
		tril m_val;
};

/* ********** Variable ********** */

class Variable : public Prop {
	public :
		Variable(freevar*);

		string to_string() const;
		tril eval() const;
		vector<freevar*> get_vars() const;

	private :
		freevar *m_var;
};

/* ********** Negation ********** */

class Neg : public Prop {
	public :
		Neg(Prop*);

		string to_string() const;
		tril eval() const;
		vector<freevar*> get_vars() const;

	private :
		Prop *m_expr;
};

/* ********** And ********** */

class And : public Prop {
	public :
		And(Prop*, Prop*);
		And(const vector<Prop*>&);

		string to_string() const;
		tril eval() const;
		vector<freevar*> get_vars() const;

	private :
		vector<Prop*> m_expr;
};

/* ********** And ********** */

class Or : public Prop {
	public :
		Or(Prop*, Prop*);
		Or(const vector<Prop*>&);

		string to_string() const;
		tril eval() const;
		vector<freevar*> get_vars() const;

	private :
		vector<Prop*> m_expr;
};

#endif
