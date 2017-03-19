%{
  open Expr
%}

%token EOF

%token CASE OF
%token PIPE ARROW

%token <string> VAR
%token <string> STRING
%token <int> INT

%token <string> BIN_MULT
%token <string> BIN_PLUS
%token <string> BIN_CMP
%token NOT
%token AND
%token OR

%token LET EQUALS IN

%token EQUALS
%token ANY

%token LPAR RPAR

%left OR
%left AND
%left NOT
%left BIN_CMP
%left EQUALS
%left BIN_PLUS
%left BIN_MULT

%nonassoc CASE OF /* checker */
%nonassoc PIPE ARROW /* checker */
%nonassoc LET EQUALS IN /* checker */

/* Les non-terminaux par lesquels l'analyse peut commencer,
 * et la donnée de leurs types. */

%start terminated_expr
%type <Expr.t> terminated_expr

%%

terminated_expr:
  | expr EOF { $1 }

pmatch :
  | prule                          { [$1] }
  | pmatch PIPE prule              { $3::$1 }

prule :
  | pat ARROW expr                { ($1, $3) }

pat :
  | STRING                         { String $1 }
  | INT                            { Int $1 }
  | ANY                            { Any }

expr:
  | CASE expr OF pmatch            { Case ($2, List.rev $4) }
  | INT                            { Int $1 }
  | VAR                            { Var $1 }
  | STRING                         { String $1 }
  | LPAR expr RPAR                 { $2 }
  | LET VAR EQUALS expr IN expr    { Let ($2,$4,$6) }
  | ANY                            { Var "_" }
  | expr EQUALS expr               { App ("=", [$1;$3])}
  | expr BIN_PLUS expr             { App ($2,[$1;$3]) }
  | expr BIN_MULT expr             { App ($2,[$1;$3]) }
  | expr BIN_CMP expr              { App ($2,[$1;$3]) }
  | expr AND expr                  { App ("and",[$1;$3]) }
  | expr OR expr                   { App ("or",[$1;$3]) }
  | NOT expr                       { App ("not",[$2]) }

