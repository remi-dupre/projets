
{
  open Parse   (* ./parse.mly *)
  open Lexing  (* librairie standard *)
}

let varname = (['a'-'z']|'_') (['a'-'z']|['A'-'Z']|['0'-'9']|'_'|''')*
let intdef =
    (['0'-'9'] ['0'-'9']*)
    | ( ("0X"|"0x") (['0'-'9']|['A'-'F']|['a'-'f']) ((['0'-'9']|['A'-'F']|['a'-'f']|'_')*) )
let stringdef = '"' (_*) '"'
let opcomp = ('='|'<'|'>'|'^') ('='|'<'|'>'|'^'|'+'|'-'|'*'|'/'|'%'|'!'|'$'|'?'|'.'|':'|';')*
let opadd = ('+'|'-') ('='|'<'|'>'|'^'|'+'|'-'|'*'|'/'|'%'|'!'|'$'|'?'|'.'|':'|';')*
let opsub = ('*'|'/'|'%') ('='|'<'|'>'|'^'|'+'|'-'|'*'|'/'|'%'|'!'|'$'|'?'|'.'|':'|';')*

rule token = parse
  | "and"               { AND }
  | "or"                { OR }
  | "not"               { NOT }

  | "let"               { LET }
  | "="                 { EQUALS }
  | "in"                { IN }

  | "case"              { CASE }
  | "of"                { OF }
  | "=>"                { ARROW }
  | "|"                 { PIPE }
  | "_"                 { ANY }

  | "-"                 { MINUS }

  | varname as v        { VAR v }

  | intdef as int_s     { INT (int_of_string int_s) }
  | stringdef as s      { STRING (String.sub s 1 (String.length s - 2)) }

  | "("                 { LPAR }
  | ")"                 { RPAR }

  | "(*"                { LCOM }
  | "*)"                { RCOM }

  | opcomp as s         { BIN_CMP s }
  | opadd as s          { BIN_PLUS s }
  | opsub as s          { BIN_MULT s }


  | [' ' '\t' '\r' '\n']+   { token lexbuf }

  | eof                { EOF }
