{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof
}

rule token = parse
  | [' ' '\t']             {token lexbuf}
  | ['\n']                 {token lexbuf}
  | "tuple"                 {TUPLE}
  | "fun"                   {FUN} 
  | "call"                  {CALL}
  | "if"                   {IF}
  | "then"                 {THEN}
  | "else"                 {ELSE}
  | "ref"                  {REF}
  | "let"                  {LET}
  | "in"                   {IN} 
  | "letrec"                {LETREC}
  | '.'                       {POINT}
  | "->"                   {TO}
  | '+'                    {PLUS} 
  | '['                    {LCR}
  | ']'                     {RCR}
  | ['0'-'9']+            {INT(int_of_string(Lexing.lexeme lexbuf))} 
  | ['a'-'z' 'A'-'Z' '_']+            {VAR(Lexing.lexeme lexbuf)}
  | ";;"                    {EOE}
  | '('                    {LPAR}
  | ')'                    {RPAR}
  | ';'                    {SEMI}
  | '<'                    {LESS}
  | '-'                    {MINUS}
  | ','                    {COMA}
  | '*'                    {TIMES}
  | '/'                    {DIV}
  | '%'                    {REM}
  | '!'                    {BANG}
  | ":="                   {SET}
  | '='                    {EQUAL}
  | eof                     {raise Eof}

