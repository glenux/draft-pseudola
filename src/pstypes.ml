
type expr=
    VAR of string
  | APP of expr * expr
  | FUN of string * expr
  | LET of string * expr * expr
  | REF of expr
  | BANG of expr
  | SET of expr * expr
  | INT of int
  | PLUS of expr * expr
  | MINUS of expr * expr
  | TIMES of expr * expr
  | DIV of expr * expr
  | REM of expr * expr
  | OPP of expr
  | LESS of expr * expr
  | EQUAL of expr * expr
  | TUPLE of expr list
  | PROJ of expr * expr
  | SEQ of expr * expr
  | IF of expr * expr * expr
  | CALLPRIM of string * expr list;;
type letrec = LETREC of string * expr;;

type prog = letrec list;;

