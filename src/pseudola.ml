open Pstype;;

let rec string_of_expr=function
   VAR(x)          -> "VAR(\""^x^"\")"
  |APP(m,n)        -> "APP("^(string_of_expr m)^","^(string_of_expr n)^")"
  |FUN(x,m)        -> "FUN(\""^x^"\","^(string_of_expr m)^")"
  |LET(x,m,n)      -> "LET(\""^x^"\","^(string_of_expr m)^","^(string_of_expr n)^")"
  |REF(m)          -> "REF("^(string_of_expr m)^")"
  |BANG(m)         -> "BANG("^(string_of_expr m)^")"
  |SET(m,n)        -> "SET("^(string_of_expr m)^","^(string_of_expr n)^")"
  |INT(a)          -> "INT("^(string_of_int a)^")"
  |PLUS(m,n)       -> "PLUS("^(string_of_expr m)^","^(string_of_expr n)^")"
  |MINUS(m,n)      -> "MINUS("^(string_of_expr m)^","^(string_of_expr n)^")"
  |TIMES(m,n)      -> "TIMES("^(string_of_expr m)^","^(string_of_expr n)^")"
  |DIV(m,n)        -> "DIV("^(string_of_expr m)^","^(string_of_expr n)^")"
  |REM(m,n)        -> "REM("^(string_of_expr m)^","^(string_of_expr n)^")"
  |OPP(m)          -> "OPP("^(string_of_expr m)^")"
  |LESS(m,n)       -> "LESS("^(string_of_expr m)^","^(string_of_expr n)^")"
  |EQUAL(m,n)      -> "EQUAL("^(string_of_expr m)^","^(string_of_expr n)^")"
  |PROJ(m,n)       -> "PROJ("^(string_of_expr m)^","^(string_of_expr n)^")"
  |SEQ(m,n)        -> "SEQ("^(string_of_expr m)^","^(string_of_expr n)^")"
  |IF(m,n,p)       -> "IF("^(string_of_expr m)^","^(string_of_expr n)^","^(string_of_expr p)^")"
  |CALLPRIM(nom,l) -> "CALLPRIM(\""^nom^"\",["^(stringliste l)^"])"
  |TUPLE(l)        -> "TUPLE(["^(stringliste l)^"])"
and stringliste=function
  |(h::t) -> if (t==[]) then (string_of_expr h) else ((string_of_expr h)^";"^(stringliste t))
  |[]     -> ""
;;

let string_of_letrec=function
    LETREC(nom,exp)->"LETREC("^nom^","^string_of_expr(exp)^")";;

let rec string_of_prog = begin 
  let string_of_tail h t =
    if t<>[] then 
      ((string_of_letrec h)^";"^(string_of_prog t)) 
    else (string_of_letrec h)^")"
  in function
    |[]     -> ")"
    |(h::t) ->string_of_tail h t
end ;;

let rec inverse=function 
  |[]->[]
  |(h::t)->(inverse t)@[h];;


let temp=ref [];;
let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
      while true do
	let result = Parser.letrec Lexer.token lexbuf in
	  temp:=(result::!temp);
	 
      done
  with Lexer.Eof-> print_string("("^(string_of_prog (inverse(!temp))));print_newline(); flush stdout;exit 0
 (* ICI appeller mon programme, pour compiler le fichier *)


     

