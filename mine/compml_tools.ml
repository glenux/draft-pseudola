(*  Projet de Programmation                                                 *)
(*  1er semestre - 1ere année de Magistère STIC - 2002                      *)
(*  Cours de Jean Goubault-Larrecq                                          *)
(* ========================== Fonctions Utiles ============================ *)


open Compml_header;;

let str_count = ref 0;;
let var_count = ref nb_saved_reg;; (* nb de registres sauvegardes *)
let label_count = ref 0;;
let cmp_stack = ref [];;

let gen_str () =
  str_count := !str_count + 1;
  ".string"^(string_of_int (!str_count));;

let gen_label () =
  label_count := !label_count + 1;
  "lbl"^(string_of_int (!label_count));;

let gen_var () =
  var_count := !var_count + 1;
  (string_of_int (!var_count * -4)) ^ "(%ebp)";;
 
let rec supprime_var (str:string) varlist =
  match varlist with
      [] -> []
    | var::sub_varlist -> (if (var = str) then (supprime_var str sub_varlist)
			   else var::(supprime_var str sub_varlist));;

let rec var_libre  (gxpr : expr) =
  match gxpr with 
      VAR(str) -> ([str]) 
    | APP(xpr1,xpr2) -> (var_libre xpr1)@(var_libre xpr2)
    | FUN(str,xpr) -> (supprime_var str (var_libre xpr))
    | LET(str,xpr1,xpr2) -> (var_libre xpr1)@(supprime_var str (var_libre xpr2))
    | REF(xpr) -> (var_libre xpr)
    | BANG(xpr) -> (var_libre xpr)
    | SET(xpr1,xpr2) -> (var_libre xpr1)@(var_libre xpr2)
    | INT(i) -> ([])
    | PLUS(xpr1,xpr2) -> (var_libre xpr1)@(var_libre xpr2)
    | MINUS(xpr1,xpr2) -> (var_libre xpr1)@(var_libre xpr2)
    | TIMES(xpr1,xpr2) -> (var_libre xpr1)@(var_libre xpr2)
    | DIV(xpr1,xpr2) -> (var_libre xpr1)@(var_libre xpr2)
    | REM(xpr1,xpr2) -> (var_libre xpr1)@(var_libre xpr2)
    | OPP(xpr) -> (var_libre xpr)
    | LESS(xpr1,xpr2) -> (var_libre xpr1)@(var_libre xpr2)
    | EQUAL(xpr1,xpr2) -> (var_libre xpr1)@(var_libre xpr2)
    | TUPLE(xprlst) -> ( match xprlst with
			     [] -> []
			   | xpr::sub_xprlst -> (var_libre xpr)@(var_libre (TUPLE(sub_xprlst)))) 
    | PROJ(xpr1,xpr2) -> (var_libre xpr1)@(var_libre xpr2)
    | SEQ(xpr1,xpr2) -> (var_libre xpr1)@(var_libre xpr2)
    | IF(xpr1,xpr2,xpr3) -> (((var_libre xpr1)@(var_libre xpr2))@(var_libre xpr3))
    | CALLPRIM(str,xprlst) -> (var_libre (TUPLE(xprlst))) (* union des variables *)
    | STRING(str) -> ([])
    | NOP -> ([]);;

(* let  fct qui inverse une liste *)
(* fonction qui envoie le dernier elem d'une liste *)

let rec lst_inverse lst = 
  match lst with
      [] -> [] ;
    | elem::sub_lst -> ((lst_inverse sub_lst)@[elem]);;

let lst_tailelem lst = 
  let inv_lst = (lst_inverse lst) in ( match inv_lst with
					   [] -> raise Empty_List;
					 | elem:: _ -> elem);;

let rec lst_hdlst lst = 
  let inv_lst = (lst_inverse lst) in (
      match inv_lst with 
	  [] -> [];
	| elem::sub_lst -> (lst_inverse sub_lst));;


let rec lst_nieme lst (n:int) = ( (* nieme en partant de la fin *)
  if (n> 0) then ( match n with
		       1 -> (lst_tailelem lst)
		     | _ -> (lst_nieme (lst_hdlst lst) (n - 1))
		 ) 
  else  ( raise Out_of_Range )
);;


let rec lst_count lst = (
  match lst with 
      [] -> 0
    | elem::sublist -> 1 + (lst_count sublist)
);;

let cmp_stck_add (cmpt:cmptyp) = 
  cmp_stack:= cmpt :: (!cmp_stack);;

let cmp_stck_extract () =
  match !cmp_stack with
      a::sub_stack -> ( cmp_stack := sub_stack;
			a )
    | [] -> raise Empty_Cmp_Stack;;

let error_fnc str = 
  begin
    print_string ("Error : "^str^"\n");
    exit;
  end;;


let set_inputfile conf str = conf := {!conf with inputfile = str };;
let set_outputfile conf str = conf := {!conf with outputfile = str };;
let set_optlevel conf n = conf := {!conf with optlevel = n };;
