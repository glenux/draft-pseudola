(*  Projet de Programmation                                                 *)
(*  1er semestre - 1ere année de Magistère STIC - 2002                      *)
(*  Cours de Jean Goubault-Larrecq                                          *)
(* ====================== Gestion de l'environnement ====================== *)

open Compml_header;;

let rec cherche_code str env =
  (* cherche la valeur (code asm) pour la variable nomée str *)
  match env with 
      [] -> raise (Not_Existing_Var str); (* [L("rien")] ou bien exception Var_non_trouvee; *)
    | ENV(var_str,var_code)::sub_env -> ( if (var_str = str) then ((var_code); (* si la valeur est la bonne, on renvoie le bout de code 
										  asm correspondant *)
								  )
					  else ((cherche_code str sub_env); (* sinon on continue la recherche sur le 'bout' 
									       d'environnement restant *)) 
					);;

let rec is_existing_var str env = (
  match env with 
      [] -> false;
    | ENV(var_str,var_code)::sub_env -> ( if (var_str = str) then (true)
					  else (is_existing_var str sub_env)
					)
);;

(*
let rec cherche_code str env = 
  sub_cherche_code str env;;
*)

(* try match (sub_cherche_code str env) with
   | Var_non_trouvee -> print_string ("-+- ERREUR -+- : La variable '"^str^"' n'existe pas dans l'environnement\n");; *)
