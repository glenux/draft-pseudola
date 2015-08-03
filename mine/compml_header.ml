(*  Projet de Programmation                                                 *)
(*  1er semestre - 1ere ann�e de Magist�re STIC - 2002                      *)
(*  Cours de Jean Goubault-Larrecq                                          *)
(* ============================== En-t�te ================================= *)


type expr=
    VAR of string (* 'd�clare' un nom de variable *)
  | APP of expr * expr (* applique expr1 � expr2 *)
  | FUN of string * expr (* assigne expr au nom string *)
  | LET of string * expr * expr (* assigne expr1 � string dans expr2 UNIQUEMENT *)
  | REF of expr (* expr est une r�f�rence *)
  | BANG of expr (* valeur point�e par expr *)
  | SET of expr * expr (* assigne expr2 � expr1 *) 
  | INT of int (* "d�clare" un entier *)
  | PLUS of expr * expr (* ajoute expr1 � expr2 *)
  | MINUS of expr * expr (* retire expr2 � expr1 *)
  | TIMES of expr * expr (* multiplie expr1 par expr2 *)
  | DIV of expr * expr (* diviste expr1 par expr2 *)
  | REM of expr * expr (* reste de la division *) (* ??? *)
  | OPP of expr  (* oppos� de la valeur calcul�e par expr *) (* ?? *)
  | LESS of expr * expr (* teste si expr1 est < expr2 *)
  | EQUAL of expr * expr (* teste si expr1 est egal � expr2 *)
  | TUPLE of expr list (* cr�e en m�moie un tableau d'elements *)
  | PROJ of expr * expr (* renvoie l'element expr1 de expr2 (:TUPLE ?) *) (* INT,TUPLE *)
  | SEQ of expr * expr (* expr1 puis expr2 *)
  | IF of expr * expr * expr (* si expr1 alors expr2 sinon expr3 *)
  | CALLPRIM of string * (expr list) (* fonction(expr1,expr2...) ou string est le nom de la fonction... *) 
  | STRING of string (* "d�clare" une chaine de caract�res *)
  | NOP  (* instruction qui ne fait rien... *);;

type letrec = LETREC of string * expr;;

type camlcode = letrec list;;

type instr = I of string * string * string (* opcode * arg1 * arg2 *)
	     | L of string (* label *)
	     | S of string (* string *)
	     | A of string (* align *)
	     | I_NOP;;



type asmcode = instr list;;

type envinode = ENV of string * (instr list) ;;(* et le r�sultat de chaque envinode va tjrs dans %eax *)

type envilist = envinode list;;


type cmptyp = CMP_LESS | CMP_EQUAL;;

exception Empty_Cmp_Stack;;
exception Out_of_Range;;
exception Not_Existing_Var of string;;
exception Empty_List;;

type config = { 
  inputfile : string;
  outputfile : string;
  optlevel : int; 
}


(* ==================== Fonctions ======================================= *)




(* === initialisation des variables === *)

(* let env = ref [];; *)

let env = [ENV("argc",[I("movl","4(%ebp)","%eax")]);
	   ENV("argv",[I("movl","8(%ebp)","%eax");
		       I("subl","$4","%eax")
 		      ])];;

let nb_saved_reg = 5 ;; (* nombre de registres sauves *)

let (functioncode: instr list ref) = (ref [I_NOP])
