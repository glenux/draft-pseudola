(*  Projet de Programmation                                                 *)
(*  1er semestre - 1ere année de Magistère STIC - 2002                      *)
(*  Cours de Jean Goubault-Larrecq                                          *)
(* ========================== Programme Pricipal ========================== *)

open Compml_header;; 
open Compml_asm;; 
open Compml_env;; 
open Compml_tools;; 
open Compml_code;;

open Sys;;

(*#use "testfile.miniml";;*)
let (lrname: string ref) = ref "";;

let str_env = ref [L("main");];;

let rec compile_expr (gxpr:expr) (env:envinode list) = 
  (match gxpr with (* [I("pushl","%ecx","")]@ *)
      OPP(xpr) -> ( (* print_string "OPP\n"; *)
		    (compile_expr xpr env)@[I("negl","%eax","")]; (* renvoie l'opposé de la valeur calculée par xpr *)
		    (* FINI *) )
    | PLUS(xpr1,xpr2) -> ( (* print_string "PLUS\n"; *)
			   (* evalue xpr1 et l'empile *)
			   (compile_expr xpr1 env)@
			   [I("pushl","%eax","")]@
			   (compile_expr xpr2 env)@
			   [I("pushl","%eax","");
			    I("popl", "%ebx",""); (* depile le 1er chiffre *)
			    I("popl", "%eax",""); (* dépile le 2nd chiffre *)
			    I("addl", "%ebx","%eax")]; (* ajoute %eax à %ebx et stocke le result dans %eax *)
			   (* FINI *) )
    | MINUS(xpr1,xpr2) -> ( (* print_string "MINUS\n"; *)
			    (* evalue xpr1* et l'empile puis xpr2 (chiffre retiré) et l'empile aussi *)
			    (compile_expr xpr1 env)@
			    [I("pushl","%eax","")]@
			    (compile_expr xpr2 env)@
			    [I("pushl","%eax","");
			     I("popl", "%ebx",""); (* depile le chiffre retiré -> %ebx *)
			     I("popl", "%eax",""); (* dépile le chiffre auquel on retire 'autre -> %eax *)
			     I("subl", "%ebx","%eax")]; (* retire %ebx à %eax et stocke le result dans %eax *)
			    (* FINI *) )
    | TIMES(xpr1,xpr2) -> ( (* print_string "TIMES\n"; *)
			    (* evalue xpr1(divisé)* et l'empile *)
			    (compile_expr xpr1 env)@
			    [I("pushl","%eax","")]@
			    (compile_expr xpr2 env)@
			    [I("pushl","%eax","");
			     I("popl", "%ebx",""); (* depile le diviseur *)
			     I("popl", "%eax",""); (* dépile le divisé dans eax*)
			     I("imull", "%ebx","%eax")]; (* multiplie %eax par %ebx et stocke le result dans %eax *)
			    (* FINI *) )
    | DIV(xpr1,xpr2) ->  ( (* print_string "DIV\n"; *)
			   (* evalue xpr1(divisé)* et l'empile *)
			   (compile_expr xpr1 env)@
			   [I("pushl","%eax","")]@
			   (compile_expr xpr2 env)@
			   [I("pushl","%eax","");
			    I("popl", "%ebx",""); (* depile le diviseur *)
			    I("popl", "%eax",""); (* dépile le divisé *)
			    I("cltd", "", ""); (* convertit le truc dans %eax en 64 bit pour la division *)
			    I("idiv", "%ebx","")]; (* div %eax (et %edx) par %ebx et stocke le result dans %eax *)
			   (* FINI *) )
    | INT(i) -> ( (* print_string "INT\n";  *)
		  [I("movl","$"^string_of_int(i),"%eax")]; (* place la valeur i dans %eax *)
		  (* FINI *) )
    | VAR(str) -> ( (* print_string ("VAR("^str^") "); *)
		    cherche_code str env; 
		    (* FINI *) )
    | REM(xpr1,xpr2) -> ( (* print_string  "REM\n"; *)
			  compile_expr (MINUS(xpr1,DIV(xpr1,xpr2))) env; 
			  (*reste de la division ?? *) (* xpr1 - (xpr1 / xpr2) *)
			  (* FINI *) )
    | STRING(str) -> ( (* print_string "STRING\n"; *)
		       let str_name = gen_str () in (
			   str_env := (L(str_name) :: S(str) :: A("4") :: !str_env);
			   [I("movl","$"^str_name,"%eax")]; (* prepare a l'empilation de la str *)			  
			 )
						      (* FINI *))
    | LET(str,xpr1,xpr2) -> print_string ("LET("^str^"\n");
	(compile_expr (APP(FUN(str,xpr2),xpr1)) env;
	 (* FINI (APP(FUN(str,xpr2),xpr1)) *))
    | CALLPRIM(str,xprlst) -> ( let lstsize = (lst_count xprlst) in 
				 ( let cheader = ref [] in
				     (let cfooter = ref [] in
					( for cnt_i = 1 to lstsize do (
					    cheader :=  ( (!cheader)@
							  (compile_expr (lst_nieme xprlst cnt_i) env)@
							  [I("pushl","%eax","")] );
					    cfooter := ( [I("addl","$4","%esp")]@ 
							 (!cfooter)
						       );
					  ) done ;
					  ( (* resultat *)
					    [I("pushl","%ecx","")]@
					    (!cheader)@
					    [I("call",str,"")]@
					    (!cfooter)@
					    [I("popl","%ecx","")]
					  )
					)
				     )
				 )
			      )

	(* print_string "CALLPRIM\n";
				match xprlst with 
				    [] -> [I("call",str,"")] 
				  | _ -> (  (compile_expr (lst_tailelem xprlst) env)@
					    [I("pushl","%eax","")]@
					    (compile_expr (CALLPRIM(str,(lst_hdlst xprlst))) env)@
					    [I("addl","$4","%esp")]					      
					 )  
				      (* FINI *) *)   
    | REF(xpr) -> ( (* print_string "REF\n"; *)
		    [I("pushl","%ecx","")]@
		    (compile_expr xpr env)@
		    [I("pushl","%eax","");
		     I("pushl","$4",""); (* on empile l'argument du malloc *)
		     I("call","malloc",""); (* on appellle la fonction malloc et le resultat -> eax *)
		     I("addl","$4","%esp"); (* on pointe sur le resultat de xpr *)
		     I("popl","%ebx",""); (* on recup le resultat de xpr dans %ebx *)
		     I("movl","%ebx","0(%eax)"); (* et on met xpr à l'addresse du malloc
		                                    I("movl","%eax",gen_var()); *)
		     I("popl","%ecx","")] (* on va créer une nouvelle variable, et on stocke l'addresse 
					     laissée par le malloc dedans *)
		    (* on renverra uniquement l'addresse de la new variable dans l'environnement... *)
		    (* FINI *) )
	
    | BANG(xpr) -> ( (* print_string "BANG\n"; *)
		     (compile_expr xpr env)@ (* il faut que xpr renvoie une addresse dans %eax*)
		     [I("movl","0(%eax)","%eax")]
		     (* FINI *) )
    | SET(xpr1,xpr2) -> ( (* print_string "SET\n"; *)
			  (compile_expr xpr2 env)@ (* on calcule xpr2, et on le met a l'addresse pointée par xpr1 *)
			  [I("pushl","%eax","")]@
			  (compile_expr xpr1 env)@
			  [I("popl","%ebx","");
			  I("movl","%ebx","0(%eax)");
			  I("movl","0(%eax)","%eax")]
			(* FINI *) )
    | LESS(xpr1,xpr2) -> ( (* print_string "LESS\n"; *)
			   begin
			     ( (compile_expr xpr2 env)@
			       [I("pushl","%eax","")]@
			       (compile_expr xpr1 env)@ (* xpr2:: pile *) (* xpr1 dans %eax *)
			       [I("popl","%ebx","");
				I("cmp","%ebx","%eax")]
			     )
			   end 
			 (* FINI *) ) 
    | EQUAL(xpr1,xpr2) -> ( (* print_string "EQUAL\n"; *)
			    begin
			     ( (compile_expr xpr2 env)@
			       [I("pushl","%eax","")]@
			       (compile_expr xpr1 env)@ (* xpr2:: pile *) (* xpr1 dans %eax *)
			       [I("popl","%ebx","");
				I("cmp","%eax","%ebx")]
			     )
			   end 
			 (* FINI *) ) 
    | IF(cond,xpr1,xpr2) -> ( (* print_string "IF "; *)
			      let condition_code = (compile_expr cond env) in (
				  let jmptyp = ( match cond with 
						     LESS(_,_) -> "jl"
						   | EQUAL(_,_) -> "je" 
						   | _ -> failwith "mauvaise condition dans le IF" ) in (
				      let true_label = (gen_label ()) in (
					  let endif_label = (gen_label ()) in (
					      (condition_code)@ (* [I("leal","$"^true_label,"%eax"); *)
					      [I(jmptyp,true_label,"")]@
					      (compile_expr xpr2 env)@ (* [I("leal","$"^endif_label,"%eax"); *)
					      [I("jmp",endif_label,"");
					       L(true_label)]@
					      (compile_expr xpr1 env)@
					      [I("jmp",endif_label,"");
					       L(endif_label)]
					      
					    )
					)
				    )
				) 
			    (* FINI *) )
    | SEQ(xpr1,xpr2) -> ( (* print_string "SEQ\n"; *)
			  (compile_expr xpr1 env)@(compile_expr xpr2 env); (* applique xpr1 puis xpr2 *)
			  (* FINI *) )
    | TUPLE(xprlst) ->  ( (* print_string "TUPLE\n"; *)
			  let xprnum = (lst_count xprlst) in (
			      let tmpheadcode = (ref []) in (
				  let tmptailcode = (ref []) in (
				      for cnt_i = 1 to xprnum do (
					tmpheadcode := ( (!tmpheadcode)@
							 (compile_expr (lst_nieme xprlst (xprnum - cnt_i + 1)) env)@
							 [I("pushl","%eax","")]
						       );
					tmptailcode :=  ( [I("popl","%ebx","");
							   I("movl","%ebx",(string_of_int ((cnt_i) * 4))^"(%eax)")]@
							  (!tmptailcode)
							);
				      ) done;
				      ( [I("pushl","%ecx","")]@
					(!tmpheadcode)@
					[I("pushl","$"^(string_of_int (4 * (xprnum + 1))),"");
					 I("call","malloc","");
					 I("popl","%ebx","");
					 I("movl","%ebx","0(%eax)")]@
					 (!tmptailcode)@
					[I("popl","%ecx","")]
				      )					  
				    )
				)
			    )
			(* FINI *) )
    | PROJ(xpr1,xpr2) -> ( (* print_string "PROJ\n"; *)
			   (compile_expr xpr1 env)@
			   [I("pushl","%eax","")]@
			   (compile_expr xpr2 env)@
			   [I("popl","%ebx","");
			    I("movl","(%eax,%ebx,4)","%eax")]
			 (* FINI *) ) (* ajouter? la verification de la taille du tableau? *)
    | APP(xpr1,xpr2) -> ( let code2 = (compile_expr xpr2 env) in
			  let code1 = (compile_expr xpr1 env) in
			    (* un pointeur sur l'ancienne pile stocke *)
			    code2@	
			    I("pushl","%ecx","")::		  
			      I("pushl","%eax","")::			  
			      code1@
			    I("pushl","%eax","")::
			      I("movl","%esp","%ecx"):: (* on pointe sur la nouvelle pile *) 
			      I("addl","$8","%ecx"):: (*sous les fnc + arg *)
			      I("call","*(%eax)",""):: (* appel de la fonction *)
			      I("addl","$8","%esp"):: (* on depile les trucs empiles *)
			      I("popl","%ecx","")::[] (* on recupere la valeur de %ecx avant l'appel *)
			      (* FINI *)  (* applique xpr1 à xpr2 *) ) 
      | FUN(str,xpr) -> ( print_string ("FUN("^str^",)\n");
			  let freevar_lst = ((var_libre gxpr)) in 
			  let varnum = (lst_count freevar_lst) in
			  let mkcloture = (ref [INT(1)] ) in
			  let newenv = (ref []) in 
			  let returnedcode = ( ref [] ) in
			  let func_num = (ref (-1)) in 
			  let func_name = ( gen_label () ) in 
			    (* on cree une cloture avec les var dedans *)
			    for cnt_i = 1 to varnum do (
			      let var_name = lst_nieme freevar_lst (varnum +1 - cnt_i) in
				(if (var_name = (!lrname)) then (
				   print_string "recursive function\n";
				   mkcloture := (!mkcloture)@[INT(-1)];
				   func_num := cnt_i;
				 ) else (
				   print_string "function not recursive\n";
				   print_string ("|free var: "^var_name^"\n");
				   mkcloture := (!mkcloture)@[VAR(var_name)];					
				   newenv := (ENV(var_name,
						  [I("movl","-8(%ecx)","%eax");
						   I("movl",(string_of_int (4 * (cnt_i + 1)))^"(%eax)","%eax")]
						 ))::(!newenv);
				 ));
			    ) done;
			    newenv := [ENV(str,[I("movl","-4(%ecx)","%eax")])]@(!newenv);
			    if ((!func_num) = -1) then (
			      (* *NOT* RECURSIVE *)
			       (* production de la cloture *)
			      returnedcode := ( (compile_expr (TUPLE(!mkcloture)) env)@
						[I("movl","$"^func_name,"0(%eax)")] );
			    ) else (
			      (* RECURSIVE *)
			      (* ajout de la fonction ellememe au nouvel env *)
			      newenv:=[ENV((!lrname),[
					     I("movl","-8(%ecx)","%eax");
					     I("movl",(string_of_int (4 * (1 + (!func_num))))^"(%eax)","%eax")
					   ])]@(!newenv);				      
			      (* production de la cloture *)
			      returnedcode := ( (compile_expr (TUPLE(!mkcloture)) env)@
						[I("movl","$"^(string_of_int (1 + (!func_num))),"%ebx");
						 I("movl","%eax","(%eax,%ebx,4)");
						 I("movl","$"^func_name,"(%eax)")]);			      
			    );
			    (* production du code execute *)
			    functioncode := ( (!functioncode)@
					      [L(func_name)]@
					      (compile_expr xpr (!newenv))@
					      [I("ret","","")]
					    );
			    (* renvoie de la valeur *)
			    (!returnedcode)
			)
      | NOP -> []);; (* movl %eax, %eax ? @[I("popl","%ecx","")] *)

let rec compile_prog (prog:camlcode) env = ( (* modifier l'env ici aussi *) 
  (* compile la liste de LETREC *)
  match prog with 
      LETREC(str,xpr)::prog_rest -> ( lrname:=str;
				      let new_var = gen_var () in 
					( let new_env =   (ENV(str,[I("movl",new_var,"%eax")])::env) in 
					    (let letrec_code = (compile_expr xpr new_env) in (
						 ((letrec_code)@[I("movl","%eax",new_var)])@
						 (compile_prog prog_rest new_env)
					       )
					    ) 
					)
				    )
    | _ -> []
					     
  );;



let asm_of_caml (prg:camlcode) = 
  ( let asm_prog = (compile_prog prg env) in 
      ( (!str_env)@
	[I("pushl","%ebp",""); (* sauvegarde les registres *)
	 I("pushl","%eax",""); 
	 I("pushl","%ebx","");
	 I("pushl","%ecx","");
	 I("pushl","%edx","");
	 I("movl","%esp","%ebp"); (* met ebp a la valeur pointée par esp *)
	 I("addl","$20","%ebp"); (* on incrémente la valeur de ebp de (4 * le nombre de truc sauvegardés) *)
	 I("subl","$"^(string_of_int (!var_count * 4)),"%esp")]@ 
	asm_prog@
	[ I("addl","$"^(string_of_int (!var_count * 4)),"%esp");
	  I("popl","%edx","");
	  I("popl","%ecx","");
	  I("popl","%ebx","");
	  I("popl","%eax","");
	  I("popl","%ebp","");
	  I("ret","","")]@
	  (!functioncode)
      )
  );;

(* execution *)

let default_config = { inputfile = "none"; outputfile = "a.out" ; optlevel = 0 };;

asm_to_file (asm_of_caml prg) ("asmsource.s");;

let read_args () = (
  let conf = ref default_config in (
      let speclist = 
	[("-o", Arg.String (set_inputfile conf), "input file");
	 ("-p", Arg.Int (set_optlevel conf), "assembly optimisation level")]
      in (
	  let usage_msg = "Usage : mlMl_comp inputfile [ -o output_file ] [ -p optlevel ]" in
	    Arg.parse speclist (set_inputfile conf) usage_msg; !conf;	      
	)
    )
);;

let main = (
  let argz = (read_args ()) in (
      if (argz.inputfile = "none") then (
	print_string ("No input file !\n");
	Sys.command ("gcc -s asmsource.s -o "^(argz.outputfile)); (* supprimer cette ligne apres la fusion 
								     avec le parser *)
	0;
      ) else (
	print_string ("Input file:"^(argz.inputfile)^"\n");
	Sys.command ("gcc -ggdb -s asmsource.s -o "^(argz.outputfile));
      )
    )
);;




