(*  Projet de Programmation                                                 *)
(*  1er semestre - 1ere année de Magistère STIC - 2002                      *)
(*  Cours de Jean Goubault-Larrecq                                          *)
(* ================= Fonctions De Traitement de l'Assembleur ============== *)

open Compml_header;;

let rec asm_write (asm: instr list) fileid =
      match asm with 
	  I(i,src,dest)::asm_rest -> ( if ((dest="") && (src="")) then ( begin
									   output_string fileid ("\t"^i^"\n");
									   (* print_string ("\t"^i^"\n"); *)
									   asm_write asm_rest fileid;
									 end)
				       else ( if (dest="") then ( begin
								    output_string fileid ("\t"^i^"\t"^src^"\n");
								    (* print_string ("\t"^i^"\t"^src^"\n"); *)
								    asm_write asm_rest fileid;
								  end) 
					      else ( begin 
						       output_string fileid ("\t"^i^"\t"^src^", "^dest^"\n");
						       (* print_string ("\t"^i^"\t"^src^", "^dest^"\n"); *)
						       asm_write asm_rest fileid;
						     end )
					    )
				     )
	| L(str)::asm_rest -> ( begin 
				  output_string fileid ("\n"^str^":\n");
				  (* print_string ("\n"^str^":\n"); *)
				  asm_write asm_rest fileid;
				end)
	| S(str)::asm_rest -> ( begin
				  output_string fileid ("\t.string \""^str^"\"\n");
				  (* print_string ("\t.string \""^str^"\"\n"); *)
				  asm_write asm_rest fileid
				end)
	| A(str)::asm_rest -> (begin
				 output_string fileid ("\t.align\t"^str^"\n");
				 (* print_string ("\t.align\t"^str^"\n"); *)
				 asm_write asm_rest fileid
			       end)
	| I_NOP::asm_rest -> (begin
				asm_write asm_rest fileid
			      end)
	| _ -> (begin 
		  (* print_string "--stop--\n"; *)
		end
	       );;

let rec asm_to_file (asm: instr list) filename =  
  (* ouvrir un fichier*)
  let fileid = (open_out filename) in ( begin 
					  print_string ("\n--fichier '"^filename^"' ouvert avec succès--\n");

					  output_string fileid (".text\n");
					  output_string fileid ("\t.align\t4\n");
					  output_string fileid (".globl main\n");
					  output_string fileid ("\n\t.type main, @function\n");
					  print_string ("--ecriture de l'en-tete : OK--\n");
					  (* print_string "--start--\n"; *)
					  asm_write asm fileid; (* ecrire les données *)			
					  print_string ("--ecriture des données : OK--\n");
					  output_string fileid ("\n");
					  print_string ("--ecriture du retour chariot de fin de fichier : OK--\n");
					  close_out fileid; (* fermer le fichier *)
					  print_string ("--fichier '"^filename^"' fermé avec succès--\n")
					end);; 

