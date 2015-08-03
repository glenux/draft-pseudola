open Compml_header;; 

(* === programme a compiler  === *)
(* renommer le programme qui vous interesse en 'prg' et sauvegardez, avant la compilation *)

let divprg = [LETREC ("division",PLUS(INT(8),INT(4)));
	   LETREC("show",CALLPRIM("printf",[STRING("le resultat est %d\\n");VAR("division")]))];;
let ifprg = [LETREC("ifprg",IF( LESS(INT(1),INT(2)),
			      CALLPRIM("printf",[STRING("1<2 VRAI\\n")]),
			      CALLPRIM("printf",[STRING("1<2 FAUX\\n")])))];;
(* refprg *)
let refprg = [LETREC("i",REF(INT(4)));
	   LETREC("main",
		  SEQ(
		    SET(VAR("i"),PLUS(BANG(VAR("i")),INT(5))),
		    CALLPRIM("printf",
			     [STRING("Valeur BANG('i') = %d\\n");
			      BANG(VAR("i"))])
		  )
		 )];;

(* tupleprog *)
let tupleprg = [ LETREC("i",INT(2));
	    LETREC(
	      "tuple",
	      PROJ(
		VAR("i"),
		TUPLE([INT(5);INT(7);
		       IF(LESS(INT(2),INT(3)),INT(1),INT(0))
		      ])
	      )
	    );
	    LETREC(
	      "main",
	      CALLPRIM(
		"printf",
		[STRING("Valeur de l'element %d = %d\\n");VAR("i");VAR("tuple")]
	      )
	    )     
	  ];;

(* ee78prg *)
let ee78prg = [LETREC("paf",FUN("x",PLUS(VAR("x"),INT(5))));
	   LETREC(
	     "main",SEQ(
	       CALLPRIM(
		 "printf",
		 [STRING("Valeur = %d\\n");APP(VAR("paf"),INT(2))]
	       ),
	       CALLPRIM(
		 "printf",
		 [STRING("Valeur = %d\\n");APP(VAR("paf"),INT(3))]
	       )
	     )
	   )  
	     ];;

(* the21prg *)
let the21prg = [LETREC("paf",FUN("x",TIMES(PLUS(VAR("x"),INT(5)),INT(3))));
	   LETREC("huhu",APP(VAR("paf"),INT(2)));
	   LETREC(
	     "main",
	     CALLPRIM(
	       "printf",
	       [STRING("Valeur = %d\\n");VAR("huhu")]
	     )
	   )  
	     ];;

(* factorielle 1*)

let factprg = [LETREC("ptr",REF(INT(0)));
	    LETREC("func",
		  FUN(
		    "a",
		    IF(
		      LESS(VAR("a"),INT(1)),
		      INT(1),
		      TIMES(
			VAR("a"),
			APP(
			  VAR("func"),
			  MINUS(VAR("a"),INT(1))
			)			
		      )
		    )    
		  )
		 );
	    LETREC("fun2",SEQ(
		     CALLPRIM("sscanf",[PROJ(INT(2),VAR("argv"));STRING("%d");VAR("ptr")]),
		     CALLPRIM("printf",[STRING("fact(%d) = %d\\n");BANG(VAR("ptr"));APP(VAR("func"),BANG(VAR("ptr")))])
		   )		 
		  )	      
	  ];;

(* fibonacci*)
let fiboprg = [LETREC("ptr",REF(INT(0)));
	   LETREC("fibo",
		  FUN("i",
		      SEQ(
			CALLPRIM("printf",[STRING("val de i=%d\\n");VAR("i")]),
			
			IF(LESS(VAR("i"),INT(1)),
			   SEQ(
			     CALLPRIM("printf",[STRING("<1\\n")]),
			     INT(0)
			   ),
			   IF(EQUAL(VAR("i"),INT(1)),
			      SEQ(
				CALLPRIM("printf",[STRING("=1\\n")]),
				INT(1)),
			      SEQ(
				CALLPRIM("printf",[STRING("val ok\\n")]),				
				PLUS(
				  APP(VAR("fibo"),MINUS(VAR("i"),INT(1))),  
				  APP(VAR("fibo"),MINUS(VAR("i"),INT(2)))
				)
			      )
			     )
			  )
		      )
		     )
		 );
	    LETREC("main",SEQ(
		     CALLPRIM("sscanf",[PROJ(INT(2),VAR("argv"));STRING("%d");VAR("ptr")]),
		     CALLPRIM("printf",[STRING("fact(%d) = %d\\n");BANG(VAR("ptr"));APP(VAR("fibo"),BANG(VAR("ptr")))])
		   )		 
		  )
	  ]


let prg = [LETREC("ptr",REF(INT(0)));
	   LETREC("fibo",
		  FUN("i",
		      SEQ(
			CALLPRIM("printf",[STRING("val de i=%d\\n");VAR("i")]),
			
			IF(LESS(VAR("i"),INT(1)),
			   SEQ(
			     CALLPRIM("printf",[STRING("<1\\n")]),
				 INT(0)
			   ),
			   IF(EQUAL(VAR("i"),INT(1)),
			      SEQ(
				CALLPRIM("printf",[STRING("=1\\n")]),
				INT(1)
			      ),
			      LET("a",
				  APP(VAR("fibo"),MINUS(VAR("i"),INT(1))),
				  LET("b",
				      APP(VAR("fibo"),MINUS(VAR("i"),INT(2))),
				      PLUS(VAR("a"),VAR("b"))
				     )
				 )
			     )
			  )
		      )
		     )
		 );
	    LETREC("main",
		     CALLPRIM("printf",[STRING("fact(%d) = %d\\n");BANG(VAR("ptr"));APP(VAR("fibo"),INT(2))]) 
		  )
	  ]

(* scanf *)
let scanfprg = [LETREC("ptr",REF(INT(0)));
	   LETREC("val1",
		  CALLPRIM("sscanf",[PROJ(INT(2),VAR("argv"));STRING("%d");VAR("ptr")]));
	   LETREC("fun2",
		    CALLPRIM("printf",[STRING("scanf = %d\\n");BANG(VAR("ptr"))])
		  )
		 ];;

(* scanf+let *)
let scanfprg = [
  LETREC("main",
	 LET("ptr",
	     REF(INT(0)),
	     SEQ(
	       CALLPRIM("sscanf",[PROJ(INT(2),VAR("argv"));STRING("%d");VAR("ptr")]),
	       CALLPRIM("printf",[STRING("scanf = %d\\n");BANG(VAR("ptr"))])
	     )
	    )
	)
];;

(* test func before scanf *)
let tfbsprg = [ 
  LETREC("val1",
	 FUN("add",
	     LET(
	       "ptr",	 
	       REF(INT(0)),
	       VAR("add")
	     )
	    )
	);
  LETREC("main", CALLPRIM("printf",[STRING("val = %d\\n");APP(VAR("val1"),INT(2))]))
];;




(*    LETREC("main",      
	     CALLPRIM("printf",[STRING("let ptr = ref 0 in (!ptr + %d) == %d\\n");
				INT(2);
				APP(VAR("val1"),INT(2))]
		     )
	)
*)


(* fun in fun in fun *)

let funfunprg = [
  LETREC("printargs",
	 FUN("a",
	     FUN("b",
		 FUN("c",
		     CALLPRIM("printf",[STRING("a=%d, b=%d, c=%d");VAR("a");VAR("b");VAR("c")])
		    )
		)
	    )
	);
  LETREC("main",
	 APP(
	   APP(
	     APP(
	       VAR("printargs"),
	       INT(1)
	     ),
	     INT(2)
	   ),
	   INT(3)
	 )
	)
]

(* test rec let *) (* pb inversion *)
let eeprg = [
  LETREC("printargs",
	 FUN("i",
	     LET("a",
		 INT(1),
		 IF(LESS(VAR("i"),INT(1)),
		    INT(3),
		    SEQ(
		      CALLPRIM("printf",[STRING("a=%d, i=%d\\n");VAR("a");VAR("i")]),
		      APP(VAR("printargs"),MINUS(VAR("i"),INT(1))
			 )
		    )
		   )
	       )
	    )
	);
  LETREC("main",
	 APP(VAR("printargs"),INT(5))
	)
]









let badcatprg = [ LETREC("boucle_interne",
		     FUN("f",
			 LET("c",CALLPRIM("fgetc",[VAR("f")]),
			     IF(EQUAL(VAR("c"),OPP(INT(1))),
				INT(0),
				SEQ(
				  CALLPRIM("fputc",[VAR("c");STRING("stdout")]),
				  APP(VAR("boucle_interne"),VAR("f"))
				)
			       )
			    )
			)
		    );
	      LETREC("boucle_externe",
		     FUN("i",
			 FUN("argc",
			     FUN("argv",
				 IF(LESS(VAR("i"),VAR("argc")),
				    LET("nom",
					PROJ(PLUS(VAR("i"),INT(1)),VAR("argv")),
					LET("f",
					    CALLPRIM(
					      "fopen",
					      [VAR("nom");STRING("r")]
					    ),
					    SEQ(APP(VAR("boucle_interne"),
						    VAR("f")),
						SEQ(CALLPRIM("fclose",
							     [VAR("f")]),
						    APP(
						      APP(
							APP(
							  VAR("boucle_externe"),
							  PLUS(
							    VAR("i"),
							    INT(1))
							),
							VAR("argc")
						      ),
						      VAR("argv")
						    )
						   )
					       )
					   )
				       ),
				    INT(0)
				   )
				)
			    )
			)
		    );
	      LETREC("main",
		      APP(
			APP(
			  APP(
			    VAR("boucle_externe"),
			    INT(1)
			  ),
			  VAR("argc")
			),
			VAR("argv")
		      )
		    )
		
	     ];;

let jlmprg=
  [ LETREC ("boucle_interne",
            FUN ("f",
                 LET ("c",
                      CALLPRIM ("fgetc",[VAR "f"]),
                      IF (EQUAL (VAR "c",INT(-1)),
                          INT 0,
                          SEQ (CALLPRIM ("putchar", [VAR "c"]),
                               APP (VAR "boucle_interne",
                                    VAR "f"))))));
    LETREC ("boucle_externe",
            FUN ("i",
                 FUN ("argc",
                      FUN ("argv",
                           IF (LESS (VAR "i",VAR "argc"), (* ok *)
			       SEQ(
				 CALLPRIM("printf",[STRING "b_ext ok \\n"]),
				 LET ("nom", (* *)
                                      PROJ (PLUS(VAR "i",INT(1)),VAR "argv"), (* *)
                                      LET ("f",
                                           CALLPRIM ("fopen",[VAR "nom";STRING "r"]), (* *)
                                           SEQ (APP (VAR "boucle_interne",VAR "f"), (* *)
						SEQ (CALLPRIM ("fclose",[VAR "f"]),
                                                     APP (APP (APP (VAR "boucle_externe",
                                                                    PLUS (VAR "i",INT 1)),
                                                               VAR "argc"),
                                                          VAR "argv")))))
			       ),
                               INT 0)))));
    LETREC ("main",
            APP (APP (APP (VAR "boucle_externe",INT 1),
                      VAR "argc"),
                 VAR "argv"))
];;

(* glenux cat *)
let gcatprg =
  [
    LETREC("boucle_interne",
	   FUN("fileid",
	       SEQ(
		 SEQ(
		   CALLPRIM("printf",[STRING("boucle interne: file desc=%d\\n");VAR("fileid")]),
		   CALLPRIM("printf",[STRING("boucle interne: address func=%d\\n");VAR("boucle_interne")])
		    ),
		 LET("c",
		     CALLPRIM("fgetc",[VAR("fileid")]),
		     IF(EQUAL(VAR("c"),OPP(INT(1))),
			INT(0),
			SEQ(
			  CALLPRIM("printf",[STRING("%c\\n");VAR("c")]),
(*			  SEQ(
			    CALLPRIM("printf",[STRING("sortie boucle interne: file desc=%d\\n");VAR("fileid")]),
			    CALLPRIM("printf",[STRING("boucle interne: address func=%d\\n");VAR("boucle_interne")])
			  ) *)
			    APP(VAR("boucle_interne"),VAR("fileid")) 
			)
		       )
		    )
	       )
	      )
	  );
     LETREC("boucle_externe",
	   FUN("i",
	       SEQ(
		 CALLPRIM("printf",[STRING("i=%d\\n");VAR("i")]),
		 FUN("argc",
		     SEQ(
		       CALLPRIM("printf",[STRING("argc=%d\\n");VAR("argc")]),
		       FUN("argv",
			   IF(LESS(VAR("i"),VAR("argc")),
			      SEQ(
				CALLPRIM("printf",[STRING("Nombre d'arguments > 1\\n")]),
				LET("nom",
				    PROJ(PLUS(VAR("i"),INT(1)),VAR("argv")),
				    SEQ(
				      CALLPRIM("printf",[STRING("openning file %s\\n");VAR("nom")]),
				      LET("f", (* file descriptor *)
					  CALLPRIM("fopen",[VAR("nom");STRING("r")]),
					  SEQ(
					    APP(
					      VAR("boucle_interne"),
					      VAR("f")
					    ),
					    CALLPRIM("fclose",[VAR("f")])
					  )
					 )
				    )
				   )
			      ),
			      CALLPRIM("printf",[STRING("Trop peu d'arguments\\n")])
			     )
			  )
		     )
		    )
	       )
	      )
	  );
    LETREC("main",
	   APP(
	     APP(
	       APP(
		 VAR("boucle_externe"),
		 INT(1)
	       ),
	       VAR("argc")
	     ),
	     VAR("argv")
	   )
	  )
  ];;
