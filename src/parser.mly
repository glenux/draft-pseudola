
%{
  open Pstypes;;

%}

%token <int> INT
%token <string> VAR
%token APP
%token FUN
%token LET
%token REF
%token BANG
%token SET
%token PLUS
%token MINUS
%token COMA
%token TIMES
%token DIV
%token REM
%token POINT
%token LETREC
%token OPP
%token LESS
%token EQUAL
%token TUPLE
%token PROJ
%token SEQ
%token IF
%token ELSE
%token THEN
%token CALL
%token TO
%token EQ
%token SEMI
%token COLON
%token LPAR
%token RPAR
%token IN
%token EOE
%token LCR
%token RCR












%nonassoc LET,IN,FUN,TO

%right SEMI
%nonassoc IF
%right SET
%nonassoc COMA 
%left EQUAL,LESS
%left PLUS,MINUS
%left TIMES,DIV,REM

%nonassoc INT,VAR
%left LCR,RCR
%left LPAR,RPAR
%nonassoc BANG


%start letrec
%type <Mltype.letrec> letrec

%%

letrec:

LETREC VAR EQUAL exp EOE   {LETREC($2,$4)};

exp:

| INT                       {INT($1)}
| VAR                       {VAR($1)}
| FUN VAR TO exp            {FUN($2,$4)}
| REF exp                   { REF($2)}
| BANG exp                  { BANG($2)}
| exp SET exp               {SET($1,$3)}
| IF exp THEN exp ELSE exp  {IF($2,$4,$6)}
| LET VAR EQUAL exp IN exp  {LET($2,$4,$6)}
| exp POINT LCR exp RCR      {PROJ($1,$4)}
| exp PLUS exp              { PLUS($1,$3) }
| exp LPAR exp RPAR         { APP($1,$3) }
| exp SEMI exp              { SEQ($1,$3)}
| exp MINUS exp             { MINUS($1,$3) }
| exp TIMES exp         { TIMES($1,$3) }
| exp SET exp           { SET($1,$3) } 
| exp DIV exp           { DIV($1,$3) }
| exp REM exp           { REM($1,$3) }
| MINUS exp             { OPP($2) }
| exp EQUAL exp         { EQUAL($1,$3) }
| exp LESS exp          { LESS($1,$3) }
| CALL VAR LCR explist RCR  {CALLPRIM($2,$4)}
| LCR explist RCR          {TUPLE($2)}
| LPAR exp RPAR             {$2}
;


explist:

| exp                        {[$1]}
| exp COMA explist          {[$1]@$3}
;







%%

            

