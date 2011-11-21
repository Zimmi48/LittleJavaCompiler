
/* Analyseur syntaxique pour Arith */

%{
  open Ast

  let position () =
    { file = $startpos.pos_fname ;
      line = $startpos.pos_lnum ;
      fChar = $startpos.pos_cnum ;
      lChar = $endpos.pos_cnum }
%}

%token <int> INT_CST
%token <string> STRING_CST
%token <string> IDENT
%token BOOLEAN, CLASS, ELSE, EXTENDS, FALSE, FOR, IF, INSTANCEOF, INT, NEW, NULL, PUBLIC, RETURN, STATIC, THIS, TRUE, VOID
%token EOF 
%token EQ
%token LP RP (* parenthèses *)
%token LB RB (* accolades : braces *)
%token LBRACKET RBRACKET
(* crochets : sert à rien, sauf pour déf du main *)
%token PLUS MINUS TIMES DIV MOD
%token PLUSPLUS MINUSMINUS
%token ISEQ NEQ LT LEQ GT GEQ AND OR
%token POINTDEXCLAMATION (* changer ce nom pourri *)
%token POINT
%token VIRGULE POINTVIRGULE

/* Définitions des priorités et associativités des tokens */

%right EQ
%left OR
%right AND
%left ISEQ NEQ
%left LT LEQ GT GEQ INSTANCEOF
%left PLUS MINUS
%left TIMES DIV MOD
%right uminus PLUSPLUS MINUSMINUS POINTDEXCLAMATION CAST
%left POINT


/* Point d'entrée de la grammaire */
%start fichier

/* Type des valeurs retournées par l'analyseur syntaxique */
%type <Ast.classe list * Ast.instruction list> fichier

%%

fichier:
| l = classe* m = classe_Main EOF { l * m }
;

classe:
| CLASS id1 = IDENT (LP EXTENDS id2 = IDENT RP)? LB d = decl* RB
    { pos = position() ; name = id1 ; extends = id2 * (* CONTINUER *)
;

decl:
| t = typ id = IDENT POINTVIRGULE
| constructeur
| methode
;

constructeur:
| id = IDENT LP parametres? RP LB instructions RB
;

methode:
| t = typ id = IDENT LP parametres? RP LB instructions RB
| VOID id = IDENT LP parametres? RP LB instructions RB
;

parametres:
| t = typ id = IDENT
| t = typ id = IDENT VIRGULE p = parametres
;

typ:
| BOOLEAN
| INT
| id = IDENT
;

classe_Main:
| CLASS cMain = IDENT LB
	PUBLIC STATIC VOID main = IDENT
		LP t = typ IDENT LBRACKET RBRACKET RP
		LB l = instructions RB
	RB
	{if cMain = "Main" & main = "main" & typ = C "String" then
	 	l
	 else failwith "Erreur parser: classe_Main"}
;

expr:
| i = INT_CST
| s = STRING_CST
| TRUE
| FALSE
| THIS
| NULL
| LP e = expr RP
| a = acces
| a = acces EQ e = expr
| a = acces LP l = l_expr? RP (* l_expr ? *)
| NEW id = IDENT LP l = l_expr? RP (* l_expr ? *)
| PLUSPLUS e = expr
| MINUSMINUS e = expr
| e = expr PLUSPLUS
| e = expr MINUSMINUS
| POINTDEXCLAMATION e = expr
| MINUS e = expr
| e1 = expr op = operateur e2 = expr
| LP t = typ RP e = expr
| e = expr INSTANCEOF t = typ
;

instructions:
| i = instruction	           { [i] }
| i = instruction l = instructions { match i with
					| None -> l
					| Some i -> i::l }
;
 
instruction:
| POINTVIRGULE { None }
| e = expr POINTVIRGULE { e }
| t = typ id = IDENT POINTVIRGULE {  }
| t = typ id = IDENT EQ e = expr POINTVIRGULE {  }
| IF LP e = expr RP i = instruction { }
| IF LP e = expr RP i1 = instruction ELSE instruction { }
| FOR	LP
		e1 = expr? POINTVIRGULE
		e2 = expr? POINTVIRGULE
		e3 = expr?
	RP
	i = instruction (* expr ? *)
| LB l = instructions RB
| RETURN e = expr? (* expr ? *)
;
 
l_expr:
| e = expr
| e = expr VIRGULE l = l_expr
;

(*
l_expr?:
| l = l_expr { Some l }
| { None }
;
*)

acces:
| id = IDENT
| e = expr POINT id = IDENT
;

operateur:
| ISEQ
| NEQ
| LT
| LEQ
| GT
| GEQ
| PLUS
| MINUS
| TIMES
| DIV
| MOD
| AND
| OR
;

(*
%inline op:
| PLUS  { Add }
| MINUS { Sub }
| TIMES { Mul }
| DIV   { Div }
;
*)


