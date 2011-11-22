
/* Analyseur syntaxique pour Arith */

%{
  open Ast

  let position () = (* actuellement on se prive de certaines infos *)
    { file = $startpos.pos_fname ;
      line = $startpos.pos_lnum ;
      fChar = $startpos.pos_cnum ;
      lChar = $endpos.pos_cnum }

  type Attr of variable * pos | Const of callable | Method of callable

  let rec attrsAndConstsAndMethods_of_decls = function
    | [] -> [] , [] , []
    | Attr a :: t -> let la , lc , lm = attrsAndConstsAndMethods_of_decls t in
                     a::la , lc , lm
    | Const c :: t -> let la , lc , lm = attrsAndConstsAndMethods_of_decls t in
                     la , c::lc , lm
    | Method m :: t -> let la , lc , lm = attrsAndConstsAndMethods_of_decls t in
                     la , lc , m::lm
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
%left AND
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
l = classe* m = classe_Main EOF { l * m }
;

classe:
  CLASS id = IDENT e = extends? LB d = decl* RB
    {
      let a, c, m = attrsAndConstsAndMethods_of_decls d in
      { pos = position() ;
        name = id ;
        extends =  e ; (* héritage simple: dans Ast, remplacer list par option*)
        attrs = a ;
        consts = c ; (* mise au pluriel : corriger Ast *)
        methods = m }
    }
;

extends:
  LP EXTENDS id = IDENT RP { id , position () }
;

decl:
| t = typ id = IDENT POINTVIRGULE { Attr ( (t, id) , position () ) }
| c = constructeur { Const c }
| m = methode { Method m }
;

constructeur:
| id = IDENT LP p = parametres? RP LB i = instructions RB
    {
      { pos = position () ;
        returnType = Void ;
        name = id ;
        params = (match p with None -> [] | Some l -> l) ;
        body = i }
    }
;

methode:
| t = typ id = IDENT LP p = parametres? RP LB i = instructions RB
    {
      { pos = position () ;
        returnType = t ;
        name = id ;
        params = (match p with None -> [] | Some l -> l) ;
        body = i }
    }
| VOID id = IDENT LP p = parametres? RP LB i = instructions RB
    {
      { pos = position () ;
        returnType = Void ;
        name = id ;
        params = (match p with None -> [] | Some l -> l) ;
        body = i }
    }
;

parametres:
| t = typ id = IDENT { [t, id] }
| t = typ id = IDENT VIRGULE p = parametres { (t, id) :: p }
;

typ:
| BOOLEAN    { Bool }
| INT        { Int }
| id = IDENT { C id }
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
| i = INT_CST    { Iconst (position () , i) }
| s = STRING_CST { Sconst (position () , s) }
| TRUE           { Bconst (position () , true) }
| FALSE          { Bconst (position () , false) }
| THIS (* l'Ast ne gère pas ce type d'accès *)
| NULL           { Null (position ()) }
| LP e = expr RP { e }
| a = acces      { Getval (position () , a) }
| a = acces EQ e = expr { Assign (position () , a , e) }
| a = acces LP l = l_expr? RP
    { Call (position () , a , match l with None -> [] | Some l -> l) }
| NEW id = IDENT LP l = l_expr? RP
    { New (position () , id , match l with None -> [] | Some l -> l) }
| PLUSPLUS e = expr   { Prefix (position () , PrefixIncr , e) }
(* Est-il vraiment nécessaire de différencier les opérateurs préfixes, infixes,
 postfixes dans l'Ast ? Dans ce cas Incr ne peut pas être à la fois une valeur
 de type prefix et postfix. Il me paraît de toute façon plus judicieux de se 
 contenter de différencier les opérateurs en fonction de leur arité (unaire,
 binaire) *)
| MINUSMINUS e = expr
| e = expr PLUSPLUS
| e = expr MINUSMINUS
(* Oubli du prof de cast dans la grammaire ? *)
| POINTDEXCLAMATION e = expr  { Prefix (position () , Not , e) }
| MINUS e = expr %prec uminus { Prefix (position () , UMinus, e) }
(* de même il faut distinguer par le nom le minus unaire du binaire *)
| e1 = expr op = operateur e2 = expr { Infix (position () , op , e1 , e2) }
| LP t = typ RP e = expr (* Conversion automatique : oublié de l'Ast ? *)
| e = expr INSTANCEOF t = typ { Instanceof (position () , e , t) }
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


