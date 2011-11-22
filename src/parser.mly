
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
%token LP RP (* parenth�ses *)
%token LB RB (* accolades : braces *)
%token LBRACKET RBRACKET
(* crochets : sert � rien, sauf pour d�f du main *)
%token PLUS MINUS TIMES DIV MOD
%token PLUSPLUS MINUSMINUS
%token ISEQ NEQ LT LEQ GT GEQ AND OR
%token POINTDEXCLAMATION (* changer ce nom pourri *)
%token DOT
%token COMMA SEMICOLON

/* D�finitions des priorit�s et associativit�s des tokens */

%right EQ
%left OR
%left AND
%left ISEQ NEQ
%left LT LEQ GT GEQ INSTANCEOF
%left PLUS MINUS
%left TIMES DIV MOD
%right uminus PLUSPLUS MINUSMINUS POINTDEXCLAMATION CAST
%left DOT


/* Point d'entr�e de la grammaire */
%start fichier

/* Type des valeurs retourn�es par l'analyseur syntaxique */
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
        extends =  e ; (* h�ritage simple: dans Ast, remplacer list par option*)
        attrs = a ;
        consts = c ; (* mise au pluriel : corriger Ast *)
        methods = m }
    }
;

extends:
  LP EXTENDS id = IDENT RP { id , position () }
;

decl:
| t = typ id = IDENT SEMICOLON { Attr ( (t, id) , position () ) }
| c = constructeur { Const c }
| m = methode { Method m }
;

constructeur:
| id = IDENT
    LP p = separated_list(COMMA, parametre) RP LB i = instructions RB
    {
      { pos = position () ;
        returnType = Void ;
        name = id ;
        params = l ;
        body = i }
    }
;

methode:
| t = typ id = IDENT
    LP p = separated_list(COMMA, parametre) RP LB i = instructions RB
    {
      { pos = position () ;
        returnType = t ;
        name = id ;
        params = p ;
        body = i }
    }
| VOID id = IDENT
    LP p = separated_list(COMMA, parametre) RP LB i = instructions RB
    {
      { pos = position () ;
        returnType = Void ;
        name = id ;
        params = p ;
        body = i }
    }
;

parametre:
t = typ id = IDENT { t, id }
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
	{ if cMain = "Main" & main = "main" & typ = C "String" then
	 	l
	 else failwith "Erreur parser: classe_Main"}
(* un moyen de ne parser que les cas qui nous int�ressent serait plus
   coh�rent avec une mani�re centralis�e de g�rer les erreurs de parsage *)
;

expr:
| i = INT_CST    { Iconst (position () , i) }
| s = STRING_CST { Sconst (position () , s) }
| TRUE           { Bconst (position () , true) }
| FALSE          { Bconst (position () , false) }
| THIS { Getval (position () , Var "this") }
(* l'Ast ne g�re pas ce type d'acc�s s�par�ment. On le traite comme les autres
   � l'environnement de faire la diff�rence *)
| NULL           { Null (position ()) }
| LP e = expr RP { e }
| a = acces      { Getval (position () , a) }
| a = acces EQ e = expr { Assign (position () , a , e) }
| a = acces LP l = separated_list(COMMA, expr) RP
    { Call (position () , a , l) }
| NEW id = IDENT LP l = separated_list(COMMA, expr) RP
    { New (position () , id , l) }
| PLUSPLUS e = expr   { Prefix (position () , PrefixIncr , e) }
(* Est-il vraiment n�cessaire de diff�rencier les op�rateurs pr�fixes,
   infixes, postfixes dans l'Ast ? Dans ce cas Incr ne peut pas �tre �
   la fois une valeur de type prefix et postfix. Il me para�t de toute
   fa�on plus judicieux de se contenter de diff�rencier les op�rateurs
   en fonction de leur arit� (unaire, binaire) *)
| MINUSMINUS e = expr
| e = expr PLUSPLUS
| e = expr MINUSMINUS
(* Oubli du prof de cast dans la grammaire ? *)
| POINTDEXCLAMATION e = expr  { Prefix (position () , Not , e) }
| MINUS e = expr %prec uminus { Prefix (position () , UMinus, e) }
(* de m�me il faut distinguer par le nom le minus unaire du binaire *)
| e1 = expr op = operateur e2 = expr { Infix (position () , op , e1 , e2) }
| LP t = typ RP e = expr (* Conversion automatique : oubli� de l'Ast ? *)
| e = expr INSTANCEOF t = typ { Instanceof (position () , e , t) }
;

instructions:
| i = instruction	           { [i] }
| SEMICOLON l = instructions    { l }
| i = instruction l = instructions { i::l }
;
(*
instructionOption:
| SEMICOLON { None }
| i = instruction { Some i }
 
Attention : on a le droit d'apr�s les sp�cifications aux instructions
  IF (expr) instruction ELSE SEMICOLON qui �quivaut � IF (expr) instruction
  et
  FOR (expr? ; expr? ; expr?) SEMICOLON
  Donc deux solutions impliquant une modif de l'Ast sont possibles :
  - ajout comme sugg�r� par la grammaire d'une instruction vide
  (beaucoup plus simple � g�rer au niveau du parser)
  - modif de For pour prendre un Instr option au lieu d'un Instr

*)
instruction:
| e = expr SEMICOLON { Expr e }
| t = typ id = IDENT e = option (preceded (EQ, expr)) SEMICOLON
    { Decl (position () , t , id , e) }
| IF LP e = expr RP
    i1 = instruction
    i2 = option (preceded (ELSE , instruction))
(* � modifier : ne g�re pas le cas IF (expr) instruction ELSE SEMICOLON *)
    { If (e , i1 , i2) }
| FOR	LP
		e1 = expr? SEMICOLON
		e2 = expr? SEMICOLON
		e3 = expr?
	RP
	i = instruction
(* � modifier : ne g�re pas le cas FOR (expr? ; expr? ; expr?) SEMICOLON *)
    { For (e1 , e2 , e3 , i) }
| LB l = instructions RB { Block l }
| RETURN e = expr? { Return e }
;

acces:
| id = IDENT { Var id }
| e = expr DOT id = IDENT { Attr (e , id) }
(* Attr me para�t �tre un mauvais nom car il ne montre pas qu'il s'ag�t
   d'acc�der � une m�thode ou un attribut d'un objet
   De m�me pourquoi pr�f�rer le nom vars � acces *)
;

%inline operateur:
| ISEQ { Eq }
| NEQ { Neq }
| LT { Lt }
| LEQ { Leq }
| GT { Gt }
| GEQ { Geq }
| PLUS { Plus }
| MINUS { Minus }
| TIMES { Star }
| DIV { Div }
| MOD { Mod }
| AND { And }
| OR { Or }
;


