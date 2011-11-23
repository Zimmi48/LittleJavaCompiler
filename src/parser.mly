
%{
  open Ast

  let position () = (* actuellement on se prive de certaines infos *)
    { file = "" (*startpos.pos_fname*) ;
      line = 0 (*startpos.pos_lnum*) ;
      fChar = 0 (*startpos.pos_cnum*) ;
      lChar = 0 (*endpos.pos_cnum*) }

  type decl = | Attribut of variable * pos
              | Const of callable
              | Method of callable

  let rec attrsAndConstsAndMethods_of_decls = function
    | [] -> [] , [] , []
    | Attribut (a , pos) :: t ->
        let la , lc , lm = attrsAndConstsAndMethods_of_decls t in
        (a , pos)::la , lc , lm
    | Const c :: t ->
        let la , lc , lm = attrsAndConstsAndMethods_of_decls t in
        la , c::lc , lm
    | Method m :: t ->
        let la , lc , lm = attrsAndConstsAndMethods_of_decls t in
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
%token PLUS MINUS TIMES DIV MOD
%token PLUSPLUS MINUSMINUS
%token ISEQ NEQ LT LEQ GT GEQ AND OR
%token POINTDEXCLAMATION (* changer ce nom pourri *)
%token DOT
%token COMMA SEMICOLON
%token CLASSMAIN (* on traite la classe Main à part *)

/* Définitions des priorités et associativités des tokens */

%right EQ
%left OR
%left AND
%left ISEQ NEQ
%left LT LEQ GT GEQ INSTANCEOF
%left PLUS MINUS
%left TIMES DIV MOD
%right uminus PLUSPLUS MINUSMINUS POINTDEXCLAMATION cast
%left DOT

%nonassoc ifsolo
%nonassoc ifelse

/* Point d'entrée de la grammaire */
%start fichier

/* Type des valeurs retournées par l'analyseur syntaxique */
%type <Ast.classe list * Ast.instruction list> fichier

%%

fichier:
l = classe* m = classe_Main EOF { l , m }
;

classe:
  CLASS id = IDENT e = extends? LB d = decl* RB
    {
      let a, c, m = attrsAndConstsAndMethods_of_decls d in
      {
        class_pos = position() ;
        class_name = id ;
        class_extends =  e ;
        class_attrs = a ;
        class_consts = c ;
        class_methods = m }
    }
;

extends:
  LP EXTENDS id = IDENT RP { id , position () }
;

decl:
| t = typ id = IDENT SEMICOLON { Attribut ( (t, id) , position () ) }
| c = constructeur { Const c }
| m = methode { Method m }
;

constructeur:
| id = IDENT
    LP p = separated_list(COMMA, parametre) RP LB i = instructions RB
    {
      { call_pos = position () ;
        call_returnType = Void ;
        call_name = id ;
        call_params = p ;
        call_body = i }
    }
;

methode:
| t = typ id = IDENT
    LP p = separated_list(COMMA, parametre) RP LB i = instructions RB
    {
      { call_pos = position () ;
        call_returnType = t ;
        call_name = id ;
        call_params = p ;
        call_body = i }
    }
| VOID id = IDENT
    LP p = separated_list(COMMA, parametre) RP LB i = instructions RB
    {
      { call_pos = position () ;
        call_returnType = Void ;
        call_name = id ;
        call_params = p ;
        call_body = i }
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
CLASSMAIN LB l = instructions RB RB { l }
;

expr:
| i = INT_CST    { Iconst (position () , i) }
| s = STRING_CST { Sconst (position () , s) }
| TRUE           { Bconst (position () , true) }
| FALSE          { Bconst (position () , false) }
| THIS { Getval (position () , Var "this") }
(* l'Ast ne gère pas ce type d'accès séparément. On le traite comme les autres
   à l'environnement de faire la différence *)
| NULL           { Null (position ()) }
| LP e = expr RP { e }
| a = acces      { Getval (position () , a) }
| a = acces EQ e = expr { Assign (position () , a , e) }
| a = acces LP l = separated_list(COMMA, expr) RP
    { Call (position () , a , l) }
| NEW id = IDENT LP l = separated_list(COMMA, expr) RP
    { New (position () , id , l) }
| PLUSPLUS e = expr | e = expr PLUSPLUS { Unaire (position () , Incr , e) }
| MINUSMINUS e = expr | e = expr MINUSMINUS { Unaire (position () , Decr , e) }
| POINTDEXCLAMATION e = expr  { Unaire (position () , Not , e) }
| MINUS e = expr %prec uminus { Unaire (position () , UMinus, e) }
| e1 = expr op = operateur e2 = expr { Binaire (position () , op , e1 , e2) }
| LP t = typ RP e = expr %prec cast { Cast (position () , t , e) }
| e = expr INSTANCEOF t = typ { Instanceof (position () , e , t) }
;

instructions:
| i = instruction	           { [i] }
| SEMICOLON l = instructions    { l }
| i = instruction l = instructions { i::l }
;

instructionOption:
| SEMICOLON { None }
| i = instruction { Some i }
 
(*Attention : on a le droit d'après les spécifications aux instructions
  IF (expr) instruction ELSE SEMICOLON qui équivaut à IF (expr) instruction
  et
  FOR (expr? ; expr? ; expr?) SEMICOLON
  Donc deux solutions impliquant une modif de l'Ast sont possibles :
  - ajout comme suggéré par la grammaire d'une instruction vide
  (beaucoup plus simple à gérer au niveau du parser)
  - modif de For pour prendre un Instr option au lieu d'un Instr

*)
instruction:
| e = expr SEMICOLON { Expr e }
| t = typ id = IDENT e = option (preceded (EQ, expr)) SEMICOLON
    { Decl (position () , t , id , e) }
| IF LP e = expr RP
    i = instruction %prec ifsolo
    { If (e , i , None) }
| IF LP e = expr RP
    i1 = instruction
    ELSE i2 = instructionOption %prec ifelse
    { If (e , i1 , i2) }
| FOR	LP
		e1 = expr? SEMICOLON
		e2 = expr? SEMICOLON
		e3 = expr?
	RP
	i = instructionOption
    { For (e1 , e2 , e3 , i) }
| LB l = instructions RB { Block l }
| RETURN e = expr? { Return e }
;

acces:
| id = IDENT { Var id }
| e = expr DOT id = IDENT { Attr (e , id) }
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


