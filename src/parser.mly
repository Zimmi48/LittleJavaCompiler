
%{
  open Ast.Past
  open Lexing

  let position startpos endpos =
    (* actuellement on se prive de certaines infos *)
    { file = startpos.pos_fname ;
      line = startpos.pos_lnum ;
      fChar = startpos.pos_cnum ;
      lChar = endpos.pos_cnum }

  type decl = | Attribut of variable
              | Const of callable
              | Method of callable
      
  (* utile à l'intérieur du parser *)
  type classouinstr = Class of classe | Instr of instruction list

  let rec attrsAndConstsAndMethods_of_decls = function
    | [] -> [] , [] , []
    | Attribut a :: t ->
        let la , lc , lm = attrsAndConstsAndMethods_of_decls t in
        a :: la , lc , lm
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
%token CLASS_MAIN MAIN LBRACKET RBRACKET
%token SYSTEM OUT PRINT
%token EOF 
%token EQ
%token LP RP (* parenthèses *)
%token LB RB (* accolades : braces *)
%token PLUS MINUS TIMES DIV MOD
%token PLUSPLUS MINUSMINUS
%token ISEQ NEQ LT LEQ GT GEQ AND OR
%token NOT
%token DOT
%token COMMA SEMICOLON

/* Définitions des priorités et associativités des tokens */

%right EQ
%left OR
%left AND
%left ISEQ NEQ
%left LT LEQ GT GEQ INSTANCEOF
%left PLUS MINUS
%left TIMES DIV MOD
%right PLUSPLUS MINUSMINUS uminus NOT cast
%left DOT
%nonassoc atome

/* Point d'entrée de la grammaire */
%start fichier

/* Type des valeurs retournées par l'analyseur syntaxique */
%type <Ast.Past.prog> fichier

%%

fichier:
l = classe* EOF {
  let rec checkClasses = function
    | [Instr m] -> [] , m
    | Class c :: t -> let l , m = checkClasses t in c :: l , m
    | _ -> raise (ClassMain (position $startpos $endpos)) in
  let l , m = checkClasses l in
  { classes = l ; instr = Block m } }
;

classe:
  | CLASS id = IDENT e = extends? LB d = decl* RB
    {
      let a, c, m = attrsAndConstsAndMethods_of_decls d in
      Class {
        class_pos = position $startpos $endpos ;
        class_name = id ;
        class_extends = e ;
        class_attrs = a ;
        class_consts = c ;
        class_methods = m }
    }
  | CLASS CLASS_MAIN
      LB
      PUBLIC STATIC VOID MAIN LP st = IDENT IDENT LBRACKET RBRACKET RP
    LB l = instructions RB
    RB
    { if st = "String" then Instr l
      else raise (ClassMain (position $startpos $endpos)) }
;

extends:
  EXTENDS id = IDENT { id , position $startpos $endpos }
;

decl:
| var = variable SEMICOLON
    { Attribut var }
| c = constructeur { Const c }
| m = methode { Method m }
;

constructeur:
| id = IDENT
    LP p = separated_list(COMMA, variable) RP LB i = instructions RB
    {
      { call_pos = position $startpos $endpos ;
        call_returnType = Void ;
        call_name = id ;
        call_params = p ;
        call_body = Block i }
    }
;

methode:
| t = typ id = IDENT
    LP p = separated_list(COMMA, variable) RP LB i = instructions RB
    {
      { call_pos = position $startpos $endpos ;
        call_returnType = t ;
        call_name = id ;
        call_params = p ;
        call_body = Block i }
    }
| VOID id = IDENT
    LP p = separated_list(COMMA, variable) RP LB i = instructions RB
    {
      { call_pos = position $startpos $endpos ;
        call_returnType = Void ;
        call_name = id ;
        call_params = p ;
        call_body = Block i }
    }

variable:
t = typ id = IDENT
    { {v_type = t ; v_name = id ; v_pos = position $startpos $endpos } }

typNatif:
| BOOLEAN    { Bool }
| INT        { Int }

typ:
| t = typNatif { t }
| id = IDENT { C id }

(* Définition recoupée en nombreux non terminaux des expressions *)
expr:
| i = INT_CST    { Iconst (position $startpos $endpos , i) }
| s = STRING_CST { Sconst (position $startpos $endpos , s) }
| TRUE           { Bconst (position $startpos $endpos , true) }
| FALSE          { Bconst (position $startpos $endpos , false) }
| NULL           { Null (position $startpos $endpos) }
| NOT e = expr   { Not (position $startpos $endpos, e) }
| MINUS e = expr %prec uminus { UMinus (position $startpos $endpos, e) }
| op = opPrefPost e = expr    { Pref (position $startpos $endpos , op , e) }
| e = expr op = opPrefPost    { Post (position $startpos $endpos , op , e) }
| e1 = expr op = opInfix e2 = expr
    { Binaire (position $startpos $endpos , op , e1 , e2) }
| LP t = typNatif RP f = expr %prec cast
    { Cast (position $startpos $endpos , t , f) }
| LP e = expr RP f = expr %prec cast
    { match e with
      | Getval (_ , Var id) -> Cast (position $startpos $endpos , C id , f)
      | _ -> raise (PasUnType (position $startpos $endpos)) }
| e1 = expr EQ e2 = expr { Assign (position $startpos $endpos , e1 , e2) }
| e = expr DOT id = IDENT LP l = separated_list(COMMA, expr) RP
       { Call (position $startpos $endpos , Some e , id , l) }
| id = IDENT LP l = separated_list(COMMA, expr) RP
       { Call (position $startpos $endpos , None , id , l) }
| THIS { Getval (position $startpos $endpos , Var "this") }
(* l'Ast ne gère pas ce type d'accès séparément. On le traite comme les autres à l'environnement de faire la différence *)
| a = acces
    { Getval (position $startpos $endpos , a) }
| e = expr INSTANCEOF t = typ
    { Instanceof (position $startpos $endpos , e , t) }
| NEW id = IDENT LP l = separated_list(COMMA, expr) RP
    { New (position $startpos $endpos , id , l) }
| SYSTEM DOT OUT DOT PRINT LP e = expr RP
    { Print (position $startpos $endpos, e) }
| LP e = expr RP %prec atome { e }

%inline opInfix:
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

%inline opPrefPost:
| PLUSPLUS { Incr }
| MINUSMINUS { Decr }

acces:
| id = IDENT { Var id }
| e = expr DOT id = IDENT { Attr (e , id) }

instructions:
| 	                           { [] }
| SEMICOLON l = instructions       { l }
| i = instruction l = instructions { i::l }
;

instruction:
| i = instructionSansFinParIfSolo { i }
| IF LP e = expr RP
    i = instructionSansFinParIfSolo
    { If (e , i , None) }
| FOR	LP
		e1 = expr? SEMICOLON
		e2 = expr? SEMICOLON
		e3 = expr?
	RP
	IF LP e = expr RP
                i = instructionSansFinParIfSolo
    { For (e1 , e2 , e3 , Some (If (e , i , None))) }
;

instructionSansFinParIfSoloOption:
| SEMICOLON { None }
| i = instructionSansFinParIfSolo { Some i }
;

instructionSansFinParIfSolo:
| e = expr SEMICOLON { Expr e }
| t = typ id = IDENT e = option (preceded (EQ, expr)) SEMICOLON
    { Decl (position $startpos $endpos , t , id , e) }
| IF LP e = expr RP
    i1 = instructionSansFinParIfSolo
    ELSE i2 = instructionSansFinParIfSoloOption
    { If (e , i1 , i2) }
| FOR	LP
		e1 = expr? SEMICOLON
		e2 = expr? SEMICOLON
		e3 = expr?
	RP
	i = instructionSansFinParIfSoloOption
    { For (e1 , e2 , e3 , i) }
| LB l = instructions RB { Block l }
| RETURN e = expr? SEMICOLON { Return(position $startpos $endpos,e) }
