(* Arbre de syntax abstraite *)

(** Syntaxe issue du parsage *)
module Past = struct
(** Position dans le fichier 
    @param file nom du fichier
    @param line numéros de la ligne
    @param fChar premier charactère
    @param lChar dernier charactère *)
type pos = { 
  file : string;
  line : int;
  fChar: int;
  lChar : int;
}
(** Les idents sont des chaines *)
type ident = string

(** Représentation des types de Petit Java *)
type types =
  | Bool
  | Int
  | C of string 

(** Opérateurs infixes sur les expressions *)
type infix =
  | Eq | Neq | Leq | Geq | Lt | Gt 
  | Plus | Minus | Star | Div 
  | Mod 
  | And | Or
    
(** Opérateurs préfixés sur les expressions *)
type prefix =
  | Incr | Decr
  | Not
  | Minus
      
(** Opérateurs postfixés sur les expressions *)
type postfix =
  | Incr
  | Decr
    
(** Appels à des variables, méthodes, et attributs *)
type vars =
  | Var of ident
  | Attr of expr * ident 

(** Représente la grammaire des expressions, le premier paramètre de chaque constructeur est la position *)
and expr = 
  | Iconst of pos * int
  | Sconst of pos * string
  | Bconst of pos * bool
  | Null of pos
  | Prefix of pos * prefix * expr
  | Postfix of pos *postfix * expr
  | Infix of pos * infix * expr * expr
  (** caste l'expression *)
  | Cast of pos * types * expr
  (** assigne expr *)
  | Assign of pos * vars * expr
  (** Appel d'une méthode, les paramètres sont stockés dans la liste *)
  | Call of pos * vars * expr list
  (** Accès a une variable (au sens large) *)
  | Getval of pos * vars  
  (** expression booléene, vrai si expr est une instance de types *)
  | Instanceof of pos * expr * types
  (** opérateur d'instanciation de la classe ident
      les paramètres du constructeur sont pasés sous forme de liste *)
  | New of pos * ident * expr list 

(** Repérsente la grammaire des instructions *)
type instruction  = 
  | Expr of expr (* pas besoin de pos, c'est la même que celle de l'expression *)
  (** Déclaration d'une variable, avec éventuellement une initialisation
      Le paramètre de type expr option contient l'initialisation éventuelle *)
  | Decl of pos * types * ident * expr option
  (** Branchement conditionnel, s'il y a une clause "else", elle se trouve dans le paramètre optionnel *)
  | If of  expr * instruction * instruction option
  (**Boucle for, sémantique classique *)
  | For of expr option * expr option * expr option * instruction
  | Block of instruction list
  (** Expression optionnelles, pour les méthodes void *)
  | Return of expr option

(** Les variables associèes à leur type *)
type variable = types * ident
(** les méthodes et constructeurs *)
type callable = {
  pos : pos;
  returnType : types;
  name : ident;
  params : variable list;
  body : instruction list;
}
    
(** représentation d'une classe *)
type classe = {
  pos : pos ;
  (** Le nom de la classe*)
  name : ident ;
  (** Liste des relations d'héritages *)
  extend : (ident * pos);
  (** Les attributs, sous forme de paires *)
  attrs : (variable * pos) list;
  (** les constructeurs (ils peuvent être surchargés), le champ returnType est toujours nul*)
  const : callable list;
  (** la liste des méthodes, pouvant avoir des noms identiques *)
  methods : callable list;
}

(** representation d'un programme petit java *)
type prog = {
  (** liste des classes *)
  classes : classe list ;
  (** liste des instructions dans main*)
  instr : instruction list ;
}
end

(** Syntaxe issue de l'analyse de portée et du typage *)
module Sast = struct

(** Position dans le fichier 
    @param file nom du fichier
    @param line numéros de la ligne
    @param fChar premier charactère
    @param lChar dernier charactère *)
type pos = { 
  file : string;
  line : int;
  fChar: int;
  lChar : int;
}
(** Représentation des types de Petit Java *)
type types =
  | TypeNull
  | Void 
  | Bool
  | Int
  | C of string 

(** Les idents contiennent les infos de typage *)
type ident = { ident : string; t : types }

(** Opérateurs infixes sur les expressions *)
type infix =
  | Eq | Neq | Leq | Geq | Lt | Gt 
  | Plus | Minus | Star | Div 
  | Mod 
  | And | Or
    
(** Opérateurs préfixés sur les expressions *)
type prefix =
  | Incr | Decr
  | Not
  | Minus
      
(** Opérateurs postfixés sur les expressions *)
type postfix =
  | Incr
  | Decr
    
(** Appels à des variables, méthodes, et attributs *)
type vars =
  | Var of ident
  | Attr of expr * ident 

(** Représente la grammaire des expressions, le premier paramètre de chaque constructeur est la position *)
and expr = 
  | Iconst of pos * int
  | Sconst of pos * string
  | Bconst of pos * bool
  | Null of pos
  | Prefix of pos * prefix * expr
  | Postfix of pos *postfix * expr
  | Infix of pos * infix * expr * expr
  (** caste l'expression *)
  | Cast of pos * types * expr
  (** assigne expr *)
  | Assign of pos * vars * expr
  (** Appel d'une méthode, les paramètres sont stockés dans la liste *)
  | Call of pos * vars * expr list
  (** Accès a une variable (au sens large) *)
  | Getval of pos * vars  
  (** expression booléene, vrai si expr est une instance de types *)
  | Instanceof of pos * expr * types
  (** opérateur d'instanciation de la classe ident
      les paramètres du constructeur sont pasés sous forme de liste *)
  | New of pos * ident * expr list 

(** Repérsente la grammaire des instructions *)
type instruction  = 
  | Expr of expr (* pas besoin de pos, c'est la même que celle de l'expression *)
  (** Déclaration d'une variable, avec éventuellement une initialisation
      Le paramètre de type expr option contient l'initialisation éventuelle *)
  | Decl of pos * types * ident * expr option
  (** Branchement conditionnel, s'il y a une clause "else", elle se trouve dans le paramètre optionnel *)
  | If of  expr * instruction * instruction option
  (**Boucle for, sémantique classique *)
  | For of expr option * expr option * expr option * instruction
  | Block of instruction list
  (** Expression optionnelles, pour les méthodes void *)
  | Return of expr option

(** Les variables associèes à leur type *)
type variable = types * ident
(** les méthodes et constructeurs *)
type callable = {
  pos : pos;
  returnType : types;
  name : ident;
  params : variable list;
  body : instruction list;
}
    
(** représentation d'une classe *)
type classe = {
  pos : pos ;
  (** Le nom de la classe*)
  name : ident ;
  (** Liste des relations d'héritages *)
  extend : (ident * pos);
  (** Les attributs, sous forme de paires *)
  attrs : (variable * pos) list;
  (** les constructeurs (ils peuvent être surchargés), le champ returnType est toujours nul*)
  const : callable list;
  (** la liste des méthodes, pouvant avoir des noms identiques *)
  methods : callable list;
}

end
