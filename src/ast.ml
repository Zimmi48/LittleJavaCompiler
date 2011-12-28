(* Arbre de syntaxe abstraite *)

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
  | Void
  | Bool
  | Int
  | C of string 

(** Opérateurs binaires (toujours infixes) sur les expressions *)
type binaire =
  | Eq | Neq | Leq | Geq | Lt | Gt 
  | Plus | Minus | Star | Div 
  | Mod 
  | And | Or
    
(** Opérateurs unaires sur les expressions *)
type unaire =
  | Incr | Decr
  | Not
  | UMinus
    
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
  | Unaire of pos * unaire * expr
  | Binaire of pos * binaire * expr * expr
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
  | For of expr option * expr option * expr option * instruction option
  | Block of instruction list
  (** Expression optionnelles, pour les méthodes void *)
  | Return of expr option

(** Les variables associèes à leur type *)
type variable = types * ident
(** les méthodes et constructeurs *)
type callable = {
  call_pos : pos;
  call_returnType : types;
  call_name : ident;
  call_params : variable list;
  call_body : instruction list;
}
    
(** représentation d'une classe *)
type classe = {
  class_pos : pos ;
  (** Le nom de la classe*)
  class_name : ident ;
  (** Liste des relations d'héritages *)
  class_extends : (ident * pos) option;
  (** Les attributs, sous forme de paires *)
  class_attrs : (variable * pos) list;
  (** les constructeurs (ils peuvent être surchargés), le champ returnType est toujours nul*)
  class_consts : callable list;
  (** la liste des méthodes, pouvant avoir des noms identiques *)
  class_methods : callable list;
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
  sfile : string;
  sline : int;
  sfChar: int;
  slChar : int;
}

(** Représentation des types (au sens large) de Petit Java *)
type types =
  | STypeNull
  | SVoid 
  | SBool
  | SInt
  | SC of string 

(** Les idents contiennent les infos de typage *)
type ident = { sident : string; st : types }

(** Opérateurs binaires (toujours infixes) sur les expressions *)
type binaire =
  | SEq | SNeq | SLeq | SGeq | SLt | SGt 
  | SPlus | SMinus | SStar | SDiv 
  | SMod 
  | SAnd | SOr
    
(** Opérateurs unaires sur les expressions *)
type unaire =
  | SIncr | SDecr
  | SNot
  | SUMinus
    
(** Appels à des variables, méthodes, et attributs *)
type vars =
  | SVar of ident
  | SAttr of expr * ident 

(** Représente la grammaire des expressions, le premier paramètre de chaque constructeur est la position *)
and expr_v = 
  | SIconst of pos * int 
  | SSconst of pos * string 
  | SBconst of pos * bool 
  | SNull of pos 
  | SUnaire of pos * unaire * expr 
  | SBinaire of pos * binaire * expr * expr 
  (** caste l'expression *)
  | SCast of pos * types * expr 
  (** assigne expr *)
  | SAssign of pos * vars * expr 
  (** Appel d'une méthode, les paramètres sont stockés dans la liste *)
  | SCall of pos * vars * expr list 
  (** Accès a une variable (au sens large) *)
  | SGetval of pos * vars 
  (** expression booléene, vrai si expr est une instance de types *)
  | SInstanceof of pos * expr * types 
  (** opérateur d'instanciation de la classe ident
      les paramètres du constructeur sont pasés sous forme de liste *)
  | SNew of pos * ident * expr list  

and expr = { sv : expr_v; st : types }

(** Repérsente la grammaire des instructions *)
type instruction  = 
  | SExpr of expr (* pas besoin de pos, c'est la même que celle de l'expression *)
  (** Déclaration d'une variable, avec éventuellement une initialisation
      Le paramètre de type expr option contient l'initialisation éventuelle *)
  | SDecl of pos * types * ident * expr option
  (** Branchement conditionnel, s'il y a une clause "else", elle se trouve dans le paramètre optionnel *)
  | SIf of  expr * instruction * instruction option
  (**Boucle for, sémantique classique *)
  | SFor of expr option * expr option * expr option * instruction option
  | SBlock of instruction list
  (** Expression optionnelles, pour les méthodes void *)
  | SReturn of expr option

(** Les variables associèes à leur type *)
type variable = types * ident
(** les méthodes et constructeurs *)
type callable = {
  scall_pos : pos;
  scall_returnType : types;
  scall_name : ident;
  scall_params : variable list;
  scall_body : instruction list;
}
    
(** représentation d'une classe *)
type classe = {
  sclass_pos : pos ;
  (** Le nom de la classe*)
  sclass_name : ident ;
  (** Liste des relations d'héritages *)
  sclass_extends : (ident * pos) option;
  (** Les attributs, sous forme de paires *)
  sclass_attrs : (variable * pos) list;
  (** les constructeurs (ils peuvent être surchargés), le champ returnType est toujours nul*)
  sclass_consts : callable list;
  (** la liste des méthodes, pouvant avoir des noms identiques *)
  sclass_methods : callable list;
}

end
