
(* Arbre de syntaxe abstraite *)
module General = struct

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

  (** Opérateurs binaires (toujours infixes) sur les expressions *)
  type binaire =
    | Eq | Neq | Leq | Geq | Lt | Gt 
    | Plus | Minus | Star | Div 
    | Mod 
    | And | Or
        
  (** Opérateurs unaires prefixes et postfixes sur les expressions *)
  type prefpost = Incr | Decr

end

module Cmap = Map.Make(String)

(** Syntaxe issue du parsage *)
module Past = struct

  include General

  (** Exception à usage dans le parser *)
  exception ClassMain of pos
  exception PasUnType of pos
  exception CommentaireNonTermine

  (** Les idents sont des chaines *)
  type ident = string
      
  (** Représentation des types de Petit Java *)
  type types =
    | Void
    | Bool
    | Int
    | C of string 
        
  (** Valeurs gauches *)
  type vars =
    | Var of ident
    | Attr of expr * ident

  (** Représente la grammaire des expressions, le premier paramètre de chaque constructeur est la position *)
  and expr = 
    | Iconst of pos * int
    | Sconst of pos * string
    | Bconst of pos * bool
    | Null of pos
    | Not of pos * expr
    | UMinus of pos * expr
    | Pref of pos * prefpost * expr
    | Post of pos * prefpost * expr
    | Binaire of pos * binaire * expr * expr
    (** caste l'expression *)
    | Cast of pos * types * expr
    (** assigne expr *)
    | Assign of pos * expr * expr
    (** Appel d'une méthode, les paramètres sont stockés dans la liste 
        expr option : la classe, si None, alors classe courante 
        ident : le nom de la méthode *)
    | Call of pos * expr option * ident * expr list
    (** Accès a une variable (au sens large) *)
    | Getval of pos * vars  
    (** expression booléene, vrai si expr est une instance de types *)
    | Instanceof of pos * expr * types
    (** opérateur d'instanciation de la classe ident
        les paramètres du constructeur sont pasés sous forme de liste *)
    | New of pos * ident * expr list
    (** System.Print.Out(e) *)
    | Print of pos * expr 

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
  type variable = {v_type : types ; v_name : ident; v_pos : pos }

  (** les méthodes et constructeurs *)
  type callable = {
    call_pos : pos;
    call_returnType : types;
    call_name : ident;
    call_params : variable list;
    call_body : instruction;
  }
      
  (** représentation d'une classe *)
  type classe = {
    class_pos : pos ;
    (** Le nom de la classe*)
    class_name : ident ;
    (** Liste des relations d'héritages *)
    class_extends : (ident * pos) option;
    (** Les attributs, sous forme de paires *)
    class_attrs : variable list;
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
    instr : instruction;
  }

end

(** syntaxe issue de l'analyse et du typage des classes *) 
module Oast = struct 
  include Past 
      
  (** Les méthodes/constructeurs dans le descripteur *)
  type oSimple = {
    osimple_pos : pos ;
    osimple_id : int ;
    osimple_classe : string ;
    osimple_n : int;
    osimple_params : variable list;
    osimple_name : string;
    osimple_returnType : types;
  }
      
  let osimplEmpty =  {
    osimple_pos = { 
    file = "";
    line = 0;
    fChar = 0;
    lChar = 0;
    } ;
    osimple_id = 0;
    osimple_classe ="";
    osimple_n = 0;
    osimple_params = [];
    osimple_name = "";
    osimple_returnType = Void }
      
  type ocallable = { 
    ocall_params : variable list ;
    ocall_body : instruction;
  }
      
  (** représentation d'une classe *)
  type oclasse = {
    oclass_pos : pos ;
    (** Le nom de la classe*)
    oclass_name : ident ;
    (** relations d'héritages *)
    oclass_extends : (ident*pos) option;
    (** Les attributs, sous forme d'une Map qui à chacun associe une variable *)
    oclass_attrs : variable Cmap.t;
    (** descripteur de classe *)
    oclass_methodesdesc : oSimple array;
    (** descripteur des constructeur *)
    oclass_constsdesc : oSimple array;    
  }

  type oprog = {
    (** tableau des méthodes/map *)
    omeths : ocallable array;
    (** Map des classes *)
    oclasses : oclasse Cmap.t ;
    (** liste des instructions dans main*)
    oinstr : instruction;
  }
end

(** Syntaxe issue de l'analyse de portée et du typage *)
module Sast = struct

  include General

  (** Représentation des types (au sens large) de Petit Java *)
  type stypes =
    | STypeNull
    | SVoid 
    | SBool
    | SInt
    | SC of string

  (** Les idents contiennent les infos de typage *)
  type sident = { id_id : string; id_typ : stypes }
      
  (** Valeurs gauches *)
  type svars =
    | SVar of sident
    (** le expr de la classe * l'ident de l'attribu *)
    | SAttr of sexpr * sident 

  (** les méthodes et constructeurs *)
  and scallable = {
    scall_returnType : stypes;
    scall_params : sident list;
    scall_body : sinstruction;
  }

  (** Représente la grammaire des expressions, le premier paramètre de chaque constructeur est la position *)
  and expr_v = 
    | SIconst of int 
    | SSconst of string 
    | SBconst of bool 
    | SNull
    | SNot of sexpr
    | SUMinus of sexpr
    | SPref of prefpost * svars
    | SPost of prefpost * svars
    | SBinaire of binaire * sexpr * sexpr 
    (** caste l'expression *)
    | SCast of stypes * sexpr 
    (** assigne expr *)
    | SAssign of svars * sexpr 
    (** Appel d'une méthode, les paramètres sont stockés dans la liste *)
    | SCall of sexpr * int * sexpr list 
    (** Accès a une variable (au sens large) *)
    | SGetval of svars 
    (** expression booléene, vrai si expr est une instance de types *)
    | SInstanceof of sexpr * stypes 
    (** opérateur d'instanciation de la classe ident
        les paramètres du constructeur sont pasés sous forme de liste *)
    | SNew of sident * int * sexpr list
    (** System.print.out(e) *)
    | SPrint of sexpr 

  and sexpr = { sv : expr_v; st : stypes ; sp : pos }

  (** Repérsente la grammaire des instructions *)
  and sinstruction  = 
    | SExpr of sexpr (* pas besoin de pos, c'est la même que celle de l'expression *)
    (** Déclaration d'une variable, avec éventuellement une initialisation
        Le paramètre de type expr option contient l'initialisation éventuelle *)
    | SDecl of pos * stypes * sident * sexpr option
    (** Branchement conditionnel, s'il y a une clause "else", elle se trouve dans le paramètre optionnel *)
    | SIf of  sexpr * sinstruction * sinstruction option
    (**Boucle for, sémantique classique *)
    | SFor of sexpr option * sexpr * sexpr option * sinstruction option
    | SBlock of sinstruction list
    (** Expression optionnelles, pour les méthodes void *)
    | SReturn of sexpr option
	
  (** représentation d'une classe *)
  type sclasse = {
    sclass_pos : pos ;
    (** Le nom de la classe*)
    sclass_name : string ;
    (** Liste des relations d'héritages *)
    sclass_extends : sident option;
    (** Les attributs *)
    sclass_attrs : sident Cmap.t;
    (** les constructeurs (ils peuvent être surchargés), le champ returnType est toujours nul*)
    sclass_consts : int array;
    (** le tableau des méthodes *)
    sclass_methods : int array;
  }

  (** representation d'un programme petit java *)
  type sprog = {
    (** liste des classes *)
    sclasses : sclasse Cmap.t ;
    (** Tableau des méthodes **)
    smeths : scallable array;
    (** liste des instructions dans main*)
    sinstr : sinstruction ;
  }

end
