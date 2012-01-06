(** Typage statique *)
(*open Ast

module Cmap = Map.Make(String)
module Cset = Set.Make(String)


(** L'ensemble des exceptions de l'analyse sémantique *)
module Exceptions = struct 
  open Past
  (** Classe déjà définie (position, ident, position de la définition antèrieure *)
  exception AlreadyDefined of pos * ident * pos
     
  (** identifiant non définie *)
  exception Undefined of pos * ident
      
  (** erreur d'Héritage *)
  exception Her of pos * ident * string
     
  (** mauvais type, optionnellement le type attendu *)
  exception WrongType of pos * types * types option

end

  
(** Définition des classes et vérification d'unicité *)
module ClassAnalysis = struct
  open Exceptions
   
  open Past

  (** construit une Map de classes en vérifiant l'unicité du nommage *)
  let buildClassMap prog =
    let addClass map c =
      if Cmap.mem c.class_name map then
	let c1 = Cmap.find c.class_name map in
	raise (AlreadyDefined (c.class_pos,c.class_name,c1.class_pos))
      else
	Cmap.add c.class_name c map in
    List.fold_left addClass Cmap.empty prog.classes 

  (** vérifie que l'héritage est correct et cohérent (aka sans cycle) *)
  let checkHerit cmap =
    (** parcourt les parents de le classe
	@param c la classe à partir de laquelle on parcourt
	@param graySet L'ensemble des classes découvertes actuellement
	@param blackSet l'ensemble des classes déjà traitées *)
    let rec dfsVisit c graySet blackSet = 
      if Cset.mem c.class_name blackSet then 
	graySet
      else
	if Cset.mem c.class_name graySet then 
	  raise (Her(c.class_pos,c.class_name,"Cycle")) 
	else
	  match c.class_extends with
            | None -> failwith "dfsVisit : doit-on déjà hériter au moins de Object ?"
	    | Some ("String" , pos) -> raise (Her(pos,c.class_name,"Cannot herite of string"))
	    | Some ("Object", _) -> graySet
	    | Some (c1name, pos) -> 
	      let c1 = 
		try 
		  Cmap.find c1name cmap 
		with Not_found -> raise (Undefined(pos,c1name))
	      in
	      dfsVisit c1 (Cset.add c1name graySet) blackSet
    in
    Cmap.fold (fun _ c black -> Cset.union (dfsVisit c Cset.empty black) black) cmap Cset.empty    
end

module CheckClass = struct
  open Exceptions
  open Past
  open Sast

  (** calcule si une classe c1 hérite d'une autre classe c2 *)
  let rec isSubClass classes c1 c2 =
    match c1.class_extends with
      | None -> failwith "Horreur en isSubClass"
      | Some (extends,_) ->
        if c2.class_name = extends then true
        else
          match c2.class_extends with
            | None -> failwith "Horreur en isSubClass"
            | Some ("Object", _) -> false
	    | Some (c,_) -> isSubClass classes c1 (Cmap.find c classes)

  
  (** calcul si t1 est un sous type de t2 *)
  let isSubType classes t1 t2 = 
    match t1,t2 with
      | STypeNull,_ -> true
      | SBool,_ | SInt,_ -> (t1 = t2)
      | (SC i1),(SC i2) -> isSubClass (Cmap.find i1 classes) (Cmap.find i2 classes) (* erreur d'utilisation de isSubClass *)

  (** calcule si un profil de type p1 est un sous profil de p2*)      
  let profIsSubType cmap p1 p2 = 
    List.for_all2 (isSubType  cmap ) p1 p2 

  (** vérifie si le type est bien formé *)
  let isBF classes = function
    | Int | Bool -> true
    | C "Object" | C "String" -> true
    | C c -> Cmap.mem c classes
      
  (** vérfie l'unicité des champs et que leurs types sont bien formés
      renvoit la map des atributs *)
  let checkAttr classes c = 
    let aux accmap  attr = 
      let name = attr.v_name in
      if Cmap.mem name accmap then 
	let a = Cmap.find name accmap in
	raise (AllreadyDefined(attr.pos,name,a.pos))
      else
	if not (isBF classes attr.v_type) then 
	  raise (WrongType(attr.pos,attr.v_type,None))
	else
	  Cmap.add name attr accmap
    in
    List.fold_left aux Cmap.empty c.attrs 
      
  (** vérifie les constructeurs *)
  (**let checkConst *)
end	

module  CheckInstr = struct 
    
  open Past
  open Sast
  open Exception

  let isLeft = function
    | Getval _ -> true
    | _ -> false
  inw


  let rec typExpr env = function
    | Iconst (p,i) -> { sv = SIconst (p,i) ; st = SInt }
    | Sconst (p,i) -> { sv = SSconst (p,i) ; st = SC ("String")}
    | Bconst (p,i) -> { sv = SBconst (p,i) ; st = SBool }
    | Null p -> { sv = SNull p ; st = STypeNull }
    | Getval (p,Var "this") -> (
      try let t = Cmap.find "this" env in
	  { sv = SGetval(p,SVar "this"), st = t }
      with Not_found -> raise (Undefined (p,"this"))
    )
(*    |

  let rec typInstr env = 
    | 
*)

end
*)
