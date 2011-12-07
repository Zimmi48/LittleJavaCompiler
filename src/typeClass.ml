(** Typage statique *)
open Ast

module Cmap = Map.Make(String)
module Cset = Set.Make(String)


(** L'ensemble des exceptions de l'analyse sémantique *)
module Exceptions = struct 
  open Past
  (** Classe déjà définie (position, ident, position de la définition antèrieure *)
  exception AllreadyDefined of pos * ident * pos
     
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
      if Cmap.mem c.name map then
	let c1 = Cmap.find c.name map in
	raise (AllreadyDefined (c.pos,c.name,c1.pos))
      else
	Cmap.add c.name c map in
    List.fold_left addClass Cmap.empty prog.classes 

  (** vérifie que l'héritage est correct et cohérent (aka sans cycle) *)
  let checkHerit cmap =
    (** parcours les parents de le classe
	@param c la classe à partir de laquelle on parcours 
	@param graySet L'ensemble des classes découvertes actuellement
	@param blackSet l'ensemble des classes déjà traitées *)
    let rec dfsVisit c graySet blackSet = 
      if Cset.mem c.name blackSet then 
	graySet
      else
	if Cset.mem c.name graySet then 
	  raise (Her(c.pos,c.name,"Cycle")) 
	else
	  match (fst c.extend) with
	    | "String" -> raise (Her(c.pos,c.name,"Cannot herite of string"))
	    | "Object" -> graySet
	    | c1name -> 
	      let c1 = 
		try 
		  Cmap.find c1name cmap 
		with Not_found -> raise (Undefined((snd c.extend),c1name))
	      in
	      dfsVisit c1 (Cset.add c1name graySet) blackSet
    in
    Cmap.fold (fun _ c black -> Cset.union (dfsVisit c Cset.empty black) black) cmap Cset.empty    
end

module CheckClass = struct
  open Exceptions
  open Past
  (** calcule si une classe c1 hérite d'une autre classe c2 *)
  let rec isSubClass classes c1 c2 =
    if c2.name = (fst c1.extend) then true else
      match (fst c2.extend) with
	| "Object" -> false
	| c -> isSubClass classes c1 (Cmap.find c classes)

  
  (** calcul si t1 est un sous type de t2 *)
  let isSubType classes t1 t2 = 
    match t1,t2 with
      | Sast.TypeNull,_ -> true
      | Sast.Bool,_ | Sast.Int,_ -> (t1 = t2)
      | (Sast.C i1),(Sast.C i2) -> isSubClass (Cmap.find i1 classes) (Cmap.find i2 classes)

  (** calcule si un profile de type p1 est un sous profile de p2*)      
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
  open  Exception

  let isLeft = function
    | Getval _ -> true
    | _ -> false
  inw


  let rec typExpr env =
    | Iconst (p,i) -> { Sast.v = Sast.Iconst (p,i) ; t = Sast.Int }
    | Sconst (p,i) -> { Sast.v = Sast.Sconst (p,i) ; t = Sast.C ("String")}
    | Bconst (p,i) -> { Sast.v = Sast.Bconst (p,i) ; t = Sast.Bool }
    | Null p -> { Sast.v = Sast.Null p ; t = Sast.TypeNull }
    | Getval (p,Var "this") -> (
      try let t = Cmap.find "this" env in
	  { Sast.v = Sast.Getval(p,Sast.Var "this"), t = t }
      with Not_found -> raise (Undefined (p,"this"))
    )
    |

  let rec typInstr env = 
    | 
