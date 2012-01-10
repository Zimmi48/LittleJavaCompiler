(** Typage statique *)
open Ast

module Cset = Set.Make(String)


(** L'ensemble des exceptions de l'analyse sémantique *)
module Exceptions = struct 
  open Sast

  (** Convertit un type Past vers un type Sast *)
  let types_to_Sast = function
    | Past.Void -> SVoid
    | Past.Bool -> SBool
    | Past.Int -> SInt
    | Past.C(s) -> SC(s)

  (** Classe déjà définie (position, ident, position de la définition antèrieure *)
  exception AlreadyDefined of pos * string * pos
     
  (** identifiant non définie *)
  exception Undefined of pos * string
      
  (** erreur d'Héritage *)
  exception Her of pos * ident * string
     
  (** mauvais type, optionnellement le type attendu *)
  exception WrongType of pos * types * types option
      
  (** Utilisation du même nom plusieurs fois *)
  exception Duplicated of pos * string

end

  
(** Définition des classes et vérification d'unicité
    Passe d'ub arbre Past à un arbre Oast *)
module ClassAnalysis = struct
  open Exceptions
  open Oast
  open Past 
    
  (** construit une Map de classes en vérifiant l'unicité du nommage *)
  let buildClassMap prog =
    let addClass map c =
      if Cmap.mem c.class_name map then
	let c1 = Cmap.find c.class_name map in
	raise (AlreadyDefined (c.class_pos,c.class_name,  c1.class_pos))
      else
	Cmap.add c.class_name c map in
    List.fold_left addClass Cmap.empty prog.classes 
   

  (** calcule si une classe c1 hérite d'une autre classe c2 *)
  let rec isSubClass classes c1 c2 =
    match c1.class_extends with
      | None -> if c2.class_name = "Object" then true else false
      | Some (extends,_) ->
        if c2.class_name = extends then true
        else
          match c2.class_extends with
            | None -> false
	    | Some (c,_) -> isSubClass classes c1 (Cmap.find c classes)

  
  (** calcul si t1 est un sous type de t2 *)
  let isSubType classes t1 t2 = 
    match t1,t2 with
      | STypeNull,_ -> true
      | SBool,_ | SInt,_ -> (t1 = t2)
      | (SC i1),(SC i2) -> isSubClass (Cmap.find i1 classes) (Cmap.find i2 classes)
      | _ -> false


  (** vérifie si le type est bien formé *)
  let isBF classes = function
    | Int | Bool -> true
    | C "Object" | C "String" -> true
    | C c -> Cmap.mem c classes
  
      
  (** vérfie l'unicité des champs et que leurs types soient bien formés
      renvoit la map des attributs *)
  let checkAttr classes c = 
    let aux accmap  attr = 
      let name = attr.v_name in
      if Cmap.mem name accmap then 
	let a = Cmap.find name accmap in
	raise (AllreadyDefined( attr.pos,name, a.pos))
      else
	if not (isBF classes attr.v_type) then 
	  raise (WrongType( attr.pos,types_to_Sast attr.v_type,None))
	else
	  Cmap.add name attr accmap
    in
    List.fold_left aux Cmap.empty c.attrs 
      
  (** vérifie qu'un profil prend bien une liste d'arguments de noms différents 
      renvoit un booléen *)
  let isBFCall call = 
    let accSet = ref Cset.empty in
      let checkParams v = 
	if Cset.mem v.v_name !accSet then 
	  (raise (AllreadyDefined ( call.call_pos, v.v_name)))
	else 
	  begin
	    if (isBF classes v.v_type ) then
	      (accSet := (Cset.add v.v_name !accSet) ; true)
	    else
		(raise (WrongType( call.call_pos, types_to_Sast v.v_type ,None)))
	  end
      in
      List.for_all checkParams call.call_params
	
  (** Vérifie que call et tout les élements de la liste ont des profils différents et des types de retour identiques *)
  let isDiff list call = 
    (* compare deux profils *)
    let comp  c2 = 
      try
	List.for_all2 (fun v1 v2 -> v1.v_type != v2.v_type) call.call_params c2.call_params
      with  Invalid_argument -> true
    in 
    let forAll c =
      if comp c then
	(if call.call_returnType = c.call_returnType then
	    true
	 else
	    (raise (WrongType(c.call_pos, c.call_returnType, call.call_returnType))))
      else
	(raise (AllreadyDefined(call.call_pos, call.call_name,c.call_pos)))
    in
    (* on compare tout avec les profils *)
    if List.for_all  forAll liste then 
      try 
	List.tl liste 
      with Failure _ -> []
    else
      false

	
  (** vérifie les constructeurs, renvoit () si aucune exception n'est levée*)
  let checkConst classes c =
    (* vérifie que tout les constructeurs ont des profils différents *)    
    if List.for_all isBFCall c.class_consts  then
      let tail = 
	try
	  List.tl c.class_consts 
	with Failure _ -> []
      and _ = List.fold_left isDiff tail c.class_consts 
      in
      ()
    else
      ()

  (** construit une Map des méthodes de la classe en vérifiant que tout est correct *)
  let checkMethods  classes c =
    (* On vérifie que toutes les méthodes ont des profils bien formés *)
    let _ = List.for_all  isBFCall c.class_methods in
    (* on construit d'abord une Map*)
    let bMap acc m = 
      let l =
	try 
	  Cmap.find m.call_name acc 
	with Not_found -> []
      in
      Cmap.add m.call_name (m::l) acc 
    in
    let mMap = List.fold_left bMap Cmap.empty c.class_methods in
    (* on vérifie les méthodes polymorphes  *)
    let _ = Cmap.iter (fun _ l -> 
      let tail = 
	try 
	  List.tl l 
	with Failure _ -> []
      and _ = List.fold_left isDiff tail l 
      in () ) mMap 
    in
    mMap 
      

  (** vérifie que l'héritage est sans cycle
      vérifie que les méthodes redéfinies ont le même type de retour *)
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
            | None -> graySet
	    | Some ("String" , pos) -> raise (Her(pos,c.class_name,"Cannot herite of string"))
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

(** typage des expressions et des instructions *)
module  CheckInstr = struct 
    
  open Past
  open Sast
  open Exception

  let isLeft = function
    | Getval _ -> true
    | _ -> false

  let rec typExpr env = function
    | Iconst (p,i) -> { sv = SIconst i ; st = SInt ; sp = p }
    | Sconst (p,i) -> { sv = SSconst i ; st = SC ("String") ; sp = p }
    | Bconst (p,i) -> { sv = SBconst i ; st = SBool ; sp = p }
    | Null p -> { sv = SNull ; st = STypeNull ; sp = p }
    | Unaire (p, op, e) ->
      let se = typExpr env e in
      let sop = match op with
        | Incr ->
          if se.st = SInt then SIncr
          else raise (WrongType (se.sp, se.st, Some SInt))
        | Decr ->
          if se.st = SInt then SDecr
          else raise (WrongType (se.sp, se.st, Some SInt))
        | Not ->
          if se.st = SBool then SNot
          else raise (WrongType (se.sp, se.st, Some SBool))
        | UMinus ->
          if se.st = SInt then SUMinus
          else raise (WrongType (se.sp, se.st, Some SInt))
      in
      { sv = SUnaire (sop, se) ; st = se.st ; sp = p }
    | Binaire (p, op, e1, e2) ->
      let se1 = typExpr env e1 in
      let se2 = typExpr env e2 in
      (* à modifier pour faire les vérifs de types *)
      let sop = match op with
        | Eq -> SEq | Neq -> SNeq | Leq -> SLeq | Geq -> SGeq
        | Lt -> SLt | Gt -> SGt
        | Plus -> SPlus | Minus -> SMinus | Star -> SStar | Div -> SDiv
        | Mod -> SMod
        | And -> SAnd | Or -> SOr
      in
      { sv = SBinaire (sop, se1, se2) ; st = se1.st ; sp = p }
    | Getval (p,Var "this") -> (
      try let t = Cmap.find "this" env in
	  { sv = SGetval (SVar { id_id = "this" ; id_typ = t} ) ;
            st = t ; sp = p }
      with Not_found -> raise (Undefined (p,"this"))
    )
    | _ -> failwith "Not implemented"

  let rec typInstr env = function
    | Expr e -> SExpr (typExpr env e)
    | _ -> failwith "Not implemented"

end
