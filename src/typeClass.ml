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
  exception AlreadyDefined of pos * string * pos option
     
  (** identifiant non définie *)
  exception Undefined of pos * string
      
  (** erreur d'Héritage *)
  exception Her of pos * string * string
     
  (** mauvais type, optionnellement le type attendu *)
  exception WrongType of pos * stypes * stypes option
      
  (** Utilisation du même nom plusieurs fois *)
  exception Duplicated of pos * string

  (** const/attribu/méthode manquante pour la classe C (la pos, la classe, l'attribu)*)
  exception Missing of pos * string * string 

end

  
(** Définition des classes et vérification d'unicité
    Passe d'un arbre Past à un arbre Oast *)
module ClassAnalysis = struct
  open Exceptions
  open Oast
  open Past 
    
  (**    construit une Map de classes en vérifiant l'unicité du nommage *)
  let buildClassMap prog =
    let    addClass map c =
      if Cmap.mem c.class_name map then
	let c1 = Cmap.find c.class_name map in
	raise (AlreadyDefined (c.class_pos,c.class_name,  Some (c1.class_pos)))
      else
	Cmap.add c.class_name c map in
    List.fold_left addClass Cmap.empty prog.classes 
   

  (** vérifie si le type est bien formé *)
  let isBF classes = function
    | Int | Bool -> true
    | C "Object" | C "String" -> true
    | C c -> Cmap.mem c classes
    | _ -> false 
  
      
  (** vérfie l'unicité des champs et que leurs types soient bien formés
      renvoit la map des attributs *)
  let checkAttr classes c = 
    let aux accmap  attr = 
      let name = attr.v_name in
      if Cmap.mem name accmap then 
	let a = Cmap.find name accmap in
	raise (AlreadyDefined( attr.v_pos,name, Some (a.v_pos)))
      else
	if not (isBF classes attr.v_type) then 
	  raise (WrongType( attr.v_pos,types_to_Sast attr.v_type,None))
	else
	  Cmap.add name attr accmap
    in
    List.fold_left aux Cmap.empty c.class_attrs 
      
  (** vérifie qu'un profil prend bien une liste d'arguments de noms différents 
      renvoit un booléen *)
  let isBFCall classes call = 
    let accSet = ref Cset.empty in
      let checkParams v = 
	if Cset.mem v.v_name !accSet then 
	  (raise (AlreadyDefined ( call.call_pos, v.v_name, None)))
	else 
	  begin
	    if (isBF classes v.v_type ) then
	      (accSet := (Cset.add v.v_name !accSet) ; true)
	    else
		(raise (WrongType( call.call_pos, types_to_Sast v.v_type ,None)))
	  end
      in
      List.for_all checkParams call.call_params
	
  (** Vérifie que call et tout les élements de la liste ont des profils différents*)
  let isDiff liste call = 
    (* compare deux profils *)
    let comp  c2 = 
      try
	List.for_all2 (fun v1 v2 -> v1.v_type != v2.v_type) call.ocall_params c2.ocall_params
      with  Invalid_argument _ -> (raise (AlreadyDefined(call.ocall_pos, call.ocall_name,None)))
    in 
    (* on compare tout avec les profils *)
    if List.for_all comp liste then 
      try 
	List.tl liste 
      with Failure _ -> []
    else
      (raise (AlreadyDefined(call.ocall_pos,call.ocall_name,None)))

	
  (** vérifie les constructeurs, renvoit la liste* sa taille si aucune exception n'est levée*)
  let checkConst classes c =
    (* vérifie que tout les constructeurs ont des profils différents *)    
    if List.for_all (isBFCall classes) c.class_consts  then
   
      let li,n = List.fold_left (fun (acclist,n) elt -> 
	(({ocall_pos = elt.call_pos ;
	  ocall_id = n;
	  ocall_returnType = elt.call_returnType;
	  ocall_name = elt.call_name;
	  ocall_params = elt.call_params;
	  ocall_body = elt.call_body })::acclist),(n+1)) ([],0) c.class_consts 
      in
      let tail = 
	try
	  List.tl li  
	with Failure _ -> []
      in
      let  _ = List.fold_left isDiff tail li 
      in
      li,n
    else
      [],0

  (** construit une Map des méthodes de la classe en vérifiant que tout est correct *)
  let checkMethods  classes c =
    (* On vérifie que toutes les méthodes ont des profils bien formés *)
    let _ = List.for_all (isBFCall classes) c.class_methods in
    (* on construit d'abord une Map*)
    let bMap (acc,n) m = 
      let m = { 
	ocall_pos = m.call_pos;
	ocall_id = n;
	ocall_returnType = m.call_returnType;
	ocall_name = m.call_name;
	ocall_params = m.call_params;
	ocall_body = m.call_body }
      in
      let l =
	try 
	  Cmap.find m.ocall_name acc 
	with Not_found -> []
      in
      (Cmap.add m.ocall_name (m::l) acc),(n+1)
    in
    (* on trans-type les méthodes vers Oast *)
    let mMap,n = List.fold_left bMap (Cmap.empty,0) c.class_methods in
    (* on vérifie les méthodes polymorphes  *)
    let _ = Cmap.iter (fun _ l -> 
      let tail = 
	try 
	  List.tl l 
	with Failure _ -> []
      in
      let _ = List.fold_left isDiff tail l 
      in () ) mMap 
    in
    mMap,n
      

  (** vérifie que l'héritage est sans cycle
      vérifie que les méthodes redéfinies ont le même type de retour *)
  let checkHerit cmap =
    (** parcourt les parents de le classe
	@param c la classe à partir de laquelle on parcourt
	@param graySet L'ensemble des classes découvertes actuellement
	@param blackSet l'ensemble des classes déjà traitées *)
    let rec dfsVisit c graySet blackSet = 
      if Cset.mem c.oclass_name blackSet then 
	graySet
      else
	if Cset.mem c.oclass_name graySet then 
	  raise (Her(c.oclass_pos,c.oclass_name,"Cycle")) 
	else
	  match c.oclass_extends with
            | None -> graySet
	    | Some ("String" , pos) -> raise (Her(pos,c.oclass_name,"Cannot herite of string"))
	    | Some (c1name, pos) -> 
	      let c1 = 
		try 
		  Cmap.find c1name cmap 
		with Not_found -> raise (Undefined(pos,c1name))
	      in
	      dfsVisit c1 (Cset.add c1name graySet) blackSet
    in
    Cmap.fold (fun _ c black -> Cset.union (dfsVisit c Cset.empty black) black) cmap Cset.empty 

  (** construit un arbre Oast à partir d'un arbre Past en effectuant les tests 
      quivontbien *)
  let typClasses prog = 
    (* vérification d'unicité des noms des classes *)
    let classes = buildClassMap prog in
    (* on traduit les classes en oclasses *)
    let typClass cmap n c accmap =
      let attrMap = checkAttr cmap c in
      let constList,sizeConst = checkConst cmap c in
      let mMap,sizeMeth = checkMethods cmap c in
      Cmap.add n 
	{ oclass_pos = c.class_pos ;
	  oclass_name = c.class_name;
	  oclass_extends = ( c.class_extends);
	  oclass_attrs = attrMap;
	  oclass_consts = constList;
	  oclass_cn = sizeConst;
	  oclass_methods = mMap;
	  oclass_cm = sizeMeth;
	} accmap 
    in
    let classes = Cmap.fold (typClass classes) classes Cmap.empty in
    (* vérification de l'héritage *)
    let _ = checkHerit classes in
    { oclasses = classes ; oinstr = prog.instr }
      
    
end	

(** typage des expressions et des instructions *)
module  CheckInstr = struct 
    
  open Oast
  open Sast
  open Exceptions

 (** calcule si une classe c1 hérite d'une autre classe c2 *)
  let rec isSubClass classes c1 c2 =
    match c1.oclass_extends with
      | None -> if c2.oclass_name = "Object" then true else false
      | Some (extends,_) ->
        if c2.oclass_name = extends then true
        else
          match c2.oclass_extends with
            | None -> false
	    | Some (c,_) -> isSubClass classes c1 (Cmap.find c classes)

 
  (** calcul si t1 est un sous type de t2 *)
  let isSubType classes t1 t2 = 
    match t1,t2 with
      | STypeNull,_ -> true
      | SBool,_ | SInt,_ -> (t1 = t2)
      | (SC i1),(SC i2) -> isSubClass  classes (Cmap.find i1 classes) (Cmap.find i2 classes)
      | _ -> false


  (** renvoit true  si p1 est un sous profil de p2 *)  
  let isSubProf classes p1 p2 = 
    try 
      List.for_all2 (fun t1 t2 -> isSubType classes t1 t2) p1 p2
    with Failure _ -> false

  (** compare deux const/méthodes, relation d'ordre supposée totale *)
  let compCall classes c1 c2 = 
    let p1 = List.map (fun v -> types_to_Sast v.v_type) c1.ocall_params in
    let p2 = List.map (fun v -> types_to_Sast v.v_type) c2.ocall_params in
    if isSubProf classes p1 p2 then
      (if (isSubProf classes p2 p1) then
	  0 
       else
	  -1)
    else
      1
    
  (** construit l'ensemble meth(C,m,prof) sous forme de liste  
      en cherchant récursivement dans les sur-classes *)
  let rec findMeth classes c m args acc= 
    let li = Cmap.find m c.oclass_methods in
    (* on choisit seulement les méthodes qui vérifient la relation *)
    let li = List.fold_left (fun acclist elt ->
      if (isSubProf classes args (List.map (fun v -> types_to_Sast v.v_type) elt.ocall_params))then
	elt::acclist 
      else
	acclist )
      [] li 
    in
    (* on concatène les méthodes qui correspondent *)
    let acc = List.rev_append li acc in 
    match c.oclass_extends with
      | Some (name,_) -> 
	let cl = Cmap.find name classes in
	findMeth classes cl m args acc
      | None -> acc
 
	
  type leftValue = { lv : svars; lt:  stypes }
       
  (** Type les varleurs gauches *)
  let rec typLeft classes c env p = function
    |Var(id) -> 
      let t =
	try 
	  Cmap.find id env 
	with Not_found ->
	  try 
	   types_to_Sast (Cmap.find id c.oclass_attrs).v_type
	  with Not_found -> (raise (Undefined(p,id)))
      in
      { lv = SVar({id_id = id; id_typ = t}) ; lt = t }
    | Attr(e,id) ->
      let  exp = typExpr classes c env e in
      let cl =
	match exp.st with
	  | SC f ->(
	    try 
	      Cmap.find f classes 
	    with Not_found -> (raise (Undefined(exp.sp,f))))
	  | t -> (raise (WrongType (exp.sp,t,(Some(SC ("foo"))))))
      in
      let attr =
	try 
	  Cmap.find id cl.oclass_attrs 
	with Not_found -> (raise (Missing(p,cl.oclass_name,id)))
      in
      { lv = SAttr(exp,{id_id = id; id_typ = types_to_Sast attr.v_type}); lt =  types_to_Sast attr.v_type }

  (** type les expressions *)
  and typExpr classes c env = function
    | Iconst (p,i) -> { sv = SIconst i ; st = SInt ; sp = p }
    | Sconst (p,i) -> { sv = SSconst i ; st = SC ("String") ; sp = p }
    | Bconst (p,i) -> { sv = SBconst i ; st = SBool ; sp = p }
    | Null p -> { sv = SNull ; st = STypeNull ; sp = p }
    | Getval(p,left) ->
      let  left = typLeft classes c env p left in
      { sv = SGetval(left.lv);  st = left.lt ; sp = p }
    | Assign(p,left,e) ->
      let left = typLeft classes c env p left in
      let exp = typExpr classes c env e in
      if not (isSubType classes exp.st left.lt) then
	raise (WrongType(exp.sp, exp.st, Some(left.lt)))
      else
	{ sp = p; st = left.lt; sv = SAssign(left.lv,exp)}
    | Pref(p,op,left) ->
      let left = typLeft classes c env p left in
      if left.lt != SInt then
	(raise (WrongType (p,left.lt, Some (SInt))))
      else
	{sv = SPref(op,left.lv); st = SInt; sp = p }
    | Post(p,op,left) -> 
      let left = typLeft classes c env p left in
      if left.lt != SInt then
	(raise (WrongType (p,left.lt, Some (SInt))))
      else
	{sv = SPost(op,left.lv); st = SInt; sp = p}
    | UMinus (p,e) ->
      let e = typExpr classes c env e in
      if e.st != SInt then
	(raise (WrongType (e.sp,e.st, Some (SInt))))
      else
	{sv = SUMinus(e); st = SInt; sp = p }
    | Not(p,e) ->
      let e =  typExpr classes c env e in
      if e.st != SBool then
	(raise (WrongType (e.sp,e.st, Some (SBool))))
      else
	{sv = SNot(e); st = SBool; sp = p }
    | Binaire(p,op,e1,e2) ->
      let e1 = typExpr classes c env e1 in
      let e2 = typExpr classes c env e2 in
      begin
	match op with 
	  | Eq | Neq -> 
	    if (isSubType classes e1.st e2.st)or (isSubType classes e2.st e1.st) then
	      { sv = SBinaire(op,e1,e2); sp = p; st = SBool }
	    else
	    (raise (WrongType(e2.sp,e2.st,None)))
	  | Leq | Geq | Lt | Gt -> 
	    if e1.st = SInt then
	      ( if e2.st = SInt then
		  { sv = SBinaire(op,e1,e2); sp = p; st = SBool}
		else
		  (raise (WrongType(e2.sp,e2.st,Some(SInt)))))
	    else
	      (raise (WrongType(e1.sp,e1.st,Some(SInt))))
	  | Minus | Star | Div | Mod ->
	    if e1.st = SInt then
	      ( if e2.st = SInt then
		  { sv = SBinaire(op,e1,e2); sp = p; st = SInt}
		else
		  (raise (WrongType(e2.sp,e2.st,Some(SInt)))))
	    else
	      (raise (WrongType(e1.sp,e1.st,Some(SInt))))
	  | And | Or ->
	    if e1.st = SBool then
	      ( if e2.st = SBool then
		  { sv = SBinaire(op,e1,e2); sp = p; st = SBool}
		else
		  (raise (WrongType(e2.sp,e2.st,Some(SBool)))))
	    else
	      (raise (WrongType(e1.sp,e1.st,Some(SBool))))
		
	  | Plus ->
	    begin
	      match e1.st,e2.st with
		| SInt,SInt ->  { sv = SBinaire(op,e1,e2); sp = p; st = SInt }
		| (SC "String"),SInt | SInt,(SC "String") | (SC "String"),(SC "String") -> 
		  { sv = SBinaire(op,e1,e2); sp = p; st = SC "String"}
		| SInt,_ ->  (raise (WrongType(e2.sp,e2.st,None)))
		| _,_ -> (raise (WrongType(e1.sp,e1.st,None)))
	    end	      
      end
    | Print(p,e) -> 
      let e = typExpr classes c env e in
      if e.st = (SC "String") then 
	{sv = SPrint(e) ; st = SVoid ; sp = p }
      else
	(raise (WrongType(e.sp,e.st,Some (SC "String"))))
    | Call(p,e,m,li) ->
      let expr,cl = match e with 
	| Some f ->  
	  begin 
	    let e = (typExpr classes c env f ) in
	    let name = match e.st with
	      |SC n -> n
	      | t -> (raise (WrongType(e.sp,e.st,(Some t))))
	    in
	    let cl = 
	      try
		Cmap.find name classes
	      with Not_found -> (raise (Undefined (e.sp,name)))
	    in
	    e,cl
	  end
	| None ->  
	  ({ sp = p; st = SC c.oclass_name; sv = SGetval(SVar({id_id = "this"; id_typ = SC  c.oclass_name}))}),c
      in
      (* Il faut traiter String et sa méthode equals différemment *)
      if (expr.st = (SC "String")) && ( m = "equals") then (
	if (List.length li) = 1 then
	  let e1 = typExpr classes c env (List.hd li) in
	  { sv = SCall(expr,0,[e1]); st = SBool; sp = p }
	else
	  (raise (Missing(p,"String",m)))
      )
      else
	(
	  let li = List.map (typExpr classes c env) li in
	  let mList = findMeth classes cl m (List.map (fun elt -> elt.st) li) [] in
	  (* On doit prendre en compte les redéfinition en prenant la méthode la plus basse *)
	  let mList = List.rev mList in
	  let mList = List.stable_sort (compCall classes) mList in
	  let meth = 
	    try
	      List.hd mList 
	    with Failure _ -> (raise (Missing(p,cl.oclass_name,m)))
	  in
	  { sv = SCall(expr,meth.ocall_id,li); sp = p; st = types_to_Sast meth.ocall_returnType }
	)
    | New(p,n,args) ->
      let cl = 
	try 
	  Cmap.find n classes 
	with Not_found -> (raise (Undefined(p,n)))
      in
      let args = List.map (typExpr classes c env) args in
      let li = List.fold_left (fun acclist elt ->
	if (isSubProf classes (List.map (fun e -> e.st) args)  (List.map (fun v -> types_to_Sast v.v_type) elt.ocall_params)) then
	  (elt::acclist)
	else
	  acclist)
	[] cl.oclass_consts  in
      let const = 
	try
	  List.hd li 
	with Failure _ -> (raise (Missing(p,cl.oclass_name,cl.oclass_name)))
      in
      { sv = SNew({id_id = n; id_typ = (SC n)},const.ocall_id,args); sp = p ; st = types_to_Sast const.ocall_returnType }
    | Cast(p,t,e) -> 
      let e = typExpr classes c env e in
      let t = types_to_Sast t in
      if ((isSubType classes t e.st) && (isSubType classes e.st t) ) then
	{ sv = SCast(t,e); sp = p; st = t }
      else
	(raise  (WrongType(e.sp,e.st,Some t)))
    | Instanceof(p,e,t) ->
      let e = typExpr classes c env e in
      let t = types_to_Sast t in
      begin
	match e.st with 
	  | SC _ | STypeNull ->  
	    if ((isSubType classes t e.st) && (isSubType classes e.st t) ) then
	      { sv = SInstanceof(e,t); sp = p; st = SBool }
	    else
	      (raise (WrongType(e.sp,e.st,None)))
	  | _ -> (raise (WrongType(e.sp,e.st,None)))
      end
	
	
  (** typage des instructions *)
  let rec typInstr classes c env = function
    | Expr(e) ->
      let e = typExpr classes c env e in
      env,SExpr(e)
    | Decl(p,t,id,None) ->       
      if not (ClassAnalysis.isBF classes t) then (raise (WrongType(p,(types_to_Sast t),None)));
      let t = types_to_Sast t in
      let id = { id_id = id; id_typ = t } in
      (Cmap.add id.id_id id.id_typ env),(SDecl(p,t,id,None))
    | Decl(p,t,id,Some e) ->
      if not (ClassAnalysis.isBF classes t) then (raise (WrongType(p,(types_to_Sast t),None)));
      let t = types_to_Sast t in
      let id = { id_id = id; id_typ = t } in
      let e = typExpr classes c env e in
      if not (isSubType classes e.st t) then (raise (WrongType(e.sp,e.st,Some t)));
      ((Cmap.add id.id_id id.id_typ env),(SDecl(p,t,id,Some e)))
    | If(e,i1,i2) ->
      let e = typExpr classes c env e in
      if not (e.st = SBool) then (raise (WrongType(e.sp,e.st,Some SBool)));
      let env1,i1 = typInstr classes c env i1 in
      let i2 = match i2 with 
	| None -> None 
	| Some i -> let _,i = typInstr classes c env i in
		    Some (i)
      in
      (env,(SIf(e,i1,i2)))
    | For(e1,e2,e3,i) -> 
      let e1 = match e1 with
	| None -> None 
	| Some e -> Some (typExpr classes c env e)
      in
      let e3 = match e3 with
	| None -> None 
	| Some e ->  Some (typExpr classes c env e)
      in
      let e2 = match e2 with 
	| None -> {sv = SBconst true ; sp = { file = "unknown"; line = 0; fChar = 0; lChar = 0}; st = SBool}
	| Some e -> (
	  let e2 = typExpr classes c env e in
	  if e2.st != SBool then (raise (WrongType(e2.sp,e2.st,Some SBool)));
	  e2)
      in
      let i = match i with
	| None -> None
	| Some f -> let _,i = typInstr classes c env f in
		    Some i
      in
      (env,(SFor(e1,e2,e3,i)))
    | Block(li) -> 
      let _,li = List.fold_left (fun (accenv,acclist) i ->
	let nenv,i = typInstr classes c accenv i in
	(nenv,(i::acclist))) (env,[]) li 
      in
      let li = List.rev li in
      (env,(SBlock(li)))
    | Return(e) ->
      let e = match e with 
	| None -> None 
	| Some f -> Some (typExpr classes c env f)
      in
      (env,(SReturn(e)))
	
end	
		    
