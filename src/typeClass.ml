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

  (**N'est pas une valeur gauche **)
  exception NotALeftValue of pos

  (** Le constructeur n'as pas le bon nom  nom du const * nom de la classe*)
  exception BadConst of pos * string * string 

  (** Deux constructeurs/méthodes conviennent *)
  exception Ambiguous of pos * string

  (** Unf flot d'éxcution peut ne pas rencontrer d'instruction return, pos de la classe, pos du début de la branche *)
  exception EReturn of pos * pos

end

  
(** Définition des classes et vérification d'unicité
    Passe d'un arbre Past à un arbre Oast *)
module ClassAnalysis = struct
  open Exceptions
  open Oast
  open Past 
  
  (* PAST TO ZAST *)
  let emptyPos = {  
    file = "Internal";
    line = 0;
    fChar = 0;
    lChar =  0;
  }
    
  (**construit une Map de classes en vérifiant l'unicité du nommage *)
  let buildClassMap prog =
    let addClass (map,hmap) c =
      if c.class_name == "Object" or c.class_name == "String" then 
	raise (AlreadyDefined (c.class_pos,c.class_name,None)) ;
      if Cmap.mem c.class_name map then
	let c1 = Cmap.find c.class_name map in
	raise (AlreadyDefined (c.class_pos,c.class_name,  Some (c1.class_pos)))
      else (
	let hmap = (match c.class_extends with 
	  | None -> 
	    let obj = Cmap.find "Object" hmap in
	    Cmap.add "Object" ((c.class_name,emptyPos)::obj) hmap 
	  | Some ("String",p) -> raise (Her(p,c.class_name,"Ne peut pas hériter de String")) 
	  | Some (n,p) -> 
	    let c2 = (
	      try 
		Cmap.find n hmap 
	      with Not_found -> [] )in
	    Cmap.add n ((c.class_name,p)::c2) hmap
	)
	in
	((Cmap.add c.class_name c map),hmap)) in
    let hmap = Cmap.add "Object" [] Cmap.empty in
    List.fold_left addClass (Cmap.empty,hmap) prog.classes
   
  (* ZAST TO OAST *)

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
      
  (** vérifie qu'un profil prend bien une liste d'arguments de noms différents *)
  let isBFCall classes call = 
    let accSet = ref Cset.empty in
      let checkParams v = 
	if Cset.mem v.v_name !accSet then 
	  (raise (Duplicated ( call.call_pos, v.v_name)))
	else 
	  begin
	    if (isBF classes v.v_type ) then
	      (accSet := (Cset.add v.v_name !accSet) ;)
	    else
		(raise (WrongType(call.call_pos, types_to_Sast v.v_type ,None)))
	  end
      in
      List.iter checkParams call.call_params
	
  (** compare deux profils (liste de variable) *)
  let isDiffProf p1 p2 = 
    try
      List.for_all2 (fun v1 v2 -> v1.v_type != v2.v_type) p1 p2  
    with  Invalid_argument _ -> true
      
  (** vérifie que le callable simple a des profils différents de ceux des callables de liste et renvoit la nouvelle liste des callables en tenant compte des redéfinissions *)    
  let diff c liste scall =
    let atomic (acclist,check) elt =
      if not (isDiffProf scall.osimple_params elt.osimple_params) then (
	if elt.osimple_classe == c.class_name then
	  (raise (AlreadyDefined(scall.osimple_pos,scall.osimple_name,Some elt.osimple_pos)))
	else
	  (* on peut redéfinir une méthode UNE fois dans une classe *)
	  (if check then 
	      (raise (AlreadyDefined (scall.osimple_pos,scall.osimple_name,Some elt.osimple_pos)));
	   (* on doit vérifier que la redéfinition a le même type de retour *)
	   if elt.osimple_returnType != scall.osimple_returnType then 
	     (raise (WrongType(scall.osimple_pos,types_to_Sast scall.osimple_returnType,Some (types_to_Sast elt.osimple_returnType))));
	   (* Pour redéfinir, on remplace simplement la méthode *)
	   let s = { scall with osimple_n = elt.osimple_n } in
	   ((s::acclist),true)
	  )
      )
      else 
	(elt::acclist,check)
    in
    let l,check = List.fold_left atomic ([],false) liste in
    if check then
      l
    else 
      (scall::l)
    

  (** Vérifie les constructeurs, et renvoit le tableau descripteur
      @param classes la map des classes 
      @param meths la liste des méthodes/constructeurs
      @param c la classe courante
      @param lastId le dernier identifiant des classes/constructeurs 
      @param d la liste descripteur de la classe mère *)
  let checkConst classes meths c lastId d =
    let size = ref ((List.length d) - 1 ) in
    let bDesc (accList,accMeths) const = 
      (* on vérifie que les profils sont bien formés *)
      let _ = isBFCall classes const in
    (* on vérifie que tout les constructeurs ont le même nom *)
      if const.call_name != c.class_name then
	(raise (BadConst(const.call_pos,const.call_name,c.class_name)));
      let _ = isBFCall classes const in
      let s = {
	osimple_pos = const.call_pos;
	osimple_returnType = Void;
	osimple_name = const.call_name;
	osimple_params = const.call_params;
	osimple_id = (incr lastId; !lastId);
	osimple_n = (incr size; !size);
	osimple_classe = c.class_name;
      } 
      in
      let o = {
	ocall_params = const.call_params;
	ocall_body = const.call_body;
      }
      in
      ((diff c accList s),(o::accMeths))
    in
    let d,meths = List.fold_left bDesc (d,meths) c.class_consts in
    let descriptor = Array.make (!size) osimplEmpty in
    List.iter (fun elt -> descriptor.(elt.osimple_n) <- elt) d ;
    (d,meths,descriptor)
    

  (** Vérifie les méthodes de la classe c, et renvoit le descripteur de classe d 
      @param classes la map de toutes les classes
      @param meths la liste des méthodes
      @param d le descripteur de la classe mère sous forme de CMap
      @param lastId ref vers le dernier id des callables 
      @param c la classe*)
  let checkMethods classes meths d lastId c = 
    let size = ref ((Cmap.cardinal d) - 1) in 
    (* construit une map des méthodes *)
    let bMap (accMap,accMeth) m =
      let _ = isBFCall classes m in
      let s = { 
	osimple_pos = m.call_pos;
	osimple_id = ( incr lastId ; !lastId);
	osimple_n = ( incr size ; !size);
	osimple_params = m.call_params;
	osimple_classe = c.class_name;
	osimple_name = m.call_name;
	osimple_returnType = m.call_returnType ;
      }
      in
      let o = {
	ocall_params = m.call_params;
	ocall_body = m.call_body;
      }
      in
      let l = 
	try 
	  Cmap.find m.call_name accMap
	with Not_found -> [] 
      in
      let newList = diff c l s in
      ((Cmap.add m.call_name newList accMap),(o::accMeth))
    in
    let d,meths = List.fold_left bMap (d,meths) c.class_methods in
    (* On vérifie que toutes les méthodes définies de même nom ont des profils différents, si ce n'est pas le cas, c'est une redéfinition à traiter en conséquence *) 
    let descriptor = Array.make (!size + 1) osimplEmpty in
    Cmap.iter (fun n li -> 
      List.iter (fun elt -> descriptor.(elt.osimple_n) <- elt) li ) d ;
    (* on renvoit la nouvelle map la nouvelle liste de méthode, et le tableau *)
    (d,meths,descriptor)
      
      
      
  (** parcourt la map des classes, vérifie que l'héritage est sans cycle, et type les classes *)
  let checkHerit prog=
    let classes,hMap = buildClassMap prog in
    let lastId = ref 0 in
    let rec dfsVisit  md cd blackSet (accMap,meths) (c,pos)=
      if Cset.mem c blackSet then
	let cl = Cmap.find c classes in
	(raise (Her(cl.class_pos,cl.class_name,"Cycle")))
      else
	let cl = 
	  try 
	    Cmap.find c classes 
	  with Not_found -> (raise (Undefined(pos,c))) 
	in
	let attrs = checkAttr classes cl in
	let md,meths,mdescriptor = checkMethods classes meths md lastId cl in
	let cd,meths,cdescriptor = checkConst classes meths cl lastId cd  in 
	let cl = {
	  oclass_pos = cl.class_pos;
	  oclass_name = cl.class_name;
	  oclass_extends = cl.class_extends;
	  oclass_attrs = attrs;
	  oclass_methodesdesc = mdescriptor;
	  oclass_constsdesc = cdescriptor;
	} 
	in
	let blackSet = Cset.add c blackSet in
	let accMap = Cmap.add c cl accMap in
	(* on trouve la liste des sous-classes *)
	let liste = Cmap.find c hMap in
	let accMap,meths = List.fold_left (dfsVisit md cd blackSet) (accMap,meths) liste  in
	accMap,meths
    in     
    let liste = Cmap.find "Object" hMap in
    let classes,meths = List.fold_left (dfsVisit Cmap.empty [] Cset.empty) (Cmap.empty,[]) liste in
    let ar = Array.make !lastId { ocall_params = []; ocall_body = Block [] } in
    lastId := 0;
    List.iter (fun o -> ar.(!lastId) <- o ; incr lastId) meths;
    { omeths = ar; oclasses = classes; oinstr = prog.instr }
      
    
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
    let p1 = List.map (fun v -> types_to_Sast v.v_type) c1.osimple_params in
    let p2 = List.map (fun v -> types_to_Sast v.v_type) c2.osimple_params in
    if isSubProf classes p1 p2 then
      (if (isSubProf classes p2 p1) then
	  0 
       else
	  -1)
    else
      1
    
  (** construit l'ensemble meth(C,m,prof) sous forme de liste et renvoit le minimum selon la relation de sous typage*)
  let rec findCall classes cl p desc m args =
    let mList = Array.fold_left (fun accList elt -> 
      if elt.osimple_name = m then (
	if (isSubProf classes args (List.map (fun v -> types_to_Sast v.v_type) elt.osimple_params)) then 
	  elt::accList
	else
	  accList )
      else 
	accList) [] desc in
    (* liste triée selon la relation d'ordre *)
    let mList = List.stable_sort (compCall classes) mList in
    let meth = 
      try
	List.hd mList 
      with Failure _ -> (raise (Missing(p,cl.oclass_name,m)))
    in
    (* on vérifie qu'il n'y a qu'un seul minimum *)
    (try 
       let elt = List.hd mList  in
       if ( compCall classes meth elt) = 0 then
	 (raise (Ambiguous(p,m)))
     with Failure _ ->  ()  );
    meth
	
  type leftValue = { lv : svars; lt:  stypes }
       
  (** Type les varleurs gauches *)
  let rec typLeft classes c env p = function
    |Var(id) -> 
      begin
      try 
	let t = Cmap.find id env in
	{lv = SVar({id_id = id; id_typ = t}); lt = t}
	with Not_found -> (
	  try 
	    (* si la variable est un attribut de la classe courante, 
	       on transtype l'expression vers SAttr, 
	       en utilisant le mot clé "this" *)
	    let t = types_to_Sast (Cmap.find id c.oclass_attrs).v_type in
	    { lv = SAttr({ sv = SGetval(SVar({id_id ="this"; id_typ = SC c.oclass_name}));
			   sp = p; 
			   st = SC c.oclass_name },
			 {id_id = id; id_typ = t}) ; lt = t}
	  with Not_found -> (raise (Undefined(p,id))) )
      end
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
    | Assign(p,(Getval(p1,left)),e) ->
      let left = typLeft classes c env p1 left in
      let exp = typExpr classes c env e in
      if not (isSubType classes exp.st left.lt) then
	raise (WrongType(exp.sp, exp.st, Some(left.lt)))
      else
	{ sp = p; st = left.lt; sv = SAssign(left.lv,exp)}
    | Assign(p,e,e2) ->
      let e = typExpr classes c env e in
      (raise (NotALeftValue e.sp))
    | Pref(p,op,(Getval(p1,left))) ->
      let left = typLeft classes c env p1 left in
      if left.lt != SInt then
	(raise (WrongType (p,left.lt, Some (SInt))))
      else
	{sv = SPref(op,left.lv); st = SInt; sp = p }
    | Pref(p,op,e) ->
      let e = typExpr classes c env e in
      (raise (NotALeftValue(e.sp)))
    | Post(p,op,(Getval(p1,left))) -> 
      let left = typLeft classes c env p1 left in
      if left.lt != SInt then
	(raise (WrongType (p,left.lt, Some (SInt))))
      else
	{sv = SPost(op,left.lv); st = SInt; sp = p}
    | Post(p,op,e) ->
      let e = typExpr classes c env e in
      (raise (NotALeftValue(e.sp)))
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
      if (expr.st = (SC "String")) then (
	if (List.length li) = 1 && ( m = "equals") then
	  let e1 = typExpr classes c env (List.hd li) in
	  { sv = SCall(expr,0,[e1]); st = SBool; sp = p }
	else
	  (raise (Missing(p,"String",m)))
      )
      else
	(
	  let li = List.map (typExpr classes c env) li in
	  let meth = findCall classes cl p cl.oclass_methodesdesc m 
	    (List.map (fun elt -> elt.st) li) 
	  in
	  { sv = SCall(expr,meth.osimple_n,li); sp = p; st = types_to_Sast meth.osimple_returnType }
	)
    | New(p,n,args) ->
      let cl = 
	try 
	  Cmap.find n classes 
	with Not_found -> (raise (Undefined(p,n)))
      in
      let args = List.map (typExpr classes c env) args in
      (* S'il n'y a pas de constructeur correspondant ET aucun argument
	 on considère qu'on utilise aucun constructeur *)
      let constId =
	try 
	  let const = findCall classes cl p cl.oclass_constsdesc n 
	    (List.map (fun elt -> elt.st)args) in
	  Some const.osimple_n 
	with Missing(ep,ec,en) -> (
	  if args = [] then 
	    None 
	  else
	    (raise (Missing(ep,ec,en))))
      in
      { sv = SNew(n,constId,args); sp = p ; st = (SC n) }
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
	
  (** typage des instructions 
      @param classes   la map des classes 
      @param c  la classe courante
      @param env l'enrivronnement de typage
      @param return booléen décrivant si on a trouvé un Return dans la branche
  *)
  let rec typInstr classes return returnType pos c env = function
    | Expr(e) ->
      let e = typExpr classes c env e in
      (env,SExpr(e),return)
    | Decl(p,t,id,None) ->       
      if not (ClassAnalysis.isBF classes t) then (raise (WrongType(p,(types_to_Sast t),None)));
      let t = types_to_Sast t in
      let id = { id_id = id; id_typ = t } in
      ((Cmap.add id.id_id id.id_typ env),(SDecl(p,t,id,None)),return)
    | Decl(p,t,id,Some e) ->
      if not (ClassAnalysis.isBF classes t) then (raise (WrongType(p,(types_to_Sast t),None)));
      let t = types_to_Sast t in
      let id = { id_id = id; id_typ = t } in
      let e = typExpr classes c env e in
      if not (isSubType classes e.st t) then (raise (WrongType(e.sp,e.st,Some t)));
      ((Cmap.add id.id_id id.id_typ env),(SDecl(p,t,id,Some e)),return)
    | If(e,i1,i2) ->
      let e = typExpr classes c env e in
      if not (e.st = SBool) then (raise (WrongType(e.sp,e.st,Some SBool)));
      let env1,i1,subreturn = typInstr classes return returnType pos c env i1 in
      let i2,return = match i2 with 
	| None -> None,(subreturn || return)
	| Some i -> (
	  let _,i,subreturn2 = typInstr classes  return returnType pos c env i in
	  (* Si on a trouvé un return dans l'une des deux branches mais pas 
	     au dessus dans la branche courante, il y a un flux d'execution
	     potentiellement sans retour *)
	  if (not return ) && (subreturn || subreturn2 ) && ( not (subreturn && subreturn2)) 
	  then
	    (raise (EReturn(pos,e.sp))) ;
	  (Some (i)),(return || subreturn || subreturn2))
      in
      (env,(SIf(e,i1,i2)),return)
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
      (* on considère qu'un return dans un for ne compte pas
	 il en faut un à l'extèrieur du for, pour être sûr de sa présence,
	 car on pourrait ne jamais entrer dans le for *)
      let i = match i with
	| None -> None
	| Some f -> let _,i,_ = 
		      typInstr classes return returnType pos c env f in
		    (Some i)
      in
      (env,(SFor(e1,e2,e3,i)),return)
    | Block(li) -> 
      let _,li,return = List.fold_left (fun (accenv,acclist,accreturn) i ->
	let nenv,i,accreturn = 
	  typInstr classes accreturn returnType pos c accenv i 
	in
	(nenv,(i::acclist),accreturn)) (env,[],return) li 
      in
      let li = List.rev li in
      (env,(SBlock(li)),return)
    | Return(p,e) ->
      let e = match e with 
	| None -> (
	  if returnType != SVoid then 
	    (raise (WrongType(p,SVoid,Some returnType)));
	  None )
	| Some f -> (
	  let exp = typExpr classes c env f in
	  if not (isSubType classes exp.st returnType) then
	    (raise (WrongType(exp.sp,exp.st,Some returnType)));
	  Some exp)
      in
      (env,(SReturn(e)),true)
	
  (** type un programme Oast vers un programme Sast *)
  let typProg prog =
    let meths = Array.make (Array.length prog.omeths) 
      {  scall_returnType = SVoid;
	 scall_params = [];
	 scall_body = SBlock([]);
      }
    in
    let atomic n c accMap = 
      let env,attrMap = Cmap.fold 
	(fun n v (e,a) ->
	  (Cmap.add n (types_to_Sast v.v_type) e),(Cmap.add n { id_id = n; id_typ = types_to_Sast v.v_type} a)
	) c.oclass_attrs (Cmap.empty,Cmap.empty) in
      let env = Cmap.add "this" (SC c.oclass_name) env in
      (* typage d'un callable *)
      let tCall call returnType pos =
	let lenv,params = List.fold_left (fun (accEnv,accList) elt ->
	  let t = types_to_Sast elt.v_type in
	  (Cmap.add elt.v_name t accEnv),({id_id =elt.v_name;id_typ =t}::accList)
	) (env,[]) call.ocall_params in
	let _,instr,return = typInstr prog.oclasses false returnType pos c lenv call.ocall_body in
	if not return then (raise (EReturn(pos,pos)));
	{scall_returnType = returnType;
	 scall_params = params;
	 scall_body = instr; }
      in
      let mDescriptor = Array.make (Array.length c.oclass_methodesdesc) 0 in
      Array.iter (fun elt -> 
	mDescriptor.(elt.osimple_n) <- elt.osimple_id;
	let call = prog.omeths.(elt.osimple_id) in
	let call = tCall call (types_to_Sast elt.osimple_returnType) elt.osimple_pos in
	meths.(elt.osimple_id) <- call;
      ) c.oclass_methodesdesc ;
      let cDescriptor = Array.make (Array.length c.oclass_constsdesc) 0 in
      Array.iter (fun elt -> 
	cDescriptor.(elt.osimple_n) <- elt.osimple_id;
	let call = prog.omeths.(elt.osimple_id) in
	let call = tCall call SVoid elt.osimple_pos in
	meths.(elt.osimple_id) <- call;
      ) c.oclass_constsdesc ;
      let ext = match c.oclass_extends with
	| None -> None
	| Some foo -> Some (fst foo) in
      let nClass = {
	sclass_pos = c.oclass_pos;
	sclass_name = n;
	sclass_extends = ext;
	sclass_attrs = attrMap;
	sclass_consts = cDescriptor;
	sclass_methods = mDescriptor;
      }
      in
      Cmap.add n nClass accMap
    in
    let classes = Cmap.fold atomic prog.oclasses Cmap.empty in
    (* on crée une classe artificielle *)
    let classMain = {
      oclass_pos = ClassAnalysis.emptyPos;
      oclass_name = "Main";
      oclass_extends = None;
      oclass_attrs = Cmap.empty;
      oclass_methodesdesc = [||];
      oclass_constsdesc = [||];
    } in
    let env = Cmap.add "this" (SC "Main") Cmap.empty in
    let _,instr,return = typInstr (Cmap.add "Main" classMain prog.oclasses) false SVoid ClassAnalysis.emptyPos classMain env prog.oinstr in
    if not return then (raise (EReturn(ClassAnalysis.emptyPos,ClassAnalysis.emptyPos)));
    { sclasses = classes;
      smeths = meths;
      sinstr = instr; 
    }

end
	  
  
