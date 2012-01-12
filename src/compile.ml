open Mips (* certains mots clefs communs sont écrasés par Ast.Sast *)
open Ast
open Sast
open Format


let compile_program p ofile =

  (* cette map permet de savoir quelles classes ont été traitées et retient
     des maps associant les champs à des numéros *)
  let classe_attrs = ref Cmap.empty in
  (** détermine la place de chaque champ de chaque classe *)
  let rec positionne_attrs name classe =
    let attrs =
    begin
      match classe.sclass_extends with
          Some pere ->
            if not (Cmap.mem pere.id_id !classe_attrs) then
              positionne_attrs pere.id_id (Cmap.find pere.id_id p.sclasses);
            Cmap.find pere.id_id !classe_attrs
        | _ -> Cmap.empty
    end
    in
    let last_id_nb = ref (Cmap.cardinal attrs - 1) in
    let attrs = Cmap.fold
      (fun id_name _ attrs ->
        incr last_id_nb ;
        Cmap.add id_name !last_id_nb attrs)
      classe.sclass_attrs
      attrs
    in
    classe_attrs := Cmap.add name attrs !classe_attrs
  in
  let () = Cmap.iter positionne_attrs p.sclasses in
    

  let string_nb = ref 0 in
  let condition_nb = ref 0 in
  let for_nb = ref 0 in
  let soi = string_of_int in
  let int_of_bool = function true -> 1 | false -> 0 in
  let data = ref [DLabel "null" ; Word 0] (* l'adresse de NULL *)
  in
  (** convention : SP pointe toujours sur le prochain mot libre de la pile*)



  (** calcule l'adresse de var en utilisant la pile, stocke le
           résultat dans V0 ;
      env est une table d'association dont les clés sont les variables
      locales (des chaînes de caractères) et où la valeur associée est la
      position par rapport à $fp (en octets) ;
      les instructions compilées sont placées devant l'accumulateur acc *)
  let rec compile_vars var env acc = match var with
    | SVar { id_id = id } ->
      begin
      try let pos_relative = Cmap.find id env in
          (* la variable doit être locale à la méthode car on aura rajouté
             this devant sinon au typage *)
          Arith (Add, V0, FP, Oimm pos_relative) :: acc
      with Not_found -> failwith "Variable inconnue. Analyse de portée mal faite !"
      end
    | SAttr (e, id) ->
      try
        match e.st with
          | SC classe ->
            compile_expr e env (
              let pos = Cmap.find id.id_id (Cmap.find classe !classe_attrs) 
              in
              let pos = (pos + 1) * 4 in
              Lw (V0, Areg(pos, V0)) :: acc )
          | _ -> failwith "Typage mal fait : n'est pas un objet."
      with Not_found -> failwith "Erreur 42"
        

  (** calcule en utilisant la pile et les registres V0 et T0,
     stocke le résultat dans V0 *)
  and compile_expr expr env acc = match expr.sv with
    | SIconst i -> Li (V0, i) :: acc
    | SSconst s ->
      let adresse = "string_" ^ soi !string_nb in
      data := DLabel adresse :: Asciiz s :: !data ;
      incr string_nb ;
      La (V0 , adresse) :: acc
    | SBconst b -> Li (V0, int_of_bool b) :: acc
    | SNull -> La (V0, "null") :: acc
    | SNot e -> compile_expr e env (Arith (Mips.Eq, V0, V0, Oimm 0) :: acc)
    | SUMinus e -> compile_expr e env (Neg (V0, V0) :: acc)
    | SPref (op, var) ->
      compile_vars var env (
        Move (T0, V0) ::
          Lw (V0, Areg (0, T0)) ::
          Arith ( begin match op with Incr -> Add | Decr -> Sub end ,
            V0, V0, Oimm 1 ) ::
          Sw (V0, Areg (0, T0)) :: acc )
        (* la nouvelle valeur est replacée à l'adresse indiquée par T0
           et reste dans V0 en tant que valeur de retour *)
    | SPost (op, var) ->
      compile_vars var env (
        Move (T0, V0) ::
          Lw (V0, Areg (0, T0)) ::
          Arith ( begin match op with Incr -> Add | Decr -> Sub end ,
            T1, V0, Oimm 1 ) ::
          Sw (T1, Areg (0, T0)) :: acc )
        (* la nouvelle valeur est replacée à l'adresse indiquée par T0 et
           l'ancienne valeur reste dans V0 en tant que valeur de retour *)
    | SBinaire (op, e1, e2) ->
      (* surcharges de + et de = non gérées :
         seulement fonctionne avec les ints et bool *)
      (* attention : l'évaluation de and et or doit être paresseuse *)
      compile_expr e1 env ( (* compile e1 et stocke sur la pile *)
        Sw (V0, Areg (0, SP)) ::
          Arith (Sub, SP, SP, Oimm 4) ::
          compile_expr e2 env ( (* compile e2 et laisse dans V0 *)
            Arith (Add, SP, SP, Oimm 4) ::
              Lw (T0, Areg (0, SP)) :: (* cherche la valeur de e1 *)
              begin
              if e1.st = SInt & e2.st = SInt then
                Arith (
                  begin
                    match op with
                      | Eq -> Mips.Eq | Neq -> Mips.Neq
                      | Leq -> Mips.Leq | Geq -> Mips.Geq
                      | Lt -> Mips.Lt | Gt -> Mips.Gt
                      | Plus -> Add
                      | Minus -> Sub
                      | Star -> Mul
                      | Div -> Mips.Div | Mod -> Mips.Mod
                      | _ -> failwith "Typage mal fait"
                  end
                    , V0, T0, Oreg V0 )
                :: acc
              else if e1.st = SBool & e2.st = SBool then
                Arith (Mips.Neq, V0, V0, Oimm 0) ::
              (* on renormalise : V0 != 0 devient 1 ; 0 reste 0
                 afin que la comparaison Eq ou Neq ait lieu correctement *)
                  Arith (Mips.Neq, T0, T0, Oimm 0) ::
                  Arith (
                    begin
                      match op with
                      | Eq -> Mips.Eq | Neq -> Mips.Neq
                      | Or -> Add
                      | And -> Mul
                      | _ -> failwith "Typage mal fait"
                  end
                    , V0, T0, Oreg V0 )
                :: acc
              else failwith "Not implemented"
              (* rajouter les comparaisons généralisées et la concaténation 
                 de chaînes *)
              end ) )
    | SAssign (var, e) ->
      (* compile la valeur gauche avant l'expression comme javac *)
      compile_vars var env ( (* compile var et stocke sur la pile *)
        Sw (V0, Areg (0, SP)) ::
          Arith (Sub, SP, SP, Oimm 4) ::
          compile_expr e env ( (* compile e et laisse dans V0 car ce sera
                                  aussi la valeur de retour *)
              Arith (Add, SP, SP, Oimm 4) ::
              Lw (T0, Areg (0, SP)) :: (* cherche la valeur de var et place 
                                          dans T0 *)
              Sw (V0, Areg (0, T0)) :: acc ) ) (* assign proprement dit *)
    | SCall (e, i, args) ->
      let label_descripteur =
        match e.st with
          | SC classe -> "descr_meth_" ^ classe
          | _ -> failwith "Typage mal fait"
      in
      (* compile l'objet e et place sur la pile *)
      compile_expr e env (
        Sw (V0, Areg (0, SP)) ::
          Arith (Sub, SP, SP, Oimm 4) ::
          (* évalue les arguments de gauche à droite *)
          List.fold_right
          (fun expr acc -> compile_expr expr env
            (Arith (Sub, SP, SP, Oimm 4) :: acc) )
          args
          (La (T0, label_descripteur)
           :: Lw (T0, Areg(4 * i, T0) )
           :: Jalr (T0, RA)
           :: Arith(Add, SP, SP, Oimm ((List.length args + 1) * 4))
           (* désalloue la place qu'occupaient les arguments *)
           :: acc) )
    | SGetval var ->
      (* la variable considérée est toujours un mot (4 octets) *)
      compile_vars var env (
        Lw (V0, Areg (0, V0)) :: acc )
(*    | SNew (name, constr, args) ->*)
      
    | SPrint e ->
      compile_expr e env (
        Move (A0, V0) ::
        Jal "print" :: acc )
    | _ -> failwith "Not implemented"
  in



  (** compile une liste d'instructions et fait suivre le résultat de acc
      retient la dernière position occupée
      et la taille de frame nécessaire (comptée en négatif)
      renvoit un produit Mips list * int = code * frame_size *)
  let rec compile_instrs instrs env label_retour pos frame_size acc =
    let compile_instr x = compile_instrs [x] env label_retour pos in
    match instrs with
      | [] -> acc , frame_size
      | instr :: t -> match instr with
          | SExpr e ->
            let t , frame_size =
              compile_instrs t env label_retour pos frame_size acc in
            compile_expr e env t , frame_size
          | SDecl (_, _, id, e) ->
            let frame_size =
              if pos = frame_size then frame_size - 4 else frame_size
            in
            let pos = pos - 4 in
            let t , frame_size =
              compile_instrs
                t
                (Cmap.add id.id_id pos env) (* ajoute l'id à l'env *)
                label_retour pos frame_size acc
            in
            begin
              match e with
                | Some e -> compile_expr e env
                | None -> function acc -> acc
            end (
              Arith (Add, T0, FP, Oimm pos) :: (* calcule la position *)
                Sw (V0, Areg (0, T0)) :: (* enregistre la valeur *)
                (* si elle n'a pas été calculée la valeur n'est pas
                   définie *)
                t ) , frame_size
          | SIf (e, i, i') ->
            let label_fin = "endif_" ^ (soi !condition_nb) in
            let label_else_ou_fin =
              if i' = None then label_fin
              else "else_" ^ (soi !condition_nb) in
            incr condition_nb ;
            let t , frame_size =
              compile_instrs t env label_retour pos frame_size acc in
            let t , frame_size =
              match i' with
                | Some i' ->
                  let t , frame_size =
                    compile_instr i' frame_size
                      (Label label_fin :: t)
                  in
                  J label_fin :: Label label_else_ou_fin :: t , frame_size
                | None -> Label label_else_ou_fin :: t , frame_size
            in
            let t , frame_size =
              compile_instr i frame_size t
            in
            compile_expr e env ( Beqz (V0, label_else_ou_fin) :: t ) ,
            frame_size
          | SFor (e1, e2, e3, i) ->
            let label_debut = "for_debut_" ^ (soi !for_nb) in
            let label_fin = "for_fin_" ^ (soi !for_nb) in
            incr for_nb ;
            let t , frame_size =
              compile_instrs t env label_retour pos frame_size acc in
            let t , frame_size =
              begin (* instruction *)
                match i with
                  | Some i -> compile_instr i frame_size
                  | None -> function acc -> acc , frame_size
              end (
                begin (* incrémentation *)
                  match e3 with
                    | Some e -> compile_expr e env
                    | None -> function acc -> acc
                end (
                  Label label_fin ::
                    t ) )
            in
            begin (* initialisation *)
              match e1 with
                | Some e -> compile_expr e env
                | None -> function acc -> acc
            end (
              Label label_debut ::
                compile_expr e2 env (
                  Beqz (V0, label_fin) ::
                    t ) ) , frame_size
          | SBlock instrs ->
            let t , frame_size =
              compile_instrs t env label_retour pos frame_size acc in
            compile_instrs instrs env label_retour pos frame_size t
          | SReturn e ->
            let t , frame_size =
              compile_instrs t env label_retour pos frame_size acc in
            begin
            match e with
                Some e -> compile_expr e env
              | None -> function acc -> acc
            end
              (J label_retour :: t) , frame_size
  in


  (** compilation d'une méthode
      création du tableau d'activation, sauvegarde de RA et de FP
      on considère que la totalité des arguments doivent être passés
      sur la pile, ce qui sera donc le début du tableau d'activation,
      this est passé comme "premier argument",
      FP pointera sur la case contenant RA qui suit celle sauvegardant FP
      de l'appelant, et précède les variables locales *)
  let compile_meth meth i acc =
    let env , pos = List.fold_left (
      fun (env,pos) id -> Cmap.add id.id_id pos env, pos + 4 )
      (Cmap.empty, 8)
      meth.scall_params
    in
    let env = Cmap.add "this" pos env in
    let label_retour = "fin_meth_" ^ soi i in
    let label_debut = "debut_meth_" ^ soi i in
    let body , frame_size =
      compile_instrs
        [meth.scall_body]
        env
        label_retour
        (-4)
        (-4)
        [Label label_retour]
    in
    Label label_debut ::
      Sw (FP, Areg(0, SP)) :: (* sauvegarde FP *)
      Arith (Sub, FP, SP, Oimm 4) :: (* initialise FP *)
      Sw (RA, Areg(0, FP)) :: (* sauvegarde RA *)
      Arith (Add, SP, FP, Oimm frame_size) :: (* Alloue la frame *)
      body @
      [Arith (Add, SP, SP, Oimm (-frame_size)); (* désalloue la frame *)
       Lw (RA, Areg(0, SP)); (* récupère la valeur de RA *)
       Arith (Add, SP, SP, Oimm 4);
       Lw (FP, Areg(0, SP)); (* et celle de FP de l'appelant *)
       Jr RA] @ acc
       (* remarque : la valeur de retour se trouve déjà dans V0 *)
  in   
      
  let main =
   let label_retour = "fin_main" in
    let label_debut = "main" in
    let body , frame_size =
      compile_instrs
        [p.sinstr]
        Cmap.empty
        label_retour
        (-4)
        (-4)
        [Label label_retour]
    in
    Label label_debut ::
      Move(FP, SP) :: (* initialise FP *)
      Sw (RA, Areg(0, FP)) :: (* sauvegarde RA *)
      Arith (Add, SP, FP, Oimm frame_size) :: (* Alloue la frame *)
      body @
      [Arith (Add, SP, SP, Oimm (-frame_size)); (* désalloue la frame *)
       Lw (RA, Areg(0, SP)); (* récupère la valeur de RA *)
       Jr RA]
  in


  let methodes = ref
      [Label "print"; (* implémentation de print *)
       Li (V0, 1);
       Syscall;
       Jr RA] in
  let n = Array.length p.smeths in
  for i = 0 to n - 1 do
    methodes := compile_meth p.smeths.(i) i !methodes
  done;


      
  (** compilation des classes : crée un descripteur à placer dans le tas
      suivi de acc *)
  let compile_classe name classe acc =
    let methodes = ref [] in (* rajouter String *)
    let n = Array.length classe.sclass_methods in
    for i = n - 1 downto 0 do
      methodes := AWord ("debut_meth_" ^ soi classe.sclass_methods.(i) )
        :: !methodes
    done;
    let constructeurs = ref [] in
    let n = Array.length classe.sclass_consts in
    for i = n - 1 downto 0 do
      constructeurs := AWord ("debut_meth_" ^ soi classe.sclass_consts.(i) )
        :: !constructeurs
    done;
    DLabel ("descr_general_" ^ name) ::
      begin (* la classe parent *)
        match classe.sclass_extends with
            Some s -> AWord ("descr_" ^ s.id_id)
          | None -> Word 0
      end ::
      !constructeurs @
      DLabel ("descr_meth_" ^ name) ::
      !methodes
  in
     

  let classes = Cmap.fold compile_classe p.sclasses [] in

  let p = { text = main @ !methodes ; data = !data @ classes } in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  print_program fmt p
