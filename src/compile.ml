open Mips (* certains mots clefs communs sont écrasés par Ast.Sast *)
open Ast
open Sast
open Format


let compile_program p ofile =

  (** cette map permet de savoir quelles classes ont été traitées et retient
     des maps associant les champs à des numéros *)
  let classe_attrs = ref Cmap.empty in

  (** détermine la place de chaque champ de chaque classe *)
  let rec positionne_attrs name classe =
    let attrs =
    begin
      match classe.sclass_extends with
          Some pere ->
            if not (Cmap.mem pere !classe_attrs) then
              positionne_attrs pere (Cmap.find pere p.sclasses);
            Cmap.find pere !classe_attrs
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
    
  (** compteurs permettant d'avoir des labels différents *)
  let string_nb = ref 0 in
  let condition_nb = ref 0 in
  let for_nb = ref 0 in
  let expr_bool_nb = ref 0 in
  let print_preparation_nb = ref 0 in
  let soi = string_of_int in
  let int_of_bool = function true -> 1 | false -> 0 in
  let data = ref [
    DLabel "cast_failure" ;
    Asciiz "Cast failure" ;
    DLabel "print_failure" ;
    Asciiz "Erreur : la chaine passee en argument est un pointeur egal a NULL";
    DLabel "division_by_zero_failure" ;
    Asciiz "Division by zero failure" ;
    DLabel "null" ;
    Word 0] (* l'adresse de NULL *)
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
              let pos = Cmap.find id.id_id (Cmap.find classe !classe_attrs) in
              let pos = (pos + 1) * 4 in
              Arith (Add, V0, V0, Oimm pos) :: acc )
          | _ -> failwith "Typage mal fait : n'est pas un objet."
      with Not_found -> failwith "Erreur 42"
        

  (** calcule en utilisant la pile et les registres V0 et T0,
     stocke le résultat dans V0 *)
  and compile_expr expr env acc = match expr.sv with
    | SIconst i -> Li (V0, i) :: acc
    | SSconst s -> (* renvoit un objet de type String *)
      let adresse = "string_" ^ soi !string_nb in
      data := DLabel adresse :: Asciiz s :: !data ;
      incr string_nb ;
      (* alloc dynamique *)
      Li (V0, 9)
      :: Li (A0, 8)
      :: Syscall
      :: La(T0, "descr_general_String")
      :: Sw (T0, Areg(0, V0) )
      :: La (T0 , adresse)
      (* place l'adresse de la chaine dans le champ correspondant *)
      :: Sw (T0, Areg(4, V0))
      :: acc
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
      (* l'évaluation de and et or est paresseuse *)
      compile_expr e1 env ( (* compile e1 *)
        if e1.st = SBool & op = Or then (* nécessairement e2.st = SBool *)
          let label_fin = "expr_bool_fin_" ^ soi !expr_bool_nb in
          incr expr_bool_nb ;
            Bnez (V0, label_fin)
            :: compile_expr e2 env ( (* compile e2 et laisse dans V0 *)
              Label label_fin (* la valeur de retour se trouve déjà dans V0 *)
              :: acc )
        else if e1.st = SBool & op = And then
          let label_fin = "expr_bool_fin_" ^ soi !expr_bool_nb in
          incr expr_bool_nb ;
            Beqz (V0, label_fin)
            :: compile_expr e2 env ( (* compile e2 et laisse dans V0 *)
              Label label_fin (* la valeur de retour se trouve déjà dans V0 *)
              :: acc )
        else
          Sw (V0, Areg (0, SP)) :: (* et stocke sur la pile *)
            Arith (Sub, SP, SP, Oimm 4) ::
            compile_expr e2 env ( (* compile e2 et laisse dans V0 *)
                begin
                  if e1.st = SInt & e2.st = SInt then
                    begin
                      if op = Div or op = Mod then
                        function acc ->
                          Beqz (V0, "throw_division_by_zero_failure")
                          :: acc
                      else function acc -> acc
                    end (
                      Arith (Add, SP, SP, Oimm 4)
                      :: Lw (T0, Areg (0, SP)) (* cherche la valeur de e1 *)
                       :: Arith (
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
                       :: acc )
                  else if e1.st = SBool then (* nécessairement e2.st = SBool *)
                    Arith (Add, SP, SP, Oimm 4)
                    :: Lw (T0, Areg (0, SP)) (* cherche la valeur de e1 *)
                    :: Arith (Mips.Neq, V0, V0, Oimm 0)
                    (* on renormalise : V0 != 0 devient 1 ; 0 reste 0 afin
                       que la comparaison Eq ou Neq ait lieu correctement *)
                    :: Arith (Mips.Neq, T0, T0, Oimm 0)
                    :: Arith (
                      begin
                        match op with
                          | Eq -> Mips.Eq | Neq -> Mips.Neq
                          | _ -> failwith "Typage mal fait ou erreur 42"
                      end
                        , V0, T0, Oreg V0 )
                    :: acc
                  else if op = Eq then
                    Arith (Add, SP, SP, Oimm 4)
                    :: Lw (T0, Areg (0, SP)) (* cherche la valeur de e1 *)
                    :: Arith (Mips.Eq, V0, V0, Oreg T0) (* égalité physique *)
                    :: acc
                  else if op = Neq then
                    Arith (Add, SP, SP, Oimm 4)
                    :: Lw (T0, Areg (0, SP)) (* cherche la valeur de e1 *)
                    :: Arith (Mips.Neq, V0, V0, Oreg T0) (* égalité physique *)
                    :: acc
                  else if op = Plus & e1.st = SC "String" & e2.st = SC "String"
                  then
                    Arith (Add, SP, SP, Oimm 4)
                    :: Lw (A0, Areg (0, SP)) (* cherche la valeur de e1 *)
                    :: Move (A1, V0)
                    :: Jal "String_concat"
                    :: acc
                  else failwith "String of int not implemented"
                end ) )
    | SCast (typ, e) ->
      compile_expr e env (
        match typ with
          | STypeNull -> Bnez (V0, "cast_false") :: acc
          | SInt | SBool -> acc (* rien à faire *)
          | SC classe ->
            Move (A0, V0)
            :: La (A1, "descr_general_" ^ classe)
            :: Jal "cast"
            :: acc
          | _ -> failwith "On ne devrait pas avoir de cast avec void" )
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
            (Sw (V0, Areg (0, SP)) :: Arith (Sub, SP, SP, Oimm 4) :: acc) )
          args
          (La (T0, label_descripteur)
           :: Lw (T0, Areg(4 * i, T0) )
           :: Jalr T0
           :: Arith(Add, SP, SP, Oimm ((List.length args + 1) * 4))
           (* désalloue la place qu'occupaient les arguments *)
           :: acc) )
    | SGetval var ->
      (* la variable considérée est toujours un mot (4 octets) *)
      compile_vars var env (
        Lw (V0, Areg (0, V0)) :: acc )
    | SInstanceof (e, typ) ->
      compile_expr e env (
        match typ with
          | STypeNull -> Arith (Mips.Eq, V0, V0, Oimm 0) :: acc
          | SC classe ->
            Move (A0, V0)
            :: La (A1, "descr_general_" ^ classe)
            :: Jal "instanceof"
            :: acc
          | _ -> failwith "Typage de instanceof incorrect" )
    | SNew (name, constr, args) ->
      if name = "String" then
        (* alloc dynamique *)
        Li (V0, 9)
        :: Li (A0, 8)
        :: Syscall
        :: La (T0, "descr_general_String")
        :: Sw (T0, Areg(0, V0) )
        :: Li (T0, 0)
        :: Sw (T0, Areg(4, V0) )
        :: acc
      else begin
        try
          (* alloc dynamique *)
          Li (V0, 9)
          :: Li (A0, (Cmap.cardinal (Cmap.find name !classe_attrs) + 1) * 4)
          :: Syscall
          :: La(T0, "descr_general_" ^ name)
          :: Sw (T0, Areg(0, V0) )
          :: begin
            match constr with
                None -> acc
              | Some i ->
                (* appel du constructeur *)
                (* enregistre la valeur de l'objet sur la pile *)
                Sw (V0, Areg (0, SP))
                :: Arith (Sub, SP, SP, Oimm 4)
                (* évalue les arguments de gauche à droite *)
                :: List.fold_right
                  (fun expr acc -> compile_expr expr env
                    (Sw (V0, Areg (0, SP))
                     :: Arith (Sub, SP, SP, Oimm 4) :: acc) )
                  args
                  (La (T0, "descr_general_" ^ name)
                   :: Lw (T0, Areg(4 * (i + 1), T0) )
                   :: Jalr T0
                   (* désalloue la place qu'occupaient les arguments *)
                   :: Arith(Add, SP, SP, Oimm ((List.length args + 1) * 4))
                   (* récupère la valeur de l'objet *)
                   :: Lw (V0, Areg(0, SP) )
                   :: acc)
          end
        with _ -> failwith "Compilation de New"
      end
    | SPrint e -> (* prend en argument un objet de type String *)
      let label = "print_preparation_" ^ soi !print_preparation_nb in
      incr print_preparation_nb;
      compile_expr e env (
        Bnez (V0, label)
        (* sinon la chaîne pointe sur NULL, on lève une erreur *)
        :: La (A0, "print_failure") (* on affiche l'erreur *)
        :: Jal "print"
        :: Li (V0, 17) (* on termine avec un code d'erreur *)
        :: Li (A0, 2)
        :: Syscall
          
        :: Label label
        (* récupère l'adresse de la chaîne de caractères *)
        :: Lw (A0, Areg(4, V0))
        :: Jal "print" :: acc )
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
      Sw (RA, Areg(-4, FP)) :: (* sauvegarde RA *)
      Arith (Add, SP, FP, Oimm (frame_size - 4)) :: (* Alloue la frame *)
      body @
      [Arith (Add, SP, SP, Oimm (-frame_size + 4)); (* désalloue la frame *)
       Lw (RA, Areg(-4, FP)); (* récupère la valeur de RA *)
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
      Sw (RA, Areg(-4, FP)) :: (* sauvegarde RA *)
      Arith (Add, SP, FP, Oimm (frame_size - 4)) :: (* Alloue la frame *)
      body @
      [Arith (Add, SP, SP, Oimm (-frame_size + 4)); (* désalloue la frame *)
       Lw (RA, Areg(-4, FP)); (* récupère la valeur de RA *)
       Jr RA]
  in


  let methodes = ref
      [Label "print"; (* implémentation de print *)
       Li (V0, 4);
       Syscall;
       Jr RA;

       Label "instanceof"; (* implémentation de instanceof *)
       (* A0 contient un objet , A1 l'adresse du descripteur de la classe
          dont on teste si elle est parent *)
       Beqz (A0, "instanceof_true"); (* teste si l'objet est null *)

       (* on récupère l'adresse du descripteur de la classe de l'objet A0 *)
       (* ou celui de la classe parent *)
       Label "instanceof_aux";
       Lw (A0, Areg(0, A0));
       Beq (A0, A1, "instanceof_true"); (* si les classes sont les mêmes *)
       Beqz (A0, "instanceof_false"); (* sinon si on a atteint Object *)
       J "instanceof_aux";

       Label "instanceof_true";
       Li (V0, 1);
       Jr RA;
       Label "instanceof_false";
       Li (V0, 0);
       Jr RA;

       Label "cast"; (* implémentation de cast *)
       (* A0 contient un objet , A1 l'adresse du descripteur de la classe
          dont on teste si elle est parent *)
       Move (V0, A0); (* on renverra l'objet si le cast marche *)
       Beqz (A0, "cast_true"); (* teste si l'objet est null *)

       (* on récupère l'adresse du descripteur de la classe de l'objet A0 *)
       (* ou celui de la classe parent *)
       Label "cast_aux";
       Lw (A0, Areg(0, A0));
       Beq (A0, A1, "cast_true"); (* si les classes sont les mêmes *)
       Beqz (A0, "cast_false"); (* sinon si on a atteint Object *)
       J "cast_aux";

       Label "cast_true";
       Jr RA;
       Label "cast_false";
       La (A0, "cast_failure"); (* on affiche l'erreur *)
       Jal "print";
       Li (V0, 17); (* on termine avec un code d'erreur *)
       Li (A0, 2);
       Syscall;

       Label "String_equals"; (* implémentation de String.equals *)
       (* this et l'argument à comparer sont sur la pile *)
       Lw (A0, Areg(4, SP)) ;
       Lw (A1, Areg(8, SP)) ;
       (* si les pointeurs sont les mêmes, a fortiori c'est la même chaîne *)
       Beq (A0, A1, "String_equals_true") ;
       (* si l'un seulement est NULL *)
       Beqz (A0, "String_equals_false") ;
       Beqz (A1, "String_equals_false") ;
       (* sinon on compare les chaînes caractère par caractère *)
       Lw (A0, Areg(4, A0)) ; (* chargement des adresse des chaînes *)
       Lw (A1, Areg(4, A1)) ;
       
       Label "String_equals_boucle";
       Bne (A0, A1, "String_equals_false");
       Beqz (A0, "String_equals_true");
       Arith (Add, A0, A0, Oimm 4);
       Arith (Add, A1, A1, Oimm 4);
       J "String_equals_boucle";

       Label "String_equals_false" ;
       Li (V0, 0) ;
       Jr RA ;
       
       Label "String_equals_true" ;
       Li (V0, 1) ;
       Jr RA ;

       Label "throw_division_by_zero_failure" ;
       La (A0, "division_by_zero_failure") ;
       Jal "print" ;
       Li (A0, 2) ;
       Li (V0, 17);
       Syscall;

       Label "String_concat" ; (* implémentation de la concaténation *)
       Li (T0 , 1); (* on compte la nouvelle longueur *)
       (* ne lève pas d'erreur si on essaye la concaténation avec NULL *)
       Lw (A0, Areg(4, A0)) ; (* chargement des adresses des chaînes *)
       Lw (A1, Areg(4, A1)) ;

       Sw (A0, Areg(0, SP)); (* sauve les adresses des chaînes sur la pile *)
       Sw (A1, Areg(-4, SP));
       (* pas de mise a jour de SP car pas d'appels de fcts a venir *)
       
       Label "String_concat_boucle1";
       Lb (T1, Areg(0, A0));
       Beqz (T1, "String_concat_boucle2");
       Arith (Add, T0, T0, Oimm 1);
       Arith (Add, A0, A0, Oimm 1);
       J "String_concat_boucle1";
       
       Label "String_concat_boucle2";
       Lb (T1, Areg(0, A1));
       Beqz (T1, "String_concat_allocs");
       Arith (Add, T0, T0, Oimm 1);
       Arith (Add, A1, A1, Oimm 1);
       J "String_concat_boucle2";
       
       Label "String_concat_allocs";
       (* alloc dynamique : création de la chaîne *)
       Li (V0, 9);
       Move (A0, T0);
       Syscall;
       Move (T0, V0);

       (* récupération des adresses des deux chaînes *)
       Lw (A0, Areg(0, SP));
       Lw (A1, Areg(-4, SP));
       
       (* construction de la chaîne concaténée *)
       Label "String_concat_boucle3";
       Lb (T1, Areg(0, A0)) ;
       Beqz (T1, "String_concat_avantboucle4");
       Sb (T1, Areg(0, T0)) ; (* copie un caractère *)
       Arith (Add, T0, T0, Oimm 1); (* position dans la nouvelle chaîne *)
       Arith (Add, A0, A0, Oimm 1);
       J "String_concat_boucle3";
       
       Label "String_concat_avantboucle4";
       Lb (T1, Areg(0, A1)) ; (* copie un caractère *)
       Sb (T1, Areg(0, T0)) ;
       Label "String_concat_boucle4";
       (* on sort après avoir écrit le caractère de fin de chaîne *)
       Beqz (T1, "String_concat_allocs2");
       Arith (Add, T0, T0, Oimm 1);
       Arith (Add, A1, A1, Oimm 1);
       Lb (T1, Areg(0, A1)) ; (* copie un caractère *)
       Sb (T1, Areg(0, T0)) ;
       J "String_concat_boucle4";
       
       Label "String_concat_allocs2";
       Move (T0, V0); (* sauve l'adresse de la chaîne *)
       (* alloc dynamique *)
       Li (V0, 9);
       Li (A0, 8);
       Syscall;
       (* place l'adresse de la chaine dans le champ correspondant *)
       Sw (T0, Areg(4, V0));
      
       La(T0, "descr_general_String");
       Sw (T0, Areg(0, V0) );
       Jr RA
      (* rajouter les implémentations de concat et String_ofint *)
      ] in
  let n = Array.length p.smeths in
  for i = 0 to n - 1 do
    methodes := compile_meth p.smeths.(i) i !methodes
  done;


      
  (** compilation des classes : crée un descripteur à placer dans le tas
      suivi de acc *)
  let compile_classe name classe acc =
    let methodes = ref [] in
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
            Some s -> AWord ("descr_general_" ^ s)
          | None -> Word 0
      end ::
      !constructeurs @
      DLabel ("descr_meth_" ^ name) ::
      !methodes
      @ acc
  in

  let classes = Cmap.fold compile_classe p.sclasses [
    DLabel "descr_general_String"; (* description de String , à part *)
    Word 0; (* hérite de Object *)
    DLabel "descr_meth_String";
    AWord ("String_equals") (* seule méthode de String disponible *)
  ] in

  let p = { text = main @ !methodes ; data = !data @ classes } in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  print_program fmt p
