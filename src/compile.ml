open Mips (* certains mots clefs communs sont écrasés par Ast.Sast *)
open Ast.Sast
open Format

(* A faire :
   Lors de la compilation d'une méthode.
   calcul de la taille du tableau d'activation pour chaque méthode
   idem pour le main *)

module Smap = Map.Make(String)
  

let compile_program p ofile =
  let string_nb = ref 0 in
  let condition_nb = ref 0 in
  let for_nb = ref 0 in
  let soi = string_of_int in
  let int_of_bool = function true -> 1 | false -> 0 in
  let data = ref [Word ("null", 0)] (* l'adresse de NULL *)
  in
  (** convention : SP pointe toujours sur le prochain mot libre de la pile*)
  (** compilation d'une méthode
      création du tableau d'activation, sauvegarde de RA et de FP
      on considère que la totalité des arguments doivent être passés
      sur la pile, ce qui sera donc le début du tableau d'activation ;
      les instructions compilées sont placées devant l'accumulateur acc *)
 (* let compile_meth *)

  (** calcule l'adresse de var en utilisant la pile, stocke le
           résultat dans A0 ;
      env est une table d'association dont les clés sont les variables
      locales (des chaînes de caractères) et où la valeur associée est la
      position par rapport à $fp (en octets) *)
  let rec compile_vars var env acc = match var with
    | SVar { id_id = id } ->
      begin
      try let pos_relative = Smap.find id env in
          (* la variable doit être locale à la méthode car on aura rajouté
             this devant sinon au typage *)
          Arith (Sub, A0, FP, Oimm pos_relative) :: acc
      with Not_found -> failwith "Variable inconnue. Analyse de portée mal faite !"
      end
    | SAttr (e, id) -> failwith "Not implemented"
(* D'abord compiler les classes
      compile e env ( *)
        
        

  (** calcule en utilisant la pile et les registres A0 et A1,
     stocke le résultat dans A0 *)
  and compile_expr expr env acc = match expr.sv with
    | SIconst i -> Li (A0, i) :: acc
    | SSconst s ->
      let adresse = "string_" ^ soi !string_nb in
      data := Asciiz (adresse , s) :: !data ;
      incr string_nb ;
      La (A0 , adresse) :: acc
    | SBconst b -> Li (A0, int_of_bool b) :: acc
    | SNull -> La (A0, "null") :: acc
    | SNot e -> compile_expr e env (Arith (Mips.Eq, A0, A0, Oimm 0) :: acc)
    | SUMinus e -> compile_expr e env (Neg (A0, A0) :: acc)
    | SPref (op, var) ->
      compile_vars var env (
        Move (T0, A0) ::
          Lw (A0, Areg (0, T0)) ::
          Arith ( begin match op with Incr -> Add | Decr -> Sub end ,
            A0, A0, Oimm 1 ) ::
          Sw (A0, Areg (0, T0)) :: acc )
        (* la nouvelle valeur est replacée à l'adresse indiquée par T0
           et reste dans A0 en tant que valeur de retour *)
    | SPost (op, var) ->
      compile_vars var env (
        Move (T0, A0) ::
          Lw (A0, Areg (0, T0)) ::
          Arith ( begin match op with Incr -> Add | Decr -> Sub end ,
            T1, A0, Oimm 1 ) ::
          Sw (T1, Areg (0, T0)) :: acc )
        (* la nouvelle valeur est replacée à l'adresse indiquée par T0 et
           l'ancienne valeur reste dans A0 en tant que valeur de retour *)
    | SBinaire (op, e1, e2) ->
      (* surcharges de + et de = non gérées :
         seulement fonctionne avec les ints et bool *)
      compile_expr e1 env ( (* compile e1 et stocke sur la pile *)
        Sw (A0, Areg (0, SP)) ::
          Arith (Sub, SP, SP, Oimm 4) ::
          compile_expr e2 env ( (* compile e2 et laisse dans A0 *)
            Arith (Add, SP, SP, Oimm 4) ::
              Lw (A1, Areg (0, SP)) :: (* cherche la valeur de e1 *)
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
                    , A0, A1, Oreg A0 )
                :: acc
              else if e1.st = SBool & e2.st = SBool then
                Arith (Mips.Neq, A0, A0, Oimm 0) ::
              (* on renormalise : A0 != 0 devient 1 ; 0 reste 0
                 afin que la comparaison Eq ou Neq ait lieu correctement *)
                  Arith (Mips.Neq, A1, A1, Oimm 0) ::
                  Arith (
                    begin
                      match op with
                      | Eq -> Mips.Eq | Neq -> Mips.Neq
                      | Or -> Add
                      | And -> Mul
                      | _ -> failwith "Typage mal fait"
                  end
                    , A0, A1, Oreg A0 )
                :: acc
              else failwith "Not implemented"
              (* rajouter les comparaisons généralisées et la concaténation 
                 de chaînes *)
              end ) )
    | SAssign (var, e) ->
      (* compile la valeur gauche avant l'expression comme javac *)
      compile_vars var env ( (* compile var et stocke sur la pile *)
        Sw (A0, Areg (0, SP)) ::
          Arith (Sub, SP, SP, Oimm 4) ::
          compile_expr e env ( (* compile e et laisse dans A0 car ce sera
                                  aussi la valeur de retour *)
              Arith (Add, SP, SP, Oimm 4) ::
              Lw (T0, Areg (0, SP)) :: (* cherche la valeur de var et place 
                                          dans T0 *)
              Sw (A0, Areg (0, T0)) :: acc ) ) (* assign proprement dit *)
    | SGetval var ->
      (* la variable considérée est toujours un mot (4 octets) *)
      compile_vars var env (
        Lw (A0, Areg (0, A0)) :: acc )
    | SPrint e ->
      compile_expr e env (
        Jal "print" :: acc )
    | _ -> failwith "Not implemented"
  in
  (** compile une liste d'instructions et fait suivre le résultat de acc
      retient la dernière position occupée
      et la taille de frame nécessaire *)
  let rec compile_instrs instrs env pos frame_size acc =
    match instrs with
      | [] -> acc
      | instr :: t -> match instr with
          | SExpr e ->
            compile_expr e env (compile_instrs t env pos frame_size acc)
          | SDecl (_, _, id, e) ->
            begin
              match e with
                | Some e -> compile_expr e env
                | None -> function acc -> acc
            end (
              let frame_size =
                if pos = frame_size then frame_size + 4 else frame_size
              in
              let pos = pos + 4 in
              Arith (Sub, T0, FP, Oimm nb_vars) :: (* calcule la position *)
                Sw (A0, Areg (0, T0)) :: (* enregistre la valeur *)
        (* si elle n'a pas été calculée la valeur n'est pas
           définie *)
                (compile_instrs
                   t
                   (Smap.add id.id_id pos env) (* ajoute l'id à l'env *)
                   pos frame_size acc) )
          | SIf (e, i, Some i') ->
            let label_else = "else" ^ (soi !condition_nb) in
            let label_fin = "endif" ^ (soi !condition_nb) in
            incr condition_nb ;
            compile_expr e env (
              Beqz (A0, label_else) ::
                compile_instr i env (
                  Label label_else ::
                    compile_instr i' env (
                      Label label_fin ::
                        (compile_instrs t env pos frame_size acc) ) ) )
          | SIf (e, i, None) ->
            let label_fin = "endif" ^ (soi !condition_nb) in
            incr condition_nb ;
            compile_expr e env (
              Beqz (A0, label_fin) ::
                compile_instr i env (
                  Label label_fin ::
                    (compile_instrs t env pos frame_size acc) ) )
          | SFor (e1, e2, e3, i) ->
            let label_debut = "for_debut" ^ (soi !for_nb) in
            let label_fin = "for_fin" ^ (soi !for_nb) in
            incr for_nb ;
            begin (* initialisation *)
              match e1 with
                | Some e -> compile_expr e env
                | None -> function acc -> acc
            end (
              Label label_debut ::
                compile_expr e2 env (
                  Beqz (A0, label_fin) ::
                    begin (* instruction *)
                      match i with
                        | Some i -> compile_instr i env
                        | None -> function acc -> acc
                    end (
                      begin (* incrémentation *)
                        match e3 with
                          | Some e -> compile_expr e env
                          | None -> function acc -> acc
                      end (
                        Label label_fin ::
                          (compile_instrs t env pos frame_size acc) ) ) ) )
          | _ -> failwith "Not implemented"
  in
  let instrs =
    Label "main" ::
      Move (S0, RA) ::
(*     Arith (Mips.Sub, SP, SP, Oimm !frame_size); (* Alloue la frame *)
       Arith (Mips.Add, FP, SP, Oimm (!frame_size - 4)) (* Initialise $fp *)*)
      compile_instr p.sinstr Smap.empty [
        (* fin de main *)
        (*        Arith (Mips.Add, SP, SP, Oimm !frame_size); (* Désalloue la frame *)*)
        Move (RA, S0);
        Jr RA;
        Label "print"; (* implémentation de print *)
        Li (V0, 1);
        Syscall;
        Jr RA      
      ] in
  let p = { text = instrs ; data = !data } in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  print_program fmt p
