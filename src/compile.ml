open Mips
open Ast.Sast
open Format

(* A faire : calcul de la taille du tableau d'activation pour chaque méthode
   les variables définies à l'intérieur de blocs sont comptées *)
(* lors de l'appel d'une méthode, seul this est accessible en plus des vars
   locales *)
(* si une var a n'est pas locale, tester this.a *)
(* On utilise une table d'association dont les clés sont les variables
   locales (des chaînes de caractères) et où la valeur associée est la
   position par rapport à $fp (en octets) *)
module Smap = Map.Make(String)
  

let compile_program p ofile =
  let string_nb = ref 0 in
  let condition_nb = ref 0 in
  let soi = string_of_int in
  let int_of_bool = function true -> 1 | false -> 0 in
  let data = ref [Word ("null", 0)] (* l'adresse de NULL *)
  in
  (* calcule l'adresse de var en utilisant la pile, stocke le
           résultat dans A0 *)
  (* les instructions compilées sont placées devant l'accumulateur acc *)
  (* env est la SMap décrite plus haut *)
  let rec compile_vars var env acc = match var with
    | SVar { id_id = id } ->
      begin
      try let pos_relative = Smap.find id env in
          (* si la variable est locale à la méthode *)
          Arith (Add, A0, FP, Oimm pos_relative) :: acc
      with Not_found -> (*
        try
          let
          compile_expr (SGetval "this") env (
            
            acc ) 
        with 
          | Not_found -> *)failwith "Variable inconnue. Analyse de portée mal faite !"
      end
    | SAttr (var, id) -> failwith "Not implemented"

  (* calcule en utilisant la pile et les registres A0 et A1,
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
    | SNot e -> compile_expr e env (Arith (Eq, A0, A0, Oimm 0) :: acc)
    | SUMinus e -> compile_expr e env (Neg (A0, A0) :: acc)
    | SPref (op, var) ->
      compile_vars var env (
        Move (T0, A0) ::
          Lw (A0, Areg (0, T0)) ::
          Arith ( begin match op with SIncr -> Add | SDecr -> Sub end ,
            A0, A0, Oimm 1 ) ::
          Sw (A0, Areg (0, T0)) :: acc )
        (* la nouvelle valeur est replacée à l'adresse indiquée par T0
           et reste dans A0 en tant que valeur de retour *)
    | SPost (op, var) ->
      compile_vars var env (
        Move (T0, A0) ::
          Lw (A0, Areg (0, T0)) ::
          Arith ( begin match op with SIncr -> Add | SDecr -> Sub end ,
            T1, A0, Oimm 1 ) ::
          Sw (T1, Areg (0, T0)) :: acc )
        (* la nouvelle valeur est replacée à l'adresse indiquée par T0 et
           l'ancienne valeur reste dans A0 en tant que valeur de retour *)
    | SBinaire (op, e1, e2) ->
      (* surcharge des opérateurs non gérées :
         seulement fonctionne avec les ints et bool *)
      (* à modifier : java spécifie que les expressions sont compilées dans
         l'autre sens *)
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
                      | SEq -> Eq | SNeq -> Neq
                      | SLeq -> Leq | SGeq -> Geq
                      | SLt -> Lt | SGt -> Gt
                      | SPlus -> Add
                      | SMinus -> Sub
                      | SStar -> Mul
                      | SDiv -> Div | SMod -> Mod
                      | _ -> failwith "Typage mal fait"
                  end
                    , A0, A1, Oreg A0 )
                :: acc
              else if e1.st = SBool & e2.st = SBool then
                Arith (Neq, A0, A0, Oimm 0) ::
              (* on renormalise : A0 != 0 devient 1 ; 0 reste 0
                 afin que la comparaison Eq ou Neq ait lieu correctement *)
                  Arith (Neq, A1, A1, Oimm 0) ::
                  Arith (
                    begin
                      match op with
                      | SEq -> Eq | SNeq -> Neq
                      | SOr -> Add
                      | SAnd -> Mul
                      | _ -> failwith "Typage mal fait"
                  end
                    , A0, A1, Oreg A0 )
                :: acc
              else failwith "Not implemented"
              (* rajouter les comparaisons généralisées et la concaténation 
                 de chaînes *)
              end ) )
(*    | SCall (m, args) ->
      begin
      match m with
          SAttr (
            SAttr ( SVar { id_id = "System" } , { id_id = "out" } ) ,
            { id_id = "print" } ) ->
            begin
            match args with
              | [e] ->
                  compile_expr e env (
                    Jal "print" ::
                      acc )
              | _ -> failwith "Typage mal fait"
            end
        | _ -> failwith "Not implemented"
      end *)
    | SGetval var ->
      (* la variable considérée est toujours un mot (4 octets) *)
      compile_vars var env (
        Lw (A0, Areg (0, A0)) :: acc )
    | _ -> failwith "Not implemented"
  in
  let rec compile_instr instr env acc = match instr with
    | SExpr e -> compile_expr e env acc
    | SIf (e, i, Some i') ->
      let label_else = "else" ^ (ios condition_nb) in
      let label_fin = "endif" ^ (ios condition_nb) in
      incr condition_nb ;
      compile_expr e env (
        Beqz (A0, label_else) ::
          compile_instr i env (
            Label label_else ::
              compile_instr i' env (
                Label label_fin :: acc ) ) )
    | SIf (e, i, None) ->
      let label_fin = "endif" ^ (ios condition_nb) in
      incr condition_nb ;
      compile_expr e env (
        Beqz (A0, label_fin) ::
          compile_instr i env (
            Label label_fin :: acc ) )
    | _ -> failwith "Not implemented"
  in
  let instrs =
    [Label "main";
     Move (S0, RA);
(*     Arith (Mips.Sub, SP, SP, Oimm !frame_size); (* Alloue la frame *)
     Arith (Mips.Add, FP, SP, Oimm (!frame_size - 4)) (* Initialise $fp *)*)
    ] @
      List.fold_right (function i -> compile_instr i Smap.empty) p.sinstr [
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
