open Mips
open Ast.Sast
open Format

let compile_program p ofile =
  let string_nb = ref 0 in
  let soi = string_of_int in
  let int_of_bool = function true -> 1 | false -> 0 in
  let data = ref [Word ("null", 0)] in (* l'adresse de NULL *)
  let rec compile_vars var acc = match var with
    | SVar { id_id = id } -> failwith "Not implemented"
    | SAttr (var, id) -> failwith "Not implemented"
  (* calcule en utilisant la pile, et stocke le résultat dans A0 *)
  (* les instructions compilées sont placées devant l'accumulateur acc *)
  in
  let rec compile_expr expr acc = match expr.sv with
    | SIconst i -> Li (A0, i) :: acc
    | SSconst s ->
      let adresse = "string_" ^ soi !string_nb in
      data := Asciiz (adresse , s) :: !data ;
      incr string_nb ;
      La (A0 , adresse) :: acc
    | SBconst b -> Li (A0, int_of_bool b) :: acc
    | SNull -> La (A0, "null") :: acc
    | SNot e -> compile_expr e (Arith (Eq, A0, A0, Oimm 0) :: acc)
    | SUMinus e -> compile_expr e (Neg (A0, A0) :: acc)
(*    | SPref (op, var) ->
      compile_vars var (
    | SPost *)
    | SBinaire (op, e1, e2) ->
      (* surcharge des opérateurs non gérées :
         seulement fonctionne avec les ints et bool *)
      compile_expr e2 ( (* compile e2 et stocke sur la pile *)
        Sw (A0, Areg (0, SP)) ::
          Arith (Sub, SP, SP, Oimm 4) ::
          compile_expr e1 ( (* compile e1 et laisse dans A0 *)
            Arith (Add, SP, SP, Oimm 4) ::
              Lw (A1, Areg (0, SP)) :: (* cherche la valeur de e2 *)
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
                    , A0, A0, Oreg A1 )
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
                    , A0, A0, Oreg A1 )
                :: acc
              else failwith "Not implemented"
              (* rajouter les comparaisons généralisées et la concaténation 
                 de chaînes *)
              end ) )
    | SCall (m, args) ->
      begin
      match m with
          SAttr (
            SAttr ( SVar { id_id = "System" } , { id_id = "out" } ) ,
            { id_id = "print" } ) ->
            begin
            match args with
              | [e] ->
                  compile_expr e (
                    Jal "print" ::
                      acc )
              | _ -> failwith "Typage mal fait"
            end
        | _ -> failwith "Not implemented"
      end
    | _ -> failwith "Not implemented"
  in
  let rec compile_instr instr acc = match instr with
    | SExpr e -> compile_expr e acc
    | _ -> failwith "Not implemented"
  in
  let instrs =
    [Label "main";
     Move (S0, RA);
(*     Arith (Mips.Sub, SP, SP, Oimm !frame_size); (* Alloue la frame *)
     Arith (Mips.Add, FP, SP, Oimm (!frame_size - 4)) (* Initialise $fp *)*)
    ] @
      List.fold_right compile_instr p.sinstr [
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
