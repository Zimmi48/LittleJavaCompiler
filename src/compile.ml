open Mips
open Ast.Sast
open Format

let compile_program p ofile =
  let string_nb = ref 0 in
  let soi = string_of_int in
  let int_of_bool = function true -> 1 | false -> 0 in
  let data = ref [Word ("null", 0)] in (* l'adresse de NULL *)
  (* calcule en utilisant la pile, et stocke le résultat dans A0 *)
  (* les instructions compilées sont placées devant l'accumulateur acc *)
  let rec compile_expr expr acc = match expr.sv with
    | SIconst i -> Li (A0, i) :: acc
    | SSconst s ->
      let adresse = "string_" ^ soi !string_nb in
      data := Asciiz (adresse , s) :: !data ;
      incr string_nb ;
      La (A0 , adresse) :: acc
    | SBconst b -> Li (A0, int_of_bool b) :: acc
    | SNull -> La (A0, "null") :: acc
    | SUnaire (op, e) ->
      compile_expr e (
        match op with
          | SIncr -> Arith (Add, A0, A0, Oimm 1) :: acc
          | SDecr -> Arith (Sub, A0, A0, Oimm 1) :: acc
          | SNot -> Arith (Eq, A0, A0, Oimm 0) :: acc
          | SUMinus -> Neg (A0, A0) :: acc
      )
    | SBinaire (op, e1, e2) ->
      (* surcharge des opérateurs non gérées :
         seulement fonctionne avec les ints et bool *)
      compile_expr e2 (
        Sw (A0, Areg (0, SP)) ::
          Arith (Sub, SP, SP, Oimm 4) ::
          compile_expr e1 (
            Arith (Add, SP, SP, Oimm 4) ::
              Lw (A1, Areg (0, SP)) ::
              Arith ( begin
                match op with
                  | SEq -> Eq | SNeq -> Neq | SLeq -> Leq | SGeq -> Geq
                  | SLt -> Lt | SGt -> Gt
                  | SPlus | SOr -> Add
                  | SMinus -> Sub
                  | SStar | SAnd -> Mul
                  | SDiv -> Div | SMod -> Mod
              end
              , A0, A0, Oreg A1) :: acc) )
    | _ -> failwith "Not implemented"
  in
  let rec compile_instr instr acc = match instr with
    | SExpr e -> compile_expr e acc
    | _ -> failwith "Not implemented"
  in
  let instrs = List.fold_right compile_instr p.sinstr [] in
  let p = { text = instrs ; data = !data } in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  print_program fmt p
