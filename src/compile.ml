open Mips
open Ast
open Format

let compile_program ast ofile =
  let p = { text = [] ; data = [] } in


  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  print_program fmt p
