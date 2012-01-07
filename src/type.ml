
open Ast.Past
open Ast.Sast

open TypeClass
open CheckInstr

let type_program p =
  { sclasses = [] ;
    sinstr = List.map (typInstr Cmap.empty) p.instr }
