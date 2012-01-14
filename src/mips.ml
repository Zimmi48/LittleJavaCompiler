(* à partir d'un fichier de J-C. Filliâtre *)

type register = 
  | A0 | A1 | V0 | T0 | T1 | S0 | RA | SP | FP

type address =
  | Alab of string
  | Areg of int * register

type operand =
  | Oimm of int
  | Oreg of register

type arith = | Add | Sub | Mul | Div | Mod
             | Eq | Neq | Leq | Geq | Lt | Gt

type instruction =
  | Move of register * register
  | Li of register * int
  | La of register * string
  | Lw of register * address
  | Lb of register * address
  | Sw of register * address
  | Sb of register * address
  | Arith of arith * register * register * operand
  (* au sens large pour tout type d'opérateur binaire *)
  | Neg of register * register
  | Jal of string
  | Jr of register
  | Jalr of register
  | J of string
  | Beq of register * register * string
  | Bne of register * register * string
  | Beqz of register * string
  | Bnez of register * string
  | Syscall
  | Label of string

type data =
  | DLabel of string
  | Asciiz of string
  | Word of int
  | AWord of string

type program = {
  text : instruction list;
  data : data list;
}

open Format

let print_register fmt = function
  | A0 -> pp_print_string fmt "$a0"
  | A1 -> pp_print_string fmt "$a1"
  | V0 -> pp_print_string fmt "$v0"
  | T0 -> pp_print_string fmt "$t0"
  | T1 -> pp_print_string fmt "$t1"
  | S0 -> pp_print_string fmt "$s0"
  | RA -> pp_print_string fmt "$ra"
  | SP -> pp_print_string fmt "$sp"
  | FP -> pp_print_string fmt "$fp"

let print_arith fmt = function
  | Add -> pp_print_string fmt "add"
  | Sub -> pp_print_string fmt "sub"
  | Mul -> pp_print_string fmt "mul"
  | Div -> pp_print_string fmt "div"
  | Mod -> pp_print_string fmt "rem"
  | Eq -> pp_print_string fmt "seq"
  | Neq -> pp_print_string fmt "sne"
  | Leq -> pp_print_string fmt "sle"
  | Geq -> pp_print_string fmt "sge"
  | Lt -> pp_print_string fmt "slt"
  | Gt -> pp_print_string fmt "sgt"

let print_address fmt = function
  | Alab s -> pp_print_string fmt s
  | Areg (ofs, r) -> fprintf fmt "%d(%a)" ofs print_register r

let print_operand fmt = function
  | Oimm i -> pp_print_int fmt i
  | Oreg r -> print_register fmt r

let print_instruction fmt = function
  | Move (dst, src) -> 
    fprintf fmt "\tmove %a, %a\n" print_register dst print_register src
  | Li (r, i) ->
    fprintf fmt "\tli   %a, %d\n" print_register r i
  | La (r, s) ->
    fprintf fmt "\tla   %a, %s\n" print_register r s
  | Lw (r, a) ->
    fprintf fmt "\tlw   %a, %a\n" print_register r print_address a
  | Lb (r, a) ->
    fprintf fmt "\tlb   %a, %a\n" print_register r print_address a
  | Sw (r, a) ->
    fprintf fmt "\tsw   %a, %a\n" print_register r print_address a
  | Sb (r, a) ->
    fprintf fmt "\tsb   %a, %a\n" print_register r print_address a
  | Arith (a, dst, src, op) ->
    fprintf fmt "\t%a  %a, %a, %a\n" 
      print_arith a print_register dst print_register src print_operand op
  | Neg (dst, src) -> fprintf fmt "\tneg   %a, %a\n"
    print_register dst print_register src
  | Jal s ->
    fprintf fmt "\tjal  %s\n" s
  | Jr r ->
    fprintf fmt "\tjr   %a\n" print_register r
  | Jalr r ->
    fprintf fmt "\tjalr   %a\n" print_register r
  | J s ->
    fprintf fmt "\tj  %s\n" s
  | Beq (r1, r2, s) ->
    fprintf fmt "\tbeq   %a, %a, %s\n" print_register r1 print_register r2 s
  | Bne (r1, r2, s) ->
    fprintf fmt "\tbne   %a, %a, %s\n" print_register r1 print_register r2 s
  | Beqz (r,s) ->
    fprintf fmt "\tbeqz   %a, %s\n" print_register r s
  | Bnez (r,s) ->
    fprintf fmt "\tbnez   %a, %s\n" print_register r s
  | Syscall ->
    fprintf fmt "\tsyscall\n"
  | Label s ->
    fprintf fmt "%s:\n" s

let print_data fmt = function
  | DLabel s ->
    fprintf fmt "%s:\n" s
  | Asciiz s -> 
    fprintf fmt "\t.asciiz %S\n" s
  | Word n ->
    fprintf fmt "\t.word %d\n" n
  | AWord s ->
    fprintf fmt "\t.word %s\n" s

let print_program fmt p =
  fprintf fmt "\t.text\n";
  List.iter (print_instruction fmt) p.text;
  fprintf fmt "\t.data\n";
  List.iter (print_data fmt) p.data

