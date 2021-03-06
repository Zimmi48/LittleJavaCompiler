(* à partir d'un fichier de J-C. Filliâtre *)

type register = 
  | Zero | A0 | A1 | A2 | A3 | V0 | T0 | T1 | T2 | T3 | T4 | S0 | RA | SP | FP

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
  | Div2 of register * register
  | Mflo of register
  | Mfhi of register
  | Neg of register * register
  | Abs of register * register
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

val print_program : Format.formatter -> program -> unit

