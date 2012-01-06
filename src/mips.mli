
type register = 
  | A0 | A1 | V0 | S0 | RA | SP | FP

type address =
  | Alab of string
  | Areg of int * register

type operand =
  | Oimm of int
  | Oreg of register

type arith = Add | Sub | Mul | Div

type instruction =
  | Move of register * register
  | Li of register * int
  | La of register * string
  | Lw of register * address
  | Sw of register * address
  | Arith of arith * register * register * operand
  | Jal of string
  | Jr of register
  | Syscall
  | Label of string

type data = 
  | Asciiz of string * string
  | Word of string * int

type program = {
  text : instruction list;
  data : data list;
}

val print_program : Format.formatter -> program -> unit

