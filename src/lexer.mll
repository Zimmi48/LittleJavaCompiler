
{
  open Lexing
  open Parser
   
  exception Lexing_error of char
    
  let kwd_tbl = [
    "boolean", BOOLEAN;
    "class", CLASS;
    "else", ELSE;
    "extends", EXTENDS;
    "false", FALSE;
    "for", FOR;
    "if", IF;
    "instanceof", INSTANCEOF;
    "int", INT;
    "new", NEW;
    "null", NULL;
    "public", PUBLIC;
    "return", RETURN;
    "static", STATIC;
    "this", THIS;
    "true", TRUE;
    "void", VOID;
    "Main", CLASS_MAIN; (* faux keywords *)
    "main", MAIN;
    "System", SYSTEM;
    "out", OUT;
    "print", PRINT]
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (letter | '_' ) (letter | digit | '_' )*
let integer = '0' | ['1'-'9'] digit*
let space = [' ' '\t']

rule token = parse
  | '\n'    { newline lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id { id_or_kwd id }
  | "++"    { PLUSPLUS }
  | "--"    { MINUSMINUS }
  | "=="    { ISEQ }
  | "!="    { NEQ }
  | "<="    { LEQ }
  | ">="    { GEQ }
  | "||"    { OR }
  | "&&"    { AND }
  | '<'     { LT }
  | '>'     { GT }
  | '!'     { NOT }
  | '.'     { DOT }
  | ','     { COMMA }
  | ';'     { SEMICOLON }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '%'     { MOD }
  | '='     { EQ }
  | '('     { LP }
  | ')'     { RP }
  | '{'     { LB }
  | '}'     { RB }
  | '['     { LBRACKET }
  | ']'     { RBRACKET }
  | integer as s { INT_CST (int_of_string s) }
  | '"'     { STRING_CST (string lexbuf) }
  | "//"    { commentLine lexbuf ; token lexbuf }
  | "/*"    { comment lexbuf ; token lexbuf }
  | eof     { EOF }
  | _ as c  { raise (Lexing_error c) }

and string = parse
  | '"'    { "" }
  | "\\\"" { "\"" ^ (string lexbuf) }
  | "\\n"  { "\n" ^ (string lexbuf) }
  | _ as s { String.make 1 s ^ (string lexbuf) }

and commentLine = parse
  | '\n' { newline lexbuf }
  | _    { comment lexbuf }

and comment = parse
  | '\n' { newline lexbuf ; comment lexbuf }
  | "*/" { () }
  | _    { comment lexbuf }
  | eof  { raise Ast.Past.CommentaireNonTermine }
