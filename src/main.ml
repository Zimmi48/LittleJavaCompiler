
(* Fichier principal du compilateur *)

open Format
open Lexing

(* Option de compilation, pour s'arrêter à l'issue du parser *)
let parse_only = ref false

(* Noms des fichiers source et cible *)
let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s 

(* Les options du compilateur que l'on affiche avec l'option --help *)
let options = 
  ["-parse-only", Arg.Set parse_only, 
   "  Pour ne faire uniquement que la phase d'analyse syntaxique";
   "-o", Arg.String (set_file ofile), 
   "<file>  Pour indiquer le mom du fichier de sortie"]

let usage = "usage: littleJavaCompiler [option] file.java"

(* localise une erreur en indiquant la ligne et la colonne *)
(* Pour le lexer / parser *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

(* Pour la suite, tant qu'on travaille avec Past *)
let localisationBis { Ast.Past.file = f ; line = l ;
                      fChar = fc ; lChar = lc } =
  eprintf "File \"%s\", line %d, characters %d-%d:\n" f l fc lc

let () = 
  (* Parsing de la ligne de commande *)
  Arg.parse options (set_file ifile) usage;

  (* On vérifie que le nom du fichier source a bien été indiqué *)
  if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end; 

  (* Ce fichier doit avoir l'extension .java *)
  if not (Filename.check_suffix !ifile ".java") then begin
    eprintf "Le fichier d'entrée doit avoir l'extension .java\n@?";
    Arg.usage options usage;
    exit 1
  end;

  (* Par défaut, le fichier cible a le même nom que le fichier source, 
     seule l'extension change *)
  if !ofile="" then ofile := Filename.chop_suffix !ifile ".java" ^ ".s";
  
  (* Ouverture du fichier source en lecture *)
  let f = open_in !ifile in
    
  (* Création d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in

  (* Préparation pour l'affichage des erreurs *)
  let affichePType = function
    | Ast.Past.Void -> "Void"
    | Ast.Past.Bool -> "Bool"
    | Ast.Past.Int -> "Int"
    | Ast.Past.C s -> s
  in  
  try
    (* Parsing: la fonction  Parser.fichier transforme le tampon lexical en un 
       arbre de syntaxe abstraite si aucune erreur (lexicale ou syntaxique) 
       n'est détectée.
       La fonction Lexer.token est utilisée par Parser.prog pour obtenir 
       le prochain token. *)
    let p = Parser.fichier Lexer.token buf in
    close_in f;
    
    (* On s'arrête ici si on ne veut faire que le parsing *)
    if !parse_only then exit 0;
    
    (* Typage *)
    
    (* Compilation de l'arbre de syntaxe abstraite p. Le code machine 
       résultant de cette transformation doit être écrit dans le fichier 
       cible ofile. *)
    Compile.compile_program p !ofile

  with
    | Lexer.Lexing_error c -> 
	(* Erreur lexicale. On récupère sa position absolue et 
	   on la convertit en numéro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse lexicale: %c@." c;
	exit 1
    | Parser.Error -> 
	(* Erreur syntaxique. On récupère sa position absolue et on la 
	   convertit en numéro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse syntaxique@.";
	exit 1
    | Ast.Past.PasUnType pos ->
      (* Erreur syntaxique spéciale *)
      localisationBis pos ;
      eprintf "Erreur dans l'analyse syntaxique. Ceci n'est pas un type@.";
    | TypeClass.Exceptions.AlreadyDefined (pos1, id, pos2) ->
      localisationBis pos1 ;
      localisationBis pos2 ;
      eprintf "Erreur : cette classe est définie plusieurs fois@.";
    | TypeClass.Exceptions.Undefined (pos, id) ->
      localisationBis pos1 ;
      eprintf "Erreur : cet identifiant n'a jamais été défini@."
    | TypeClass.Exceptions.Her (pos, id, s) ->
      localisationBis pos ;
      eprintf "Erreur d'héritage : %s@." s ;
    (* Les types sont pris ici dans Past *)
    | TypeClass.Exceptions.WrongType (pos, ty, sty) ->
      localisationBis pos ;
      eprintf "Erreur : ceci a le type %s" (affichePType ty) ;
      match sty with
        | None -> eprintf "qui n'est pas le type attendu@."
        | Some ty ->
          eprintf "mais le type attendu était %s@." (affichePType ty) ;
(*  | ... *)


