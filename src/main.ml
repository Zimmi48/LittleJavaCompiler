
(* Fichier principal du compilateur *)

open Format
open Lexing
open TypeClass 
open Exceptions

(* Option de compilation, pour s'arr�ter � l'issue du parser *)
let parse_only = ref false
let type_only = ref false

(* Noms des fichiers source et cible *)
let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s 

(* Les options du compilateur que l'on affiche avec l'option --help *)
let options = 
  ["-parse-only", Arg.Set parse_only, 
   "  Pour ne faire uniquement que la phase d'analyse syntaxique";
   "-type-only", Arg.Set type_only,
   "  Pour ne faire que les phases d'analyse syntaxique et de types et de port�es";
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

let affichePType = function
  | Ast.Sast.SVoid -> "Void"
  | Ast.Sast.SBool -> "Bool"
  | Ast.Sast.SInt -> "Int"
  | Ast.Sast.SC s -> s
  | Ast.Sast.STypeNull -> "Type Null"

let () = 
  (* Parsing de la ligne de commande *)
  Arg.parse options (set_file ifile) usage;

  (* On v�rifie que le nom du fichier source a bien �t� indiqu� *)
  if !ifile="" then begin eprintf "Aucun fichier � compiler\n@?"; exit 1 end; 

  (* Ce fichier doit avoir l'extension .java *)
  if not (Filename.check_suffix !ifile ".java") then begin
    eprintf "Le fichier d'entr�e doit avoir l'extension .java\n@?";
    Arg.usage options usage;
    exit 1
  end;

  (* Par d�faut, le fichier cible a le m�me nom que le fichier source, 
     seule l'extension change *)
  if !ofile="" then ofile := Filename.chop_suffix !ifile ".java" ^ ".s";
  
  (* Ouverture du fichier source en lecture *)
  let f = open_in !ifile in
    
  (* Cr�ation d'un tampon d'analyse lexicale *)
  let buf = Lexing.from_channel f in
 
  try
    (* Parsing: la fonction  Parser.fichier transforme le tampon lexical en
       un arbre de syntaxe abstraite si aucune erreur (lexicale ou
       syntaxique) n'est d�tect�e.
       La fonction Lexer.token est utilis�e par Parser.prog pour obtenir 
       le prochain token. *)
    let p = Parser.fichier Lexer.token buf in
    close_in f;
    
    (* On s'arr�te ici si on ne veut faire que le parsing *)
    if !parse_only then exit 0;
    
    (* Typage *)
    let p = ClassAnalysis.checkHerit p in
    let p = CheckInstr.typProg p in 
    
    (* On s'arr�te ici si on ne veut faire que le typage *)
    if !type_only then exit 0;
    
    (* Compilation de l'arbre de syntaxe abstraite p. Le code machine 
       r�sultant de cette transformation doit �tre �crit dans le fichier 
       cible ofile. *)
    Compile.compile_program p !ofile

  with
    | Lexer.Lexing_error c -> 
	(* Erreur lexicale. On r�cup�re sa position absolue et 
	   on la convertit en num�ro de ligne *)
	localisation (Lexing.lexeme_start_p buf);
	eprintf "Erreur dans l'analyse lexicale: %c\n" c;
	exit 1
    | Parser.Error -> 
	(* Erreur syntaxique. On r�cup�re sa position absolue et on la 
	   convertit en num�ro de ligne *)
      localisation (Lexing.lexeme_start_p buf);
      eprintf "Erreur dans l'analyse syntaxique\n";
      exit 1
    | Ast.Past.ClassMain pos ->
      (* Erreur syntaxique sp�ciale *)
      localisationBis pos ;
      eprintf "Erreur dans l'analyse syntaxique.Mauvaise d�finition de class Main\n";
      exit 1
    | Ast.Past.PasUnType pos ->
      (* Erreur syntaxique sp�ciale *)
      localisationBis pos ;
      eprintf "Erreur dans l'analyse syntaxique. Ceci n'est pas un type\n";
      exit 1
    | Ast.Past.CommentaireNonTermine ->
      localisationBis { Ast.Past.file = "" ; line = 0 ; fChar = 0 ; lChar = 0 } ; (* position non d�termin�e *)
      eprintf "Erreur dans l'analyse lexicale : commentaire non termin�\n";
      exit 1
    | AlreadyDefined (pos1, id, pos2) ->
      localisationBis pos1 ;
      eprintf "Erreur : identifiant d�j� d�fini\n";
      begin match pos2 with | None -> () | Some p -> localisationBis p end ;
      exit 1
    | Undefined (pos, id) ->
      localisationBis pos ;
      eprintf "Erreur : %s cet identifiant n'a jamais �t� d�fini\n" id ;
      exit 1
    | Her (pos, id, s) ->
      localisationBis pos ;
      eprintf "Erreur d'h�ritage : %s %s \n" id s ;
      exit 1
    (* Les types sont pris ici dans Sast *)
    | WrongType (pos, ty, sty) ->
      localisationBis pos ;
      eprintf "Erreur : ceci a le type %s" (affichePType ty) ;
      begin
      match sty with
        | None -> eprintf "qui n'est pas le type compatible\n"
        | Some ty ->
          eprintf " mais le type attendu �tait %s \n" (affichePType ty) ;
      end;
      exit 1
    | Duplicated(pos,id) ->
      localisationBis pos ;
      eprintf "Erreur : plusieurs arguments portent le m�me nom\n";
      exit 1
    | Missing(pos,classe,id) ->
      localisationBis pos ;
      eprintf "Erreur : la classe %s n'a pas de champ %s de profil compatible\n" classe id;
      exit 1
    | NotALeftValue pos ->
      localisationBis pos ;
      eprintf "Erreur : Ceci n'est pas une valeur gauche\n" ;
      exit 1
    | BadConst(pos,const, classe) ->
      localisationBis pos ;
      eprintf "Erreur : le constructeur %s n'a pas le m�me nom que la classe %s \n" const classe;
      exit 1
    | Ambiguous(pos,name) ->
      localisationBis pos ;
      eprintf "Erreur : ambiguit� sur le choix de la m�thode/constructeur %s \n" name;
      exit 1
    | EReturn(pos,pos2) ->
      localisationBis pos ;
      eprintf "Erreur : pas de return dans cette branche :\n";
      localisationBis pos2 ;
      exit 1
	
    | Failure s ->
      eprintf "Erreur du compilateur : %s \n" s ;
      exit 2 ;
    | _ -> 
      eprintf "Erreur du compilateur \n";
      exit 2;
      
      
