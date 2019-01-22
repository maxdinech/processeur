open Core
open Lexer
open Lexing

let print_error outx (ps, pe) = 
   fprintf outx "File \"%s\", line %d, characters %d-%d" ps.pos_fname
    ps.pos_lnum (ps.pos_cnum - ps.pos_bol + 1) (pe.pos_cnum - pe.pos_bol + 1)

let print_position outx lexbuf =
  print_error outx (lexbuf.lex_start_p, lexbuf.lex_curr_p)

let parse_with_error lexbuf =
  try Parser.fichier Lexer.token lexbuf with
  | Lexer.LexingError msg ->
      fprintf stderr "%a:\n%s\n" print_position lexbuf msg;
      exit 1
  | Parser.Error ->
      fprintf stderr "%a:\nParsing error\n" print_position lexbuf;
      exit 1

let type_with_error parsed = 
  try Typer.check_prog parsed with
  | Typer.TypingError msg ->
      (* fprintf stderr "%a:\nTyping Error - " ""; *)
      fprintf stderr "%s" (msg^"\n");
      exit 1

let main parse_only type_only filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let parsed = parse_with_error lexbuf in
  In_channel.close inx;
  if not parse_only then begin 
    let prog = Compiler.traite_jumps parsed in
    let jumps = type_with_error prog in 
    let oc = Out_channel.create ((
      try let l = String.length filename in assert
        (filename.[l-2]='.' && filename.[l-1]='s');
        String.sub filename 0 (l-2)
      with _ -> fprintf stderr "Fichier .s attendu\n"; exit 1)^".bin") in
    fprintf oc "%s\n" (Compiler.production jumps prog);
    Out_channel.close oc;
  end

let compiler =
  Command.basic_spec
    ~summary:""
    ~readme:(fun () -> "")
    Command.Spec.(
      empty
        +> flag "--parse-only" no_arg ~doc:"parse only mode"
        +> flag "--type-only" no_arg ~doc:"type only mode"
        +> anon ("filename" %: file))
    main

let () = Command.run compiler
