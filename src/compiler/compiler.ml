open Format

exception Error

let output_loc ff (file, (p1, p2)) =
  fprintf ff "%s, characters %d to %d@." file p1 p2

let lexical_error loc err =
  eprintf "%aLexical error: %s.@." output_loc loc err;
  raise Error

let syntax_error loc =
  eprintf "%aSyntax error.@." output_loc loc;
  raise Error

let parse parsing_fun lexing_fun source_name =
  let ic = open_in source_name in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = source_name };
  try parsing_fun lexing_fun lexbuf with
  | Lexer.Lexical_error err ->
      let loc = (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) in
      close_in ic;
      lexical_error (source_name, loc) err
  | Parser.Error ->
      let loc = (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) in
      close_in ic;
      syntax_error (source_name, loc)

let compile_file particles program name output =
  let mlc = open_out (name ^ ".ml") in
  let mlff = Format.formatter_of_out_channel mlc in
  let output_header, output_option = 
    match output with 
    | "" -> "", "None"
    | o -> 
      ("let () = Format.printf \"==== OUTPUT ====\\n\" in", 
        Format.sprintf "(Some %s)" o)
  in
  try
    let ml_list = List.map (Mufextern.compile_program output) [program] in
    Format.fprintf mlff "%s@.%s@.%s@.%s@.@.%a@." "open Probzelus"
      "open Distribution" "open Muf" "open Infer_muf"
      (pp_print_list ~pp_sep:pp_force_newline Pprintast.structure)
      ml_list;
    Format.fprintf mlff
      "@[<v 2>@[let post_main _ = @]@;\
       @[%s@]@;\
       @[let _ = infer %d main %s in@]@;\
       @[let () = Format.printf \"\\n==== RUNTIME APPROXIMATION STATUS ====\\n\" in@]@;\
       @[let () = Format.printf \"%%s\\n\" (pp_approx_status false) in ()@]@]@.\
       @[<v 2>@[let _ =@]@;\
       @[post_main ()@]@]@." output_header particles output_option
  with Zmisc.Error ->
    close_out mlc;
    raise Error

let compile_simulator name node =
  (* let dir = Sys.getcwd () in *)
  (* let mainc = open_out (dir ^ "/" ^ node ^ ".ml") in *)
  let mainc = open_out (node ^ ".ml") in
  let mainff = Format.formatter_of_out_channel mainc in
  Format.fprintf mainff
    "@[<v> open Muf @;\
     @;\
     @[(* simulation (discrete) function *)@]@;\
     @[<v 2>@[let main =@]@;\
     @[let mem = ref (Muflib.init %s.%s) in@]@;\
     @[(fun x -> let _, s = Muflib.step !mem x in mem := s)@]@]@];;@.@[<v>(* \
     (discrete) simulation loop *)@;\
     main ();@;\
     exit(0);;@]@."
    (String.capitalize_ascii name)
    node;
  close_out mainc

let print_cmd name =
  let cmd =
    "ocamlfind ocamlc -linkpkg -package muf " ^ name ^ ".ml "
    ^ "-o " ^ name ^ ".exe"
  in
  Format.printf "%s@." cmd;
  match Sys.command cmd with 0 -> () | _ -> raise Error

let verify_approx_status output program check = 
  Format.printf "==== STATIC APPROXIMATION STATUS ====@.";

  let decls, e = program in

  (* Remove output function from analysis *)
  let decls = List.filter (fun d ->
    let open Mufextern in
    match d with
    | Ddecl _ | Dopen _ -> false
    | Dfun (s, _, _) -> not (s = output)
  ) decls in

  let ann_inf = Analysis.annotated_inference_strategy (decls, e) in

  let inferred_inf = Analysis.infer (decls, e) in
  let inferred_inf_s = Analysis.InferenceStrategy.to_string inferred_inf in

  Format.printf "%s\n" inferred_inf_s;

  try 
    Analysis.InferenceStrategy.verify ann_inf inferred_inf;
    Format.printf "Inference Strategy - Satisfiable@."
  with Analysis.Approximation_Status_Error (rv, ann, inf) ->
    Format.printf "Inference Strategy - Unsatisfiable@.";
    Format.printf "> `%s` is annotated with %s but expected to be %s@." 
      (Analysis.string_of_ident rv) 
      (Analysis.ApproximationStatus.to_string ann) 
      (Analysis.ApproximationStatus.to_string inf);
    if check then
      raise (Analysis.Inference_Strategy_Error)

let compile verbose norun analyze check particles output file =
  (* let name = Filename.basename file in *)
  let name = Filename.chop_extension file in
  let program = parse Parser.program (Lexer.token ()) file in

  if verbose then (
    Format.printf "particles: %d@." particles;
    Format.printf "%s\n" (Mufextern.show_program
      (* (fun ff () -> Format.fprintf ff "()") *)
      program));

  (* Passes that need to be done before analysis *)
  let program = Mufextern.pre_passes output program in

  if norun || analyze || check then (
    Format.printf "-- Approximation Status Analysis %s.ml@." name;
    verify_approx_status output program check
  );
  
  if not norun then (

    Format.printf "-- Generating %s.ml@." name;

    compile_file particles program name output;
    print_cmd name
  )
