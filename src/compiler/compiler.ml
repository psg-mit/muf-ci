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

let analyze_file n_iters p =
  let module SMap = Map.Make (String) in
  let open Muf in
  List.fold_left
    (fun (fctx, mctx, (mcons, unsep)) d ->
      match d.decl with
      | Dfun (name, p, e) ->
          let rs = Analysis.process_fn p e fctx mctx in
          (Analysis.VarMap.add { modul = None; name } rs fctx,
           mctx,
           (mcons, unsep))
      | Dnode (name, _, node) -> (
          let p, e = node.n_step in
          match p.patt with
          | Ptuple [ p_state; p_in ] ->
              let rs, (mcons', unsep') =
                Analysis.process_node n_iters node.n_init p_state p_in e fctx
                  mctx
              in
              (fctx,
               Analysis.VarMap.add { modul = None; name } rs mctx,
               (mcons && mcons', unsep && unsep'))
          | _ -> failwith "Stream definition lacking step (state, input).")
      | _ -> (fctx, mctx, (mcons, unsep)))
    (Analysis.VarMap.empty, Analysis.VarMap.empty, (true, true))
    p
  |> fun (_, _, (mcons, unsep)) ->
  if mcons then Format.printf "     o m-consumed analysis success@."
  else Format.printf "     x m-consumed analysis failure@.";
  if unsep then Format.printf "     o Unseparated paths analysis success@."
  else Format.printf "     x Unseparated paths analysis failure@."

let compile_file muf_list name =
  let mlc = open_out (name ^ ".ml") in
  let mlff = Format.formatter_of_out_channel mlc in
  try
    let ml_list = List.map Muf_gencode.compile_program [ muf_list ] in
    Format.fprintf mlff "%s@.%s@.%s@.%s@.%s@.@.%a@." "open Probzelus"
      "open Distribution" "open Muf" "open Infer_semi_symbolic" "open Infer_muf"
      (pp_print_list ~pp_sep:pp_force_newline Pprintast.structure)
      ml_list
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

let print_cmd name node =
  let cmd =
    "ocamlfind ocamlc -linkpkg -package muf " ^ name ^ ".ml " ^ node ^ ".ml "
    ^ "-o " ^ name ^ "_" ^ node ^ ".exe"
  in
  Format.printf "%s@." cmd;
  match Sys.command cmd with 0 -> () | _ -> raise Error

let approx_status muf_list = 
  (* let typs = Siren.approximation_status muf_list in
  Siren.pp_map std_formatter typs; *)
  (* let approx_status, muf_list = Siren.approximation_status muf_list in *)
  (* Print out approx_status *)
  (* let print_approx_status (name, approx_status) =
    Format.printf "  %s: %s@." name (Siren.string_of_approx_status approx_status)
  in *)
  (* List.iter print_approx_status approx_status; *)

  (* approx_status, muf_list *)
  muf_list

let compile verbose only_check particles node file =
  (* let name = Filename.basename file in *)
  let name = Filename.chop_extension file in
  let muf_list = parse Parser.program (Lexer.token ()) file in

  
  if verbose then (
    Format.printf "particles: %d@." particles;
    Format.printf "%s\n" (Mufextern.show_program
      (fun ff () -> Format.fprintf ff "()")
      muf_list));

  Format.printf "-- Approximation Status Analysis %s.ml@." name;
  let _ = approx_status muf_list in
  
  if not only_check then (
    Format.printf "-- Generating %s.ml@." name;

    let muf_intern : unit Muf.program = Mufextern.convert_to_intern particles verbose muf_list in
  
    if verbose then (
      Format.printf "%s\n" (Muf.show_program
        (fun ff () -> Format.fprintf ff "()")
        muf_intern));

    compile_file muf_intern name;
    Format.printf "-- Generating %s.ml@." node;
    compile_simulator name node;
    print_cmd name node)
