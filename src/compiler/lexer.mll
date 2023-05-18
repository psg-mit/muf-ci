{
  open Parser
  exception Lexical_error of string

  let string_buff = Buffer.create 256

  let keyword_table =
    let tbl = Hashtbl.create 37 in
    begin
      List.iter (fun (key, data) -> Hashtbl.add tbl key data)
	[     
          ("val", VAL);
          ("let", LET);
	      ("in", IN);
          (* ("stream", STREAM); *)
          ("fun", FUN);
          ("if", IF);
          ("then", THEN);
          ("else", ELSE);
          (* ("factor", FACTOR); *)
          (* ("sample", SAMPLE); *)
          ("observe", OBSERVE);
          ("value", VALUE);
          (* ("infer", INFER); *)
          ("true", BOOL true);
          ("false", BOOL false);
          (* ("bool", BOOLT); *)
          (* ("int", INTT); *)
          (* ("float", FLOATT); *)
          (* ("dist", DIST); *)
          (* ("unit", UNIT); *)
          (* ("arr", ARRAY); *)
          (* ("list", LIST); *)
          ("open", OPEN);
          ("exact", EXACT);
          ("approx", APPROX);
          (* ("unfold", UNFOLD); *)
          (* ("reset", RESET); *)
          (* ("exact", EXACT);
          ("approx", APPROX); *)
	]; tbl
    end

}

let newline = ('\010' | '\013' | "\013\010")
let letter = ['A'-'Z' 'a'-'z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9' ]

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = '-'? digit* (frac exp? | exp)

rule token sbuff = parse
| eof { EOF }
| "=" { EQUAL }
| "<-" { LARROW }
| "->" { RARROW }
| "(" { LPAREN }
| ")" { RPAREN }
| "," { COMMA }
| ";" { SEMI }
| ":" { COLON }
| "*" { STAR }
| "_" { UNDERSCORE }
| "." { DOT }
| "[" { LSQUARE }
| "]" { RSQUARE }
| [' ' '\t']
    { token sbuff lexbuf }
| newline
    { Lexing.new_line lexbuf; token sbuff lexbuf }
| float as f
    { FLOAT f }
| ('-'? ['0'-'9']+) as i
    { INT (int_of_string i) }
| '"'
    { Buffer.clear string_buff; string lexbuf; STRING (Buffer.contents string_buff)}
| letter identchar*
    { let s = Lexing.lexeme lexbuf in
      try Hashtbl.find keyword_table s
      with Not_found -> IDENT s }
| "(*"
    { comment 1 lexbuf; token sbuff lexbuf }
| _
    { raise (Lexical_error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and string = parse
  | "\"\"" { Buffer.add_char string_buff '"'; string lexbuf } 
  | "\013\n" { Buffer.add_char string_buff '\n'; string lexbuf }
  | "\013" { Buffer.add_char string_buff '\n'; string lexbuf }
  | '"'    { () } 
  | eof    { raise (Lexical_error "String not terminated.\n") }
  | _ as c     { Buffer.add_char string_buff c; string lexbuf }

and comment cpt = parse
  | "(*"
      { comment (cpt + 1) lexbuf }
  | "*)"
      { if cpt > 1 then comment (cpt - 1) lexbuf }
  | eof
      { raise (Lexical_error "Unterminated comment\n") }
  | newline
      { Lexing.new_line lexbuf; comment cpt lexbuf }
  | _
      { comment cpt lexbuf }

