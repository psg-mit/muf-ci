open Sirencompilerlibs
open Compiler

let norun = ref false

let analyze = ref false

let check = ref false

let output = ref ""

let particles = ref 1

let verbose = ref false

let filename = ref ""

let () =
  try
    Arg.parse
      (Arg.align
         [
           ( "--no-run",
             Arg.Set norun,
             "\t Only run the static analysis (default false)" );
           ( "--analyze",
             Arg.Set analyze,
             "\t Run the static analysis (default false)" );
           ( "--check",
             Arg.Set check,
             "\t Run the static analysis and check inference plan satisfiability (default false)" );
           ( "--output",
             Arg.Set_string output,
             "\t function (takes in marginal distribution) to call to print inference result (optional)");
           ( "--particles",
             Arg.Set_int particles,
             "<int> \t number of particles \
              (default 1)" );
            ( "--verbose", 
              Arg.Set verbose, 
              "\t verbose mode (default false)" );
         ])
      (fun f -> filename := f) "The Siren Compiler. Options are:"
  with Error -> exit 2;;
  compile !verbose !norun !analyze !check !particles !output !filename
