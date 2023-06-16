open Mufcompilerlibs
open Compiler

let only_check = ref false

let analyze = ref false

let output = ref ""

let particles = ref 1

let verbose = ref false

let filename = ref ""

let () =
  try
    Arg.parse
      (Arg.align
         [
           ( "--only-check",
             Arg.Set only_check,
             "\t Only run the static analysis (default false)" );
           ( "--analyze",
             Arg.Set analyze,
             "\t Run the static analysis (default false)" );
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
      (fun f -> filename := f) "The muF Compiler. Options are:"
  with Error -> exit 2;;
  compile !verbose !only_check !analyze !particles !output !filename
