# Siren
Siren is a first-order functional probabilistic programming language, implemented with the hybrid inference interface, with support for inference plans. Distributions encoding annotations can be added to random variables to select the representation of the variable's distribution to be used during inference. Siren is also equipped with the inference plan satisfiability analysis, which statically infers if the annotated inference plan is satisfiable. 

## Install
```bash
$ git clone https://github.com/psg-mit/siren
$ cd siren
```

This software requires Python >= 3.10. To install dependencies:
```bash
pip install -e .
```

## Quickstart
Here is an example of a Siren modeling a Kalman filter:
```ocaml
val make_observations = fun (yobs, xs) ->
  let pre_x = List.hd(xs) in
  let sample x <- gaussian(pre_x, 1.) in
  let () = observe(gaussian(x, 1.), yobs) in

  let () = resample() in
  cons(x, xs)
in

let data = List.range(1, 101) in

let x0 = 0 in
let xs = fold(make_observations, data, [x0]) in
List.rev(xs)
```
The program iterates over a range of values from 1 to 100 (inclusive) as the observed data. The variable `x` is annotated with `sample` to indicate `x` should be represented as samples during inference. To indicate `x` should be represented as a symbolic distribution, replace `sample` with `symbolic`.

To run the inference plan satisfiability analysis, and execute the program if the analysis succeeds:
```bash
siren path/to/program.si -m {method} -p {particles} --analyze
```

For example, to run the analysis and execute the program using the semi-symbolic inference algorithm with 100 particles:
```bash
siren examples/kalman.si -m ssi -p 100 --analyze
```

To execute without the analysis:
```bash
siren path/to/program.si -m {method}
```

To run the analysis only:
```bash
siren path/to/program.si -m {method} --analyze-only
```

## Tests
To run the test suite for a quick check everything works:
```bash
$ python -m pytest tests/
```
