# Siren
Siren is a first-order functional probabilistic programming language, implemented with the hybrid inference interface, with support for inference plans. Distributions encoding annotations can be added to random variables to select the representation of the variable's distribution to be used during inference. Siren is also equipped with the inference plan satisfiability analysis, which statically infers if the annotated inference plan is satisfiable. 

The Siren interpreter, including the inference plan satisfiability analysis, is implemented with semi-symbolic inference, delayed sampling, and SMC with belief propagation, with support for particle filtering (Sequential Monte Carlo) and Metropolis-Hastings as the approximate inference backend. It can be extended with other hybrid inference algorithms that can implement the hybrid inference interface. 

## Install
Clone the repository:
```
git clone https://github.com/psg-mit/siren
cd siren
```

### Docker (Recommended)
We also provide a Docker file to build an image. The artifact was tested with Docker Desktop v4.30.0. 
```bash
docker build -t siren .
docker run -it siren
```

### From Source
This software was tested on M1 MacBook and requires Python >= 3.10. To install dependencies:
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
The analysis will throw an `AnalysisViolatedAnnotationError` and abort without running the program if the annotated inference plan is not satisfiable.

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
python -m pytest tests/
```

## Smoke Test
To do a smoke test that the benchmarks can run correctly:
```bash
cd benchmarks/
python harness.py --output output_kicktires kicktires
```
This should take 1 hour.

The smoke test is configured to only run n=1 iterations for less particles, so the figures will look very distorted but that is expected. To check the visualization script will generate plots:
```bash
python visualize.py --output output_kicktires --example 
python visualize.py --output output_kicktires --example --task timestep
```
These files will be generated 
- `benchmarks/examplegood/output_kicktires/smc_ssi_example_143.png`
- `benchmarks/examplegood/output_kicktires/smc_ssi_example_time_43.png`
- `benchmarks/examplebad/output_kicktires/smc_ssi_example_time_43.png`

```bash
python visualize.py --output output_kicktires --task analysis_table
```
This will output the analysis results table in Table 1 and Table 19, as well as the runtime of the analysis.

```bash
python visualize.py --output output_kicktires --task plot
```
The plots will be located at `benchmarks/{BENCHMARK}/output_kicktires/{HANDLER}_{METHOD}_particles.png` for each BENCHMARK and METHOD and HANDLER.

To check the runtime and accuracy statistics aggregation works correctly:
```bash
 # Compares runtime of inference plans to reach the same accuracy
python visualize.py --output output_kicktires --task compare_time
# Compare accuracy of inference plans given fixed runtime
python visualize.py --output output_kicktires --task compare_accuracy 
```

## Benchmarking
The experiments from the paper were conducted on a 60-vCPU Intel Xeon Cascade Lake (up to 3.9 GHz) node with 240 GB RAM. The full set of experiments in the paper takes about 30 days of computation. The experiments can run on a general-purpose computer as well, requiring only enough computation time. 

### Main Paper Results
Due to the long amount of time needed to compute the full set of benchmarks from the paper, which uses `n=100` iterations per particle setting for each benchmark and method, to only replicate the trends of the main paper figures:
```bash
cd benchmarks/
python harness.py artifact-eval
```
This executes the example for Figure 4 and Figure 5 for `n=10` and the programs for Figure 16 for `n=5`. This will take ~4-5 hours.

Then, to visualize the results for Section 2 Figure 4:
```bash
python visualize.py --example
```
The plot will be located at `benchmarks/examplegood/output/ssi_example_143.png`.

For Figure 5:
```bash
python visualize.py --example --task timestep
```
These commands generate the figures for Section 2. The plots will be located at `benchmarks/examplegood/output_kicktires/smc_ssi_example_time_43.png` and `benchmarks/examplebad/output_kicktires/smc_ssi_example_time_43.png`.

Then, to visualize the results for Section 5 Figure 16:
```bash
python visualize.py --task plot -b outlier noise -m ssi
```
The plot will be located at `benchmarks/outlier/output/smc_ssi_particles.png` and `benchmarks/noise/output/smc_ssi_particles.png`.

Then, to produce Section 5 Table 1:
```bash
python visualize.py --task analysis_table --handlers smc
```

### Full Replication
To perform the full replication of the figures in the paper:
```bash
cd benchmarks/
python harness.py full-replication
python visualize.py --example
python visualize.py --task analysis_table
```

To compute the overall speedup and accuracy comparison from Section 5, and the tables in Appendix F:
```bash
python visualize.py --task compare_time
python visualize.py --task compare_accuracy
```

To visualize the plots of Appendix F and Appendix G:
```bash
python visualize.py --task plot
```
The plot will be located at `benchmarks/{BENCHMARK}/output/{HANDLER}_{METHOD}_particles.png` for each BENCHMARK and METHOD and HANDLER.

This will take ~30 days.

## Syntax
The Siren syntax can be found in `siren/parser.py`.

## Guide
To view a description of the source files in this repository and instructions on how to extend Siren, please see [DESCRIPTION.md](DESCRIPTION.md).
