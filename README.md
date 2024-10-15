# Siren
Siren is a first-order functional probabilistic programming language, implemented with the hybrid inference interface, with support for inference plans. Distributions encoding annotations can be added to random variables to select the representation of the variable's distribution to be used during inference. Siren is also equipped with the inference plan satisfiability analysis, which statically infers if the annotated inference plan is satisfiable. 

The Siren interpreter, including the inference plan satisfiability analysis, is implemented with semi-symbolic inference, delayed sampling, and SMC with belief propagation, with support for particle filtering (Sequential Monte Carlo) and Metropolis-Hastings as the approximate inference backend. It can be extended with other hybrid inference algorithms that can implement the hybrid inference interface. 

## Contents
- [Requirements](#requirements)
- [Install](#install)
- [Smoke Test (Kick-the-tires)](#smoke-test-kick-the-tires)
- [Artifact Evaluation](#artifact-evaluation)
  - [List of Claims](#list-of-claims)
- [Reusability Guide](#reusability-guide)

## Requirements
- Install [Docker Desktop](https://www.docker.com/get-started/). The artifact was tested with Docker Desktop v4.30.0. 
  - Note: The artifact is also compatible with [Podman](https://podman.io/), just substitute `podman` in every command that uses `docker`.
- The software requires Python >= 3.10. This should be ensured by the Dockerfile.
- The artifact instructions was tested on M1 MacBook.

## Install
Installation can be done either by running a Docker container (Recommended) or by installing from source. The following subsections describe the installation steps for each option.

### Docker Container with Pre-built Docker Image
1. Download `siren-amd64.tar.gz` or `siren-arm.tar.gz` from Zenodo depending on your platform.
2. Load the image
```bash
docker load -i siren-arm.tar.gz # or siren-amd64.tar.gz
```
3. Run the Docker container
```bash
docker run -it siren:arm # or siren:amd64
```

You should now be `root` in the Docker container, with current working directory at `~/siren`.

Commands should be run inside the Docker container, unless stated otherwise.

### Docker Container by Building Docker Image
1. Obtain source code, by doing one of the following:
    1. Download from Zenodo, or
    2. Clone the `popl25-artifact` branch from GitHub
    ```bash
    git clone --single-branch --branch popl25-artifact https://github.com/psg-mit/siren
    ```

2. Enter the repository
```bash
cd siren-popl25-artifact-v1.0.0  # or `cd siren` if cloned
```

3. Build Docker image. This should take 5-10 minutes.
```bash
docker build -t siren .
```

4. Run the Docker container
```bash
docker run -it siren
```

You should now be `root` in the Docker container, with current working directory at `~/siren`.

Commands should be run inside the Docker container, unless stated otherwise.

### From Source (Not Recommended)
You can install Siren from source instead of using the Docker image. This requires Python >= 3.10. 

1. Obtain source code, by doing one of the following:
    1. Download from Zenodo, or
    2. Clone the `popl25-artifact` branch from GitHub
    ```bash
    git clone --single-branch --branch popl25-artifact https://github.com/psg-mit/siren
    ```

2. Enter the repository
```bash
cd siren-popl25-artifact-v1.0.0  # or `cd siren` if cloned
```

3. Setup virtual environment:
```bash
pip -m venv venv
source venv/bin/activate
```

4. Install dependencies
```bash
pip install .
```

If installing from source, you can ignore instructions for copying files from the Docker container to the host machine. All generated files will appear in the described directories directly.

## Smoke Test (Kick-the-Tires)
Run the test suite for a quick check everything works:
```bash
python -m pytest tests/
```

Run the smoke test for the benchmarking script:
```bash
cd benchmarks/
python harness.py --output output_kicktires kicktires
```
This should take about 15 minutes.

This will output the analysis results table in Table 1, as well as the runtime of the analysis:
```bash
python visualize.py --output output_kicktires --task analysis_table --handlers smc
```

The following commands will generate Figures from the main paper:
```bash
python visualize.py --output output_kicktires --example 
python visualize.py --output output_kicktires --example --task timestep
python visualize.py --output output_kicktires --task plot --benchmark outlier noise --method ssi
```

In your host machine (outside of the Docker container), in the `siren` repo, run
```bash
chmod +x cp_files.sh
./cp_files.sh docker arm # or amd64
```
to copy the generated plots to the host machine to inspect.
The following generated files will be in the host machine:
- `benchmarks/examplegood/output_kicktires/smc_ssi_example_143.png`
- `benchmarks/examplegood/output_kicktires/smc_ssi_example_time_43.png`
- `benchmarks/examplebad/output_kicktires/smc_ssi_example_time_43.png`
- `benchmarks/outlier/output_kicktires/smc_ssi_particles.png`
- `benchmarks/noise/output_kicktires/smc_ssi_particles.png`

The smoke test is configured to only run `n=1` iterations using fewer particles, so the figures will look very distorted but that is expected. The smoke test should just check that the visualization script will generate plots.

To test the speedup and accuracy comparison aggregation script:
```bash
python visualize.py --task compare_time --output output_kicktires --handlers smc --methods ssi
python visualize.py --task compare_accuracy --output output_kicktires --handlers smc --methods ssi
```

## Artifact Evaluation
The experiments from the paper were conducted on a 60-vCPU Intel Xeon Cascade Lake (up to 3.9 GHz) node with 240 GB RAM. The full set of experiments in the paper takes about 30 days of computation. The experiments can run on a general-purpose computer as well, requiring only enough computation time. 

Due to the long amount of time needed to compute the full set of benchmarks from the paper, which uses `n=100` iterations per particle setting for each benchmark and method, we have configured the scripts to use fewer iterations for artifact evaluation. As such, we only expect to replicate the trends of the main paper figures, and the actual data points are expected to appear more erratic.

### List of Claims
We list here each of the figures/tables that the artifact reproduces. Due to time constraints and differences in machines, we only expect to reproduce the trend for the evaluation. Step 1 runs the programs for all the claims. 
- Figure 4 from Section 2 corresponds to Step 1 and Step 2.
- Figure 5 from Section 2 corresponds to Step 1 and Step 3.
- Figure 16 from Section 5 corresponds to Step 1 and Step 4.
- Table 1 from Section 5 corresponds to Step 1 and Step 5.
- Analysis runtime table corresponds to Step 1 and Step 5.
- Speedup and accuracy comparison in Section 5.3.1 corresponds to Step 1 and Step 6.

### Step-by-step Instructions
1. Use the harness script to run example for Figure 4 and Figure 5 for `n=10` and the programs for Figure 16 for `n=5`. This will take ~4-5 hours. 
```bash
cd benchmarks/
python harness.py artifact-eval
```

2. Visualize the results for Section 2 Figure 4:
```bash
python visualize.py --example
```
In your host machine (outside of the Docker container), in the `siren` repo, run
```bash
./cp_files.sh docker arm # or amd64
```
to copy the files locally.

The plot will be located at `benchmarks/examplegood/output/ssi_example_143.png`. The data points are expected to be more erratic than in the paper due to running for less iterations. 

3. Visualize the results for Section 2 Figure 5:
```bash
python visualize.py --example --task timestep
```
In your host machine (outside of the Docker container), in the `siren` repo, run
```bash
./cp_files.sh docker arm # or amd64
```
to copy the files locally.

The plots will be located at `benchmarks/examplegood/output_kicktires/smc_ssi_example_time_43.png` and `benchmarks/examplebad/output_kicktires/smc_ssi_example_time_43.png`. The data points are expected to be more erratic than in the paper due to running for less iterations. 

4. Visualize the results for Section 5 Figure 16:
```bash
python visualize.py --task plot -b outlier noise -m ssi --handlers smc
```
In your host machine (outside of the Docker container), in the `siren` repo, run
```bash
./cp_files.sh docker arm # or amd64
```
to copy the files locally.

The plot will be located at `benchmarks/outlier/output/smc_ssi_particles.png` and `benchmarks/noise/output/smc_ssi_particles.png`.

5. To produce Section 5 Table 1 and the runtimes of the analysis:
```bash
python visualize.py --task analysis_table --handlers smc
```
The runtimes may have variations due to system differences.

6. To compute the speedup and accuracy comparison for Noise and Outlier with SSI described in Section 5.3.1:
```bash
python visualize.py --task compare_time --handlers smc --methods ssi
python visualize.py --task compare_accuracy --handlers smc --methods ssi
```
The ratios will vary from the paper due to the system differences and running for fewer iterations, but the trends should be reproduced DS and BP columns will be left blank.

The overall speedup and accuracy comparison across all benchmarks and methods would require running the other benchmarks and methods, which would take days of compute. Thus, we only expect to replicate the trend that, aggregated across Noise and Outlier, inference plans enable speedup to reach the target accuracy and improve accuracy with less or equal execution time, as indicated by a >1 geometric mean speedup and >1 geometric mean accuracy ratio in the "Across All" section of the outputs of the commands.

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


## Reusability Guide

### Quickstart (Example)
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
siren path/to/program.si -m {method} -p {particles} -l {handler} --analyze
```

The currently available hybrid inference methods are `ssi` (semi-symbolic inference), `ds` (delayed sampling), and `bp` (hybrid belief propagation). The currently available approximate inference handlers are `smc` (Sequential Monte Carlo/Particle Filtering) and `mh` (Metropolis-Hastings).

For example, to run the analysis and execute the program using the semi-symbolic inference algorithm with SMC with 100 particles:
```bash
siren examples/kalman.si -m ssi -p 100 -l smc --analyze
```

The analysis will throw an `AnalysisViolatedAnnotationError` and abort without running the program if the annotated inference plan is not satisfiable.

To execute without the analysis:
```bash
siren path/to/program.si -m {method} -p {particles} -l {handler}
```

To run the analysis only:
```bash
siren path/to/program.si -m {method} -l {handler} --analyze-only
```

### Syntax
The Siren syntax is as described in the grammar below.
```
program: func* expression
                   
func: "val" NAME "=" "fun" patternlist "->" expression "in"
                   
expression: 
  | "true"
  | "false"
  | "(" ")"
  | NUMBER
  | INTEGER
  | STRING
  | NAME
  | "(" expression ("," expression)* ")
  | "let" patternlist "=" expression "in" expression
  | ops
  | identifier args
  | "if" expression "then" expression "else" expression
  | "fold" "(" identifier "," expression "," expression ")"
  | "let" rvpattern "<-" expression "in" expression
  | "observe" "(" expression "," expression ")
  | "resample" "(" ")"
  | list
                   
args:
  | "(" ")"
  | "(" expression ("," expression)* ")"
                   
list: 
  | "[" "]"
  | "nil"
  | "[" expression ("," expression)* "]"

rvpattern:
  | "sample" identifier
  | "symbolic" identifier
  | identifier
                   
ops:
  | expression "+" expression
  | expression "-" expression
  | expression "*" expression
  | expression "/" expression
  | expression "::" expression
  | expression "=" expression
  | expression "<" expression

patternlist:
  | "(" ")"
  | "(" patternlist ")"
  | pattern
  | pattern ("," pattern)+
                   
pattern:
  | NAME
  | "_"
  | "(" ")"
  | "(" pattern ")"
  | "(" pattern ("," pattern)+ ")"
                   
identifier: 
  | NAME
  | NAME "." NAME
```

Comments are wrapped by `(* *)`.

### Source Code Descriptions and Extension
We document in [DESCRIPTION.md](DESCRIPTION.md) a description of the source files in this repository and the steps to extend Siren to demonstrate how the hybrid inference interface enables modularity as described in the paper. We provide this documentation as a reference for developers and future researchers who wish to implement their own inference algorithms. To that end, we do not expect artifact reviewers or casual users to do so.
