import argparse
import os
import json
import subprocess
import time
from math import log10
import glob
import itertools
import numpy as np
import csv
import sys
import pandas as pd

BENCHMARK_DIR = 'benchmarks'

DEFAULT_BENCHMARKS = [
  'gaussianmixture',
  'envnoise',
  'noise',
  'smsbehavior',
  'outlier',
  'mixasymprior',
  # "outliernolist",
]

DEFAULT_METHODS = [
  'ssi',
  # 'ds',
  # 'ft',
  # 'dis',
]

N_INTERVALS = 30

CWD = '..'

def calc_error(true_x, x):
  return (true_x - x) ** 2
  
def close_to_target(target_error, program_output):
  for var, error in program_output.items():
    if log10(error) - log10(target_error[var]) > 0.5:
      return False

  return True

def is_satisfiable(annotation, distr_enc):
  if annotation == 'dynamic':
    return True
  elif annotation == 'symbolic':
    return distr_enc == 'symbolic'
  elif annotation == 'sample':
    return distr_enc == 'sample'
  else:
    raise Exception(f'Invalid annotation: {annotation}')

def get_plan_id(file):
  if os.path.basename(file) == 'baseline.si':
    return '-1'
  return str(int(os.path.basename(file)[4:-3]))

# Runs benchmark executable and computes the absolute error of each variable
def run_siren(file, p, method, true_vars):
  # run siren file
  cmd = f'{sys.executable} siren.py {file} -m {method} -p {p}'
  
  print('>', cmd)

  out = subprocess.check_output(cmd, cwd=CWD, shell=True).decode("utf-8")

  program_output = {}

  # parse output
  lines = out.strip().split('\n')

  # get execution time
  eval_time = None
  for i, line in enumerate(lines):
    line = line.strip()
    if line == '===== Evaluation Time =====':
      eval_time = float(lines[i + 1])
      break
  if eval_time is None:
    raise RuntimeError('Evaluation time not found')

  # get outputs between ===== Result ===== and ===== Runtime Inference Plan =====
  output = ''
  for i, line in enumerate(lines):
    line = line.strip()
    if line == '===== Result =====':
      output = lines[i + 1]
      break

  # eg format (0.1782178217821782, (-11.323677393073108, 8.861234052059762))
  # or (0.1782178217821782, (-11.323677393073108, [0; 1; 2]))
  split_output = output.strip().split(', ')

  # parse line into dict

  for true_var, out in zip(true_vars, split_output):
    var, true_vals = true_var
    out = out.strip(' )(')
    
    # parse out into list of values
    if '[' in out:
      out = out.strip('[]')
      out = out.split('; ')
      out = [float(x) for x in out]
    else:
      out = [float(out)]

    # compute error
    error = 0
    for true_val, val in zip(true_vals, out):
      error += calc_error(true_val, val) 

    program_output[var] = error

  return eval_time, program_output

# Run experiments for each given number of particles
def run_particles(files, n, particles, methods, true_vars, results_file):
  for method in methods:
    print(f'Running {method}...')

    for file in files:
      plan_id = get_plan_id(file)

      for p in particles:
        print(f'Running with {p} particles')

        for i in range(n):
          print(f'{plan_id} {method} - {p} particles - Run {i}')

          t, program_output = run_siren(file, p, method, true_vars)

          # write results to csv
          with open(results_file, 'a') as f:
            writer = csv.writer(f)
            writer.writerow([
              plan_id, 
              method, 
              p, 
              t,
              *(program_output.values())
            ])

# Run experiments to reach a given accuracy
def run_accuracy(files, min_p, max_p, n, target_errors, methods, true_vars, results_file):
  for method in methods:
    print(f'Running {method}...')

    for file in files:
      plan_id = get_plan_id(file)

      p = min_p
      while True:
        print(f'Running with {p} particles')

        # 90% of n runs need to be close to target for each variable
        
        close = 0
        for i in range(n):
          print(f'{plan_id} {method} - {p} particles - Run {i}')

          t, program_output = run_siren(file, p, method, true_vars)

          # write results to csv
          with open(results_file, 'a') as f:
            writer = csv.writer(f)
            writer.writerow([
              plan_id, 
              method, 
              p, 
              t,
              *program_output.values()
            ])

          # check if done
          if close_to_target(target_errors, program_output):
            close += 1

        if close / n >= 0.9:
          break

        if p > max_p:
          print(f'ERROR: Could not reach target error')
          break

        # increase number of particles
        p += 2

def find_satisfiable_plans(files, plans, knowns):
  print(plans)
  files = sorted(files, key=lambda x: int(os.path.basename(x)[4:-3]))
  satisfiable_plans = []

  # Get runtime inference plan
  print('MAKE SURE TO MANUALLY CHECK RUNTIME INFERENCE PLAN!!!\n')
  for file in files:
    plan_id = get_plan_id(file)

    print(f'Checking {file}...')

    if knowns is not None:
      # Use known checks
      pre_check = False
      for var in knowns:
        if not is_satisfiable(plans[plan_id]['plan'][var], knowns[var]):
          print('> Not satisfiable')
          pre_check = True
          break
      if pre_check:
        continue

    # get analysis output
    cmd = f'{sys.executable} siren.py {file} -p 10'
    print('>', cmd)
    try:
      out = subprocess.check_output(cmd, cwd=CWD, shell=True, stderr=subprocess.STDOUT).decode("utf-8")
    except subprocess.CalledProcessError as e:
      output = e.output.decode("utf-8")
      if 'RuntimeViolatedAnnotationError' in output:
        print('> Not satisfiable')
        continue
      else:
        print(output)
        raise RuntimeError()

    satisfiable_plans.append(plan_id)
    print('> Satisfiable')    


    # parse output
    lines = out.strip().split('\n')

    # get outputs after ===== Runtime Inference Plan =====
    true_plan = {}
    start = False
    for line in lines:
      line = line.strip()
      if line == '===== Runtime Inference Plan =====':
        start = True
      elif start:
        if line != '':
          var, enc = line.split(': ')[:2]
          true_plan[var] = enc.strip()

    # compare annotated and real plans
    # only for the ones that show up
    for var in plans[plan_id]['plan']:
      if var in true_plan:
        if not is_satisfiable(plans[plan_id]['plan'][var], true_plan[var]):
          print()
          print(f'ERROR: {file}')
          print('Annotated:', plans[plan_id]['plan'])
          print('Real:', true_plan)
          print()
          break

  return satisfiable_plans

def analyze(files, variables, satisfied_plans):
  results = {}
  # Number of plans
  results['n_plans'] = len(files)
  # Number of variables
  results['n_vars'] = len(variables)

  results['plan'] = {}

  n_true_satisfied = len(satisfied_plans.keys())
  n_inferred_satisfied = 0

  for file in files:
    plan_id = get_plan_id(file)
    results['plan'][plan_id] = {}

    results['plan'][plan_id]['true_satisfied'] = plan_id in satisfied_plans

    print(f'Analyzing {file}...')

    # get analysis output
    cmd = f'{sys.executable} siren.py {file} --analyze-only'
    print('>', cmd)
    try:
      out = subprocess.check_output(cmd, cwd=CWD, shell=True, stderr=subprocess.STDOUT).decode("utf-8")
    except subprocess.CalledProcessError as e:
      output = e.output.decode("utf-8")
      results['plan'][plan_id]['infer_satisfied'] = False
      continue

    results['plan'][plan_id]['infer_satisfied'] = True
    n_inferred_satisfied += 1

    # parse output
    lines = out.strip().split('\n')

    analysis_time = -1
    for i, line in enumerate(lines):
      line = line.strip()
      if line == '===== Analysis Time =====':
        analysis_time = float(lines[i + 1])
        break

    if analysis_time == -1:
      raise RuntimeError('Analysis time not found')
    
    results['plan'][plan_id]['analysis_time'] = analysis_time

    # get outputs after ===== Inferred Inference Plan =====
    inferred_plan = {}
    start = False
    for line in lines:
      line = line.strip()
      if line == '===== Inferred Inference Plan =====':
        start = True
      elif '=====' in line:
        break
      elif start:
        if line != '':
          var, enc = line.split(': ')
          inferred_plan[var] = enc.strip()

    # double check annotated and real plans match since satisfied
    for var in variables:
      if var not in inferred_plan:
        print()
        print(f'ERROR: {file} missing {var}')
        print('Inferred:', inferred_plan)
      elif not is_satisfiable(satisfied_plans[plan_id][var], inferred_plan[var]):
        print()
        print(f'ERROR: {file}')
        print('Annotated:', satisfied_plans[plan_id][var])
        print('Inferred:', inferred_plan)
        print()
        break

  results['n_true_satisfied'] = n_true_satisfied
  results['n_inferred_satisfied'] = n_inferred_satisfied

  n_satisfied_tp = 0
  n_satisfied_fp = 0
  n_satisfied_tn = 0
  n_satisfied_fn = 0

  for file in files:
    plan_id = get_plan_id(file)
    inferred_satisfied = results['plan'][plan_id]['infer_satisfied']
    true_satisfied = plan_id in satisfied_plans

    if inferred_satisfied and true_satisfied:
      n_satisfied_tp += 1
    elif inferred_satisfied and not true_satisfied:
      n_satisfied_fp += 1
    elif not inferred_satisfied and true_satisfied:
      n_satisfied_fn += 1
    else:
      n_satisfied_tn += 1

  results['n_satisfied_tp'] = n_satisfied_tp
  results['n_satisfied_fp'] = n_satisfied_fp
  results['n_satisfied_fn'] = n_satisfied_fn
  results['n_satisfied_tn'] = n_satisfied_tn

  return results
  
if __name__ == '__main__':
  p = argparse.ArgumentParser()
  p.add_argument('--benchmark', '-b', type=str, required=False)
  p.add_argument('--output', '-o', type=str, required=False, default='output')
  p.add_argument('--files', '-f', type=str, required=False)
  p.add_argument('--methods', '-m', type=str, required=False)

  sp = p.add_subparsers(dest='subparser_name')

  rp = sp.add_parser('run')
  rp.add_argument('--particles', '-p', type=int, required=False, nargs='+')
  rp.add_argument('--prange', '-pr', type=int, required=False, nargs=2, default=[1, 1000])
  rp.add_argument('--accuracy', '-a', action='store_true')
  rp.add_argument('--n', '-n', type=int, required=False, default=100)
  
  ap = sp.add_parser('analyze')

  rp = sp.add_parser('check')

  bp = sp.add_parser('baseline')

  args = p.parse_args()

  benchmarks = [b.strip() for b in args.benchmark.split(',')] if args.benchmark is not None else DEFAULT_BENCHMARKS
  methods = [m.strip() for m in args.methods.split(',')] if args.methods is not None else DEFAULT_METHODS

  for benchmark in benchmarks:
    print('Benchmark: {}'.format(benchmark))

    with open(os.path.join(benchmark, 'config.json')) as f:
      config = json.load(f)
    
    # If no files specified, get all files in programs directory
    if args.files is None:
      files = []
      for file in os.listdir(os.path.join(benchmark, 'programs')):
        if file.endswith('.si'):
          files.append(file)
    else: 
      files = [f.strip() for f in args.files.split(',')]

    # harness in benchmarks directory already
    for file in files:
      if not os.path.exists(os.path.join(benchmark, 'programs', os.path.basename(file))):
        raise Exception(f'File not found: {file}')

    files = sorted(files, key=lambda x: int(os.path.basename(x)[4:-3]))
    files = map(lambda x: os.path.join(BENCHMARK_DIR, benchmark, 'programs', os.path.basename(x)), files)
    files = list(files)

    methods = [method for method in methods if method in DEFAULT_METHODS]

    if args.subparser_name == 'run':
      # make output directory
      outdir = os.path.join(benchmark, args.output)
      os.makedirs(outdir, exist_ok=True)

      if args.files is None:
        # using all files
        # Filter for files that are satisfiable
        files = filter(lambda x: config['plans'][get_plan_id(x)]["satisfiable"], files)
        files = list(files)

      print(files)

      n = args.n

      true_vars = config['true_vars']

      if not args.accuracy:
        # Get list of particles
        if args.particles is None:
          particles = sorted([int(x) for x in np.unique(np.logspace(
                                                  np.log10(args.prange[0]), 
                                                  np.log10(args.prange[1]), 
                                                  N_INTERVALS, 
                                                  dtype=int
                                                ))])
          print('Particles:', particles)
        else:
          particles = args.particles

        results_file = os.path.join(benchmark, args.output, 'results.csv')
        if not os.path.exists(results_file):
          with open(results_file, 'w') as f:
            writer = csv.writer(f)
            fieldnames = ['plan_id', 'method', 'particles', 'time']
            fieldnames += [var[0] for var in true_vars]
            writer.writerow(fieldnames)

        run_particles(files, n, particles, methods, true_vars, results_file)
      else:
        target_errors = config['target_errors']
        min_p = config['min_particles'] if 'min_particles' in config else 1
        max_p = config['max_particles'] if 'max_particles' in config else 1000

        results_file = os.path.join(benchmark, args.output, 'results.csv')
        if not os.path.exists(results_file):
          with open(results_file, 'w') as f:
            writer = csv.writer(f)
            fieldnames = ['plan_id', 'method', 'particles', 'time']
            fieldnames += [var[0] for var in true_vars]
            writer.writerow(fieldnames)

        run_accuracy(files, min_p, max_p, n, target_errors, methods, true_vars, results_file)
    elif args.subparser_name == 'analyze':
      filename = os.path.join(benchmark, args.output, 'statistics.json')

      variables = config['variables']

      satisfied_plans = {}
      for plan_id, plan_data in config['plans'].items():
        if plan_data['satisfiable']:
          satisfied_plans[plan_id] = plan_data['plan']

      print(f'Satisfied plans: {list(satisfied_plans.keys())}')
      print(satisfied_plans)

      results = analyze(files, variables, satisfied_plans)

      os.makedirs(os.path.dirname(filename), exist_ok=True)

      # write statistics
      with open(os.path.join(filename), 'w') as f:
        json.dump(results, f, indent=2)

    elif args.subparser_name == 'check':
      knowns = config['known_enc'] if 'known_enc' in config else None
      satisfied_plan_ids = find_satisfiable_plans(files, config['plans'], knowns)
      
      for plan_id, plan_data in config['plans'].items():
        plan_data['satisfiable'] = plan_id in satisfied_plan_ids

      with open(os.path.join(benchmark, 'config.json'), 'w') as f:
        json.dump(config, f, indent=2)

    elif args.subparser_name == 'baseline':
      # make output directory
      outdir = os.path.join(benchmark, args.output)
      os.makedirs(outdir, exist_ok=True)

      n = 100
      particles = 1000
      file = os.path.join(BENCHMARK_DIR, benchmark, 'baseline.si')

      true_vars = config['true_vars']

      results_file = os.path.join(benchmark, args.output, 'baseline.csv')

      # run baseline if it doesn't exist
      if not os.path.exists(results_file):
        with open(results_file, 'w') as f:
          writer = csv.writer(f)
          fieldnames = ['plan_id', 'method', 'particles', 'time']
          fieldnames += [var[0] for var in true_vars]
          writer.writerow(fieldnames)

        run_particles([file], n, [particles], methods, true_vars, results_file)

      # get median accuracy of each variable without any annotations with 
      # 1000 particles
      with open(results_file, 'r') as f:
        reader = csv.reader(f)
        next(reader)
        data = np.array(list(reader))[:, 4:].astype(float)

        acc = np.median(data, axis=0)
        
        print('Median accuracy:')
        print(acc)

        config['target_errors'] = {var[0]: acc[i] for i, var in enumerate(true_vars)}

        with open(os.path.join(benchmark, 'config.json'), 'w') as f:
          json.dump(config, f, indent=2)
        
    else:
      print('Invalid subcommand')
      exit(1)
