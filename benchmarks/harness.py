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
import ast

BENCHMARK_DIR = 'benchmarks'

DEFAULT_BENCHMARKS = [
  # 'gaussianmixture',
  'envnoise',
  'noise',
  # 'smsbehavior',
  'outlier',
  # 'mixasymprior',
  'gtree',
  'outlierheavy',
  'slds',
  'runner',
]

DEFAULT_METHODS = [
  'ssi',
  'ds',
  # 'ft',
  # 'dis',
  'bp',
]

N_INTERVALS = 30

INC = 1

CWD = '..'

def calc_error(true_x, x):
  return (true_x - x) ** 2

def close_to_target(target, value):
  if value == 0:
    return True
  if log10(value) - log10(target) <= 0.5:
    return True

  return False
  
def close_to_target_error(target_error, program_output):
  checks = []
  for var, error in program_output.items():
    if not close_to_target(target_error[var], error):
      checks.append(False)
    else:
      checks.append(True)
    
  return checks

def close_to_target_runtime(target_runtime, runtime):
  if log10(runtime) - log10(target_runtime) <= 0.1:
    return True

  return False

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
  try:
    return str(int(os.path.basename(file)[4:-3]))
  except Exception:
    return None
  
def flatten_nested(structure):
  flattened_list = []
  for element in structure:
    if isinstance(element, tuple):
      flattened_list.extend(flatten_nested(element))
    elif isinstance(element, list):
      flattened_list.append(element)
    else:
      flattened_list.append([element])
  return flattened_list

# Runs benchmark executable and computes the absolute error of each variable
def run_siren(file, p, method, true_vars, error_func):
  # run siren file
  cmd = f'{sys.executable} siren.py {file} -m {method} -p {p}'
  
  print('>', cmd)

  try:
    out = subprocess.check_output(cmd, cwd=CWD, shell=True, stderr=subprocess.STDOUT).decode("utf-8")
  except subprocess.CalledProcessError as e:
    output = e.output.decode("utf-8")
    print(output)
    return None
  

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
  # or (0.1782178217821782, (-11.323677393073108, [0, 1, 2]))
  split_output = ast.literal_eval(output)

  # parse line into dict
  split_output = flatten_nested(split_output)

  for true_var, out in zip(true_vars, split_output):
    var, true_vals = true_var

    if error_func == 'mse':
      # compute MSE error
      error = 0
      for true_val, val in zip(true_vals, out):
        error += calc_error(true_val, val) 
      error /= len(true_vals)
    else:
      # print('compute max error')
      # compute max error
      error = 0
      for true_val, val in zip(true_vals, out):
        # print(true_val, val)
        error = max(error, calc_error(true_val, val))

    program_output[var] = error

  return eval_time, program_output

# Run experiments for each given number of particles
def run_particles(files, n, particles, methods, true_vars, results_file, error_func):
  for method in methods:
    print(f'Running {method}...')

    for file in files:
      plan_id = get_plan_id(file)

      for p in particles:
        print(f'Running with {p} particles')

        for i in range(n):
          print(f'{plan_id} {method} - {p} particles - Run {i}')

          run_outputs = None
          while run_outputs is None:
            run_outputs = run_siren(file, p, method, true_vars, error_func)
            if run_outputs is not None:
              break
          t, program_output = run_outputs

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
def run_accuracy(benchmark, files, n, plans, target_errors, methods, true_vars, results_file, error_func):
  if len(files) == 0:
    # If no files specified, get all files in programs directory
    files = []
    for file in os.listdir(os.path.join(benchmark, 'programs')):
      if file.endswith('.si'):
        files.append(file)

    # harness in benchmarks directory already
    for file in files:
      if not os.path.exists(os.path.join(benchmark, 'programs', os.path.basename(file))):
        raise Exception(f'File not found: {file}')

    files = sorted(files, key=lambda x: int(os.path.basename(x)[4:-3]))
    files = map(lambda x: os.path.join(BENCHMARK_DIR, benchmark, 'programs', os.path.basename(x)), files)
    files = list(files)

  for method in methods:
    print(f'Running {method}...')
    files = filter(lambda x: config['plans'][get_plan_id(x)]["satisfiable"][method], files)
    files = list(files)
    print(files)

    for file in files:
      plan_id = get_plan_id(file)
      if plan_id is None:
        print(f'Invalid file: {file}')
        continue
      min_p = plans[plan_id]['min_particles'] if 'min_particles' in plans[plan_id] else 1
      max_p = plans[plan_id]['max_particles'] if 'max_particles' in plans[plan_id] else 10

      # binary search for number of particles
      attempted_p = set()
      df = pd.read_csv(results_file)
      existing_p = df.loc[df['method'] == method]\
              .loc[df['plan_id'] == int(plan_id)]['particles'].unique()
      for p in existing_p:
        attempted_p.add(p)

      print(existing_p)

      for var in target_errors:
        print(f'Running for {var}')
        lower_p = min_p
        upper_p = max_p

        while True:
          p = (lower_p + upper_p) // 2
          print('Trying', p)

          # get errors for this p
          if p in attempted_p:
            print(f'Already ran {p} particles')
            df = pd.read_csv(results_file)
            df = df.loc[df['method'] == method]\
                    .loc[df['plan_id'] == int(plan_id)]\
                    .loc[df['particles'] == p][var]
            
            n_close = 0
            for error in df:
              if close_to_target(target_errors[var], error):
                n_close += 1

          else:
            print(f'Running with {p} particles')

            n_close = 0
            for i in range(n):
              print(f'{plan_id} {method} - {p} particles - Run {i}')

              run_outputs = None
              while run_outputs is None:
                run_outputs = run_siren(file, p, method, true_vars, error_func)
                if run_outputs is not None:
                  break
              t, program_output = run_outputs

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

              if close_to_target(target_errors[var], program_output[var]):
                n_close += 1

            attempted_p.add(p)

          if n_close / n > 0.9:
            print('Too close to target')
            # decrease number of particles
            upper_p = p - 1
          else:
            print('Not close enough to target')
            # increase number of particles
            lower_p = p + 1

          if lower_p > upper_p:
            print('No more particles to try')
            break

# Run experiments to reach a given runtime
def run_runtime(benchmark, files, n, plans, target_runtime, methods, true_vars, results_file, error_func):
  if len(files) == 0:
    # If no files specified, get all files in programs directory
    files = []
    for file in os.listdir(os.path.join(benchmark, 'programs')):
      if file.endswith('.si'):
        files.append(file)

    # harness in benchmarks directory already
    for file in files:
      if not os.path.exists(os.path.join(benchmark, 'programs', os.path.basename(file))):
        raise Exception(f'File not found: {file}')

    files = sorted(files, key=lambda x: int(os.path.basename(x)[4:-3]))
    files = map(lambda x: os.path.join(BENCHMARK_DIR, benchmark, 'programs', os.path.basename(x)), files)
    files = list(files)

  for method in methods:
    print(f'Running {method}...')
    files = filter(lambda x: config['plans'][get_plan_id(x)]["satisfiable"][method], files)
    files = list(files)
    print(files)

    for file in files:
      plan_id = get_plan_id(file)
      if plan_id is None:
        print(f'Invalid file: {file}')
        continue
      min_p = plans[plan_id]['min_particles'] if 'min_particles' in plans[plan_id] else 1
      max_p = plans[plan_id]['max_particles'] if 'max_particles' in plans[plan_id] else 1000

      # binary search for number of particles
      attempted_p = set()
      df = pd.read_csv(results_file)
      existing_p = df.loc[df['method'] == method]\
              .loc[df['plan_id'] == int(plan_id)]['particles'].unique()
      for p in existing_p:
        attempted_p.add(p)

      print(existing_p)

      lower_p = min_p
      upper_p = max_p

      while True:
        p = (lower_p + upper_p) // 2
        print('Trying', p)

        # get errors for this p
        if p in attempted_p:
          print(f'Already ran {p} particles')
          df = pd.read_csv(results_file)
          df = df.loc[df['method'] == method]\
                  .loc[df['plan_id'] == int(plan_id)]\
                  .loc[df['particles'] == p]['time']
          
          n_close = 0
          for error in df:
            if close_to_target_runtime(target_runtime, error):
              n_close += 1

        else:
          print(f'Running with {p} particles')

          n_close = 0
          for i in range(n):
            print(f'{plan_id} {method} - {p} particles - Run {i}')

            run_outputs = None
            while run_outputs is None:
              run_outputs = run_siren(file, p, method, true_vars, error_func)
              if run_outputs is not None:
                break
            t, program_output = run_outputs

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

            if close_to_target_runtime(target_runtime, t):
              n_close += 1

          attempted_p.add(p)

        if n_close / n > 0.5:
          print('Too close to target')
          # decrease number of particles
          upper_p = p - 1
        else:
          print('Not close enough to target')
          # increase number of particles
          lower_p = p + 1

        if lower_p > upper_p:
          print('No more particles to try')
          break

def find_satisfiable_plans(benchmark, files, methods, plans, knowns):
  # print(plans)
  if len(files) == 0:
    # If no files specified, get all files in programs directory
    files = []
    for file in os.listdir(os.path.join(benchmark, 'programs')):
      if file.endswith('.si'):
        files.append(file)

    # harness in benchmarks directory already
    for file in files:
      if not os.path.exists(os.path.join(benchmark, 'programs', os.path.basename(file))):
        raise Exception(f'File not found: {file}')

    files = sorted(files, key=lambda x: int(os.path.basename(x)[4:-3]))
    files = map(lambda x: os.path.join(BENCHMARK_DIR, benchmark, 'programs', os.path.basename(x)), files)
    files = list(files)

  satisfiable_plans = {}

  # Get runtime inference plan
  print('MAKE SURE TO MANUALLY CHECK RUNTIME INFERENCE PLAN!!!\n')
  for method in methods:
    print(f'For {method}...')
    satisfiable_plans[method] = []

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
      cmd = f'{sys.executable} siren.py {file} -p 10 -m {method}'
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

      satisfiable_plans[method].append(plan_id)
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

def analyze(benchmark, files, methods, variables, plans, results):
  if len(files) == 0:
    # If no files specified, get all files in programs directory
    files = []
    for file in os.listdir(os.path.join(benchmark, 'programs')):
      if file.endswith('.si'):
        files.append(file)

    # harness in benchmarks directory already
    for file in files:
      if not os.path.exists(os.path.join(benchmark, 'programs', os.path.basename(file))):
        raise Exception(f'File not found: {file}')

    files = sorted(files, key=lambda x: int(os.path.basename(x)[4:-3]))
    files = map(lambda x: os.path.join(BENCHMARK_DIR, benchmark, 'programs', os.path.basename(x)), files)
    files = list(files)

  # Number of plans
  results['n_plans'] = len(files)
  # Number of variables
  results['n_vars'] = len(variables)

  for method in methods:
    print(f'For {method}...')
    method_results = {}
    method_results['plan'] = {}

    satisfied_plans = {}
    for plan_id, plan_data in plans.items():
      if plan_data['satisfiable'][method]:
        satisfied_plans[plan_id] = plan_data['plan']

    print(f'Satisfied plans: {list(satisfied_plans.keys())}')
    print(satisfied_plans)

    n_true_satisfied = len(satisfied_plans.keys())
    n_inferred_satisfied = 0

    for file in files:
      plan_id = get_plan_id(file)
      method_results['plan'][plan_id] = {}

      method_results['plan'][plan_id]['true_satisfied'] = plan_id in satisfied_plans

      print(f'Analyzing {file}...')

      # get analysis output
      cmd = f'{sys.executable} siren.py {file} -m {method} --analyze-only'
      print('>', cmd)
      try:
        out = subprocess.check_output(cmd, cwd=CWD, shell=True, stderr=subprocess.STDOUT).decode("utf-8")
      except subprocess.CalledProcessError as e:
        output = e.output.decode("utf-8")
        method_results['plan'][plan_id]['infer_satisfied'] = False
        continue

      method_results['plan'][plan_id]['infer_satisfied'] = True
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
      
      method_results['plan'][plan_id]['analysis_time'] = analysis_time

      # get outputs after ===== Inferred Inference Plan =====
      inferred_plan = {}
      start = False
      for line in lines:
        line = line.strip()
        if line == '===== Inferred Inference Plan =====':
          start = True
        elif start:
          if '=====' in line:
            break
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
          print('Annotated:', satisfied_plans[plan_id])
          print('Inferred:', inferred_plan)
          print()
          break

    method_results['n_true_satisfied'] = n_true_satisfied
    method_results['n_inferred_satisfied'] = n_inferred_satisfied

    n_satisfied_tp = 0
    n_satisfied_fp = 0
    n_satisfied_tn = 0
    n_satisfied_fn = 0

    for file in files:
      plan_id = get_plan_id(file)
      inferred_satisfied = method_results['plan'][plan_id]['infer_satisfied']
      true_satisfied = plan_id in satisfied_plans

      if inferred_satisfied and true_satisfied:
        n_satisfied_tp += 1
      elif inferred_satisfied and not true_satisfied:
        n_satisfied_fp += 1
      elif not inferred_satisfied and true_satisfied:
        n_satisfied_fn += 1
      else:
        n_satisfied_tn += 1

    method_results['n_satisfied_tp'] = n_satisfied_tp
    method_results['n_satisfied_fp'] = n_satisfied_fp
    method_results['n_satisfied_fn'] = n_satisfied_fn
    method_results['n_satisfied_tn'] = n_satisfied_tn

    results[method] = method_results

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
  rp.add_argument('--runtime', '-r', action='store_true')
  rp.add_argument('--n', '-n', type=int, required=False, default=100)
  rp.add_argument('--error-func', '-ef', type=str, required=False, default='mse')
  
  ap = sp.add_parser('analyze')

  rp = sp.add_parser('check')

  bp = sp.add_parser('baseline')
  bp.add_argument('--error-func', '-ef', type=str, required=False, default='mse')

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
    else: 
      files = [f.strip() for f in args.files.split(',')]

    for file in files:
      if not os.path.exists(file):
        raise Exception(f'File not found: {file}')

    methods = [method for method in methods if method in DEFAULT_METHODS]

    if args.subparser_name == 'run':
      # make output directory
      outdir = os.path.join(benchmark, args.output)
      os.makedirs(outdir, exist_ok=True)
      
      n = args.n

      true_vars = config['true_vars']

      if args.accuracy and args.runtime:
        raise Exception('Cannot run both accuracy and runtime')
      elif args.runtime:
        target_runtime = config['target_runtime']
        plan_config = config['plans']

        results_file = os.path.join(benchmark, args.output, 'results.csv')
        if not os.path.exists(results_file):
          with open(results_file, 'w') as f:
            writer = csv.writer(f)
            fieldnames = ['plan_id', 'method', 'particles', 'time']
            fieldnames += [var[0] for var in true_vars]
            writer.writerow(fieldnames)

        run_runtime(benchmark, files, n, plan_config, target_runtime, methods, true_vars, results_file, args.error_func)

      elif args.accuracy:
        target_errors = config['target_errors']
        plan_config = config['plans']

        results_file = os.path.join(benchmark, args.output, 'results.csv')
        if not os.path.exists(results_file):
          with open(results_file, 'w') as f:
            writer = csv.writer(f)
            fieldnames = ['plan_id', 'method', 'particles', 'time']
            fieldnames += [var[0] for var in true_vars]
            writer.writerow(fieldnames)

        run_accuracy(benchmark, files, n, plan_config, target_errors, methods, true_vars, results_file, args.error_func)
      else:
        # Just run with given number of particles
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

        run_particles(files, n, particles, methods, true_vars, results_file, args.error_func)
    elif args.subparser_name == 'analyze':
      filename = os.path.join(benchmark, args.output, 'statistics.json')

      results = {}
      if os.path.exists(filename):
        with open(os.path.join(filename)) as f:
          results = json.load(f)

      variables = config['variables']

      results = analyze(benchmark, files, methods, variables, config['plans'], results)

      os.makedirs(os.path.dirname(filename), exist_ok=True)

      # write statistics
      with open(os.path.join(filename), 'w') as f:
        json.dump(results, f, indent=2)

    elif args.subparser_name == 'check':
      knowns = config['known_enc'] if 'known_enc' in config else None
      satisfied_plan_ids = find_satisfiable_plans(benchmark, files, methods, config['plans'], knowns)
      
      for plan_id, plan_data in config['plans'].items():
        plan_data['satisfiable'] = {} if 'satisfiable' not in plan_data else plan_data['satisfiable']
        for method in methods:
          plan_data['satisfiable'][method] = (plan_id in satisfied_plan_ids[method])

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

        run_particles([file], n, [particles], methods, true_vars, results_file, args.error_func)

      # get median accuracy of each variable and median runtime without any annotations with 
      # 1000 particles
      config['target_errors'] = {}
      config['target_runtime'] = {}
      
      with open(results_file, 'r') as f:
        reader = csv.reader(f)
        next(reader)
        all_data = np.array(list(reader))

        for method in methods:
          data = all_data[all_data[:, 1] == method]

          acc = np.median(data[:, 4:].astype(float), axis=0)
          
          print('Median accuracy:')
          print(acc)

          runtimes = data[:, 3].astype(float)
          runtime = np.median(runtimes)

          print('Median runtime:')
          print(runtime)

          config['target_errors'][method] = {var[0]: acc[i] for i, var in enumerate(true_vars)}
          config['target_runtime'][method] = runtime

        with open(os.path.join(benchmark, 'config.json'), 'w') as f:
          json.dump(config, f, indent=2)
        
    else:
      print('Invalid subcommand')
      exit(1)
