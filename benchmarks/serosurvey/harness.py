import argparse
import os
import json
import subprocess
import time
from math import log
import re
import matplotlib.pyplot as plt
import numpy as np


def run_muf(filename, output, p, n, streaming, verbose=False):
  # run muf file
  cmd = './{}_main.exe'.format(os.path.splitext(os.path.basename(filename))[0])
  
  if verbose:
    print('>', cmd)

  t1 = time.time()
  out = subprocess.check_output(cmd, shell=True).decode("utf-8") 
  t2 = time.time()

  if verbose:
    print('> Took {} seconds'.format(t2 - t1))

    with open(os.path.join(output, 'muf.log'), 'a') as f:
      f.write(' '.join([str(x) for x in [p, n, t2 - t1, streaming]]) + '\n' + out + '\n')
  
  # parse output
  lines = out.strip().split('\n')

  # only need the final line, because each "iteration" is an observation
  if streaming:
    raise NotImplementedError('Streaming not implemented')
  else:
    line = lines[-1]

  # parse line
  # p: -0.653009934653 sens: 0.316569055538 spec: 0.754852958023 p_mse: 0.469238610573 sens_mse: 0.290985743843 spec_mse: 0.0591204840223
  p_mean, sens_mean, spec_mean, p_mse, sens_mse, spec_mse = [float(x) for x in line.split(' ')[1::2]]

  return p_mse, sens_mse, spec_mse, (t2 - t1)

def close_to_target(target_accuracy, accuracy):
  return log(accuracy) - log(target_accuracy) < 0.5

# Run experiments
def run(filename, output, particles, accuracy, n, streaming, results, verbose=False):
  if len(particles) == 0:
    particles = [x for x in range(1, 5001)]
  
  found_p, found_sens, found_spec = False, False, False

  for p in particles:
    if verbose:
      print('Running with {} particles'.format(p))

    # Compile muf
    # This is a hack. Edit the file to change the number of particles
    with open(filename, 'r') as f:
      content = f.read()
    pattern = re.compile(r'infer \((\d+), serosurvey\)')
    content = pattern.sub('infer ({}, serosurvey)'.format(p), content)
    with open(filename, 'w') as f:
      f.write(content)

    cmd = 'mufc {}'.format(filename)
    if verbose:
      print('>', cmd)
    if subprocess.call(cmd, shell=True, stdout=subprocess.DEVNULL) != 0:
      raise Exception('Failed to compile muf file')

    mses = []
    runtimes = []
    for i in range(n):
      if verbose:
        print('{} particles - Run {}'.format(p, i))

      p_mse, sens_mse, spec_mse, t = run_muf(filename, output, p, n, streaming, verbose)
      mses.append((p_mse, sens_mse, spec_mse))
      runtimes.append(t)

    # quantiles of mse for runs
    p_mse_sorted = sorted([x[0] for x in mses])
    sens_mse_sorted = sorted([x[1] for x in mses])
    spec_mse_sorted = sorted([x[2] for x in mses])
    runtimes = sorted(runtimes)

    # percentiles: 10, 50, 90
    p_mse_lower = p_mse_sorted[int(0.10 * len(p_mse_sorted))]
    p_mse_median = p_mse_sorted[int(0.50 * len(p_mse_sorted))]
    p_mse_upper = p_mse_sorted[int(0.90 * len(p_mse_sorted))]

    sens_mse_lower = sens_mse_sorted[int(0.10 * len(sens_mse_sorted))]
    sens_mse_median = sens_mse_sorted[int(0.50 * len(sens_mse_sorted))]
    sens_mse_upper = sens_mse_sorted[int(0.90 * len(sens_mse_sorted))]

    spec_mse_lower = spec_mse_sorted[int(0.10 * len(spec_mse_sorted))]
    spec_mse_median = spec_mse_sorted[int(0.50 * len(spec_mse_sorted))]
    spec_mse_upper = spec_mse_sorted[int(0.90 * len(spec_mse_sorted))]
    
    runtime_lower = runtimes[int(0.10 * len(runtimes))]
    runtime_median = runtimes[int(0.50 * len(runtimes))]
    runtime_upper = runtimes[int(0.90 * len(runtimes))]

    if verbose:
      with open(os.path.join(output, 'muf.log'), 'a') as f:
        f.write('p_mse: ' + ' '.join(map(str, p_mse_sorted)) + '\n')
        f.write('sens_mse: ' + ' '.join(map(str, sens_mse_sorted)) + '\n')
        f.write('spec_mse: ' + ' '.join(map(str, spec_mse_sorted)) + '\n')
        f.write('runtimes: ' + ' '.join(map(str, runtimes)) + '\n')

    # save results
    if filename not in results:
      results[filename] = {}

    results[filename][p] = {
      'p_mse': {
        'lower': p_mse_lower,
        'median': p_mse_median,
        'upper': p_mse_upper
      },
      'sens_mse': {
        'lower': sens_mse_lower,
        'median': sens_mse_median,
        'upper': sens_mse_upper
      },
      'spec_mse': {
        'lower': spec_mse_lower,
        'median': spec_mse_median,
        'upper': spec_mse_upper
      },
      'runtime': {
        'lower': runtime_lower,
        'median': runtime_median,
        'upper': runtime_upper
      }
    }

    # if not found_p:
    #   if close_to_target(accuracy, p_mse_upper):
    #     found_p = True
    #     if verbose:
    #       print('Found p meeting target accuracy: {}'.format(p))
    #     results['p_target'] = p
    # if not found_sens:
    #   if close_to_target(accuracy, sens_mse_upper):
    #     found_sens = True
    #     if verbose:
    #       print('Found sens meeting target accuracy: {}'.format(p))
    #     results['sens_target'] = p
    # if not found_spec:
    #   if close_to_target(accuracy, spec_mse_upper):
    #     found_spec = True
    #     if verbose:
    #       print('Found spec meeting target accuracy: {}'.format(p))
    #     results['spec_target'] = p

  return results

def plot(output, streaming, particles, verbose=False):
  if streaming:
    raise NotImplementedError('Plotting not implemented for streaming')

  # Load results
  with open(os.path.join(output, 'results.json')) as f:
    results = json.load(f)

  # Plot results

  types = ['accuracy', 'runtime']
  # subplot for each variable
  fig, axes = plt.subplots(3, 2, figsize=(8, 10))

  colors = ['#00bfbf', '#ff4d32', '#ffbf00', '#00b300']
  markers = ['s', 'v', 'd', 'o']

  # runtime
  for j, (filename, data) in enumerate(results.items()):

    p = []
    lower, median, upper = [], [], []
    for p_, data_ in data.items():
      if particles is not None and int(p_) not in particles:
        continue

      p.append(float(p_))

      measurement_label = 'runtime'

      lower.append(data_[measurement_label]['lower'])
      median.append(data_[measurement_label]['median'])
      upper.append(data_[measurement_label]['upper'])

    # Only one set of file labels
    fmt = markers[j]

    axes[1][1].scatter(p, median, marker=markers[j], color=colors[j])
    # axes[1][1].errorbar(p, median, yerr=[lower, upper], fmt=fmt, color=colors[j], capsize=5)
    # axes[1][1].set_yscale('log')
    # axes[1][1].set_ylim(1e-4, 1e3)
    # axes[1][1].set_xlabel('log')
    axes[0][1].set_visible(False)
    axes[2][1].set_visible(False)
  
  # accuracy
  for j, (filename, data) in enumerate(results.items()):
    vars = ['p', 'sens', 'spec']

    for k, v in enumerate(vars):
      p = []
      lower, median, upper = [], [], []
      for p_, data_ in data.items():
        if particles is not None and int(p_) not in particles:
          continue
        p.append(float(p_))

        measurement_label = '{}_mse'.format(v)

        lower.append(data_[measurement_label]['lower'])
        median.append(data_[measurement_label]['median'])
        upper.append(data_[measurement_label]['upper'])

      # Only one set of file labels
      if k == 0:
        label = os.path.splitext(os.path.basename(filename))[0].split('_')[1]
      else:
        label = None
      fmt = markers[j]

      axes[k][0].scatter(p, median, marker=markers[j], color=colors[j], label=label)
      # axes[k][0].errorbar(p, median, yerr=[lower, upper], fmt=fmt, color=colors[j], capsize=5, label=label)
      axes[k][0].set_yscale('log')
      # axes[k][0].set_ylim(1e-4, 1e3)
      # axes[k][0].set_xscale('log')
      axes[k][0].set_title('{} Accuracy'.format(v))

    axes[2][0].set_xlabel('Particles')
    axes[2][1].set_xlabel('Particles')

  axes[1][0].set_ylabel('Mean Squared Error (logscale)')
  axes[1][1].set_ylabel('Elapsed Time in seconds (logscale)')

  fig.legend(ncols=3, frameon=False, loc='upper center')
  fig.tight_layout()
      

  if verbose:
    print('Saving plots')

  name = os.path.splitext(os.path.basename(filename))[0].split('_')[0]
  fig.savefig(os.path.join(output, '{}.png'.format(name)))
  fig.savefig(os.path.join(output, '{}.pdf'.format(name)))

  # print ratio of runtime
  particles = 2000
  print('runtime: {}'.format(
    results['serosurvey_default.muf']['2000']['runtime']['median'] / \
    results['serosurvey_alt.muf']['2000']['runtime']['median']))




if __name__ == '__main__':
  p = argparse.ArgumentParser()
  p.add_argument('--verbose', '-v', action='store_true', required=False)
  p.add_argument('--output', '-o', type=str, required=False, default='output')
  p.add_argument('--streaming', '-s', action='store_true', required=False, default=False)

  sp = p.add_subparsers(dest='subparser_name')

  rp = sp.add_parser('run')
  rp.add_argument('--file', '-f', type=str, required=False)
  rp.add_argument('--particles', '-p', type=int, required=False, nargs='+')
  rp.add_argument('--accuracy', '-a', type=float, required=False, default=0.01)
  rp.add_argument('--n', '-n', type=int, required=False, default=1000)
  
  pp = sp.add_parser('plot')
  pp.add_argument('--particles', '-p', type=int, required=False, nargs='+')

  args = p.parse_args()

  # If plotting, just plot and exit. Assumes output is in args.output
  if args.subparser_name == 'plot':
    plot(args.output, args.streaming, args.particles, args.verbose)
    exit()

  elif args.subparser_name == 'run':
    # make output directory
    os.makedirs(args.output, exist_ok=True)

    if not os.path.exists(args.file):
      raise Exception('File not found: {}'.format(args.file))

    results = {}
    if os.path.exists(os.path.join(args.output, 'results.json')):
      with open(os.path.join(args.output, 'results.json')) as f:
        try:
          results = json.load(f)
        except:
          results = {}

    try:
      results = run(args.file, args.output, args.particles, args.accuracy, args.n, args.streaming, results, args.verbose)
    except KeyboardInterrupt:
      pass

    # write results
    with open(os.path.join(args.output, 'results.json'), 'w') as f:
      json.dump(results, f, indent=2)
  

  
  