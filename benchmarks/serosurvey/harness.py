import argparse
import os
import json
import subprocess
import time
from math import log
import re
import matplotlib.pyplot as plt


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

    # with open(os.path.join(output, 'muf.log'), 'a') as f:
    #   f.write(' '.join([str(x) for x in [p, n, t2 - t1, streaming]]) + '\n' + out + '\n')
  
  # parse output
  lines = out.strip().split('\n')

  # only need the final line, because each "iteration" is an observation
  if streaming:
    raise NotImplementedError('Streaming not implemented')
  else:
    line = lines[-1]

  # parse line into dict
  program_output = {}
  # p: -0.653009934653 sens: 0.316569055538 spec: 0.754852958023 p_mse: 0.469238610573 sens_mse: 0.290985743843 spec_mse: 0.0591204840223
  line = line.strip()
  for part in line.split(' '):
    k, v = part.split(':')
    program_output[k] = float(v)
  
  return program_output, (t2 - t1)

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

    mses = {}
    runtimes = []
    for i in range(n):
      if verbose:
        print('{} particles - Run {}'.format(p, i))

      program_output, t = run_muf(filename, output, p, n, streaming, verbose)
      for k, v in program_output.items():
        if k.endswith('_mse'):
          if k not in mses:
            mses[k] = []
          mses[k].append(v)
      
      runtimes.append(t)

    # quantiles of mse for runs
    mses_sorted = {k: sorted(v) for k, v in mses.items()}
    # p_mse_sorted = sorted([x[0] for x in mses])
    # sens_mse_sorted = sorted([x[1] for x in mses])
    # spec_mse_sorted = sorted([x[2] for x in mses])
    runtimes = sorted(runtimes)

    # percentiles: 10, 50, 90
    mses_lower = {k: v[int(0.10 * len(v))] for k, v in mses_sorted.items()}
    mses_median = {k: v[int(0.50 * len(v))] for k, v in mses_sorted.items()}
    mses_upper = {k: v[int(0.90 * len(v))] for k, v in mses_sorted.items()}

    # p_mse_lower = p_mse_sorted[int(0.10 * len(p_mse_sorted))]
    # p_mse_median = p_mse_sorted[int(0.50 * len(p_mse_sorted))]
    # p_mse_upper = p_mse_sorted[int(0.90 * len(p_mse_sorted))]

    # sens_mse_lower = sens_mse_sorted[int(0.10 * len(sens_mse_sorted))]
    # sens_mse_median = sens_mse_sorted[int(0.50 * len(sens_mse_sorted))]
    # sens_mse_upper = sens_mse_sorted[int(0.90 * len(sens_mse_sorted))]

    # spec_mse_lower = spec_mse_sorted[int(0.10 * len(spec_mse_sorted))]
    # spec_mse_median = spec_mse_sorted[int(0.50 * len(spec_mse_sorted))]
    # spec_mse_upper = spec_mse_sorted[int(0.90 * len(spec_mse_sorted))]
    
    runtime_lower = runtimes[int(0.10 * len(runtimes))]
    runtime_median = runtimes[int(0.50 * len(runtimes))]
    runtime_upper = runtimes[int(0.90 * len(runtimes))]

    if verbose:
      with open(os.path.join(output, 'muf.log'), 'a') as f:
        # for k, v in mses_lower.items():
        #   f.write('{}: {} '.format(k, v))
        # f.write('p_mse: ' + ' '.join(map(str, p_mse_sorted)) + '\n')
        # f.write('sens_mse: ' + ' '.join(map(str, sens_mse_sorted)) + '\n')
        # f.write('spec_mse: ' + ' '.join(map(str, spec_mse_sorted)) + '\n')
        f.write('runtimes: ' + ' '.join(map(str, runtimes)) + '\n')

    # save results
    if filename not in results:
      results[filename] = {}
    if p not in results[filename]:
      results[filename][p] = {}

    variables = mses.keys()
    for v in variables:
      if v not in results[filename][p]:
        results[filename][p][v] = {}
      results[filename][p][v]['lower'] = mses_lower[v]
      results[filename][p][v]['median'] = mses_median[v]
      results[filename][p][v]['upper'] = mses_upper[v]

    if 'runtime' not in results[filename][p]:
      results[filename][p]['runtime'] = {}
    results[filename][p]['runtime']['lower'] = runtime_lower
    results[filename][p]['runtime']['median'] = runtime_median
    results[filename][p]['runtime']['upper'] = runtime_upper

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

  # get n variables
  n_variables = 0
  for filename, data in results.items():
    for p, data_ in data.items():
      n_variables = max(n_variables, len(data_))

  n_variables -= 1 # runtime

  # create subplots
  base_y = 3
  fig, axes = plt.subplots(n_variables, 2, figsize=(8, base_y * n_variables))

  colors = ['#00bfbf', '#ff4d32', '#ffbf00', '#00b300', '#0000b3', '#b300b3', '#bfbfbf', '#4d4d4d', '#ff8080', '#ffff00', '#80ff80', '#8080ff']
  markers = ['s', 'v', 'd', 'o', 'x', 'p', 'h', 'D', 'H', '8', 'P', 'X']

  # runtime
  for i, (filename, data) in enumerate(results.items()):

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
    label = os.path.splitext(os.path.basename(filename))[0]
    label = ' '.join(label.split('_')[1:])
    fmt = markers[i]

    axes[0][1].scatter(p, median, marker=markers[i], color=colors[i], label=label)
    # axes[0][1].errorbar(p, median, yerr=[lower, upper], fmt=fmt, color=colors[j], capsize=5)
    # axes[0][1].set_yscale('log')
    # axes[0][1].set_ylim(1e-4, 1e3)
    # axes[0][1].set_xlabel('log')
    # axes[0][1].legend(loc='upper center', bbox_to_anchor=(0.5, -0.1),
    #       fancybox=True)
  axes[0][1].set_xlabel('Particles')
  axes[0][1].set_ylabel('Elapsed Time in seconds')

  for i in range(1, n_variables):
    axes[i][1].set_visible(False)
  
  # accuracy
  for i, (filename, data) in enumerate(results.items()):
    p = []
    all_mses = {}
    for j, (p_, data_) in enumerate(data.items()):
      if particles is not None and int(p_) not in particles:
        continue
      p.append(float(p_))

      for k, (v, mses) in enumerate(data_.items()):
        if v == 'runtime':
          continue
        if v not in all_mses:
          all_mses[v] = {
            'lower': [],
            'median': [],
            'upper': [],
          }
        all_mses[v]['lower'].append(mses['lower'])
        all_mses[v]['median'].append(mses['median'])
        all_mses[v]['upper'].append(mses['upper'])

    for k, (v, mses) in enumerate(list(all_mses.items())[::-1]):
      fmt = markers[i]

      axes[k][0].plot(p, mses['median'], marker=markers[i], color=colors[i])
      # axes[k][0].errorbar(p, mses['median'], yerr=[mses['lower'], mses['upper']], fmt=fmt, color=colors[j], capsize=5)
      axes[k][0].set_yscale('log')
      # axes[k][0].set_ylim(1e-4, 1e3)
      # axes[k][0].set_xlabel('log')
      variable_name = v.split('_')[0]
      axes[k][0].set_title(variable_name + ' Accuracy')
      axes[k][0].set_ylabel('MSE')

  axes[n_variables-1][0].set_xlabel('Particles')


  fig.legend(loc='upper right', bbox_to_anchor=(0.95, 0.92), ncols=2)
  # ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05),
  #         fancybox=True, shadow=True, ncol=5)
  fig.tight_layout()
      

  if verbose:
    print('Saving plots')

  name = os.path.splitext(os.path.basename(filename))[0].split('_')[0]
  fig.savefig(os.path.join(output, '{}.png'.format(name)))
  fig.savefig(os.path.join(output, '{}.pdf'.format(name)))

  # print ratio of runtime
  print('runtime: {}'.format(
    results['serosurvey_default.muf']['60']['runtime']['median'] / \
    results['serosurvey_alt.muf']['60']['runtime']['median']))




if __name__ == '__main__':
  p = argparse.ArgumentParser()
  p.add_argument('--verbose', '-v', action='store_true', required=False)
  p.add_argument('--output', '-o', type=str, required=False, default='output')
  p.add_argument('--streaming', '-s', action='store_true', required=False, default=False)

  sp = p.add_subparsers(dest='subparser_name')

  rp = sp.add_parser('run')
  rp.add_argument('--files', '-f', type=str, required=True, nargs="+")
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

    for file in args.files:
      if not os.path.exists(file):
        raise Exception('File not found: {}'.format(file))

      results = {}
      if os.path.exists(os.path.join(args.output, 'results.json')):
        with open(os.path.join(args.output, 'results.json')) as f:
          try:
            results = json.load(f)
          except:
            results = {}

      try:
        results = run(file, args.output, args.particles, args.accuracy, args.n, args.streaming, results, args.verbose)
      except KeyboardInterrupt:
        pass

      # write results
      with open(os.path.join(args.output, 'results.json'), 'w') as f:
        json.dump(results, f, indent=2)
  

  
  