import argparse
import os
import json
import subprocess
import time
from math import log
import re
import matplotlib.pyplot as plt

BENCHMARK = 'sms_behavior'
base_y = 5
base_x = 4
n_y = 2
n_x = 2

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
  

  for p in particles:
    if verbose:
      print('Running with {} particles'.format(p))

    print(p)

    # Compile muf
    # This is a hack. Edit the file to change the number of particles
    with open(filename, 'r') as f:
      content = f.read()
    pattern = re.compile(r'infer\(\d+, \w+\)')
    if pattern.search(content) is None:
      raise Exception('Could not find infer call in muf file')
    content = pattern.sub('infer({}, {})'.format(p, BENCHMARK), content)
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
    runtimes = sorted(runtimes)

    # percentiles: 10, 50, 90
    mses_lower = {k: v[int(0.10 * len(v))] for k, v in mses_sorted.items()}
    mses_median = {k: v[int(0.50 * len(v))] for k, v in mses_sorted.items()}
    mses_upper = {k: v[int(0.90 * len(v))] for k, v in mses_sorted.items()}

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

def plot(output, streaming, files, particles, verbose=False):
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
  n_files = len(results)

  colors = ['#00bfbf', '#ff4d32', '#ffbf00', '#00b300', '#0000b3', '#b300b3', '#bfbfbf', '#4d4d4d', '#ff8080', '#ffff00', '#80ff80', '#8080ff']
  markers = ['s', 'v', 'd', 'o', 'x', 'p', 'h', 'D', 'H', '8', 'P', 'X']

  # runtime
  all_labels = []
  fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(base_y, base_x))
  for i, (filename, data) in enumerate(results.items()):
    if files is not None and filename not in files:
      continue

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
    all_labels.append(label)
    fmt = markers[i]

    ax.scatter(p, median, marker=markers[i], color=colors[i], label=label)
    # ax.set_xticks(p)

    # ax.errorbar(p, median, yerr=[lower, upper], fmt=fmt, color=colors[i], capsize=5, label=label)
    # ax.set_yscale('log')
    # ax.set_ylim(1e-4, 1e3)
    # ax.set_xlabel('log')
    # ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.1), fancybox=True)
  ax.set_xlabel('Particles')
  ax.set_ylabel('Elapsed Time in seconds')

  fig.suptitle('Elapsed Time')
  ax.legend(ncols=2, loc='upper center', bbox_to_anchor=(0.5, -0.2))
  fig.tight_layout()

  name = os.path.splitext(os.path.basename(filename))[0].split('_')[0]
  fig.savefig(os.path.join(output, '{}_runtime.pdf'.format(name)), bbox_inches='tight')
  fig.savefig(os.path.join(output, '{}_runtime.png'.format(name)), bbox_inches='tight')
  plt.close(fig)  


  # accuracy

  fig1, axes1 = plt.subplots(n_y, n_x, figsize=(base_y * n_y, base_x * n_x))
  fig2, axes2 = plt.subplots(n_y, n_x, figsize=(base_y * n_y, base_x * n_x))
  fig3, axes3 = plt.subplots(n_y, n_x, figsize=(base_y * n_y, base_x * n_x))
  fig4, axes4 = plt.subplots(n_y, n_x, figsize=(base_y * n_y, base_x * n_x))
  fig5, axes5 = plt.subplots(n_y, n_x, figsize=(base_y * n_y, base_x * n_x))

  for i, (filename, data) in enumerate(results.items()):
    if files is not None and filename not in files:
      continue
    p = []
    all_mses = {}
    for j, (p_, data_) in enumerate(sorted(data.items(), key=lambda x: int(x[0]))):
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
            'range': [],
          }
        all_mses[v]['lower'].append(mses['lower'])
        all_mses[v]['median'].append(mses['median'])
        all_mses[v]['upper'].append(mses['upper'])
        all_mses[v]['range'].append(mses['upper'] - mses['lower'])

    for k, (v, mses) in enumerate(list(all_mses.items())):
      plot_i = k % n_x
      plot_j = k // n_x
      fmt = markers[i]

      if k == 0:
        # Only one set of file labels
        label = os.path.splitext(os.path.basename(filename))[0]
        label = ' '.join(label.split('_')[1:])
      else:
        label = None

      axes1[plot_j][plot_i].plot(p, mses['median'], marker=markers[i], color=colors[i], label=label)
      axes2[plot_j][plot_i].plot(p, mses['lower'], marker=markers[i], color=colors[i], label=label)
      axes3[plot_j][plot_i].plot(p, mses['upper'], marker=markers[i], color=colors[i], label=label)
      axes4[plot_j][plot_i].errorbar(p, mses['median'], yerr=[mses['lower'], mses['upper']], fmt=fmt, color=colors[i], capsize=5, label=label)
      axes5[plot_j][plot_i].plot(p, mses['range'], marker=markers[i], color=colors[i], label=label)

      # axes1[plot_j][plot_i].set_xticks(p)
      # axes2[plot_j][plot_i].set_xticks(p)
      # axes3[plot_j][plot_i].set_xticks(p)
      # axes4[plot_j][plot_i].set_xticks(p)
      # axes5[plot_j][plot_i].set_xticks(p)

      # axes1[plot_j][plot_i].set_yscale('log')
      # axes1[k][0].set_ylim(1e-4, 1e3)
      # axes1[k][0].set_xlabel('log')
      variable_name = '_'.join(v.split('_')[:-1])
      axes1[plot_j][plot_i].set_title(variable_name)
      axes2[plot_j][plot_i].set_title(variable_name)
      axes3[plot_j][plot_i].set_title(variable_name)
      axes4[plot_j][plot_i].set_title(variable_name)
      axes5[plot_j][plot_i].set_title(variable_name)
      # axes1[plot_j][plot_i].set_ylabel('MSE')

  for j in range(n_y):
    axes1[j][0].set_ylabel('MSE')
    axes2[j][0].set_ylabel('MSE')
    axes3[j][0].set_ylabel('MSE')
    axes4[j][0].set_ylabel('MSE')
    axes5[j][0].set_ylabel('MSE')

  for i in range(n_x):
    axes1[n_y - 1][i].set_xlabel('Particles')
    axes2[n_y - 1][i].set_xlabel('Particles')
    axes3[n_y - 1][i].set_xlabel('Particles')
    axes4[n_y - 1][i].set_xlabel('Particles')
    axes5[n_y - 1][i].set_xlabel('Particles')

  # axes1[n_y - 1, n_x - 1].axis('off')
  # axes2[n_y - 1, n_x - 1].axis('off')
  # axes3[n_y - 1, n_x - 1].axis('off')
  # axes4[n_y - 1, n_x - 1].axis('off')
  # axes5[n_y - 1, n_x - 1].axis('off')

  fig1.legend()
  fig2.legend()
  fig3.legend()
  fig4.legend()
  fig5.legend()
  
  if verbose:
    print('Saving plots')

  fig1.suptitle('Accuracy - Median')
  fig2.suptitle('Accuracy - 10 Percentile')
  fig3.suptitle('Accuracy - 90 Percentile')
  fig4.suptitle('Accuracy')
  fig5.suptitle('Accuracy - Range')
  fig1.tight_layout()
  fig2.tight_layout()
  fig3.tight_layout()
  fig4.tight_layout()
  fig5.tight_layout()
  name = os.path.splitext(os.path.basename(filename))[0].split('_')[0]
  fig1.savefig(os.path.join(output, '{}_accuracy_median.png'.format(name)), bbox_inches='tight')
  fig2.savefig(os.path.join(output, '{}_accuracy_lower.png'.format(name)), bbox_inches='tight')
  fig3.savefig(os.path.join(output, '{}_accuracy_upper.png'.format(name)), bbox_inches='tight')
  fig4.savefig(os.path.join(output, '{}_accuracy.png'.format(name)), bbox_inches='tight')
  fig5.savefig(os.path.join(output, '{}_accuracy_range.png'.format(name)), bbox_inches='tight')

  fig1.savefig(os.path.join(output, '{}_accuracy_median.pdf'.format(name)), bbox_inches='tight')
  fig2.savefig(os.path.join(output, '{}_accuracy_lower.pdf'.format(name)), bbox_inches='tight')
  fig3.savefig(os.path.join(output, '{}_accuracy_upper.pdf'.format(name)), bbox_inches='tight')
  fig4.savefig(os.path.join(output, '{}_accuracy.pdf'.format(name)), bbox_inches='tight')
  fig5.savefig(os.path.join(output, '{}_accuracy_range.pdf'.format(name)), bbox_inches='tight')

  plt.close(fig1)
  plt.close(fig2)
  plt.close(fig3)
  plt.close(fig4)
  plt.close(fig5)

  # save legend as separate figure
  # figlegend = plt.figure(figsize=(8, 1))
  # ax = figlegend.add_subplot(111)
  # ax.axis('off')
  # ax.legend(*ax.get_legend_handles_labels(), loc='center', ncol=5)
  # figlegend.savefig(os.path.join(output, 'legend.png'), bbox_inches='tight')
  # figlegend.savefig(os.path.join(output, 'legend.pdf'), bbox_inches='tight')


  # print ratio of runtime
  # print('runtime: {}'.format(
  #   results['serosurvey_default.muf']['60']['runtime']['median'] / \
  #   results['serosurvey_alt.muf']['60']['runtime']['median']))




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
  pp.add_argument('--files', '-f', type=str, required=False, nargs="+")
  pp.add_argument('--particles', '-p', type=int, required=False, nargs='+')

  args = p.parse_args()

  # If plotting, just plot and exit. Assumes output is in args.output
  if args.subparser_name == 'plot':
    plot(args.output, args.streaming, args.files, args.particles, args.verbose)
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
  

  
  