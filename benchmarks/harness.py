import argparse
import os
import json
import subprocess
import time
from math import log
import matplotlib.pyplot as plt
import glob

def error_func(x):
  # squared error
  # return x ** 2
  # absolute error
  return x

def run_muf(benchmark, filename, output, p, n, config, verbose=False):
  # run muf file
  cmd = './{}_main.exe'.format(os.path.splitext(os.path.basename(filename))[0])
  
  if verbose:
    print('>', cmd)

  t1 = time.time()
  wd = os.path.join(os.getcwd(), benchmark)
  out = subprocess.check_output(cmd, cwd=wd, shell=True).decode("utf-8") 
  t2 = time.time()

  if verbose:
    print('> Took {} seconds'.format(t2 - t1))

    # with open(os.path.join(output, 'muf.log'), 'a') as f:
    #   f.write(' '.join([str(x) for x in [p, n, t2 - t1]]) + '\n' + out + '\n')
  
  # parse output
  lines = out.strip().split('\n')
  line = lines[-1]

  # parse line into dict
  program_output = {}
  # [2.738712, 0.295334, 1.406225, -1.462784, 1.211603, 0.803564, -0.674772, -1.833021, 0.007779, 0.496084, 1.222381, 0.749403, 0.745628]
  line = line.strip()[1:-1]
  for k, v in enumerate(line.split(', ')):
    program_output[config["true_vars"][k][0]] = abs(float(v) - config["true_vars"][k][1])
  
  return program_output, (t2 - t1)

def close_to_target(target_accuracy, accuracy):
  return log(accuracy) - log(target_accuracy) < 0.5

# Run experiments
def run(benchmark, filename, output, particles, accuracy, n, results, config, verbose=False):
  if len(particles) == 0:
    particles = [x for x in range(1, 5001)]
  
  for p in particles:
    if verbose:
      print('Running with {} particles'.format(p))

    # Compile muf
    cmd = 'mufc --particles {} {}'.format(p, filename)
    if verbose:
      print('>', cmd)

    wd = os.path.join(os.getcwd(), benchmark)
    if subprocess.call(cmd, cwd=wd, shell=True, stdout=subprocess.DEVNULL) != 0:
      raise Exception('Failed to compile muf file')

    mses = {}
    runtimes = []
    for i in range(n):
      if verbose:
        print('{} particles - Run {}'.format(p, i))

      program_output, t = run_muf(benchmark, filename, output, p, n, config, verbose)
      for k, v in program_output.items():
        if k not in mses:
          mses[k] = []
        mses[k].append(v)
      
      runtimes.append(t)

    # quantiles of mse for runs
    mses_sorted = {k: sorted(v) for k, v in mses.items()}
    runtimes = sorted(runtimes)

    # if verbose:
    #   with open(os.path.join(output, 'muf.log'), 'a') as f:
        # for k, v in mses_lower.items():
        #   f.write('{}: {} '.format(k, v))
        # f.write('p_mse: ' + ' '.join(map(str, p_mse_sorted)) + '\n')
        # f.write('sens_mse: ' + ' '.join(map(str, sens_mse_sorted)) + '\n')
        # f.write('spec_mse: ' + ' '.join(map(str, spec_mse_sorted)) + '\n')
        # f.write('runtimes: ' + ' '.join(map(str, runtimes)) + '\n')

    # save results
    if filename not in results:
      results[filename] = {}
    if p not in results[filename]:
      results[filename][p] = {}

    variables = mses.keys()
    for v in variables:
      if v not in results[filename][p]:
        results[filename][p][v] = {}
      results[filename][p][v]['all'] = mses_sorted[v]

    if 'runtime' not in results[filename][p]:
      results[filename][p]['runtime'] = {}
    results[filename][p]['runtime']['all'] = runtimes

  return results

def plot(benchmark, output, files, particles, config, verbose=False):
  # Load results
  with open(os.path.join(benchmark, output, 'results.json')) as f:
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

  # colors = ["#ec8688", "#feb140", "#f3dd8d", "#a2b128", "#7fcad5", "#567bb7", "#ac88b3"]
  colors = ['#f4827b', '#feb140', '#9baa20', '#7cc7d2', '#9879a4', '#f7dd73', '#5999d3']
  markers = ['s', 'v', 'd', 'o', 'x', 'p', 'h', 'D', 'H', '8', 'P', 'X']

  # runtime
  all_labels = []
  fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(5, 4))
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

      runtimes = sorted(map(error_func, data_[measurement_label]['all']))

      lower.append(runtimes[int(0.10 * len(runtimes))])
      median.append(runtimes[int(0.50 * len(runtimes))])
      upper.append(runtimes[int(0.90 * len(runtimes))])

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
  ax.legend(ncols=3, loc='upper center', bbox_to_anchor=(0.5, -0.2))
  fig.tight_layout()

  name = os.path.splitext(os.path.basename(filename))[0].split('_')[0]
  fig.savefig(os.path.join(benchmark, output, '{}_runtime.pdf'.format(name)), bbox_inches='tight')
  fig.savefig(os.path.join(benchmark, output, '{}_runtime.png'.format(name)), bbox_inches='tight')
  plt.close(fig)  


  # accuracy

  fig1, axes1 = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_y'] * config['n_y'], config['base_x'] * config['n_x']))
  fig2, axes2 = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_y'] * config['n_y'], config['base_x'] * config['n_x']))
  fig3, axes3 = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_y'] * config['n_y'], config['base_x'] * config['n_x']))
  fig4, axes4 = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_y'] * config['n_y'], config['base_x'] * config['n_x']))

  for ax in [axes1, axes2, axes3, axes4]:
    for a in ax.flatten():
      a.set_visible(False)

  for i, (filename, data) in enumerate(results.items()):
    if files is not None and filename not in files:
      continue
    p = []
    all_errors = {}
    for j, (p_, data_) in enumerate(sorted(data.items(), key=lambda x: int(x[0]))):
      if particles is not None and int(p_) not in particles:
        continue
      p.append(float(p_))

      for k, (v, errors) in enumerate(data_.items()):
        if v == 'runtime':
          continue

        transformed_errors = sorted(map(error_func, errors['all']))

        if v not in all_errors:
          all_errors[v] = {
            'lower': [],
            'median': [],
            'upper': [],
          }
        all_errors[v]['lower'].append(transformed_errors[int(0.10 * len(transformed_errors))])
        all_errors[v]['median'].append(transformed_errors[int(0.50 * len(transformed_errors))])
        all_errors[v]['upper'].append(transformed_errors[int(0.90 * len(transformed_errors))])

    for k, (v, mses) in enumerate(list(all_errors.items())):
      plot_i = k % config['n_x']
      plot_j = k // config['n_x']
      fmt = markers[i]

      if k == 0:
        # Only one set of file labels
        label = os.path.splitext(os.path.basename(filename))[0]
        label = ' '.join(label.split('_')[1:])
      else:
        label = None

      for ax in [axes1, axes2, axes3, axes4]:
        ax[plot_j][plot_i].set_visible(True)


      axes1[plot_j][plot_i].plot(p, mses['median'], marker=markers[i], color=colors[i], label=label)
      axes2[plot_j][plot_i].plot(p, mses['lower'], marker=markers[i], color=colors[i], label=label)
      axes3[plot_j][plot_i].plot(p, mses['upper'], marker=markers[i], color=colors[i], label=label)
      axes4[plot_j][plot_i].errorbar(p, mses['median'], yerr=[mses['lower'], mses['upper']], fmt=fmt, color=colors[i], capsize=5, label=label)

      # axes1[plot_j][plot_i].set_xticks(p)
      # axes2[plot_j][plot_i].set_xticks(p)
      # axes3[plot_j][plot_i].set_xticks(p)
      # axes4[plot_j][plot_i].set_xticks(p)

      axes4[plot_j][plot_i].set_yscale('log')
      # axes1[k][0].set_ylim(1e-4, 1e3)
      # axes1[k][0].set_xlabel('log')
      # variable_name = '_'.join(v.split('_')[:-1])
      variable_name = v
      for ax in [axes1, axes2, axes3, axes4]:
        ax[plot_j][plot_i].set_title(variable_name)
      # axes1[plot_j][plot_i].set_ylabel('MSE')

  for j in range(config['n_y']):
    for i in range(config['n_x']):
      # if i == 0 and j == 0:
      #   axes1[j][i].set_ylabel('MSE')
      #   axes2[j][i].set_ylabel('MSE')
      #   axes3[j][i].set_ylabel('MSE')
      #   axes4[j][i].set_ylabel('MSE')
      # else:
      if i == 0:
        for ax in [axes1, axes2, axes3, axes4]:
          ax[j][i].set_ylabel('Error')

  for i in range(config['n_x']):
    for ax in [axes1, axes2, axes3, axes4]:
      ax[config['n_y'] - 1][i].set_xlabel(f'Particles')

  for ax in [axes1, axes2, axes3, axes4]:
    ax[config['n_y'] - 1, config['n_x'] - 1].axis('off')

  for fig in [fig1, fig2, fig3, fig4]:
    fig.legend(loc='lower right', ncols=2, bbox_to_anchor=(0.96, 0.125), frameon=False)
    
  if verbose:
    print('Saving plots')

  fig1.suptitle('Accuracy - Median')
  fig2.suptitle('Accuracy - 10 Percentile')
  fig3.suptitle('Accuracy - 90 Percentile')
  fig4.suptitle('Accuracy')

  for fig in [fig1, fig2, fig3, fig4]:
    fig.tight_layout()

  name = os.path.splitext(os.path.basename(filename))[0].split('_')[0]
  fig1.savefig(os.path.join(benchmark, output, '{}_accuracy_median.png'.format(name)), bbox_inches='tight')
  fig2.savefig(os.path.join(benchmark, output, '{}_accuracy_lower.png'.format(name)), bbox_inches='tight')
  fig3.savefig(os.path.join(benchmark, output, '{}_accuracy_upper.png'.format(name)), bbox_inches='tight')
  fig4.savefig(os.path.join(benchmark, output, '{}_accuracy.png'.format(name)), bbox_inches='tight')

  fig1.savefig(os.path.join(benchmark, output, '{}_accuracy_median.pdf'.format(name)), bbox_inches='tight')
  fig2.savefig(os.path.join(benchmark, output, '{}_accuracy_lower.pdf'.format(name)), bbox_inches='tight')
  fig3.savefig(os.path.join(benchmark, output, '{}_accuracy_upper.pdf'.format(name)), bbox_inches='tight')
  fig4.savefig(os.path.join(benchmark, output, '{}_accuracy.pdf'.format(name)), bbox_inches='tight')

  for fig in [fig1, fig2, fig3, fig4]:
    plt.close(fig)

  # save legend as separate figure
  # figlegend = plt.figure(figsize=(8, 1))
  # ax = figlegend.add_subplot(111)
  # ax.axis('off')
  # ax.legend(*ax.get_legend_handles_labels(), loc='center', ncol=5)
  # figlegend.savefig(os.path.join(output, 'legend.png'), bbox_inches='tight')
  # figlegend.savefig(os.path.join(output, 'legend.pdf'), bbox_inches='tight')


  # print ratio of runtime
  print('runtime: {}'.format(
    results['serosurvey_default.muf']['60']['runtime']['median'] / \
    results['serosurvey_sens_fpr_exact.muf']['60']['runtime']['median']))




if __name__ == '__main__':
  p = argparse.ArgumentParser()
  p.add_argument('--benchmark', '-b', type=str, required=True)
  p.add_argument('--verbose', '-v', action='store_true', required=False)
  p.add_argument('--output', '-o', type=str, required=False, default='output')
  p.add_argument('--files', '-f', type=str, required=False, nargs="+")

  sp = p.add_subparsers(dest='subparser_name')

  rp = sp.add_parser('run')
  rp.add_argument('--particles', '-p', type=int, required=False, nargs='+')
  rp.add_argument('--accuracy', '-a', type=float, required=False, default=0.01)
  rp.add_argument('--n', '-n', type=int, required=False, default=1000)
  
  pp = sp.add_parser('plot')
  pp.add_argument('--particles', '-p', type=int, required=False, nargs='+')

  args = p.parse_args()

  with open(os.path.join(args.benchmark, 'config.json')) as f:
    config = json.load(f)

  # If no files specified, use all files in benchmark directory
  if args.files is None:
    args.files = glob.glob(os.path.join(args.benchmark, '*.muf'))
    args.files = [os.path.basename(f) for f in args.files]


  # If plotting, just plot and exit. Assumes output is in args.output
  if args.subparser_name == 'plot':
    plot(args.benchmark, args.output, args.files, args.particles, config, args.verbose)
    exit()

  elif args.subparser_name == 'run':
    # make output directory
    os.makedirs(os.path.join(args.benchmark, args.output), exist_ok=True)

    for file in args.files:
      if not os.path.exists(os.path.join(args.benchmark, file)):
        raise Exception('File not found: {}'.format(file))

      results = {}
      if os.path.exists(os.path.join(args.benchmark, args.output, 'results.json')):
        with open(os.path.join(args.benchmark, args.output, 'results.json')) as f:
          try:
            results = json.load(f)
          except:
            results = {}

      try:
        results = run(args.benchmark, file, args.output, args.particles, args.accuracy, args.n, results, config, args.verbose)
      except KeyboardInterrupt:
        pass

      # write results
      with open(os.path.join(args.benchmark, args.output, 'results.json'), 'w') as f:
        json.dump(results, f, indent=2)
  

  
  