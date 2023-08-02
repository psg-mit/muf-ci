import argparse
import os
import json
import subprocess
import time
from math import log
import matplotlib.pyplot as plt
import glob
import numpy as np
import matplotlib

N_INTERVALS = 30

def error_func(config, x):
  if config['error_func'] == "se":
  # squared error
    return x ** 2
  # absolute error
  elif config['error_func'] == "ae":
    return abs(x)
  else:
    return

def plot(benchmark, output, files, particles, config, verbose=False):
  # Load results
  with open(os.path.join(benchmark, output, 'results.json')) as f:
    results = json.load(f)

  # Plot results

  # get n variables
  n_variables = 0
  for filename, data in results.items():
    for p, data_ in data.items():
      n_variables = max(n_variables, len(data_))

  n_variables -= 1 # runtime

  # colors = ["#ec8688", "#feb140", "#f3dd8d", "#a2b128", "#7fcad5", "#567bb7", "#ac88b3"]
  # red blue orange green purple indigo yellow brown magenta
  colors = ["#f4827b", "#7cc7d2", "#feb140", "#9baa20", "#9879a4", "#5999d3", "#f7dd73", "#865042", "#d146b6", "#303030"]
  # edgecolors = ["#b7575c","#c78200","#c0ab5f","#708200","#4d9aa4","#204f87","#204f87"]
  edgecolors = ["#b7575c","#4d9aa4", "#c0ab5f","#708200","#7c5b83","#204f87", "#c78200", "#56281b", "#9c0085", "#171616"]

  markers = ['s', 'v', 'd', 'o', 'X', 'p', 'h', 'P', '*', '<']

  markersize = 12

  gridparams = {'which': 'major', 'color': 'gray', 'linestyle': '--', 'alpha': 0.5}

  font_settings = {
    'font.size': 20, 
    # 'fontname': 'Calibri', 
    # 'font.weight': 'bold',
    # 'axes.labelweight': 'bold',
  }
  plt.rcParams.update(font_settings)

  # time to accuracy
  fig, axes = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_x'] * config['n_x'], config['base_y'] * config['n_y'] + 1))

  for ax in axes.flat:
    ax.set_visible(False)

  for i, (filename, data) in enumerate(results.items()):
    if files is not None and filename not in files:
      continue
    p = []
    all_errors = {}
    runtimes = []
    for j, (p_, data_) in enumerate(sorted(data.items(), key=lambda x: int(x[0]))):
      if particles is not None and int(p_) not in particles:
        continue
      p.append(float(p_))

      for k, (v, errors) in enumerate(data_.items()):
        if v == 'runtime':
          median_runtime = np.median(errors['all'])
          runtimes.append(median_runtime)
          continue

        transformed_errors = sorted(errors['all'])

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

      if config['n_y'] != 1:
        ax = axes[plot_j][plot_i]
      else:
        ax = axes[plot_i]

      if k == 0:
        # Only one set of file labels
        # label = os.path.splitext(os.path.basename(filename))[0]
        # label = ' '.join(label.split('_')[1:])
        label = config['labels'][i]
      else:
        label = None

      ax.set_visible(True)

      # Sort by runtime
      runtimes, mses['lower'], mses['median'], mses['upper'] = zip(*sorted(zip(runtimes, mses['lower'], mses['median'], mses['upper'])))

      ax.plot(runtimes, mses['upper'], marker=markers[i], color=colors[i], label=label, linestyle = 'None',
                                 markerfacecolor=colors[i], markeredgecolor=edgecolors[i], markersize=markersize)
      
      # min non-zero value
      nonzero = [x for x in mses['lower'] if x > 0]
      thresh = min(nonzero) if len(nonzero) > 0 else 1e-10

      ax.set_yscale('symlog', linthresh=thresh)
      ax.set_xscale('log')
      ax.grid(**gridparams)
      ax.minorticks_on()

      variable_name = v
      ax.set_title(f'Accuracy of {variable_name}', fontsize=20)
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
        if config['n_y'] != 1:
          axes[j][i].set_ylabel('Error (log scale)')
        else:
          axes[i].set_ylabel('Error (log scale)')

  for i in range(config['n_x']):
    if config['n_y'] != 1:
      axes[config['n_y'] - 1][i].set_xlabel(f'Execution Time in s (log scale)')
    else:
      axes[i].set_xlabel(f'Execution Time in s (log scale)')

  fig.legend(loc='upper center', ncols=config['legend_width'], bbox_to_anchor=(0.5, 0.0))
    
  if verbose:
    print('Saving plots')

  fig.tight_layout()

  name = os.path.splitext(os.path.basename(filename))[0].split('_')[0]
  fig.savefig(os.path.join(benchmark, output, '{}_runtime_accuracy_upper.png'.format(name)), bbox_inches='tight')

  fig.savefig(os.path.join(benchmark, output, '{}_runtime_accuracy_upper.pdf'.format(name)), bbox_inches='tight')

  plt.close(fig)

if __name__ == '__main__':
  p = argparse.ArgumentParser()
  p.add_argument('--benchmark', '-b', type=str, required=True)
  p.add_argument('--verbose', '-v', action='store_true', required=False)
  p.add_argument('--output', '-o', type=str, required=False, default='output_veriprop')
  p.add_argument('--files', '-f', type=str, required=False, nargs="+")

  p.add_argument('--particles', '-p', type=int, required=False, nargs='+')

  args = p.parse_args()

  with open(os.path.join(args.benchmark, 'config_veriprop.json')) as f:
    config = json.load(f)

  if args.files is None:
    args.files = glob.glob(os.path.join(args.benchmark, '*.muf'))
    args.files = [os.path.basename(f) for f in args.files]

  plot(args.benchmark, args.output, args.files, args.particles, config, args.verbose)
  exit()
