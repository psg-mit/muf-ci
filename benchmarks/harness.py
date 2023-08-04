import argparse
import os
import json
import subprocess
import time
from math import log
import matplotlib.pyplot as plt
import glob
import itertools
import numpy as np
import matplotlib

DEFAULT_BENCHMARKS = [
  'gaussianmixture',
  'noise',
  'smsbehavior',
  'outlier',
  'mixasymprior',
  "outliernolist",
]

BENCHMARK_LATEX_NAMES = {
  'gaussianmixture': r"\bGaussianmix{}",
  'mixasymprior': r"\bAsymprior{}",
  'noise': r"\bNoise{}",
  'outlier': r"\bOutlier{}",
  'smsbehavior': r"\bSmsbehavior{}",
  "outliernolist": r"\bOutliernolist{}",
  "noisenolist": r"\bNoisenolist{}"
}


TABLE_STR = r"""
\begin{tabular}[h]{lrrrrrrrrr}
  \toprule
  Benchmark & \# Plans & \# Sat & TP & TN & FP & FN & Precision & Recall & Accuracy \\
  \midrule
CONTENT
  \bottomrule
\end{tabular}
"""

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

# Runs benchmark executable and computes the absolute error of each variable
def run_siren(benchmark, filename, config, verbose=False):
  # run siren file
  cmd = './{}.exe'.format(os.path.splitext(os.path.basename(filename))[0])
  
  if verbose:
    print('>', cmd)

  t1 = time.time()
  wd = os.path.join(os.getcwd(), benchmark)
  out = subprocess.check_output(cmd, cwd=wd, shell=True).decode("utf-8") 
  t2 = time.time()

  if verbose:
    print('> Took {} seconds'.format(t2 - t1))

    # with open(os.path.join(output, 'siren.log'), 'a') as f:
    #   f.write(' '.join([str(x) for x in [p, n, t2 - t1]]) + '\n' + out + '\n')
  
  # parse output
  lines = out.strip().split('\n')

  # get outputs between ==== OUTPUT ==== and ==== APPROXIMATION STATUS ====
  output_lines = []
  start = False
  for line in lines:
    if '==== OUTPUT ====' in line:
      start = True
    elif '====' in line:
      break
    elif start:
      output_lines.append(line)

  # print(output_lines)

  # parse line into dict
  program_output = {}

  for i, (k, true_vs) in enumerate(config['true_vars']):
    line = output_lines[i].strip()
    values = line.split(' ')
    error = [error_func(config, float(v) - true_vs[i]) for i, v in enumerate(values)]
    error = sum(error) / len(error)
    program_output[k] = error

  return program_output, (t2 - t1)

def close_to_target(target_accuracy, accuracy):
  return log(accuracy) - log(target_accuracy) < 0.5

# Run experiments
def run(benchmark, filename, output, particles, accuracy, n, results, config, verbose=False):  
  for p in particles:
    if verbose:
      print('Running with {} particles'.format(p))

    # Compile siren
    # sirenc -- --particles 1 --output output test.siren
    cmd = 'dune exec siren -- --particles {} --output output {}'.format(p, filename)
    if verbose:
      print('>', cmd)

    wd = os.path.join(os.getcwd(), benchmark)
    if subprocess.call(cmd, cwd=wd, shell=True, stdout=subprocess.DEVNULL) != 0:
      raise Exception('Failed to compile Siren file')

    mses = {}
    runtimes = []
    for i in range(n):
      if verbose:
        print('{} particles - Run {}'.format(p, i))

      program_output, t = run_siren(benchmark, filename, config, verbose)
      for k, v in program_output.items():
        if k not in mses:
          mses[k] = []
        mses[k].append(v)
      
      runtimes.append(t)

    # save results
    if filename not in results:
      results[filename] = {}
    if p not in results[filename]:
      results[filename][p] = {}

    variables = mses.keys()
    for v in variables:
      results[filename][p][v] = mses[v]

    results[filename][p]['runtime'] = runtimes

  return results

def plot(benchmark, output, files, particles, config, verbose=False):
  # Load results
  with open(os.path.join(benchmark, output, 'results.json')) as f:
    results = json.load(f)

  # Plot results

  # subplot for each variable

  # get n variables
  n_variables = 0
  for filename, data in results.items():
    for p, data_ in data.items():
      n_variables = max(n_variables, len(data_))

  n_variables -= 1 # runtime
  n_files = len(results)

  # colors = ["#ec8688", "#feb140", "#f3dd8d", "#a2b128", "#7fcad5", "#567bb7", "#ac88b3"]
  # red orange green blue purple indigo yellow brown magenta
  colors = ["#f4827b", "#feb140", "#9baa20", "#7cc7d2", "#9879a4", "#5999d3", "#f7dd73", "#865042", "#d146b6", "#303030"]
  # edgecolors = ["#b7575c","#c78200","#c0ab5f","#708200","#4d9aa4","#204f87","#204f87"]
  edgecolors = ["#b7575c",
  "#c0ab5f","#708200","#4d9aa4","#7c5b83","#204f87", "#c78200", "#56281b", "#9c0085", "#171616"]

  markers = ['s', 'v', 'd', 'o', 'X', 'p', 'h', 'P', '*', '<']

  markersize = 12

  gridparams = {'which': 'major', 'color': 'gray', 'linestyle': '--', 'alpha': 0.5}

  font_settings = {
    'font.size': 24, 
    'font.family': 'serif', 
    'font.serif': 'Times New Roman', 
    # 'font.weight': 'bold',
    # 'axes.labelweight': 'bold',
  }
  plt.rcParams.update(font_settings)


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

      runtimes = sorted(data_[measurement_label])

      lower.append(runtimes[int(0.10 * len(runtimes))])
      median.append(runtimes[int(0.50 * len(runtimes))])
      upper.append(runtimes[int(0.90 * len(runtimes))])

    # Only one set of file labels
    # label = os.path.splitext(os.path.basename(filename))[0]
    # label = ' '.join(label.split('_')[1:])
    label = config['labels'][i]
    all_labels.append(label)
    fmt = markers[i]
    

    ax.scatter(p, median, marker=markers[i], color=colors[i], label=label, 
               edgecolors=edgecolors[i], s=50)
        
    # ax.set_xticks(p)

    # ax.errorbar(p, median, yerr=[lower, upper], fmt=fmt, color=colors[i], capsize=5, label=label)
    # ax.set_yscale('log')
    # ax.set_ylim(1e-4, 1e3)
    # ax.set_xlabel('log')
    # ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.1), fancybox=True)

  ax.set_yscale('log')
  ax.set_xscale('log')
  ax.grid(**gridparams)
  ax.minorticks_on()
  ax.set_xlabel('Number of Particles (log scale)')
  ax.set_ylabel('Time in s (log scale)')

  ax.set_title('Execution Time')
  # ax.legend(ncols=config['legend_width'], loc='upper center', bbox_to_anchor=(0.5, -0.2))
  fig.tight_layout()

  name = os.path.splitext(os.path.basename(filename))[0].split('_')[0]

  # save legend as separate figure
  figlegend = plt.figure(figsize=(1, 1))
  axf = figlegend.add_subplot(111)
  axf.axis('off')
  axf.legend(*ax.get_legend_handles_labels(), loc='center', ncol=config['legend_width'], markerscale=2)
  figlegend.tight_layout()
  # figlegend.savefig(os.path.join(benchmark, output, f'{name}_legend.png'), bbox_inches='tight')
  # figlegend.savefig(os.path.join(benchmark, output, f'{name}_legend.pdf'), bbox_inches='tight')

  fig.savefig(os.path.join(benchmark, output, '{}_runtime.pdf'.format(name)), bbox_inches='tight')
  fig.savefig(os.path.join(benchmark, output, '{}_runtime.png'.format(name)), bbox_inches='tight')
  plt.close(fig)  


  # accuracy

  fig1, axes1 = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_x'] * config['n_x'], config['base_y'] * config['n_y']))
  fig2, axes2 = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_x'] * config['n_x'], config['base_y'] * config['n_y']))
  fig3, axes3 = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_x'] * config['n_x'], config['base_y'] * config['n_y']))
  fig4, axes4 = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_x'] * config['n_x'], config['base_y'] * config['n_y']))

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

        transformed_errors = sorted(errors)

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

      if config['n_y'] != 1:
        ax1 = axes1[plot_j][plot_i]
        ax2 = axes2[plot_j][plot_i]
        ax3 = axes3[plot_j][plot_i]
        ax4 = axes4[plot_j][plot_i]
      else:
        ax1 = axes1[plot_i]
        ax2 = axes2[plot_i]
        ax3 = axes3[plot_i]
        ax4 = axes4[plot_i]

      fmt = markers[i]

      if k == 0:
        # Only one set of file labels
        label = config['labels'][i]
        # label = os.path.splitext(os.path.basename(filename))[0]
        # label = ' '.join(label.split('_')[1:])
      else:
        label = None

      for ax in [ax1, ax2, ax3, ax4]:
        ax.set_visible(True)

      ax1.plot(p, mses['median'], marker=markers[i], color=colors[i], label=label, 
                                 markerfacecolor=colors[i], markeredgecolor=edgecolors[i], markersize=markersize)
      ax2.plot(p, mses['lower'], marker=markers[i], color=colors[i], label=label, 
                                 markerfacecolor=colors[i], markeredgecolor=edgecolors[i], markersize=markersize)
      ax3.plot(p, mses['upper'], marker=markers[i], color=colors[i], label=label, 
                                 markerfacecolor=colors[i], markeredgecolor=edgecolors[i], markersize=markersize)
      
      median = mses['median']
      lower_err = [abs(median[i] - mses['lower'][i]) for i in range(len(median))]
      upper_err = [abs(mses['upper'][i] - median[i]) for i in range(len(median))]
      ax4.errorbar(p, median, yerr=[lower_err, upper_err], fmt=fmt, color=colors[i], label=label, 
                                 markerfacecolor=colors[i], markeredgecolor=edgecolors[i], capsize=5, markersize=markersize)

      # axes1[plot_j][plot_i].set_xticks(p)
      # axes2[plot_j][plot_i].set_xticks(p)
      # axes3[plot_j][plot_i].set_xticks(p)
      # axes4[plot_j][plot_i].set_xticks(p)

      # min non-zero value
      nonzero = [x for x in mses['lower'] if x > 0]
      thresh = min(nonzero) if len(nonzero) > 0 else 1e-10

      for ax in [ax1, ax2, ax3, ax4]:
        ax.set_yscale('symlog', linthresh=thresh)
        # ax.tick_params(numticks=nbins)
        # ax.locator_params(axis='y', numticks=nbins)
        ax.set_xscale('log')
        ax.grid(**gridparams)
        ax.minorticks_on()

        # ax[plot_j][plot_i].set_yscale('log', nonpositive='clip')


      # axes1[k][0].set_ylim(1e-4, 1e3)
      # axes1[k][0].set_xlabel('log')
      # variable_name = '_'.join(v.split('_')[:-1])
      variable_name = v
      for ax in [ax1, ax2, ax3, ax4]:
        ax.set_title(variable_name)
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
          if config['n_y'] != 1:
            ax[j][i].set_ylabel('Error (log scale)')
          else:
            ax[i].set_ylabel('Error (log scale)')

  for i in range(config['n_x']):
    for ax in [axes1, axes2, axes3, axes4]:
      if config['n_y'] != 1:
        ax[config['n_y'] - 1][i].set_xlabel(f'Number of Particles (log scale)')
      else:
        ax[i].set_xlabel(f'Number of Particles (log scale)')

  # for ax in [axes1, axes2, axes3, axes4]:
  #   if config['n_y'] != 1:
  #     ax[config['n_y'] - 1, config['n_x'] - 1].axis('off')
  #   else:


  # for fig in [fig1, fig2, fig3, fig4]:
    # fig.legend(loc='lower right', ncols=config['legend_width'], bbox_to_anchor=(0.96, 0.125), frameon=False)
    # fig.legend(loc='upper center', ncols=config['legend_width'], frameon=False)
    
  if verbose:
    print('Saving plots')

  # fig1.suptitle('Accuracy - Median')
  # fig2.suptitle('Accuracy - 10 Percentile')
  # fig3.suptitle('Accuracy - 90 Percentile')
  # fig4.suptitle('Accuracy')

  for fig in [fig1, fig2, fig3, fig4]:
    fig.tight_layout()

  name = os.path.splitext(os.path.basename(filename))[0].split('_')[0]
  # fig1.savefig(os.path.join(benchmark, output, '{}_accuracy_median.png'.format(name)), bbox_inches='tight')
  # fig2.savefig(os.path.join(benchmark, output, '{}_accuracy_lower.png'.format(name)), bbox_inches='tight')
  fig3.savefig(os.path.join(benchmark, output, '{}_accuracy_upper.png'.format(name)), bbox_inches='tight')
  fig4.savefig(os.path.join(benchmark, output, '{}_accuracy.png'.format(name)), bbox_inches='tight')

  # fig1.savefig(os.path.join(benchmark, output, '{}_accuracy_median.pdf'.format(name)), bbox_inches='tight')
  # fig2.savefig(os.path.join(benchmark, output, '{}_accuracy_lower.pdf'.format(name)), bbox_inches='tight')
  fig3.savefig(os.path.join(benchmark, output, '{}_accuracy_upper.pdf'.format(name)), bbox_inches='tight')
  fig4.savefig(os.path.join(benchmark, output, '{}_accuracy.pdf'.format(name)), bbox_inches='tight')

  for fig in [fig1, fig2, fig3, fig4]:
    plt.close(fig)

  # time to accuracy
  fig1, axes1 = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_x'] * config['n_x'], config['base_y'] * config['n_y'] + 1))
  fig2, axes2 = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_x'] * config['n_x'], config['base_y'] * config['n_y'] + 1))
  fig3, axes3 = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_x'] * config['n_x'], config['base_y'] * config['n_y'] + 1))
  fig4, axes4 = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_x'] * config['n_x'], config['base_y'] * config['n_y'] + 1))
  fig5, axes5 = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_x'] * config['n_x'], config['base_y'] * config['n_y'] + 1))
  fig6, axes6 = plt.subplots(config['n_y'], config['n_x'], figsize=(config['base_x'] * config['n_x'], config['base_y'] * config['n_y'] + 1))

  for ax in [axes1, axes2, axes3, axes4, axes5]:
    for a in ax.flatten():
      a.set_visible(False)

  for i, (filename, data) in enumerate(results.items()):
    if files is not None and filename not in files:
      continue
    p = []
    all_errors = {}
    all_runtimes = {
      'lower': [],
      'median': [],
      'upper': [],
      'all': [],
    }
    for j, (p_, data_) in enumerate(sorted(data.items(), key=lambda x: int(x[0]))):
      if particles is not None and int(p_) not in particles:
        continue
      p.append(float(p_))

      for k, (v, errors) in enumerate(data_.items()):
        if v == 'runtime':
        
          all_runtimes['lower'].append(errors[int(0.10 * len(errors))])
          all_runtimes['median'].append(errors[int(0.50 * len(errors))])
          all_runtimes['upper'].append(errors[int(0.90 * len(errors))])
          all_runtimes['all'] += errors
          continue

        transformed_errors = sorted(errors)

        if v not in all_errors:
          all_errors[v] = {
            'lower': [],
            'median': [],
            'upper': [],
            'all': [],
          }
        all_errors[v]['lower'].append(transformed_errors[int(0.10 * len(transformed_errors))])
        all_errors[v]['median'].append(transformed_errors[int(0.50 * len(transformed_errors))])
        all_errors[v]['upper'].append(transformed_errors[int(0.90 * len(transformed_errors))])
        all_errors[v]['all'] += errors

    for k, (v, mses) in enumerate(list(all_errors.items())):
      plot_i = k % config['n_x']
      plot_j = k // config['n_x']
      fmt = markers[i]

      if config['n_y'] != 1:
        ax1 = axes1[plot_j][plot_i]
        ax2 = axes2[plot_j][plot_i]
        ax3 = axes3[plot_j][plot_i]
        ax4 = axes4[plot_j][plot_i]
        ax5 = axes5[plot_j][plot_i]
        ax6 = axes6[plot_j][plot_i]
      else:
        ax1 = axes1[plot_i]
        ax2 = axes2[plot_i]
        ax3 = axes3[plot_i]
        ax4 = axes4[plot_i]
        ax5 = axes5[plot_i]
        ax6 = axes6[plot_i]

      if k == 0:
        # Only one set of file labels
        # label = os.path.splitext(os.path.basename(filename))[0]
        # label = ' '.join(label.split('_')[1:])
        label = config['labels'][i]
      else:
        label = None

      for ax in [ax1, ax2, ax3, ax4, ax5]:
        ax.set_visible(True)

      # Sort by runtime
      # runtimes, mses['lower'], mses['median'], mses['upper'] = zip(*sorted(zip(all_runtimes, mses['lower'], mses['median'], mses['upper'])))

      ax1.plot(all_runtimes['median'], mses['median'], marker=markers[i], color=colors[i], label=label, linestyle = 'None',
                                 markerfacecolor=colors[i], markeredgecolor=edgecolors[i], markersize=markersize)
      ax2.plot(all_runtimes['median'], mses['lower'], marker=markers[i], color=colors[i], label=label, linestyle = 'None',
                                 markerfacecolor=colors[i], markeredgecolor=edgecolors[i], markersize=markersize)
      ax3.plot(all_runtimes['median'], mses['upper'], marker=markers[i], color=colors[i], label=label, linestyle = 'None',
                                 markerfacecolor=colors[i], markeredgecolor=edgecolors[i], markersize=markersize)
      
      
      ax5.plot(all_runtimes['all'], mses['all'], marker=markers[i], color=colors[i], label=label, linestyle = 'None',
                                  markerfacecolor=colors[i], markeredgecolor=edgecolors[i], markersize=markersize)
      
      median = mses['median']
      lower_err = [abs(median[i] - mses['lower'][i]) for i in range(len(median))]
      upper_err = [abs(mses['upper'][i] - median[i]) for i in range(len(median))]
      ax4.errorbar(all_runtimes['median'], median, yerr=[lower_err, upper_err], fmt=fmt, color=colors[i], label=label, 
                                 markerfacecolor=colors[i], markeredgecolor=edgecolors[i], capsize=5, markersize=markersize)
      
      lower_time = [abs(all_runtimes['median'][i] - all_runtimes['lower'][i]) for i in range(len(median))]
      upper_time = [abs(all_runtimes['upper'][i] - all_runtimes['median'][i]) for i in range(len(median))]
      ax6.errorbar(all_runtimes['median'], median, xerr=[lower_time, upper_time], yerr=[lower_err, upper_err], fmt=fmt, color=colors[i], label=label, 
                                 markerfacecolor=colors[i], markeredgecolor=edgecolors[i], capsize=5, markersize=markersize)

      # axes1[plot_j][plot_i].set_xticks(p)
      # axes2[plot_j][plot_i].set_xticks(p)
      # axes3[plot_j][plot_i].set_xticks(p)
      # axes4[plot_j][plot_i].set_xticks(p)

      # min non-zero value
      nonzero = [x for x in mses['lower'] if x > 0]
      thresh = min(nonzero) if len(nonzero) > 0 else 1e-10

      for ax in [ax1, ax2, ax3, ax4, ax5, ax6]:
        ax.set_yscale('symlog', linthresh=thresh)
        # ax.tick_params(numticks=nbins)
        # ax.locator_params(axis='y', numticks=nbins)
        ax.set_xscale('log')
        ax.grid(**gridparams)
        ax.minorticks_on()

      variable_name = v
      for ax in [ax1, ax2, ax3, ax4, ax5, ax6]:
        ax.set_title(f'Accuracy of {variable_name}')
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
        for ax in [axes1, axes2, axes3, axes4, axes5, axes6]:
          if config['n_y'] != 1:
            ax[j][i].set_ylabel('Error (log scale)')
          else:
            ax[i].set_ylabel('Error (log scale)')

  for i in range(config['n_x']):
    for ax in [axes1, axes2, axes3, axes4, axes5, axes6]:
      if config['n_y'] != 1:
        ax[config['n_y'] - 1][i].set_xlabel(f'Execution Time in s (log scale)')
      else:
        ax[i].set_xlabel(f'Execution Time in s (log scale)')

  # for ax in [axes1, axes2, axes3, axes4]:
  #   ax[config['n_y'] - 1, config['n_x'] - 1].axis('off')

  for fig in [fig1, fig2, fig3, fig4, fig5, fig6]:
    fig.legend(loc='upper center', ncols=config['legend_width'], bbox_to_anchor=(0.5, 0.0))
    
  if verbose:
    print('Saving plots')

  # fig1.suptitle('Median Runtime to Accuracy - Median')
  # fig2.suptitle('Median Runtime to Accuracy - 10 Percentile')
  # fig3.suptitle('Median Runtime to Accuracy - 90 Percentile')
  # fig4.suptitle('Median Runtime to Accuracy')

  for fig in [fig1, fig2, fig3, fig4, fig5, fig6]:
    fig.tight_layout()

  name = os.path.splitext(os.path.basename(filename))[0].split('_')[0]
  # fig1.savefig(os.path.join(benchmark, output, '{}_runtime_median_accuracy_median.png'.format(name)), bbox_inches='tight')
  # fig2.savefig(os.path.join(benchmark, output, '{}_runtime_median_accuracy_lower.png'.format(name)), bbox_inches='tight')
  fig3.savefig(os.path.join(benchmark, output, '{}_runtime_median_accuracy_upper.png'.format(name)), bbox_inches='tight')
  fig4.savefig(os.path.join(benchmark, output, '{}_runtime_median_accuracy.png'.format(name)), bbox_inches='tight')
  fig5.savefig(os.path.join(benchmark, output, '{}_runtime_accuracy_all.png'.format(name)), bbox_inches='tight')
  fig6.savefig(os.path.join(benchmark, output, '{}_runtime_accuracy.png'.format(name)), bbox_inches='tight')

  # fig1.savefig(os.path.join(benchmark, output, '{}_runtime_median_accuracy_median.pdf'.format(name)), bbox_inches='tight')
  # fig2.savefig(os.path.join(benchmark, output, '{}_runtime_median_accuracy_lower.pdf'.format(name)), bbox_inches='tight')
  fig3.savefig(os.path.join(benchmark, output, '{}_runtime_median_accuracy_upper.pdf'.format(name)), bbox_inches='tight')
  fig4.savefig(os.path.join(benchmark, output, '{}_runtime_median_accuracy.pdf'.format(name)), bbox_inches='tight')
  fig5.savefig(os.path.join(benchmark, output, '{}_runtime_accuracy_all.pdf'.format(name)), bbox_inches='tight')
  fig6.savefig(os.path.join(benchmark, output, '{}_runtime_accuracy.pdf'.format(name)), bbox_inches='tight')

  for fig in [fig1, fig2, fig3, fig4, fig5, fig6]:
    plt.close(fig)

def analyze(statistics, benchmark, output, verbose, config):

  if benchmark not in statistics:
    statistics[benchmark] = {}

  with open(os.path.join(benchmark, 'all_programs', f'{benchmark}_template.si')) as template_file:
    template = template_file.read()

  variables = config['vars']
  statuses = ['EXACT', 'APPROX']

  # generate all possible combinations
  all_strats = list(itertools.product(statuses, repeat=len(variables)))

  # Number of strats
  statistics[benchmark]['n_strats'] = len(all_strats)
  # Number of variables
  statistics[benchmark]['n_vars'] = len(variables)
  # Number of annotations
  statistics[benchmark]['n_annotations'] = len(all_strats) * len(variables)

  n_true_satisfied = 0

  n_satisfied_tp = 0
  n_satisfied_fp = 0
  n_satisfied_tn = 0
  n_satisfied_fn = 0

  n_sat_vars = 0
  n_exacts = 0
  n_satisfiable_exact = 0
  n_exact_tp = 0
  n_exact_fp = 0
  n_exact_tn = 0
  n_exact_fn = 0

  for strat_index, strat in enumerate(all_strats):
    # generate program
    program = template
    annotations = {}
    for i, v in enumerate(variables):
      annotations[v] = strat[i]
      program = program.replace('PLACEHOLDER', strat[i].lower(), 1)

    print('> Strat:', annotations)

    n_exacts += len([x for x in strat if x == 'EXACT'])
  
    filename = f'{benchmark}_{strat_index}.si'

    # write program to file
    with open(os.path.join(benchmark, 'all_programs', filename), 'w') as program_file:
      program_file.write(program)

    # get analysis output
    cmd = 'dune exec siren -- {} --analyze --output output'.format(filename)
    if verbose:
      print('>', cmd)

    wd = os.path.join(os.getcwd(), benchmark, 'all_programs')
    out = subprocess.check_output(cmd, cwd=wd, shell=True).decode("utf-8") 

    # parse output
    lines = out.strip().split('\n')

    static_satisfied = False

    inferred_strat = {}
    start = False
    for line in lines:
      if '==== STATIC APPROXIMATION STATUS ====' in line:
        start = True
      elif 'Inference Strategy - ' in line:
        # Final judgement
        is_sat = line.split(' - ')[1].strip()
        if is_sat == 'Satisfiable':
          static_satisfied = True
        else:
          static_satisfied = False
        break
      elif start:
        if line != '':
          var, status = line.strip().split(': ')
          inferred_strat[var] = status
      
    # Double check inferred strat is correct
    if static_satisfied:
      for var in variables:
        if inferred_strat[var] != annotations[var]:
          print('Inferred strat is incorrect')
          print('Inferred:', inferred_strat)
          print('Annotations:', annotations)
          exit(1)
    
    # run program for runtime analysis
    cmd = './{}.exe'.format(os.path.splitext(os.path.basename(filename))[0])
    if verbose:
      print('>', cmd)

    wd = os.path.join(os.getcwd(), benchmark, 'all_programs')
    out = subprocess.check_output(cmd, cwd=wd, shell=True).decode("utf-8")

    # parse output
    lines = out.strip().split('\n')

    # get outputs after ==== RUNTIME APPROXIMATION STATUS ====
    real_strat = {}
    start = False
    for line in lines:
      if '==== RUNTIME APPROXIMATION STATUS ====' in line:
        start = True
      elif start:
        if line != '':
          var, status = line.strip().split(' ')[:2]
          var = var.strip(':').strip()
          real_strat[var] = status.strip()

    # compare inferred and real strats
    runtime_satisfied = True
    for var in variables:
      if (annotations[var] == 'EXACT' and real_strat[var] != 'EXACT') or \
          (annotations[var] == 'APPROX' and real_strat[var] != 'APPROX'):
        runtime_satisfied = False
        break

    if runtime_satisfied:
      n_true_satisfied += 1
      n_satisfiable_exact += len([x for x in strat if x == 'EXACT'])
      

      known = False
      for known_strats in config['strats']:
        if annotations == known_strats:
          known = True
          break

      if not known:
        print()
        print(f'MISSED: {filename}')
        print('Unknown strat:', annotations)
        print('Inferred:', inferred_strat)
        print('Real:', real_strat)

    if static_satisfied and runtime_satisfied:
      n_satisfied_tp += 1
    elif static_satisfied and not runtime_satisfied:
      print()
      print('False Positive')
      print('Inferred:', inferred_strat)
      print('Real:', real_strat)
      print('Annotations:', annotations)
      exit()
      n_satisfied_fp += 1
    elif not static_satisfied and runtime_satisfied:
      print()
      print('False Negative')
      print('Inferred:', inferred_strat)
      print('Real:', real_strat)
      print('Annotations:', annotations)
      n_satisfied_fn += 1
    elif not static_satisfied and not runtime_satisfied:
      n_satisfied_tn += 1


  statistics[benchmark]['n_true_satisfied'] = n_true_satisfied
  statistics[benchmark]['n_satisfied_tp'] = n_satisfied_tp
  statistics[benchmark]['n_satisfied_fp'] = n_satisfied_fp
  statistics[benchmark]['n_satisfied_tn'] = n_satisfied_tn
  statistics[benchmark]['n_satisfied_fn'] = n_satisfied_fn
  statistics[benchmark]['n_exacts'] = n_exacts
  statistics[benchmark]['n_satisfiable_exact'] = n_satisfiable_exact
  statistics[benchmark]['n_exact_tp'] = n_exact_tp
  statistics[benchmark]['n_exact_fp'] = n_exact_fp
  statistics[benchmark]['n_exact_tn'] = n_exact_tn
  statistics[benchmark]['n_exact_fn'] = n_exact_fn

  return statistics

  # # total number of exact and approx variables
  # n_exact = 0
  # n_approx = 0

  # # total number of missed exact variables
  # n_missed_exact = 0

  # # total number of missed approx variables
  # n_missed_approx = 0

  # for file_index, filename in enumerate(files):
  #   if verbose:
  #     print('Analyzing {}'.format(filename))

  #   # get analysis output
  #   cmd = 'dune exec siren -- {} --analyze --output output'.format(filename)
  #   if verbose:
  #     print('>', cmd)

  #   wd = os.path.join(os.getcwd(), benchmark)
  #   try:
  #     out = subprocess.check_output(cmd, cwd=wd, shell=True).decode("utf-8") 
  #   except subprocess.CalledProcessError as e:
  #     out = e.output.decode("utf-8")

  #   # parse output
  #   lines = out.strip().split('\n')

  #   satisfied = False

  #   inferred_strat = {}
  #   start = False
  #   for line in lines:
  #     if '==== STATIC APPROXIMATION STATUS ====' in line:
  #       start = True
  #     elif 'Inference Strategy - ' in line:
  #       # Final judgement
  #       is_sat = line.split(' - ')[1].strip()
  #       if is_sat == 'Satisfiable':
  #         satisfied = True
  #       else:
  #         satisfied = False
  #       break
  #     elif start:
  #       if line != '':
  #         var, status = line.strip().split(': ')
  #         inferred_strat[var] = status
    
  #   # Double check it is satisfied
  #   ann_strat = config['strats'][file_index]
  #   if satisfied and ann_strat != inferred_strat:
  #     print('Annotation and inference strategy do not match but says SATISFIABLE')
  #     print('Annotation: {}'.format(ann_strat))
  #     print('Inferred: {}'.format(inferred_strat))
  #     exit(2)

  #   if satisfied:
  #     n_correct += 1

  #   for v, s in ann_strat.items():
  #     if (s == 'EXACT' and inferred_strat[v] != 'EXACT') or (s == 'APPROX' and inferred_strat[v] != 'APPROX'):
  #       print(f'>> {v} is supposed to be {s} but inferred as {inferred_strat[v]}')

  #   n_vars += len(ann_strat)
  #   n_exact += len([v for v in ann_strat if ann_strat[v] == 'EXACT'])
  #   n_approx += len([v for v in ann_strat if ann_strat[v] == 'APPROX'])
  #   n_missed_exact += len([v for v in ann_strat if ann_strat[v] == 'EXACT' and inferred_strat[v] != 'EXACT'])
  #   n_missed_approx += len([v for v in ann_strat if ann_strat[v] == 'APPROX' and inferred_strat[v] != 'APPROX'])

  # statistics[benchmark]['n_correct'] = n_correct
  # statistics[benchmark]['n_vars'] = n_vars
  # statistics[benchmark]['n_exact'] = n_exact
  # statistics[benchmark]['n_missed_exact'] = n_missed_exact
  # statistics[benchmark]['n_approx'] = n_approx
  # statistics[benchmark]['n_missed_approx'] = n_missed_approx

  # print()
  
if __name__ == '__main__':
  p = argparse.ArgumentParser()
  p.add_argument('--benchmark', '-b', type=str, required=False, nargs="+")
  p.add_argument('--output', '-o', type=str, required=False, default='output')
  p.add_argument('--files', '-f', type=str, required=False, nargs="+")
  p.add_argument('--verbose', '-v', action='store_true', required=False)

  sp = p.add_subparsers(dest='subparser_name')

  rp = sp.add_parser('run')
  rp.add_argument('--particles', '-p', type=int, required=False, nargs='+')
  rp.add_argument('--prange', '-pr', type=int, required=False, nargs=2, default=[1, 1000])
  rp.add_argument('--accuracy', '-a', type=float, required=False, default=0.01)
  rp.add_argument('--n', '-n', type=int, required=False, default=1000)
  
  pp = sp.add_parser('plot')
  pp.add_argument('--particles', '-p', type=int, required=False, nargs='+')

  ap = sp.add_parser('analyze')
  ap.add_argument('--no-run', '-n', action='store_true', required=False)

  args = p.parse_args()

  # If no benchmark specified, use default list
  if args.benchmark is None:
    args.benchmark = DEFAULT_BENCHMARKS

  use_config_for_files = False
  if args.files is None:
    use_config_for_files = True

  for benchmark in args.benchmark:
    print('Benchmark: {}'.format(benchmark))

    with open(os.path.join(benchmark, 'config.json')) as f:
      config = json.load(f)
    
    # If no files specified, use all files in benchmark directory
    if use_config_for_files:
      args.files = config['files']
    args.files = [os.path.basename(f) for f in args.files]

    # If plotting, just plot and exit. Assumes output is in args.output
    if args.subparser_name == 'plot':
      plot(benchmark, args.output, args.files, args.particles, config, args.verbose)
      exit()

    elif args.subparser_name == 'run':
      # make output directory
      os.makedirs(os.path.join(benchmark, args.output), exist_ok=True)

      if args.particles is None and args.prange is None:
        args.particles = [x for x in range(1, 1001)]

      elif args.particles is None and args.prange is not None:
        args.particles = [int(x) for x in np.unique(np.logspace(np.log10(args.prange[0]), np.log10(args.prange[1]), N_INTERVALS, dtype=int))]

        print(args.particles)

      for file in args.files:
        if not os.path.exists(os.path.join(benchmark, file)):
          raise Exception('File not found: {}'.format(file))

        results = {}
        if os.path.exists(os.path.join(benchmark, args.output, 'results.json')):
          with open(os.path.join(benchmark, args.output, 'results.json')) as f:
            try:
              results = json.load(f)
            except:
              results = {}

        try:
          results = run(benchmark, file, args.output, args.particles, args.accuracy, args.n, results, config, args.verbose)
        except KeyboardInterrupt:
          pass

        # write results
        with open(os.path.join(benchmark, args.output, 'results.json'), 'w') as f:
          json.dump(results, f, indent=2)
    
    elif args.subparser_name == 'analyze' and not args.no_run:
      statistics = {}
      if os.path.exists(os.path.join('statistics.json')):
        with open(os.path.join('statistics.json')) as f:
          try:
            statistics = json.load(f)
          except:
            statistics = {}
      statistics = analyze(statistics, benchmark, args.output, args.verbose, config)

      # write statistics
      with open(os.path.join('statistics.json'), 'w') as f:
        json.dump(statistics, f, indent=2)

  if args.subparser_name == 'analyze':

    if os.path.exists(os.path.join('statistics.json')):
      with open(os.path.join('statistics.json')) as f:
        try:
          statistics = json.load(f)
        except:
          print('Error loading statistics.json')
          exit()
    else:
      print('Error: statistics.json not found')
      exit()
    # Benchmark & \# Plans & \# Sat & TP & TN & FP & FN & Precision & Recall & Accuracy \\
    # \bGaussianmix{}    &  & 8 & 8 &  8 & 0 & 0 & 100\% & 100\% & 100\%\\
    content = ''
    for benchmark in DEFAULT_BENCHMARKS:
      precision = statistics[benchmark]['n_satisfied_tp'] / (statistics[benchmark]['n_satisfied_tp'] + statistics[benchmark]['n_satisfied_fp'])
      recall = statistics[benchmark]['n_satisfied_tp'] / (statistics[benchmark]['n_satisfied_tp'] + statistics[benchmark]['n_satisfied_fn'])
      accuracy = (statistics[benchmark]['n_satisfied_tp'] + statistics[benchmark]['n_satisfied_tn']) / statistics[benchmark]['n_strats']

      row = '    {} & {} & {} & {} & {} & {} & {} & {:.0f}\% & {:.0f}\% & {:.0f}\%\\\\'.format(
        BENCHMARK_LATEX_NAMES[benchmark],
        statistics[benchmark]['n_strats'],
        statistics[benchmark]['n_true_satisfied'],
        statistics[benchmark]['n_satisfied_tp'],
        statistics[benchmark]['n_satisfied_tn'], 
        statistics[benchmark]['n_satisfied_fp'],
        statistics[benchmark]['n_satisfied_fn'],
        precision * 100,
        recall * 100,
        accuracy * 100,
      )
      content += row + '\n'

    print(TABLE_STR.replace('CONTENT', content))

