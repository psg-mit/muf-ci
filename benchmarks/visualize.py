import argparse
import os
import json
import matplotlib.pyplot as plt
import csv
import pandas as pd
from typing import List, Tuple, Dict
import numpy as np

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

BENCHMARK_LATEX_NAMES = {
  'gaussianmixture': r"\bGaussianmix{}",
  'envnoise': r"\bEnvnoise{}",
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
  \midrule CONTENT
  \bottomrule
\end{tabular}
"""

N_INTERVALS = 30

# red orange green blue purple indigo yellow brown magenta
COLORS = ["#f4827b", "#feb140", "#9baa20", "#7cc7d2", "#9879a4", "#5999d3", "#f7dd73", "#865042", "#d146b6", "#303030"]
# red blue orange green purple indigo yellow brown magenta
# colors = ["#f4827b", "#7cc7d2", "#feb140", "#9baa20", "#9879a4", "#5999d3", "#f7dd73", "#865042", "#d146b6", "#303030"]
EDGECOLORS = ["#b7575c", "#c78200", "#708200","#4d9aa4","#7c5b83","#204f87", "#c0ab5f", "#56281b", "#9c0085", "#171616"]
# edgecolors = ["#b7575c", "#4d9aa4", "#c0ab5f","#708200","#7c5b83","#204f87", "#c78200", "#56281b", "#9c0085", "#171616"]

MARKERS = ['s', 'v', 'd', 'o', 'X', 'p', 'h', 'P', '*', '<']

MARKERSSIZE = 50

BARWIDTH = 0.25

GRIDPARAMS = {'which': 'major', 'color': 'gray', 'linestyle': '--', 'alpha': 0.5}

PLT_SETTINGS = {
  # 'font.size': 24, 
  'font.family': 'serif', 
  # 'font.serif': 'Times New Roman', 
  # 'font.weight': 'bold',
  # 'axes.labelweight': 'bold',
  # 'figure.autolayout': True,
  "pgf.texsystem": "pdflatex",        # change this if using xetex or lautex
  "text.usetex": True,                # use LaTeX to write all text
  "font.family": "serif",
  "font.serif": [],                   # blank entries should cause plots to inherit fonts from the document
  "font.sans-serif": [],
  "font.monospace": [],
  # "axes.labelsize": 10,               # LaTeX default is 10pt font.
  # "font.size": 10,
  # "legend.fontsize": 8,               # Make the legend/label fonts a little smaller
  # "xtick.labelsize": 8,
  # "ytick.labelsize": 8,
  # "figure.figsize": figsize(0.9),     # default fig size of 0.9 textwidth
  # "pgf.preamble": "\n".join([         # plots will use this preamble
  #     r"\usepackage[utf8x]{inputenc}",    # use utf8 fonts becasue your computer can handle it :)
  #     r"\usepackage[T1]{fontenc}",        # plots will be generated using this preamble
      # ])
}

def table(statistics):
  # Benchmark & \# Plans & \# Sat & TP & TN & FP & FN & Precision & Recall & Accuracy \\
  # \bGaussianmix{}    &  & 8 & 8 &  8 & 0 & 0 & 100\% & 100\% & 100\%\\
  content = ''
  for benchmark in DEFAULT_BENCHMARKS:
    n_plans = statistics[benchmark]['n_plans']
    n_vars = statistics[benchmark]['n_vars']
    n_true_satisfied = statistics[benchmark]['n_true_satisfied']
    # n_inferred_satisfied = statistics[benchmark]['n_inferred_satisfied']
    n_satisfied_tp = statistics[benchmark]['n_satisfied_tp']
    n_satisfied_tn = statistics[benchmark]['n_satisfied_tn']
    n_satisfied_fp = statistics[benchmark]['n_satisfied_fp']
    n_satisfied_fn = statistics[benchmark]['n_satisfied_fn']

    precision = n_satisfied_tp / (n_satisfied_tp + n_satisfied_fp)
    recall = n_satisfied_tp / (n_satisfied_tp + n_satisfied_fn)
    accuracy = (n_satisfied_tp + n_satisfied_tn) / n_plans

    row = '  {} & {} & {} & {} & {} & {} & {} & {:.0f}\% & {:.0f}\% & {:.0f}\%\\\\'.format(
      BENCHMARK_LATEX_NAMES[benchmark],
      n_plans,
      n_true_satisfied,
      n_satisfied_tp,
      n_satisfied_tn,
      n_satisfied_fp,
      n_satisfied_fn,
      precision * 100,
      recall * 100,
      accuracy * 100,
    )
    content += '\n' + row

  print(TABLE_STR.replace('CONTENT', content))

def get_label(plan):
  symbolic = []
  sample = []
  dynamic = []
  for var in sorted(plan.keys()):
    if plan[var] == 'symbolic':
      symbolic.append(var)
    elif plan[var] == 'sample':
      sample.append(var)
    elif plan[var] == 'dynamic':
      dynamic.append(var)
    else:
      raise ValueError(plan[var])
  
  label = ''
  if len(symbolic) > 0:
    label += ', '.join(symbolic)
  else:
    label += 'None'

  label += ' | '
  if len(sample) > 0:
    label += ', '.join(sample)
  else:
    label += 'None'

  label += ' | '
  if len(dynamic) > 0:
    label += ', '.join(dynamic)
  else:
    label += 'None'

  return label

def plot_particles(data, output, methods, plans, particles, n_y, n_x, base_x, base_y, legend_width):
  plt.rcParams.update(PLT_SETTINGS)

  # first 4 columns are metadata
  # n_variables = data.columns[4:].shape[0]
  variables = data.columns[4:]

  # drop rows with particles not in particles
  if particles is not None:
    data = data.loc[data['particles'].isin(particles)]

  fig, axes = plt.subplots(
              n_y, n_x,
              figsize=(base_x * n_x, base_y * n_y), 
              sharex=True, 
              # sharey=True
  )

  for ax in axes.flatten():
    ax.set_visible(False)

  for method_i, method in enumerate(methods):
    use_label = True
    for var_i, var in enumerate(variables):
      plot_i = var_i % n_x
      plot_j = var_i // n_x

      if n_y == 1:
        ax = axes[plot_i]
      else:
        ax = axes[plot_j, plot_i]

      ax.set_visible(True)

      for plan_i, (plan_id, plan) in enumerate(plans):
        if use_label:
          label = get_label(plan)
        else:
          label = None

        plan_id = int(plan_id)

        # 90th percentile error over n runs
        errors = data.loc[data['method'] == method]\
                     .loc[data['plan_id'] == plan_id]\
                     .groupby(['particles'])[var]\
                     .quantile(0.9)
        
        # min non-zero value
        
        # nonzero = [x for x in mses['lower'] if x > 0]
        # thresh = min(nonzero) if len(nonzero) > 0 else 1e-10
        
        # median time over n runs
        runtimes = data.loc[data['method'] == method]\
                        .loc[data['plan_id'] == plan_id]\
                        .groupby(['particles'])['time']\
                        .median()

        ax.scatter(runtimes, errors, marker=MARKERS[plan_i], color=COLORS[plan_i], label=label, 
                                    edgecolor=EDGECOLORS[plan_i], s=MARKERSSIZE)
        ax.set_xscale('log')
        ax.set_yscale('symlog', linthresh=1e-10)
        ax.grid(**GRIDPARAMS)
        ax.minorticks_on()

      use_label = False
      ax.set_title(f'{var}')

    print('Saving particles plots')

    fig.suptitle(f'Variable Accuracy to Execution Time')
    # fig.tight_layout()
    fig.tight_layout(rect=[0.04, 0.04, 1, 1.1])
    lgd = fig.legend(loc='lower center', ncols=legend_width, bbox_to_anchor=(0.53, -0.35))

    # fig.supxlabel('Execution Time in s (log scale)')
    # fig.supylabel('Error (log scale)')

    if n_y == 1:
      plt.setp(axes[1], xlabel='Execution Time in s (log scale)')
      plt.setp(axes[0], ylabel='Error (log scale)')
    else:
      plt.setp(axes[n_vars // 2, :], xlabel='Execution Time in s (log scale)')
      plt.setp(axes[:, 0], ylabel='Error (log scale)')

    fig.savefig(os.path.join(output, f'particles.pdf'), bbox_inches='tight')
    fig.savefig(os.path.join(output, f'particles.png'), bbox_inches='tight')
    # fig.savefig(os.path.join(output, f'particles.pdf'), bbox_extra_artists=(lgd,), bbox_inches='tight')
    # fig.savefig(os.path.join(output, f'particles.png'), bbox_extra_artists=(lgd,), bbox_inches='tight')

    plt.close(fig)

def plot_accuracy(data, output, methods, plans, target_errors, n_y, n_x, base_x, base_y, legend_width):
  plt.rcParams.update(PLT_SETTINGS)

  # first 4 columns are metadata
  # n_variables = data.columns[4:].shape[0]
  variables = data.columns[4:]

  fig, axes = plt.subplots(
              n_y, n_x,
              figsize=(base_x * n_x, base_y * n_y), 
              sharex=True, 
              # sharey=True
  )

  for ax in axes.flatten():
    ax.set_visible(False)

  for method_i, method in enumerate(methods):
    use_label = True
    for var_i, var in enumerate(variables):
      plot_i = var_i % n_x
      plot_j = var_i // n_x

      if n_y == 1:
        ax = axes[plot_i]
      else:
        ax = axes[plot_j, plot_i]

      ax.set_visible(True)

      for target_error in target_errors[var]:
        # draw target error line
        ax.axhline(y=target_error, color='red', linestyle='--')

        for plan_i, (plan_id, plan) in enumerate(plans):
          if use_label:
            label = get_label(plan)
          else:
            label = None

          plan_id = int(plan_id)

          # 90th percentile error over n runs
          errors = data.loc[data['method'] == method]\
                      .loc[data['plan_id'] == plan_id]\
                      .groupby(['particles'])[var]\
                      .quantile(0.9)
          
          # print( errors[errors < target_error])

          # get max particle with error less than target error
          smallest_error_particle = errors[errors < target_error].index.max()

          # print(smallest_error_particle)

          if np.isnan(smallest_error_particle):
            runtimes = data.loc[data['method'] == method]\
                          .loc[data['plan_id'] == plan_id]\
                          .groupby(['particles'])['time']\
                          .median()
            
            runtime = runtimes.max() + 1

            # ax.plot(runtime, target_error, marker='x', color=COLORS[plan_i], label=label, markeredgecolor=COLORS[plan_i], markersize=MARKERSSIZE)
            ax.scatter(runtime, target_error, marker='x', color=COLORS[plan_i], label=label, s=MARKERSSIZE)
          else:
            # get runtime of max particle rows
            runtimes = data.loc[data['method'] == method]\
                            .loc[data['plan_id'] == plan_id]\
                            .loc[data['particles'] == smallest_error_particle]\
                            .groupby(['particles'])['time']\
                            .median()
            
            # ax.scatter(runtimes, target_error, marker=MARKERS[plan_i], color=COLORS[plan_i], label=label, 
            #                           edgecolor=EDGECOLORS[plan_i], markersize=MARKERSSIZE)
            ax.scatter(runtimes, target_error, marker=MARKERS[plan_i], color=COLORS[plan_i], label=label, 
                                    edgecolor=EDGECOLORS[plan_i], s=MARKERSSIZE)

          
        ax.set_xscale('log')
        ax.set_yscale('symlog', linthresh=1e-10)
        ax.grid(**GRIDPARAMS)
        ax.minorticks_on()

        use_label = False
      ax.set_title(f'{var}')

    print('Saving accuracy plots')

    fig.suptitle(f'Variable Accuracy to Execution Time')
    # fig.tight_layout()
    fig.tight_layout(rect=[0.04, 0.04, 1, 1.1])
    lgd = fig.legend(loc='lower center', ncols=legend_width, bbox_to_anchor=(0.53, -0.35))

    # fig.supxlabel('Execution Time in s (log scale)')
    # fig.supylabel('Error (log scale)')

    if n_y == 1:
      plt.setp(axes[1], xlabel='Execution Time in s (log scale)')
      plt.setp(axes[0], ylabel='Error (log scale)')
    else:
      plt.setp(axes[n_vars // 2, :], xlabel='Execution Time in s (log scale)')
      plt.setp(axes[:, 0], ylabel='Error (log scale)')

    fig.savefig(os.path.join(output, f'accuracy.pdf'), bbox_inches='tight')
    fig.savefig(os.path.join(output, f'accuracy.png'), bbox_inches='tight')
    # fig.savefig(os.path.join(output, f'particles.pdf'), bbox_extra_artists=(lgd,), bbox_inches='tight')
    # fig.savefig(os.path.join(output, f'particles.png'), bbox_extra_artists=(lgd,), bbox_inches='tight')

    plt.close(fig)
  
def plot_accuracy_bar(data, baseline, output, methods, plans, target_errors, n_y, n_x, base_x, base_y, legend_width):
  plt.rcParams.update(PLT_SETTINGS)

  spacing = 0.02
  # barwidth = 1/(len(plans) + 1 + spacing * (len(plans) - 1)) 
  barwidth = 0.1

  # first 4 columns are metadata
  # n_variables = data.columns[4:].shape[0]
  variables = data.columns[4:]

  for method_i, method in enumerate(methods):
    fig = plt.figure(figsize=(base_x * n_x, base_y * n_y))
    ax = fig.add_subplot(111)

    # y axis is runtime in log seconds
    # x axis is variable * plan
    # plan x variable

    # baseline
    # median time over n runs
    runtime = baseline.loc[baseline['method'] == method]['time'].median()
    all_runtimes = [[runtime for _ in range(len(variables))]]
    all_particles = [[1000 for _ in range(len(variables))]]
    x_pos = [list(np.arange(len(variables)))]
    labels = ['Baseline'] + [get_label(plan) for plan_id, plan in plans]
 
    for var_i, var in enumerate(variables):
      target_error = target_errors[var]

      x_pos.append([x + barwidth + spacing for x in x_pos[-1]])

      for plan_i, (plan_id, plan) in enumerate(plans):
        plan_id = int(plan_id)
        print(plan_id)

        # 90th percentile error over n runs
        errors = data.loc[data['method'] == method]\
                    .loc[data['plan_id'] == plan_id]\
                    .groupby(['particles'])[var]\
                    .quantile(0.9)
        
        # print(errors)
        # print(errors[errors < target_error])

        # get max particle with error less than target error
        smallest_error_particle = errors[errors < 10].index.max()

        # print(smallest_error_particle)


        if np.isnan(smallest_error_particle):
          runtimes = data.loc[data['method'] == method]\
                        .loc[data['plan_id'] == plan_id]\
                        .groupby(['particles'])['time']\
                        .median()
          
          runtime = runtimes.max() + 1

          all_runtimes.append([runtime for _ in range(len(variables))])
          all_particles.append([None for _ in range(len(variables))])

        else:
          # get runtime of max particle rows
          runtimes = data.loc[data['method'] == method]\
                          .loc[data['plan_id'] == plan_id]\
                          .loc[data['particles'] == smallest_error_particle]\
                          .groupby(['particles'])['time']\
                          .median()
          
          all_runtimes.append(list(runtimes))
          all_particles.append([smallest_error_particle for _ in range(len(variables))])

    for i, (x_pos, runtimes, particles) in enumerate(zip(x_pos, all_runtimes, all_particles)):
      p = ax.bar(x_pos, runtimes, width=barwidth, color=COLORS[i], edgecolor=EDGECOLORS[i], label=labels[i])

      particles = [f'{p:,}' if p is not None else '> 1,000' for p in particles]
      
      ax.bar_label(p, labels=particles, padding=3, label_type = 'center', rotation=90)
          
    # ax.set_xscale('log')
    ax.set_yscale('log')
    # ax.set_yscale('symlog', linthresh=1e-10)
    ax.grid(**{'which': 'major', 'axis': 'y', 'color': 'gray', 'linestyle': '--', 'alpha': 0.5})
    # ax.minorticks_on()
    # ax.tick_params(axis='x', which='minor', bottom=False)
    ax.tick_params(axis='x', which='major')
    ax.tick_params(axis='y', which='minor')
    # ax.tick_params(axis='both', color='gray', linestyle='--', alpha=0.5)

    ax.set_xticks([r + barwidth for r in range(len(variables))], [var for var in variables])

    print('Saving accuracy plots')

    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    # ax.spines['bottom'].set_visible(False)
    ax.spines['left'].set_visible(False)
    for spine in ax.spines.values():
      spine.set_edgecolor('#696969')

    ax.set_title(f'{benchmark}')
    # fig.tight_layout()
    # fig.tight_layout(rect=[0.04, 0.04, 1, 1.1])
    lgd = fig.legend(loc='lower center', ncols=legend_width, bbox_to_anchor=(0.53, -0.2))

    ax.set_ylabel('Execution Time in s (log scale)')
    # ax.set_ylabel('Error (log scale)')

    fig.savefig(os.path.join(output, f'accuracy.pdf'), bbox_extra_artists=(lgd,), bbox_inches='tight')
    fig.savefig(os.path.join(output, f'accuracy.png'), bbox_extra_artists=(lgd,), bbox_inches='tight')
    fig.savefig(os.path.join(output, f'accuracy.pgf'), bbox_extra_artists=(lgd,), bbox_inches='tight', format='pgf')

    plt.close(fig)

if __name__ == '__main__':
  p = argparse.ArgumentParser()
  p.add_argument('--benchmark', '-b', type=str, required=False, nargs="+", default=DEFAULT_BENCHMARKS)
  p.add_argument('--output', '-o', type=str, required=False, default='output')
  p.add_argument('--plan-ids', '-pi', type=int, required=False, nargs="+")
  p.add_argument('--methods', '-m', type=str, required=False, nargs="+", default=DEFAULT_METHODS)
  p.add_argument('--particles', '-p', type=int, required=False, nargs='+')

  sp = p.add_subparsers(dest='subparser_name')

  table_parser = sp.add_parser('table')
  plot_parser = sp.add_parser('plot')

  args = p.parse_args()


  methods = [method for method in args.methods if method in DEFAULT_METHODS]

  particles = [int(particle) for particle in args.particles] if args.particles is not None else None

  if args.subparser_name == 'table':
    # Load statistics
    all_statistics = {}

    for benchmark in args.benchmark:
      output = os.path.join(benchmark, args.output)
      with open(os.path.join(output, 'statistics.json')) as f:
        statistics = json.load(f)
      all_statistics[benchmark] = statistics
    table(all_statistics)

  else:
    print(args.benchmark)

    for benchmark in args.benchmark:
      print('Benchmark: {}'.format(benchmark))

      with open(os.path.join(benchmark, 'config.json')) as f:
        config = json.load(f)

      plans: List[Tuple[str, Dict[str, str]]] = []
      if args.plan_ids is None:
        plans = [(plan_id, data['plan']) for plan_id, data in config['plans'].items() if data['satisfiable']]
      else:
        plans = [(plan_id, config[plan_id]['plan']) for plan_id in args.plan_ids]

      output = os.path.join(benchmark, args.output)

      n_vars = len(config['true_vars'])
      # default to number of variables as number of columns
      n_y = int(config['n_y']) if 'n_y' in config else 1
      n_x = int(config['n_x']) if 'n_x' in config else n_vars
      base_x = int(config['base_x']) if 'base_x' in config else 5
      base_y = int(config['base_y']) if 'base_y' in config else 6
      legend_width = int(config['legend_width']) if 'legend_width' in config else n_vars


      if os.path.exists(os.path.join(output, 'results.csv')):
        data = pd.read_csv(os.path.join(output, 'results.csv'), delimiter=',')

        if os.path.exists(os.path.join(output, 'baseline.csv')):
          baseline = pd.read_csv(os.path.join(output, 'baseline.csv'), delimiter=',')

          target_errors = config['target_errors']

          plot_accuracy_bar(data, baseline, output, methods, plans, target_errors, n_y, n_x, base_x, base_y, legend_width)

      # if os.path.exists(os.path.join(output, 'particles.csv')):
      #   data = pd.read_csv(os.path.join(output, 'particles.csv'), delimiter=',')

      #   plot_particles(data, output, methods, plans, particles, n_y, n_x, base_x, base_y, legend_width)

      # if os.path.exists(os.path.join(output, 'accuracy.csv')):
      #   data = pd.read_csv(os.path.join(output, 'accuracy.csv'), delimiter=',')

      #   target_errors = config['target_errors']

      #   plot_accuracy(data, output, methods, plans, target_errors, n_y, n_x, base_x, base_y, legend_width)

      

