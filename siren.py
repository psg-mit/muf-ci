import argparse
from enum import Enum
import os
import time
import cProfile
import pstats

from siren import parser, evaluate, analyze
from siren.inference import SSIState, DSState
from siren.analysis import AbsSSIState, AbsDSState
from siren.inference_plan import runtime_inference_plan

class methods(Enum):
  ssi = 'ssi'
  ds = 'ds'
  ft = 'ft'
  dis = 'dis'

method_states = {
  methods.ssi: (SSIState, AbsSSIState),
  methods.ds: (DSState, AbsDSState)
}

def main():
  p = argparse.ArgumentParser()
  p.add_argument('filename', type=str)
  p.add_argument('--verbose', '-v', action='store_true')
  p.add_argument('--particles', '-p', type=int, default=100)
  p.add_argument('--analyze', '-a', action='store_true')
  p.add_argument('--analyze-only', '-ao', action='store_true')
  p.add_argument('--method', '-m', type=methods, default=methods.ssi)
  p.add_argument('--multiprocess', '-mp', action='store_true')
  p.add_argument('--profile', '-pr', action='store_true')
  args = p.parse_args()

  profiler = cProfile.Profile()
  if args.profile:
    profiler.enable()

  with open(args.filename, 'r') as f:
    program = parser.parse_program(f.read())
    # print(program)

  (inference_method, analysis_method) = method_states[args.method]

  if args.analyze or args.analyze_only:
    print('===== Inferred Inference Plan =====')
    t1 = time.time()
    inferred_plan = analyze.analyze(program, analysis_method)
    t2 = time.time()
    print(inferred_plan)
    print('===== Analysis Time =====')
    print(f'{t2 - t1}')

  if not args.analyze_only:
    file_dir = os.path.dirname(os.path.realpath(args.filename))
    t1 = time.time()
    res, particles = evaluate.evaluate(
      program, 
      args.particles, 
      inference_method, 
      file_dir, 
      args.multiprocess,
    )
    t2 = time.time()
    print('===== Evaluation Time =====')
    print(f'{t2 - t1}')

    print('===== Result =====')
    print(res)

    if args.verbose:
      print('===== Mixture =====')
      print(particles.mixture())
      print('===== Particles =====')
      print(particles)

    print('===== Runtime Inference Plan =====')
    print(runtime_inference_plan(particles))

  if args.profile:
    profiler.disable()
    stats = pstats.Stats(profiler).sort_stats('cumulative')
    stats.print_stats()

if __name__ == '__main__':
  main()