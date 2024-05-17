import argparse
from enum import Enum
import os
import time
import cProfile
import pstats

from . import parser, evaluate, analyze
from .inference import SSIState, DSState, BPState
from .analysis import AbsSSIState, AbsDSState, AbsBPState
from .inference_plan import runtime_inference_plan
import sys


method_states = {
    "ssi": (SSIState, AbsSSIState),
    "ds": (DSState, AbsDSState),
    "bp": (BPState, AbsBPState),
}


def main():
    sys.setrecursionlimit(5000)
    p = argparse.ArgumentParser()
    p.add_argument("filename", type=str)
    p.add_argument("--verbose", "-v", action="store_true")
    p.add_argument("--particles", "-p", type=int, default=100)
    p.add_argument("--analyze", "-a", action="store_true")
    p.add_argument("--analyze-only", "-ao", action="store_true")
    p.add_argument(
        "--method",
        "-m",
        type=str,
        default="ssi",
        choices=["ssi", "ds", "bp"],
    )
    p.add_argument("--multiprocess", "-mp", action="store_true")
    p.add_argument("--profile", "-pr", action="store_true")
    p.add_argument("--seed", "-s", type=int, default=None)
    p.add_argument("--exclude-marginalizing", action="store_true", default=False)
    args = p.parse_args()

    profiler = cProfile.Profile()
    if args.profile:
        profiler.enable()

    with open(args.filename, "r") as f:
        program = parser.parse_program(f.read())
        # print(program)

    (inference_method, analysis_method) = method_states[args.method]

    print("===== Inferred Algorithm =====")
    match args.method:
        case "ssi":
            print("SSI")
        case "ds":
            print("DS")
        case "bp":
            print("BP")
        case _:
            raise ValueError("Invalid method")

    if args.analyze or args.analyze_only:
        print("===== Inferred Inference Plan =====")
        t1 = time.time()
        inferred_plan = analyze.analyze(
            program, analysis_method, args.exclude_marginalizing
        )
        t2 = time.time()
        print(inferred_plan)
        print("===== Analysis Time =====")
        print(f"{t2 - t1}")

    if not args.analyze_only:
        file_dir = os.path.dirname(os.path.realpath(args.filename))
        t1 = time.time()
        res, particles = evaluate.evaluate(
            program,
            args.particles,
            inference_method,
            file_dir,
            args.multiprocess,
            args.exclude_marginalizing,
            args.seed,
        )
        t2 = time.time()
        print("===== Evaluation Time =====")
        print(f"{t2 - t1}")

        print("===== Result =====")
        print(res)

        plan = runtime_inference_plan(particles)

        if args.verbose:
            # Only for debugging
            particles.simplify()
            print("===== Mixture =====")
            print(particles.mixture())
            print("===== Particles =====")
            print(particles)

        print("===== Runtime Inference Plan =====")
        print(plan)

    if args.profile:
        profiler.disable()
        stats = pstats.Stats(profiler).sort_stats("cumulative")
        stats.print_stats()


if __name__ == "__main__":
    main()
