import pytest
import os

from siren.inference import SSIState, DSState
import siren.parser as parser
import siren.evaluate as evaluate
from siren.inference_plan import runtime_inference_plan, InferencePlan, DistrEnc
from siren.grammar import Const, Identifier
from siren.utils import get_lst, get_pair

def run(program_path, inference_method):
  with open(program_path) as f:
    program = parser.parse_program(f.read())

    file_dir = os.path.dirname(os.path.realpath(program_path))
    res, particles = evaluate.evaluate(
      program, 
      10, 
      inference_method, 
      file_dir, 
      False,
    )
    runtime_plan = runtime_inference_plan(particles)

    return res, runtime_plan

@pytest.mark.parametrize("method", [SSIState, DSState])
def test_coin(method):
  program_path = os.path.join('test', 'programs', 'coin.si')
  var = Identifier(module=None, name='xt')

  res, runtime_plan = run(program_path, method)
  assert res == Const(0.9)
  assert runtime_plan[var] == DistrEnc.symbolic

@pytest.mark.parametrize("method", [SSIState, DSState])
def test_kalman(method):
  program_path = os.path.join('test', 'programs', 'kalman.si')
  var = Identifier(module=None, name='x')

  res, runtime_plan = run(program_path, method)
  l = get_lst(res)
  assert isinstance(l[-1], Const)
  assert 98 <= round(l[-1].v) <= 100
  assert runtime_plan[var] == DistrEnc.sample

@pytest.mark.parametrize("method", [SSIState, DSState])
def test_envnoise(method):
  program_path = os.path.join('test', 'programs', 'envnoise.si')

  res, runtime_plan = run(program_path, method)
  x, res = get_pair(res)
  q, r = get_pair(res)
  assert isinstance(x, Const)
  assert isinstance(q, Const)
  assert isinstance(r, Const)
  assert x.v <= 10 and x.v >= -10
  assert q.v >= 0
  assert r.v >= 0
  plan1 = InferencePlan({
    Identifier(module=None, name='invq'): DistrEnc.symbolic,
    Identifier(module=None, name='invr'): DistrEnc.sample,
    Identifier(module=None, name='x0'): DistrEnc.sample,
    Identifier(module=None, name='x'): DistrEnc.sample,
    Identifier(module=None, name='env'): DistrEnc.sample,
    Identifier(module=None, name='other'): DistrEnc.sample,
  })
  plan2 = InferencePlan({
    Identifier(module=None, name='invq'): DistrEnc.symbolic,
    Identifier(module=None, name='invr'): DistrEnc.sample,
    Identifier(module=None, name='x0'): DistrEnc.sample,
    Identifier(module=None, name='x'): DistrEnc.sample,
    Identifier(module=None, name='env'): DistrEnc.sample,
  })
  assert runtime_plan == plan1 or runtime_plan == plan2

@pytest.mark.parametrize("method", [SSIState, DSState])
def test_tree(method):
  program_path = os.path.join('test', 'programs', 'tree.si')
  true_bs = [2, 4, 6]

  res, runtime_plan = run(program_path, method)
  a, bs = get_pair(res)
  bs = get_lst(bs)
  assert isinstance(a, Const)
  assert round(a.v) == 3
  for b, true_b in zip(bs, true_bs):
    assert isinstance(b, Const)
    if method == SSIState:
      assert round(b.v) == true_b
    elif method == DSState:
      assert round(b.v) <= true_b + 2 and round(b.v) >= true_b - 2
  assert runtime_plan[Identifier(module=None, name='a')] == DistrEnc.symbolic
  if method == SSIState:
    assert runtime_plan[Identifier(module=None, name='b')] == DistrEnc.symbolic
  elif method == DSState:
    assert runtime_plan[Identifier(module=None, name='b')] == DistrEnc.sample

if __name__ == '__main__':
  pytest.main()