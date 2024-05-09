import pytest
import os

from siren.analysis import AbsSSIState, AbsDSState, AbsBPState
import siren.parser as parser
import siren.analyze as analyze
from siren.inference_plan import InferencePlan, DistrEnc
from siren.grammar import Const, Identifier
from siren.utils import get_abs_lst, get_abs_pair

def run(program_path, inference_method):
  with open(program_path) as f:
    program = parser.parse_program(f.read())

    res = analyze.analyze(
      program, 
      inference_method, 
    )

    return res

@pytest.mark.parametrize("method", [AbsSSIState, AbsDSState, AbsBPState])
def test_coin(method):
  program_path = os.path.join('tests', 'programs', 'coin.si')
  var = Identifier(module=None, name='xt')

  inferred_plan = run(program_path, method)
  if method == AbsBPState:
    assert inferred_plan[var] == DistrEnc.sample
  else:
    assert inferred_plan[var] == DistrEnc.symbolic

@pytest.mark.parametrize("method", [AbsSSIState, AbsDSState, AbsBPState])
def test_kalman(method):
  program_path = os.path.join('tests', 'programs', 'kalman.si')
  var = Identifier(module=None, name='x')

  inferred_plan = run(program_path, method)
  assert inferred_plan[var] == DistrEnc.sample

@pytest.mark.parametrize("method", [AbsSSIState, AbsDSState])
def test_envnoise(method):
  program_path = os.path.join('tests', 'programs', 'envnoise.si')

  inferred_plan = run(program_path, method)
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
  assert inferred_plan == plan1 or inferred_plan == plan2

@pytest.mark.parametrize("method", [AbsSSIState, AbsDSState, AbsBPState])
def test_tree(method):
  program_path = os.path.join('tests', 'programs', 'tree.si')

  inferred_plan = run(program_path, method)
  assert inferred_plan[Identifier(module=None, name='a')] == DistrEnc.symbolic
  if method == AbsSSIState:
    assert inferred_plan[Identifier(module=None, name='b')] == DistrEnc.symbolic
  elif method == AbsDSState:
    assert inferred_plan[Identifier(module=None, name='b')] == DistrEnc.sample

if __name__ == '__main__':
  pytest.main()