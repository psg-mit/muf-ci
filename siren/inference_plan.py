from enum import Enum
from typing import Any, Optional, Dict

from siren.grammar import Annotation, Identifier
from siren.inference.interface import ProbState, Particle

class DistrEnc(Enum):
  symbolic = 1
  sample = 2
  dynamic = 3

  def __lt__(self, other):
    match self, other:
      case DistrEnc.symbolic, DistrEnc.dynamic:
        return True
      case DistrEnc.sample, DistrEnc.dynamic:
        return True
      case _, _:
        return False
      
  def __or__(self, __value: 'DistrEnc') -> 'DistrEnc':
    if self == __value:
      return self
    else:
      return DistrEnc.dynamic
    
  def __str__(self):
    match self:
      case DistrEnc.symbolic:
        return "Symbolic"
      case DistrEnc.sample:
        return "Sample"
      case DistrEnc.dynamic:
        return "Dynamic"
      case _:
        raise ValueError(self)
      
  def __repr__(self) -> str:
    return self.__str__()
  

  @staticmethod
  def from_annotation(ann: Optional[Annotation]) -> 'DistrEnc':
    if ann is None:
      return DistrEnc.dynamic
    match ann:
      case Annotation.symbolic:
        return DistrEnc.symbolic
      case Annotation.sample:
        return DistrEnc.sample
      case _:
        raise ValueError(ann)
      
class InferencePlan(object):
  def __init__(self, plan=None):
    super().__init__()
    self.plan: Dict[Identifier, DistrEnc] = plan if plan is not None else {}

  def __str__(self):
    s = '\n'.join(f"{k}: {v.name}" for k, v in self.plan.items())
    return f"{s}"
  
  def __repr__(self) -> str:
    return self.__str__()
  
  def __contains__(self, __key: Any) -> bool:
    return __key in self.plan
  
  def __getitem__(self, __key: Any) -> Any:
    return self.plan[__key]
   
  def __setitem__(self, __key: Any, __value: Any) -> None:
    if __key in self.plan and self.plan[__key] == DistrEnc.dynamic:
      return
    
    self.plan[__key] = __value

  def __iter__(self):
    return iter(self.plan)
  
  def __len__(self) -> int:
    return len(self.plan)
  
  def __lt__(self, other):
    lt = True
    for x in self.plan:
      if x not in other.plan:
        continue
      if self.plan[x] > other.plan[x]:
        lt = False
        break

    return lt
  
  def __eq__(self, other):
    eq = True
    for x in self.plan:
      if x not in other.plan or self.plan[x] != other.plan[x]:
        eq = False
        break

    return eq

  def __or__(self, __value: Any) -> 'InferencePlan':
    if isinstance(__value, InferencePlan):
      res = InferencePlan()
      for x in self.plan:
        if x in __value.plan:
          res[x] = self.plan[x] | __value.plan[x]
        else:
          res[x] = self.plan[x]
      for x in __value.plan:
        if x not in self.plan:
          res[x] = __value.plan[x]
      return res
    else:
      raise ValueError(__value)
    
def distribution_encodings(particle: Particle) -> InferencePlan:
  inf_plan = InferencePlan()
  for rv in particle.state:
    pv = particle.state.get_entry(rv, 'pv')
    if pv is None:
      continue

    if particle.state.is_sampled(rv):
      inf_plan[pv] = DistrEnc.sample
    else:
      if pv in inf_plan and inf_plan[pv] == DistrEnc.sample:
        continue
      inf_plan[pv] = DistrEnc.symbolic
  return inf_plan
    
def runtime_inference_plan(prob: ProbState) -> InferencePlan:
    inf_plan = InferencePlan()
    for particle in prob.particles:
      ip = distribution_encodings(particle)
      inf_plan = inf_plan | ip
    return inf_plan