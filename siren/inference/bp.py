from typing import Any, Set
from copy import copy
from collections import deque

import siren.inference.conjugate as conj
from siren.inference.interface import *

@dataclass(frozen=True)
class BPNode():
  pass

@dataclass(frozen=True)
class BPRealized(BPNode):
  def __str__(self) -> str:
    return 'BPRealized'

@dataclass(frozen=True)
class BPMarginalized(BPNode):
  def __str__(self):
    return f'BPMarginalized()'

@dataclass(frozen=True)
class BPInitialized(BPNode):
  parent: RandomVar

  def __str__(self):
    return f'BPInitialized({self.parent})'

class BPState(SymState): 
  ###
  # State entry:
  #   rv: (pv, distribution, node)
  ###

  def __str__(self):
    s = '\n\t'.join(map(str, self.state.items()))
    return f"BPState(\n\t{s}\n)" if s else "BPState()"
  
  ## Accessors
  def node(self, rv: RandomVar) -> BPNode:
    return self.get_entry(rv, 'node')

  ## Mutators
  def set_node(self, rv: RandomVar, node: BPNode) -> None:
    self.set_entry(rv, node=node)

  def assume(self, name: Optional[Identifier], annotation: Optional[Annotation], distribution: SymDistr[T]) -> RandomVar[T]:
    def _check_conjugacy(prior : SymDistr, likelihood : SymDistr, rv_par : RandomVar, rv_child : RandomVar) -> bool:
      match prior, likelihood:
        case Normal(_), Normal(_):
          return conj.gaussian_conjugate_check(self, prior, likelihood, rv_par, rv_child)
        case _:
          return False

    rv = self.new_var()
    if annotation is not None:
      if name is None:
        raise ValueError('Cannot annotate anonymous variable')
      else:
        self.annotations[name] = annotation
    distribution = self.eval_distr(distribution)

    if len(distribution.rvs()) == 0:
      node = BPMarginalized()
    else:
      parents = []
      for rv_par in distribution.rvs():
        if rv_par not in parents:
          parents.append(rv_par)

      # keep if conjugate, else sample it
      canonical_parent = None
      has_parent = False
      for rv_par in parents:
        if rv_par not in distribution.rvs():
          continue
        if not has_parent:
          match self.node(rv_par):
            case BPRealized():
              distribution = self.eval_distr(distribution)
              continue
            case _: # BPInitialized or BPMarginalized
              if _check_conjugacy(self.distr(rv_par), distribution, rv_par, rv):
                canonical_parent = rv_par
                has_parent = True
                continue
        self.value(rv_par)
        distribution = self.eval_distr(distribution)

      # all parents were sampled
      if len(distribution.rvs()) == 0:
        node = BPMarginalized()
      else:
        assert canonical_parent is not None
        node = BPInitialized(canonical_parent)

    self.set_pv(rv, name)
    self.set_distr(rv, distribution)
    self.set_node(rv, node)

    return rv

  def observe(self, rv: RandomVar[T], value: Const[T]) -> float:
    match self.node(rv):
      case BPRealized():
        raise ValueError(f'Cannot observe {rv} twice')
      case BPMarginalized():
        s = self.score(rv, value.v)
        self.intervene(rv, Delta(value, sampled=False))
        return s
      case BPInitialized(rv_par):
        assert rv_par is not None
        self.marginalize(rv_par)

        if self.condition_cd(rv_par, rv):
          s = self.score(rv, value.v)
          self.intervene(rv, Delta(value, sampled=False))
          self.set_distr(rv_par, self.eval_distr(self.distr(rv_par)))
          self.set_node(rv_par, BPMarginalized())
          return s
        else:
          self.value(rv_par)
          return self.observe(rv, value)
      case _:
        raise ValueError(f'{rv} is {self.node(rv)}')

  def value(self, rv: RandomVar[T]) -> Const[T]:
    self.marginalize(rv)
    match self.node(rv):
      case BPInitialized(_):
        raise ValueError(f'{rv} is {self.node(rv)}')
      case _:
        v = self.draw(rv)
        self.intervene(rv, Delta(Const(v), sampled=True))
        return Const(v)
  
  # make rv a root
  # postcondition: rv is not BPInitialized
  def marginalize(self, rv: RandomVar) -> None:
    match self.node(rv):
      case BPInitialized(rv_par):
        self.marginalize(rv_par)

        if self.condition_cd(rv_par, rv):
          return
        else:
          self.value(rv_par)
          self.marginalize(rv)

  ########################################################################

  def score(self, rv: RandomVar[T], v: T) -> float:
    return self.distr(rv).score(v)

  def draw(self, rv: RandomVar) -> Any:
    return self.distr(rv).draw(self.rng)

  def intervene(self, rv: RandomVar[T], v: Delta[T]) -> None:
    self.set_node(rv, BPRealized())
    self.set_distr(rv, v)

  def condition_cd(self, rv_par: RandomVar, rv_child: RandomVar) -> bool:
    def _update(marginal_posterior: Optional[Tuple[SymDistr, SymDistr]]) -> bool:
      if marginal_posterior is None:
        return False
      
      marginal, posterior = marginal_posterior
      self.set_distr(rv_par, posterior)
      self.set_distr(rv_child, marginal)

      self.set_node(rv_child, BPMarginalized())
      
      match self.node(rv_par):
        case BPRealized():
          pass
        case _:
          self.set_node(rv_par, BPInitialized(rv_child))

      return True

    match self.distr(rv_par), self.distr(rv_child):
      case Delta(v, sampled), cdistr:
        return _update((self.eval_distr(cdistr), Delta(v, sampled)))
      case Normal(_), Normal(_):
        return _update(conj.gaussian_conjugate(self, rv_par, rv_child))
          # return True
      #   else:
      #     return _update(conj.normal_inverse_gamma_normal_conjugate(self, rv_par, rv_child))
      # case Bernoulli(_), Bernoulli(_):
      #   return _update(conj.bernoulli_conjugate(self, rv_par, rv_child))
      # case Beta(_), Bernoulli(_):
      #   return _update(conj.beta_bernoulli_conjugate(self, rv_par, rv_child))
      # case Beta(_), Binomial(_):
      #   return _update(conj.beta_binomial_conjugate(self, rv_par, rv_child))
      # case Gamma(_), Poisson(_):
      #   return _update(conj.gamma_poisson_conjugate(self, rv_par, rv_child))
      # case Gamma(_), Normal(_):
      #   return _update(conj.gamma_normal_conjugate(self,rv_par, rv_child))
      case _:
        return False