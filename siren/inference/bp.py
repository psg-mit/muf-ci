from typing import Any, Set
from copy import copy
from collections import deque

import siren.inference.conjugate as conj
from siren.inference.interface import *

class BPType(Enum):
  INITIALIZED = 1
  MARGINALIZED = 2
  REALIZED = 3

class BPState(SymState):
  def __init__(self, seed=None) -> None:
    super().__init__(seed)
    self.type : Dict[RandomVar, BPType] = {}
    self.parent : Dict[RandomVar, RandomVar] = {}

  def __copy__(self):
    new_state = super().__copy__()
    new_state.type = copy(self.type)
    new_state.parent = copy(self.parent)
    return new_state
  
  def __str__(self):
    s = '\n\t'.join(map(str, self.state.items()))
    return f"BPState(\n\t{s}\n)" if s else "BPState()"

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
      self.type[rv] = BPType.MARGINALIZED
    else:
      self.type[rv] = BPType.INITIALIZED

      parents = []
      for rv_par in distribution.rvs():
        if rv_par not in parents:
          parents.append(rv_par)

      # keep if conjugate, else sample it
      has_parent = False
      for rv_par in parents:
        if not has_parent and _check_conjugacy(self.distr(rv_par), distribution, rv_par, rv):

          self.parent[rv] = rv_par
          has_parent = True
        else:
          self.value(rv_par)
          distribution = self.eval_distr(distribution)

      # all parents were sampled
      if len(distribution.rvs()) == 0:
          self.type[rv] = BPType.MARGINALIZED

    self.state[rv] = (name, distribution)

    return rv

  def observe(self, rv: RandomVar[T], value: Const[T]) -> float:
    match self.type[rv]:
      case BPType.REALIZED:
        raise ValueError(f'Cannot observe {rv} twice')
      case BPType.MARGINALIZED:
        s = self.score(rv, value.v)
        self.intervene(rv, Delta(value, sampled=False))
        return s
      case BPType.INITIALIZED:
        assert rv in self.parent
        rv_par = self.parent[rv]
        self.marginalize(rv_par)

        if self.condition_cd(rv_par, rv):
          s = self.score(rv, value.v)
          self.intervene(rv, Delta(value, sampled=False))
          self.set_distr(rv_par, self.eval_distr(self.distr(rv_par)))
          self.type[rv_par] = BPType.MARGINALIZED
          return s
        else:
          self.value(rv_par)
          return self.observe(rv, value)

  def value(self, rv: RandomVar[T]) -> Const[T]:
    self.marginalize(rv)
    assert self.type[rv] != BPType.INITIALIZED
    v = self.draw(rv)
    self.intervene(rv, Delta(Const(v), sampled=True))
    return Const(v)
  
  # make rv a root
  def marginalize(self, rv: RandomVar) -> None:
    match self.type[rv]:
      case BPType.MARGINALIZED:
        return
      case BPType.REALIZED:
        return
      case BPType.INITIALIZED:
        assert rv in self.parent
        rv_par = self.parent[rv]
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
    self.type[rv] = BPType.REALIZED
    self.set_distr(rv, v)

  def condition_cd(self, rv_par: RandomVar, rv_child: RandomVar) -> bool:
    def _update(marginal_posterior: Optional[Tuple[SymDistr, SymDistr]]) -> bool:
      if marginal_posterior is None:
        return False
      
      marginal, posterior = marginal_posterior
      self.set_distr(rv_par, posterior)
      self.set_distr(rv_child, marginal)

      self.type[rv_child] = BPType.MARGINALIZED
      del self.parent[rv_child]

      if self.type[rv_par] != BPType.REALIZED:
        self.type[rv_par] = BPType.INITIALIZED
        self.parent[rv_par] = rv_child

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