from typing import Any, Set
from copy import copy
from collections import deque

import siren.analysis.conjugate as conj
from siren.analysis.interface import *
from siren.inference.bp import BPType

class AbsBPState(AbsSymState):
  def __init__(self) -> None:
    super().__init__()
    self.type : Dict[AbsRandomVar, BPType] = {}
    self.parent : Dict[AbsRandomVar, AbsRandomVar] = {}

  def __copy__(self):
    new_state = super().__copy__()
    new_state.type = copy(self.type)
    new_state.parent = copy(self.parent)
    return new_state
  
  def __str__(self):
    s = '\n\t'.join(map(str, self.state.items()))
    return f"AbsBPState(\n\t{s}\n)" if s else "AbsBPState()"

  def assume(self, name: Optional[Identifier], annotation: Optional[Annotation], distribution: AbsSymDistr[T]) -> AbsRandomVar[T]:
    def _check_conjugacy(prior : AbsSymDistr, likelihood : AbsSymDistr, rv_par : AbsRandomVar, rv_child : AbsRandomVar) -> bool:
      match prior, likelihood:
        case AbsNormal(_), AbsNormal(_):
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

    pv = {name} if name is not None else set()
    self.state[rv] = (pv, distribution)

    return rv

  def observe(self, rv: AbsRandomVar[T], value: AbsConst[T]) -> None:
    match self.type[rv]:
      case BPType.REALIZED:
        raise ValueError(f'Cannot observe {rv} twice')
      case BPType.MARGINALIZED:
        self.intervene(rv, AbsDelta(value, sampled=False))
      case BPType.INITIALIZED:
        assert rv in self.parent
        rv_par = self.parent[rv]
        self.marginalize(rv_par)

        if self.condition_cd(rv_par, rv):
          self.intervene(rv, AbsDelta(value, sampled=False))
          self.set_distr(rv_par, self.eval_distr(self.distr(rv_par)))
          self.type[rv_par] = BPType.MARGINALIZED
        else:
          self.value(rv_par)
          self.observe(rv, value)

  def value_impl(self, rv: AbsRandomVar[T]) -> AbsConst[T]:
    self.marginalize(rv)
    assert self.type[rv] != BPType.INITIALIZED
    self.intervene(rv, AbsDelta(AbsConst(UnkC()), sampled=True))
    return AbsConst(UnkC())
  
  # make rv a root
  def marginalize(self, rv: AbsRandomVar) -> None:
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

  def intervene(self, rv: AbsRandomVar[T], v: AbsDelta[T]) -> None:
    self.type[rv] = BPType.REALIZED
    self.set_distr(rv, v)

  def condition_cd(self, rv_par: AbsRandomVar, rv_child: AbsRandomVar) -> bool:
    def _update(marginal_posterior: Optional[Tuple[AbsSymDistr, AbsSymDistr]]) -> bool:
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
      case AbsDelta(v), cdistr:
        return _update((AbsDelta(v), self.eval_distr(cdistr)))
      case AbsNormal(_), AbsNormal(_):
        return _update(conj.gaussian_conjugate(self, rv_par, rv_child))
          # return True
      #   else:
      #     return _update(conj.AbsNormal_inverse_gamma_AbsNormal_conjugate(self, rv_par, rv_child))
      # case Bernoulli(_), Bernoulli(_):
      #   return _update(conj.bernoulli_conjugate(self, rv_par, rv_child))
      # case Beta(_), Bernoulli(_):
      #   return _update(conj.beta_bernoulli_conjugate(self, rv_par, rv_child))
      # case Beta(_), Binomial(_):
      #   return _update(conj.beta_binomial_conjugate(self, rv_par, rv_child))
      # case Gamma(_), Poisson(_):
      #   return _update(conj.gamma_poisson_conjugate(self, rv_par, rv_child))
      # case Gamma(_), AbsNormal(_):
      #   return _update(conj.gamma_AbsNormal_conjugate(self,rv_par, rv_child))
      case _:
        return False