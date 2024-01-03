from typing import Any, Set
from copy import copy
from collections import deque

import siren.analysis.conjugate as conj
from siren.analysis.interface import *
from siren.inference.bp import BPType

class AbsBPState(AbsSymState):
  ###
  # State entry:
  #   rv: (pv, distribution, type, parent)
  ###
  def __str__(self):
    s = '\n\t'.join(map(str, self.state.items()))
    return f"AbsBPState(\n\t{s}\n)" if s else "AbsBPState()"
  
  ## Accessors
  def type_(self, rv: AbsRandomVar) -> BPType:
    return self.get_entry(rv, 'type')
  
  def parent(self, rv: AbsRandomVar) -> Optional[AbsRandomVar]:
    return self.get_entry(rv, 'parent')

  ## Mutators
  def set_type(self, rv: AbsRandomVar, type_: BPType) -> None:
    self.set_entry(rv, type=type_)

  def set_parent(self, rv: AbsRandomVar, parent: Optional[AbsRandomVar]) -> None:
    self.set_entry(rv, parent=parent)

  def entry_referenced_rvs(self, rvs: Set[AbsRandomVar]) -> Set[AbsRandomVar]:
    ref_rvs = super().entry_referenced_rvs(rvs)
    for rv in rvs:
      rv_par = self.parent(rv)
      if rv_par is not None:
        ref_rvs.add(rv_par)

    return ref_rvs
  
  def entry_rename_rv(self, rv: AbsRandomVar, old: AbsRandomVar, new: AbsRandomVar) -> None:
    super().entry_rename_rv(rv, old, new)
    self.set_parent(rv, new if self.parent(rv) == old else self.parent(rv))

  def entry_join(self, rv: AbsRandomVar, other: 'AbsBPState') -> None:
    super().entry_join(rv, other)

    self_par = self.parent(rv)
    other_par = other.parent(rv)
    if self_par is None:
      rv_par = other_par
    elif other_par is None:
      rv_par = self_par
    else:
      # Can only maintain a single parent
      # TODO
      rv_par = self.narrow_join_expr(self_par, other_par, copy(self))
      # Joining random vars only, so should get back a random var
      assert isinstance(rv_par, AbsRandomVar)
      # TODO: Type
    
    self.set_parent(rv, rv_par)
  
  ### Symbolic Interface ###
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

    canonical_parent = None
    if len(distribution.rvs()) == 0:
      self.set_type(rv, BPType.MARGINALIZED)
    else:
      self.set_type(rv, BPType.INITIALIZED)

      parents = []
      for rv_par in distribution.rvs():
        if rv_par not in parents:
          parents.append(rv_par)

      # keep if conjugate, else sample it
      has_parent = False
      for rv_par in parents:
        if not has_parent and _check_conjugacy(self.distr(rv_par), distribution, rv_par, rv):
          canonical_parent = rv_par
          has_parent = True
        else:
          self.value(rv_par)
          distribution = self.eval_distr(distribution)

      # all parents were sampled
      if len(distribution.rvs()) == 0:
          self.set_type(rv, BPType.MARGINALIZED)

    pv = {name} if name is not None else set()
    self.set_pv(rv, pv)
    self.set_distr(rv, distribution)
    self.set_parent(rv, canonical_parent)

    return rv

  def observe(self, rv: AbsRandomVar[T], value: AbsConst[T]) -> None:
    match self.type_(rv):
      case BPType.REALIZED:
        raise ValueError(f'Cannot observe {rv} twice')
      case BPType.MARGINALIZED:
        self.intervene(rv, AbsDelta(value, sampled=False))
      case BPType.INITIALIZED:
        rv_par = self.parent(rv)
        assert rv_par is not None
        self.marginalize(rv_par)

        if self.condition_cd(rv_par, rv):
          self.intervene(rv, AbsDelta(value, sampled=False))
          self.set_distr(rv_par, self.eval_distr(self.distr(rv_par)))
          self.set_type(rv_par, BPType.MARGINALIZED)
        else:
          self.value(rv_par)
          self.observe(rv, value)

  def value_impl(self, rv: AbsRandomVar[T]) -> AbsConst[T]:
    self.marginalize(rv)
    assert self.type_(rv) != BPType.INITIALIZED
    self.intervene(rv, AbsDelta(AbsConst(UnkC()), sampled=True))
    return AbsConst(UnkC())
  
  # make rv a root
  def marginalize(self, rv: AbsRandomVar) -> None:
    match self.type_(rv):
      case BPType.MARGINALIZED:
        return
      case BPType.REALIZED:
        return
      case BPType.INITIALIZED:
        rv_par = self.parent(rv)
        assert rv_par is not None
        self.marginalize(rv_par)

        if self.condition_cd(rv_par, rv):
          return
        else:
          self.value(rv_par)
          self.marginalize(rv)

  ########################################################################

  def intervene(self, rv: AbsRandomVar[T], v: AbsDelta[T]) -> None:
    self.set_type(rv, BPType.REALIZED)
    self.set_distr(rv, v)

  def condition_cd(self, rv_par: AbsRandomVar, rv_child: AbsRandomVar) -> bool:
    def _update(marginal_posterior: Optional[Tuple[AbsSymDistr, AbsSymDistr]]) -> bool:
      if marginal_posterior is None:
        return False
      
      marginal, posterior = marginal_posterior
      self.set_distr(rv_par, posterior)
      self.set_distr(rv_child, marginal)

      self.set_type(rv_child, BPType.MARGINALIZED)
      self.set_parent(rv_child, None)

      if self.type_(rv_par) != BPType.REALIZED:
        self.set_type(rv_par, BPType.INITIALIZED)
        self.set_parent(rv_par, rv_child)

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