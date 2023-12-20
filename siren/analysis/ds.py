from typing import Any, Set
from copy import copy
from siren.grammar import AbsRandomVar

import siren.analysis.conjugate as conj
from siren.analysis.interface import *
from siren.inference.ds import DSType

class AbsDSState(AbsSymState):
  ###
  # State entry:
  #   rv: (pv, distribution, type, children, parent, cdistr)
  ###

  def __str__(self):
    s = '\n\t'.join(map(str, self.state.items()))
    return f"AbsDSState(\n\t{s}\n)" if s else "AbsDSState()"
  
  ## Accessors
  def type_(self, rv: AbsRandomVar) -> DSType:
    return self.get_entry(rv, 'type')
  
  def children(self, rv: AbsRandomVar) -> List[AbsRandomVar]:
    return self.get_entry(rv, 'children')
  
  def parent(self, rv: AbsRandomVar) -> Optional[AbsRandomVar]:
    return self.get_entry(rv, 'parent')
  
  def cdistr(self, rv: AbsRandomVar) -> Optional[AbsSymDistr]:
    return self.get_entry(rv, 'cdistr')
  
  ## Mutators
  def set_type(self, rv: AbsRandomVar, type_: DSType) -> None:
    self.set_entry(rv, type=type_)

  def set_children(self, rv: AbsRandomVar, children: List[AbsRandomVar]) -> None:
    self.set_entry(rv, children=children)

  def set_parent(self, rv: AbsRandomVar, parent: Optional[AbsRandomVar]) -> None:
    self.set_entry(rv, parent=parent)

  def set_cdistr(self, rv: AbsRandomVar, cdistr: Optional[AbsSymDistr]) -> None:
    self.set_entry(rv, cdistr=cdistr)

  def entry_referenced_rvs(self, rvs: Set[AbsRandomVar]) -> Set[AbsRandomVar]:
    ref_rvs = super().entry_referenced_rvs(rvs)
    for rv in rvs:
      for rv_child in self.children(rv):
        ref_rvs.add(rv_child)
      
      rv_par = self.parent(rv)
      if rv_par is not None:
        ref_rvs.add(rv_par)

      cdistr = self.cdistr(rv)
      if cdistr is not None:
        ref_rvs = ref_rvs.union(cdistr.rvs())

    return ref_rvs
  
  def entry_rename_rv(self, rv: AbsRandomVar, old: AbsRandomVar, new: AbsRandomVar) -> None:
    super().entry_rename_rv(rv, old, new)
    self.set_children(rv, [rv_child if rv_child != old else new for rv_child in self.children(rv)])
    self.set_parent(rv, new if self.parent(rv) == old else self.parent(rv))
    cdistr = self.cdistr(rv)
    if cdistr is not None:
      self.set_cdistr(rv, cdistr.rename(old, new))

  def entry_join(self, rv: AbsRandomVar, other: 'AbsDSState') -> None:
    super().entry_join(rv, other)

    self.set_children(rv, self.children(rv) + other.children(rv))

    self_par = self.parent(rv)
    other_par = other.parent(rv)
    if self_par is None:
      rv_par = other_par
    elif other_par is None:
      rv_par = self_par
    else:
      # Can only maintain a single parent
      rv_par = self.narrow_join_expr(self_par, other_par, copy(self))
      # Joining random vars only, so should get back a random var
      assert isinstance(rv_par, AbsRandomVar)
    
    self.set_parent(rv, rv_par)
    cdistr = self.cdistr(rv)
    other_cdistr = other.cdistr(rv)
    if cdistr is not None and other_cdistr is not None:
      self.set_cdistr(rv, join_distr(cdistr, other_cdistr))
    elif cdistr is None and other_cdistr is not None:
      self.set_cdistr(rv, other_cdistr)

  ### Symbolic Interface ###
  def assume(self, name: Optional[Identifier], annotation: Optional[Annotation], distribution: AbsSymDistr[T]) -> AbsRandomVar[T]:
    def _check_conjugacy(prior : AbsSymDistr, likelihood : AbsSymDistr, rv_par : AbsRandomVar, rv_child : AbsRandomVar) -> bool:
      match prior, likelihood:
        case AbsNormal(_), AbsNormal(_):
          return conj.gaussian_conjugate_check(self, prior, likelihood, rv_par, rv_child) or \
            conj.normal_inverse_gamma_normal_conjugate_check(self, prior, likelihood, rv_par, rv_child)
        case AbsBernoulli(_), AbsBernoulli(_):
          return conj.bernoulli_conjugate_check(self, prior, likelihood, rv_par, rv_child)
        case AbsBeta(_), AbsBernoulli(_):
          return conj.beta_bernoulli_conjugate_check(self, prior, likelihood, rv_par, rv_child)
        case AbsBeta(_), AbsBinomial(_):
          return conj.beta_binomial_conjugate_check(self, prior, likelihood, rv_par, rv_child)
        case AbsGamma(_), AbsPoisson(_):
          return conj.gamma_poisson_conjugate_check(self, prior, likelihood, rv_par, rv_child)
        case AbsGamma(_), AbsNormal(_):
          return conj.gamma_normal_conjugate_check(self, prior, likelihood, rv_par, rv_child)
        case _:
          return False

    rv = self.new_var()
    pv = {name} if name is not None else set()
    if annotation is not None:
      if name is None:
        raise ValueError('Cannot annotate anonymous variable')
      else:
        self.annotations[name] = annotation
    distribution = self.eval_distr(distribution)
    
    children = []
    canonical_parent = None
    cdistr = None
    if len(distribution.rvs()) == 0:
      dstype = DSType.MARGINALIZED
    else:
      dstype = DSType.INITIALIZED

      parents = []
      for rv_par in distribution.rvs():
        if rv_par not in parents:
          parents.append(rv_par)

      # keep if conjugate, else sample it
      has_parent = False
      for rv_par in parents:
        if not has_parent:
          if self.type_(rv_par) == DSType.REALIZED:
            distribution = self.eval_distr(distribution)
            continue

          if self.type_(rv_par) == DSType.MARGINALIZED:
            parent_dist = self.distr(rv_par)
          else:
            parent_dist = self.cdistr(rv_par)
            if parent_dist is None:
              raise ValueError(f'{rv_par} is Initialized but has no conditional distribution')

          if _check_conjugacy(parent_dist, distribution, rv_par, rv):
            self.children(rv_par).append(rv)

            canonical_parent = rv_par
            has_parent = True
            continue

        self.value(rv_par)
        distribution = self.eval_distr(distribution)

      # all parents were sampled
      if len(distribution.rvs()) == 0:
        dstype = DSType.MARGINALIZED

    if dstype == DSType.INITIALIZED:
      cdistr = distribution

    pv = {name} if name is not None else set()
    self.set_pv(rv, pv)
    self.set_distr(rv, distribution)
    self.set_type(rv, dstype)
    self.set_children(rv, children)
    self.set_parent(rv, canonical_parent)
    self.set_cdistr(rv, cdistr)
          
    return rv

  def observe(self, rv: AbsRandomVar[T], value: AbsConst[T]) -> None:
    # Turn rv into a terminal node
    self.graft(rv)
    # observe
    assert self.type_(rv) == DSType.MARGINALIZED
    self.realize(rv, AbsDelta(value, sampled=False))
    return

  def value_impl(self, rv: AbsRandomVar[T]) -> AbsConst[T]:
    # Turn rv into terminal node
    self.graft(rv)
    return self.do_sample(rv)
  
  # Make rv marginal
  def marginalize(self, rv: AbsRandomVar) -> None:
    match self.type_(rv):
      case DSType.REALIZED:
        return
      case DSType.MARGINALIZED:
        if len(self.children(rv)) > 0:
          self.graft(rv)
        return
      case DSType.INITIALIZED:
        rv_par = self.parent(rv)
        if rv_par is None:
          raise ValueError(f'Cannot marginalize {rv} because it has no parent')
        
        match self.type_(rv_par):
          case DSType.REALIZED:
            self.eval_entry(rv)
          case DSType.MARGINALIZED:
            if not self.make_marginal(rv_par, rv):
              self.value(rv_par)
              self.eval_entry(rv)
            else:
              raise ValueError(f'Marginalizing {rv} is not possible')
          case DSType.INITIALIZED:
            self.marginalize(rv_par)
            if not self.make_marginal(rv_par, rv):
              self.value(rv_par)
              self.eval_entry(rv)
            else:
              raise ValueError(f'Marginalizing {rv} is not possible')

        if len(self.children(rv)) > 0:
            self.graft(rv)
    
  ########################################################################

  def eval_entry(self, rv: AbsRandomVar) -> None:
    self.set_distr(rv, self.eval_distr(self.distr(rv)))

  # moves rv from I to M and updates its distribution by marginalizing over its parent
  def do_marginalize(self, rv: AbsRandomVar) -> None:
    assert self.type_(rv) == DSType.INITIALIZED
    rv_par = self.parent(rv)
    if rv_par is None:
      raise ValueError(f'Cannot marginalize {rv} because it has no parent')
    
    match self.type_(rv_par):
      case DSType.INITIALIZED:
        raise ValueError(f'Cannot marginalize {rv} because {rv_par} is not marginalized')
      case DSType.REALIZED:
        d = self.distr(rv_par)
        if not isinstance(d, Delta):
          raise ValueError(d)
        # convert to marginal
        self.eval_entry(rv)
      case DSType.MARGINALIZED:
        if self.make_marginal(rv_par, rv):
          self.eval_entry(rv)
        else:
          self.value(rv_par)
          self.eval_entry(rv)
          # raise ValueError(f'Cannot marginalize {rv} because {rv_par} is not conjugate')

    self.set_type(rv, DSType.MARGINALIZED)

  def do_sample(self, rv: AbsRandomVar) -> AbsConst:
    # sample
    assert self.type_(rv) == DSType.MARGINALIZED
    self.realize(rv, AbsDelta(AbsConst(UnkC()), sampled=True))
    return AbsConst(UnkC())

  def intervene(self, rv: AbsRandomVar[T], v: AbsDelta[T]) -> None:
    self.set_distr(rv, v)

  # Invariant 2: A node always has at most one child that is marginalized
  def marginal_child(self, rv: AbsRandomVar) -> Optional[AbsRandomVar]:
    for rv_child in self.children(rv):
      if self.type_(rv_child) == DSType.MARGINALIZED:
        return rv_child
      
    return None
  
  def make_marginal(self, rv_par: AbsRandomVar, rv_child: AbsRandomVar) -> bool:
    def _update(marginal: Optional[AbsSymDistr]) -> bool:
      if marginal is None:
        return False

      self.set_distr(rv_child, self.eval_distr(marginal))
      return True

    prior = self.distr(rv_par)
    likelihood = self.cdistr(rv_child)
    match prior, likelihood:
      case AbsNormal(_), AbsNormal(_):
        if _update(conj.gaussian_marginal(self, prior, likelihood, rv_par, rv_child)):
          return True
        else:
          return _update(conj.normal_inverse_gamma_normal_marginal(self, prior, likelihood, rv_par, rv_child))
      case AbsBernoulli(_), AbsBernoulli(_):
        return _update(conj.bernoulli_marginal(self, prior, likelihood, rv_par, rv_child))
      case AbsBeta(_), AbsBernoulli(_):
        return _update(conj.beta_bernoulli_marginal(self, prior, likelihood, rv_par, rv_child))
      case AbsBeta(_), AbsBinomial(_):
        return _update(conj.beta_binomial_marginal(self, prior, likelihood, rv_par, rv_child))
      case AbsGamma(_), AbsPoisson(_):
        return _update(conj.gamma_poisson_marginal(self, prior, likelihood, rv_par, rv_child))
      case AbsGamma(_), AbsNormal(_):
        return _update(conj.gamma_normal_marginal(self, prior, likelihood, rv_par, rv_child))
      case _:
        return False

  def make_conditional(self, rv_par: AbsRandomVar, rv_child: AbsRandomVar, x: AbsSymExpr) -> bool:
    def _update(posterior: Optional[AbsSymDistr]) -> bool:
      if posterior is None:
        return False
      
      posterior = self.eval_distr(posterior)
      self.set_distr(rv_par, posterior)
      # Update original distr
      return True

    prior = self.distr(rv_par)
    likelihood = self.cdistr(rv_child)
    match prior, likelihood:
      case AbsNormal(_), AbsNormal(_):
        if _update(conj.gaussian_posterior(self, prior, likelihood, rv_par, rv_child, x)):
          return True
        else:
          return _update(conj.normal_inverse_gamma_normal_posterior(self, prior, likelihood, rv_par, rv_child, x))
      case AbsBernoulli(_), AbsBernoulli(_):
        return _update(conj.bernoulli_posterior(self, prior, likelihood, rv_par, rv_child, x))
      case AbsBeta(_), AbsBernoulli(_):
        return _update(conj.beta_bernoulli_posterior(self, prior, likelihood, rv_par, rv_child, x))
      case AbsBeta(_), AbsBinomial(_):
        return _update(conj.beta_binomial_posterior(self, prior, likelihood, rv_par, rv_child, x))
      case AbsGamma(_), AbsPoisson(_):
        return _update(conj.gamma_poisson_posterior(self, prior, likelihood, rv_par, rv_child, x))
      case AbsGamma(_), AbsNormal(_):
        return _update(conj.gamma_normal_posterior(self, prior, likelihood, rv_par, rv_child, x))
      case _:
        return False

  def realize(self, rv: AbsRandomVar, x: AbsDelta) -> None:
    assert self.type_(rv) == DSType.MARGINALIZED
    self.set_type(rv, DSType.REALIZED)

    rv_par = self.parent(rv)
    if rv_par is not None:
      # condition parent on child
      if self.make_conditional(rv_par, rv, x.v):
        self.set_type(rv_par, DSType.MARGINALIZED)
        self.children(rv_par).remove(rv)
        self.set_parent(rv, None)
      else:
        self.value(rv_par)
        self.eval_distr(self.distr(rv))
        # raise ValueError(f'Cannot realize {rv} because {rv_par} is not conjugate')

    self.intervene(rv, x)
    # new roots from children
    for rv_child in self.children(rv):
      self.do_marginalize(rv_child)
      self.set_parent(rv_child, None)

    self.set_children(rv, [])
    
  def graft(self, rv: AbsRandomVar) -> None:
    if self.type_(rv) == DSType.MARGINALIZED:
      rv_child = self.marginal_child(rv)
      if rv_child is not None:
        self.prune(rv_child)
    elif self.type_(rv) == DSType.INITIALIZED:
      rv_par = self.parent(rv)
      if rv_par is None:
        raise ValueError(f'Cannot graft {rv} because it has no parent')
      self.graft(rv_par)
      self.do_marginalize(rv)
    else:
      raise ValueError(f'Cannot graft {rv} because it is already realized')

  def prune(self, rv: AbsRandomVar) -> None:
    assert self.type_(rv) == DSType.MARGINALIZED
    rv_child = self.marginal_child(rv)
    if rv_child is not None:
      self.prune(rv_child)
    
    self.value(rv)