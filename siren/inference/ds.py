from typing import Any, Set
from copy import copy
from siren.grammar import RandomVar

import siren.inference.conjugate as conj
from siren.inference.interface import *

class DSType(Enum):
  INITIALIZED = 1
  MARGINALIZED = 2
  REALIZED = 3

class DSState(SymState):
  ###
  # State entry:
  #   rv: (pv, distribution, type, children, parent, cdistr)
  ###
  
  def type_(self, rv: RandomVar) -> DSType:
    return self.get_entry(rv, 'type')
  
  def children(self, rv: RandomVar) -> List[RandomVar]:
    return self.get_entry(rv, 'children')
  
  def parent(self, rv: RandomVar) -> Optional[RandomVar]:
    return self.get_entry(rv, 'parent')
  
  def cdistr(self, rv: RandomVar) -> Optional[SymDistr]:
    return self.get_entry(rv, 'cdistr')
  
  def set_pv(self, rv: RandomVar, pv: Optional[Identifier]) -> None:
    self.set_entry(rv, pv=pv)
  
  def set_distr(self, rv: RandomVar, distribution: SymDistr) -> None:
    self.set_entry(rv, distribution=distribution)

  def set_type(self, rv: RandomVar, type_: DSType) -> None:
    self.set_entry(rv, type=type_)

  def set_children(self, rv: RandomVar, children: List[RandomVar]) -> None:
    self.set_entry(rv, children=children)

  def set_parent(self, rv: RandomVar, parent: Optional[RandomVar]) -> None:
    self.set_entry(rv, parent=parent)

  def set_cdistr(self, rv: RandomVar, cdistr: Optional[SymDistr]) -> None:
    self.set_entry(rv, cdistr=cdistr)

  def __copy__(self):
    new_state = super().__copy__()
    # children is a list so needs to be deepcopied
    for rv in self.state:
      new_state.set_children(rv, copy(self.children(rv)))
    return new_state

  def __str__(self):
    s = '\n\t'.join(map(str, self.state.items()))
    return f"DSState(\n\t{s}\n)" if s else "DSState()"

  def assume(self, name: Optional[Identifier], annotation: Optional[Annotation], distribution: SymDistr[T]) -> RandomVar[T]:
    def _check_conjugacy(prior : SymDistr, likelihood : SymDistr, rv_par : RandomVar, rv_child : RandomVar) -> bool:
      match prior, likelihood:
        case Normal(_), Normal(_):
          return conj.gaussian_conjugate_check(self, prior, likelihood, rv_par, rv_child) or \
            conj.normal_inverse_gamma_normal_conjugate_check(self, prior, likelihood, rv_par, rv_child)
        case Bernoulli(_), Bernoulli(_):
          return conj.bernoulli_conjugate_check(self, prior, likelihood, rv_par, rv_child)
        case Beta(_), Bernoulli(_):
          return conj.beta_bernoulli_conjugate_check(self, prior, likelihood, rv_par, rv_child)
        case Beta(_), Binomial(_):
          return conj.beta_binomial_conjugate_check(self, prior, likelihood, rv_par, rv_child)
        case Gamma(_), Poisson(_):
          return conj.gamma_poisson_conjugate_check(self, prior, likelihood, rv_par, rv_child)
        case Gamma(_), Normal(_):
          return conj.gamma_normal_conjugate_check(self, prior, likelihood, rv_par, rv_child)
        case _:
          return False

    rv = self.new_var()
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

    self.set_pv(rv, name)
    self.set_distr(rv, distribution)
    self.set_type(rv, dstype)
    self.set_children(rv, children)
    self.set_parent(rv, canonical_parent)
    self.set_cdistr(rv, cdistr)

    return rv

  def observe(self, rv: RandomVar[T], value: Const[T]) -> float:
    # Turn rv into a terminal node
    self.graft(rv)
    # observe
    assert self.type_(rv) == DSType.MARGINALIZED
    s = self.score(rv, value.v)
    self.realize(rv, Delta(value, sampled=False))
    return s

  def value(self, rv: RandomVar[T]) -> Const[T]:
    # Turn rv into terminal node
    self.graft(rv)
    return self.do_sample(rv)
  
  # Make rv marginal
  def marginalize(self, rv: RandomVar) -> None:
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
              
  def eval_entry(self, rv: RandomVar) -> None:
    self.set_distr(rv, self.eval_distr(self.distr(rv)))

  # moves rv from I to M and updates its distribution by marginalizing over its parent
  def do_marginalize(self, rv: RandomVar) -> None:
    assert self.type_(rv) == DSType.INITIALIZED
    rv_par = self.parent(rv)
    if rv_par is None:
      raise ValueError(f'Cannot marginalize {rv} because it has no parent')
    
    match self.type_(rv_par):
      case DSType.INITIALIZED:
        raise ValueError(f'Cannot marginalize {rv} because {rv_par} is not marginalized')
      case DSType.REALIZED:
        d = self.distr(rv_par)
        # simplify delta
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

  def do_sample(self, rv: RandomVar) -> Const:
    # sample
    assert self.type_(rv) == DSType.MARGINALIZED
    v = self.draw(rv)
    self.realize(rv, Delta(Const(v), sampled=True))
    return Const(v)
    
  def score(self, rv: RandomVar[T], v: T) -> float:
    return self.distr(rv).score(v)

  def draw(self, rv: RandomVar) -> Any:
    return self.distr(rv).draw(self.rng)

  def intervene(self, rv: RandomVar[T], v: Delta[T]) -> None:
    self.set_distr(rv, v)

  # Invariant 2: A node always has at most one child that is marginalized
  def marginal_child(self, rv: RandomVar) -> Optional[RandomVar]:
    for rv_child in self.children(rv):
      if self.type_(rv_child) == DSType.MARGINALIZED:
        return rv_child
      
    return None
  
  def make_marginal(self, rv_par: RandomVar, rv_child: RandomVar) -> bool:
    def _update(marginal: Optional[SymDistr]) -> bool:
      if marginal is None:
        return False

      self.set_distr(rv_child, self.eval_distr(marginal))
      return True

    prior = self.distr(rv_par)
    likelihood = self.cdistr(rv_child)
    match prior, likelihood:
      case Normal(_), Normal(_):
        if _update(conj.gaussian_marginal(self, prior, likelihood, rv_par, rv_child)):
          return True
        else:
          return _update(conj.normal_inverse_gamma_normal_marginal(self, prior, likelihood, rv_par, rv_child))
      case Bernoulli(_), Bernoulli(_):
        return _update(conj.bernoulli_marginal(self, prior, likelihood, rv_par, rv_child))
      case Beta(_), Bernoulli(_):
        return _update(conj.beta_bernoulli_marginal(self, prior, likelihood, rv_par, rv_child))
      case Beta(_), Binomial(_):
        return _update(conj.beta_binomial_marginal(self, prior, likelihood, rv_par, rv_child))
      case Gamma(_), Poisson(_):
        return _update(conj.gamma_poisson_marginal(self, prior, likelihood, rv_par, rv_child))
      case Gamma(_), Normal(_):
        return _update(conj.gamma_normal_marginal(self, prior, likelihood, rv_par, rv_child))
      case _:
        return False
      
  def make_conditional(self, rv_par: RandomVar, rv_child: RandomVar, x: SymExpr) -> bool:
    def _update(posterior: Optional[SymDistr]) -> bool:
      if posterior is None:
        return False
      
      posterior = self.eval_distr(posterior)
      self.set_distr(rv_par, posterior)
      return True

    prior = self.distr(rv_par)
    likelihood = self.cdistr(rv_child)
    match prior, likelihood:
      case Normal(_), Normal(_):
        if _update(conj.gaussian_posterior(self, prior, likelihood, rv_par, rv_child, x)):
          return True
        else:
          return _update(conj.normal_inverse_gamma_normal_posterior(self, prior, likelihood, rv_par, rv_child, x))
      case Bernoulli(_), Bernoulli(_):
        return _update(conj.bernoulli_posterior(self, prior, likelihood, rv_par, rv_child, x))
      case Beta(_), Bernoulli(_):
        return _update(conj.beta_bernoulli_posterior(self, prior, likelihood, rv_par, rv_child, x))
      case Beta(_), Binomial(_):
        return _update(conj.beta_binomial_posterior(self, prior, likelihood, rv_par, rv_child, x))
      case Gamma(_), Poisson(_):
        return _update(conj.gamma_poisson_posterior(self, prior, likelihood, rv_par, rv_child, x))
      case Gamma(_), Normal(_):
        return _update(conj.gamma_normal_posterior(self, prior, likelihood, rv_par, rv_child, x))
      case _:
        return False

  def realize(self, rv: RandomVar, x: Delta) -> None:
    assert self.type_(rv) == DSType.MARGINALIZED
    self.set_type(rv, DSType.REALIZED)

    rv_par = self.parent(rv)
    if rv_par is not None:
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
    
  def graft(self, rv: RandomVar) -> None:
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

  def prune(self, rv: RandomVar) -> None:
    assert self.type_(rv) == DSType.MARGINALIZED
    rv_child = self.marginal_child(rv)
    if rv_child is not None:
      self.prune(rv_child)
    
    self.value(rv)