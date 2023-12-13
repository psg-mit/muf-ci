from typing import Any, Set
from copy import copy
from siren.grammar import AbsRandomVar

import siren.analysis.conjugate as conj
from siren.analysis.interface import *
from siren.inference.ds import DSType

class AbsDSState(AbsSymState):
  def __init__(self) -> None:
    super().__init__()
    self.type : Dict[AbsRandomVar, DSType] = {}
    self.children : Dict[AbsRandomVar, List[AbsRandomVar]] = {}
    self.parent : Dict[AbsRandomVar, AbsRandomVar] = {}
    self.original_distr : Dict[AbsRandomVar, AbsSymDistr] = {}

  def __copy__(self):
    new_state = super().__copy__()
    new_state.type = copy(self.type)
    new_state.children = {k: copy(v) for k, v in self.children.items()}
    new_state.parent = copy(self.parent)
    new_state.original_distr = copy(self.original_distr)
    return new_state

  def __str__(self):
    s = '\n\t'.join(map(str, self.state.items()))
    return f"AbsDSState(\n\t{s}\n)" if s else "AbsDSState()"

  def assume(self, name: Optional[Identifier], annotation: Optional[Annotation], distribution: AbsSymDistr[T]) -> AbsRandomVar[T]:
    def _check_conjugacy(prior : AbsSymDistr, likelihood : AbsSymDistr, rv_par : AbsRandomVar, rv_child : AbsRandomVar) -> bool:
      match prior, likelihood:
        case AbsNormal(mu0, var0), AbsNormal(mu, var):
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
    self.state[rv] = (pv, distribution)
    self.original_distr[rv] = distribution

    self.children[rv] = []
    if len(distribution.rvs()) == 0:
      self.type[rv] = DSType.MARGINALIZED
    else:
      self.type[rv] = DSType.INITIALIZED

      parents = []
      for rv_par in distribution.rvs():
        if rv_par not in parents:
          parents.append(rv_par)

      # keep if conjugate, else sample it
      for rv_par in parents:
        if _check_conjugacy(self.original_distr[rv_par], distribution, rv_par, rv):
          if rv_par not in self.children:
            self.children[rv_par] = []
          self.children[rv_par].append(rv)

          self.parent[rv] = rv_par
        else:
          self.value(rv_par)
          distribution = self.eval_distr(distribution)
          
    return rv

  def observe(self, rv: AbsRandomVar[T], value: AbsConst[T]) -> None:
    # Turn rv into a terminal node
    self.graft(rv)
    # observe
    assert self.type[rv] == DSType.MARGINALIZED
    self.realize(rv, AbsDelta(value, sampled=False))
    return

  def value_impl(self, rv: AbsRandomVar[T]) -> AbsConst[T]:
    # Turn rv into terminal node
    self.graft(rv)
    return self.do_sample(rv)
  
  # Make rv marginal
  def marginalize(self, rv: AbsRandomVar) -> None:
    match self.type[rv]:
      case DSType.REALIZED:
        return
      case DSType.MARGINALIZED:
        return
      case DSType.INITIALIZED:
        if expr not in self.parent:
          raise ValueError(f'Cannot marginalize {rv} because it has no parent')
        
        rv_par = self.parent[rv]
        match self.type[rv_par]:
          case DSType.REALIZED:
            self.set_distr(rv, self.eval_distr(self.distr(rv)))
          case DSType.MARGINALIZED:
            if not self.make_marginal(rv_par, expr):
              self.value(rv_par)
              self.set_distr(rv, self.eval_distr(self.distr(rv)))
              # raise ValueError(f'Cannot marginalize {expr} because {rv_par} is not conjugate')
          case DSType.INITIALIZED:
            self.marginalize(rv_par)
            if not self.make_marginal(rv_par, rv):
              self.value(rv_par)
              self.set_distr(rv, self.eval_distr(self.distr(rv)))
              # raise ValueError(f'Cannot marginalize {expr} because {rv_par} is not conjugate')
    
  ########################################################################

  # moves rv from I to M and updates its distribution by marginalizing over its parent
  def do_marginalize(self, rv: AbsRandomVar) -> None:
    assert self.type[rv] == DSType.INITIALIZED
    if rv not in self.parent:
      raise ValueError(f'Cannot marginalize {rv} because it has no parent')
    
    rv_par = self.parent[rv]
    match self.type[rv_par]:
      case DSType.INITIALIZED:
        raise ValueError(f'Cannot marginalize {rv} because {rv_par} is not marginalized')
      case DSType.REALIZED:
        d = self.distr(rv_par)
        match d:
          case AbsDelta(_, _):
            self.set_distr(rv, self.eval_distr(d))
          case _:
            raise ValueError(d)
      case DSType.MARGINALIZED:
        if self.make_marginal(rv_par, rv):
          self.set_distr(rv, self.eval_distr(self.distr(rv)))
        else:
          self.value(rv_par)
          self.set_distr(rv, self.eval_distr(self.distr(rv)))
          # raise ValueError(f'Cannot marginalize {rv} because {rv_par} is not conjugate')

    self.type[rv] = DSType.MARGINALIZED

  def do_sample(self, rv: AbsRandomVar) -> AbsConst:
    # sample
    assert self.type[rv] == DSType.MARGINALIZED
    self.realize(rv, AbsDelta(AbsConst(UnkC()), sampled=True))
    return AbsConst(UnkC())

  def intervene(self, rv: AbsRandomVar[T], v: AbsDelta[T]) -> None:
    self.set_distr(rv, v)

  # Invariant 2: A node always has at most one child that is marginalized
  def marginal_child(self, rv: AbsRandomVar) -> Optional[AbsRandomVar]:
    for rv_child in self.children[rv]:
      if self.type[rv_child] == DSType.MARGINALIZED:
        return rv_child
      
    return None
  
  def make_marginal(self, rv_par: AbsRandomVar, rv_child: AbsRandomVar) -> bool:
    def _update(marginal: Optional[AbsSymDistr]) -> bool:
      if marginal is None:
        return False

      self.set_distr(rv_child, self.eval_distr(marginal))
      return True

    prior = self.distr(rv_par)
    likelihood = self.original_distr[rv_child]
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

  def make_conditional(self, rv_par: AbsRandomVar, rv_child: AbsRandomVar) -> bool:
    def _update(posterior: Optional[AbsSymDistr]) -> bool:
      if posterior is None:
        return False
      
      posterior = self.eval_distr(posterior)
      self.set_distr(rv_par, posterior)
      # Update original distr
      return True

    prior = self.distr(rv_par)
    likelihood = self.original_distr[rv_child]
    match prior, likelihood:
      case AbsNormal(_), AbsNormal(_):
        if _update(conj.gaussian_posterior(self, prior, likelihood, rv_par, rv_child, x.v)):
          return True
        else:
          return _update(conj.normal_inverse_gamma_normal_posterior(self, prior, likelihood, rv_par, rv_child, x.v))
      case AbsBernoulli(_), AbsBernoulli(_):
        return _update(conj.bernoulli_posterior(self, prior, likelihood, rv_par, rv_child, x.v))
      case AbsBeta(_), AbsBernoulli(_):
        return _update(conj.beta_bernoulli_posterior(self, prior, likelihood, rv_par, rv_child, x.v))
      case AbsBeta(_), AbsBinomial(_):
        return _update(conj.beta_binomial_posterior(self, prior, likelihood, rv_par, rv_child, x.v))
      case AbsGamma(_), AbsPoisson(_):
        return _update(conj.gamma_poisson_posterior(self, prior, likelihood, rv_par, rv_child, x.v))
      case AbsGamma(_), AbsNormal(_):
        return _update(conj.gamma_normal_posterior(self, prior, likelihood, rv_par, rv_child, x.v))
      case _:
        return False

  def realize(self, rv: AbsRandomVar, x: AbsDelta) -> None:
    assert self.type[rv] == DSType.MARGINALIZED
    self.type[rv] = DSType.REALIZED
    if rv in self.parent:
      rv_par = self.parent[rv]
      # condition parent on child
      if self.make_conditional(rv_par, rv, x.v):
        self.type[rv_par] = DSType.MARGINALIZED
        self.children[rv_par].remove(rv)
        del self.parent[rv]
      else:
        self.value(rv_par)
        self.eval_distr(self.distr(rv))
        # raise ValueError(f'Cannot realize {rv} because {rv_par} is not conjugate')

    self.intervene(rv, x)
    # new roots from children
    for rv_child in self.children[rv]:
      self.do_marginalize(rv_child)
      del self.parent[rv_child]

    self.children[rv] = []
    
  def graft(self, rv: AbsRandomVar) -> None:
    if self.type[rv] == DSType.MARGINALIZED:
      rv_child = self.marginal_child(rv)
      if rv_child is not None:
        self.prune(rv_child)
    elif self.type[rv] == DSType.INITIALIZED:
      if rv not in self.parent:
        raise ValueError(f'Cannot graft {rv} because it has no parent')
      rv_par = self.parent[rv]
      self.graft(rv_par)
      self.do_marginalize(rv)
    else:
      raise ValueError(f'Cannot graft {rv} because it is already realized')

  def prune(self, rv: AbsRandomVar) -> None:
    assert self.type[rv] == DSType.MARGINALIZED
    rv_child = self.marginal_child(rv)
    if rv_child is not None:
      self.prune(rv_child)
    
    self.value(rv)