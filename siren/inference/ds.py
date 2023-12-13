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
  def __init__(self, seed=None) -> None:
    super().__init__(seed)
    self.type : Dict[RandomVar, DSType] = {}
    self.children : Dict[RandomVar, List[RandomVar]] = {}
    self.parent : Dict[RandomVar, RandomVar] = {}
    # self.state holds the marginal distribution q(x) (when done)
    # this holds the original (conditional) distribution 
    self.original_distr : Dict[RandomVar, SymDistr] = {}

  def __copy__(self):
    new_state = super().__copy__()
    new_state.type = copy(self.type)
    new_state.children = {k: copy(v) for k, v in self.children.items()}
    new_state.parent = copy(self.parent)
    new_state.original_distr = copy(self.original_distr)
    return new_state

  def __str__(self):
    s = '\n\t'.join(map(str, self.state.items()))
    return f"DSState(\n\t{s}\n)" if s else "DSState()"

  def assume(self, name: Optional[Identifier], annotation: Optional[Annotation], distribution: SymDistr[T]) -> RandomVar[T]:
    def _check_conjugacy(prior : SymDistr, likelihood : SymDistr, rv_par : RandomVar, rv_child : RandomVar) -> bool:
      match prior, likelihood:
        case Normal(mu0, var0), Normal(mu, var):
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
      has_parent = False
      for rv_par in parents:
        if not has_parent and _check_conjugacy(self.original_distr[rv_par], distribution, rv_par, rv):
          if rv_par not in self.children:
            self.children[rv_par] = []
          self.children[rv_par].append(rv)

          self.parent[rv] = rv_par
          has_parent = True
        else:
          self.value(rv_par)
          distribution = self.eval_distr(distribution)

    self.state[rv] = (name, distribution)
    self.original_distr[rv] = distribution

    return rv

  def observe(self, rv: RandomVar[T], value: Const[T]) -> float:
    # Turn rv into a terminal node
    self.graft(rv)
    # observe
    assert self.type[rv] == DSType.MARGINALIZED
    s = self.score(rv, value.v)
    self.realize(rv, Delta(value, sampled=False))
    return s

  def value(self, rv: RandomVar[T]) -> Const[T]:
    # Turn rv into terminal node
    self.graft(rv)
    return self.do_sample(rv)
  
  # Make rv marginal
  def marginalize(self, rv: RandomVar) -> None:
    match self.type[rv]:
      case DSType.REALIZED:
        return
      case DSType.MARGINALIZED:
        return
      case DSType.INITIALIZED:
        if rv not in self.parent:
          raise ValueError(f'Cannot marginalize {rv} because it has no parent')
        
        rv_par = self.parent[rv]
        match self.type[rv_par]:
          case DSType.REALIZED:
            self.set_distr(rv, self.eval_distr(self.distr(rv)))
          case DSType.MARGINALIZED:
            if not self.make_marginal(rv_par, rv):
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
  def do_marginalize(self, rv: RandomVar) -> None:
    assert self.type[rv] == DSType.INITIALIZED
    if rv not in self.parent:
      raise ValueError(f'Cannot marginalize {rv} because it has no parent')
    
    rv_par = self.parent[rv]
    match self.type[rv_par]:
      case DSType.INITIALIZED:
        raise ValueError(f'Cannot marginalize {rv} because {rv_par} is not marginalized')
      case DSType.REALIZED:
        d = self.distr(rv_par)
        # simplify delta
        match d:
          case Delta(_, _):
            self.set_distr(rv, self.eval_distr(d))
          case _:
            raise ValueError(d)
        # convert to marginal
        self.set_distr(rv, self.eval_distr(self.distr(rv)))
      case DSType.MARGINALIZED:
        if self.make_marginal(rv_par, rv):
          self.set_distr(rv, self.eval_distr(self.distr(rv)))
        else:
          self.value(rv_par)
          self.set_distr(rv, self.eval_distr(self.distr(rv)))
          # raise ValueError(f'Cannot marginalize {rv} because {rv_par} is not conjugate')

    self.type[rv] = DSType.MARGINALIZED

  def do_sample(self, rv: RandomVar) -> Const:
    # sample
    assert self.type[rv] == DSType.MARGINALIZED
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
    for rv_child in self.children[rv]:
      if self.type[rv_child] == DSType.MARGINALIZED:
        return rv_child
      
    return None
  
  def make_marginal(self, rv_par: RandomVar, rv_child: RandomVar) -> bool:
    def _update(marginal: Optional[SymDistr]) -> bool:
      if marginal is None:
        return False

      self.set_distr(rv_child, self.eval_distr(marginal))
      return True

    prior = self.distr(rv_par)
    likelihood = self.original_distr[rv_child]
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
    likelihood = self.original_distr[rv_child]
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
    assert self.type[rv] == DSType.MARGINALIZED
    self.type[rv] = DSType.REALIZED

    if rv in self.parent:
      rv_par = self.parent[rv]
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
    
  def graft(self, rv: RandomVar) -> None:
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

  def prune(self, rv: RandomVar) -> None:
    assert self.type[rv] == DSType.MARGINALIZED
    rv_child = self.marginal_child(rv)
    if rv_child is not None:
      self.prune(rv_child)
    
    self.value(rv)