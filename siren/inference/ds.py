from typing import Any, Set, Union
from copy import copy
from siren.grammar import RandomVar

import siren.inference.conjugate as conj
from siren.inference.interface import *

@dataclass(frozen=True)
class DSNode():
  pass

@dataclass(frozen=True)
class DSRealized(DSNode):
  def __str__(self) -> str:
    return 'DSRealized'

@dataclass(frozen=True)
class DSMarginalized(DSNode):
  edge: Optional[Tuple[RandomVar, SymDistr]]

  def __str__(self):
    return f'DSMarginalized({self.edge})'

@dataclass(frozen=True)
class DSInitialized(DSNode):
  edge: Tuple[RandomVar, SymDistr]

  def __str__(self):
    return f'DSInitialized({self.edge})'

class DSState(SymState):
  ###
  # State entry:
  #   rv: (pv, distribution, children, node)
  ###

  def __str__(self):
    s = '\n\t'.join(map(str, self.state.items()))
    return f"DSState(\n\t{s}\n)" if s else "DSState()"
  
  ## Accessors
  def children(self, rv: RandomVar) -> List[RandomVar]:
    return self.get_entry(rv, 'children')
  
  def node(self, rv: RandomVar) -> DSNode:
    return self.get_entry(rv, 'node')

  ## Mutators
  def set_children(self, rv: RandomVar, children: List[RandomVar]) -> None:
    self.set_entry(rv, children=children)

  def set_node(self, rv: RandomVar, node: DSNode) -> None:
    self.set_entry(rv, node=node)

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
    if len(distribution.rvs()) == 0:
      node = DSMarginalized(None)
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
            case DSRealized():
              distribution = self.eval_distr(distribution)
              continue
            case DSMarginalized(_):
              parent_dist = self.distr(rv_par)
              if _check_conjugacy(parent_dist, distribution, rv_par, rv):
                if rv not in self.children(rv_par):
                  self.children(rv_par).append(rv)

                canonical_parent = rv_par
                has_parent = True

                continue
            case DSInitialized((_, parent_dist)):
              if _check_conjugacy(parent_dist, distribution, rv_par, rv):
                if rv not in self.children(rv_par):
                  self.children(rv_par).append(rv)

                canonical_parent = rv_par
                has_parent = True
                continue
            case _:
              raise ValueError(f'{rv_par} is {self.node(rv_par)}')

        self.value(rv_par)
        distribution = self.eval_distr(distribution)

      # all parents were sampled
      if len(distribution.rvs()) == 0:
        node = DSMarginalized(None)
      else:
        assert canonical_parent is not None
        node = DSInitialized((canonical_parent, distribution))

    self.set_pv(rv, name)
    self.set_distr(rv, distribution)
    self.set_children(rv, children)
    self.set_node(rv, node)

    return rv

  def observe(self, rv: RandomVar[T], value: Const[T]) -> float:
    # Turn rv into a terminal node
    self.graft(rv)
    # observe
    match self.node(rv):
      case DSMarginalized(_):
        s = self.score(rv, value.v)
        self.realize(rv, Delta(value, sampled=False))
        return s
      case _:
        raise ValueError(f'{rv} is {self.node(rv)}')

  def value(self, rv: RandomVar[T]) -> Const[T]:
    # Turn rv into terminal node
    self.graft(rv)
    return self.do_sample(rv)
  
  # Make rv marginal
  def marginalize(self, rv: RandomVar) -> None:
    match self.node(rv):
      case DSRealized():
        assert len(self.children(rv)) == 0
      case DSMarginalized(_):
        if len(self.children(rv)) > 0:
          self.graft(rv)
      case DSInitialized((rv_par, _)):
        match self.node(rv_par):
          case DSInitialized(_):
            self.marginalize(rv_par)
            assert not isinstance(self.node(rv_par), DSInitialized)
        self.do_marginalize(rv)
            
        if len(self.children(rv)) > 0:
          self.graft(rv)
      case _:
        raise ValueError(f'{rv} is {self.node(rv)}')
    
  ########################################################################
              
  def eval_entry(self, rv: RandomVar) -> None:
    self.set_distr(rv, self.eval_distr(self.distr(rv)))

  # moves rv from I to M and updates its distribution by marginalizing over its parent
  def do_marginalize(self, rv: RandomVar) -> None:
    match self.node(rv):
      case DSInitialized((rv_par, cdistr)):
        match self.node(rv_par):
          case DSInitialized(_):
            raise ValueError(f'Cannot marginalize {rv} because {rv_par} is not marginalized')
          case DSRealized():
            d = self.distr(rv_par)
            # simplify delta
            if not isinstance(d, Delta):
              raise ValueError(d)
            # convert to marginal
            self.eval_entry(rv)
            self.set_node(rv, DSMarginalized(None))
          case DSMarginalized(_):
            if self.make_marginal(rv_par, rv):
              self.eval_entry(rv)
              self.set_node(rv, DSMarginalized((rv_par, cdistr)))
            else:
              self.value(rv_par)
              self.eval_entry(rv)
              self.set_node(rv, DSMarginalized(None))
      case _:
        raise ValueError(f'{rv} is {self.node(rv)}')

  def do_sample(self, rv: RandomVar) -> Const:
    # sample
    match self.node(rv):
      case DSMarginalized(_):
        v = self.draw(rv)
        self.realize(rv, Delta(Const(v), sampled=True))
        return Const(v)
      case _:
        raise ValueError(f'{rv} is {self.node(rv)}')
    
  def score(self, rv: RandomVar[T], v: T) -> float:
    return self.distr(rv).score(v)

  def draw(self, rv: RandomVar) -> Any:
    return self.distr(rv).draw(self.rng)

  # Invariant 2: A node always has at most one child that is marginalized
  def marginal_child(self, rv: RandomVar) -> Optional[RandomVar]:
    for rv_child in self.children(rv):
      match self.node(rv_child):
        case DSMarginalized(_):
          return rv_child
        case _:
          continue
      
    return None
  
  def make_marginal(self, rv_par: RandomVar, rv_child: RandomVar) -> bool:
    def _update(marginal: Optional[SymDistr]) -> bool:
      if marginal is None:
        return False

      self.set_distr(rv_child, self.eval_distr(marginal))
      return True

    prior = self.distr(rv_par)
    match self.node(rv_child):
      case DSInitialized((_, likelihood)):
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
      case _:
        raise ValueError(f'{rv_child} is {self.node(rv_child)}')
      
  def make_conditional(self, rv_par: RandomVar, rv_child: RandomVar, x: SymExpr) -> bool:
    def _update(posterior: Optional[SymDistr]) -> bool:
      if posterior is None:
        return False
      
      posterior = self.eval_distr(posterior)
      self.set_distr(rv_par, posterior)
      return True

    prior = self.distr(rv_par)
    match self.node(rv_child):
      case DSMarginalized((_, likelihood)):
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
      case _:
        raise ValueError(f'{rv_child} is {self.node(rv_child)}')

  def realize(self, rv: RandomVar, x: Delta) -> None:
    match self.node(rv):
      case DSMarginalized(None):
        pass
      case DSMarginalized((rv_par, _)):
        match self.node(rv_par):
          case DSMarginalized(edge):
            if self.make_conditional(rv_par, rv, x.v):
              self.set_node(rv_par, DSMarginalized(edge))
              self.children(rv_par).remove(rv)
            else:
              raise ValueError(f'Cannot realize {rv} because {rv_par} is not conjugate')
          case _:
            raise ValueError(f'{rv_par} is {self.node(rv_par)}')
      case _:
        raise ValueError(f'{rv} is {self.node(rv)}')

    self.set_distr(rv, x)
    self.set_node(rv, DSRealized())

    # new roots from children
    for rv_child in self.children(rv):
      self.do_marginalize(rv_child)

    self.set_children(rv, [])
    
  def graft(self, rv: RandomVar) -> None:
    match self.node(rv):
      case DSRealized():
        raise ValueError(f'Cannot graft {rv} because it is already realized')
      case DSMarginalized(_):
        rv_child = self.marginal_child(rv)
        if rv_child is not None:
          self.prune(rv_child)
      case DSInitialized((rv_par, _)):
        self.graft(rv_par)
        self.do_marginalize(rv)
      case _:
        raise ValueError(f'{rv} is {self.node(rv)}')

  def prune(self, rv: RandomVar) -> None:
    match self.node(rv):
      case DSMarginalized(_):
        rv_child = self.marginal_child(rv)
        if rv_child is not None:
          self.prune(rv_child)
        
        self.value(rv)
      case _:
        raise ValueError(f'{rv} is {self.node(rv)}')