from typing import Any, Set
from copy import copy
from collections import deque

import siren.inference.conjugate as conj
from siren.inference.interface import *

class NonConjugate(Exception):
  def __init__(self, message, rv_nc: RandomVar):
    super().__init__(message)

    self.rv_nc = rv_nc

class SSIState(SymState):
  def __str__(self):
    s = '\n\t'.join(map(str, self.state.items()))
    return f"SSIState(\n\t{s}\n)" if s else "SSIState()"

  def __copy__(self):
    new_state = SSIState()
    new_state.state = copy(self.state)
    new_state.ctx = copy(self.ctx)
    new_state.counter = self.counter
    new_state.annotations = self.annotations
    return new_state

  def assume(self, name: Optional[Identifier], annotation: Optional[Annotation], distribution: SymDistr[T]) -> RandomVar[T]:
    rv = self.new_var()
    if annotation is not None:
      if name is None:
        raise ValueError('Cannot annotate anonymous variable')
      else:
        self.annotations[name] = annotation
    self.state[rv] = (name, distribution)
    return rv

  def observe(self, rv: RandomVar[T], value: Const[T]) -> float:
    def _observe() -> float:
      try:
        return self.score(rv, value.v)
      except NonConjugate as e:
        self.value(e.rv_nc)
        return _observe()
      
    s = _observe()
    self.intervene(rv, Delta(value, sampled=False))
    return s

  def value(self, rv: RandomVar[T]) -> Const[T]:
    def _value() -> T:
      try:
        return self.draw(rv)
      except NonConjugate as e:
        self.value(e.rv_nc)
        return _value()

    v = _value()
    self.intervene(rv, Delta(Const(v), sampled=True))
    return Const(v)
  
  def marginalize(self, rv: RandomVar) -> None:
    try:
      self.hoist_and_eval(rv)
    except NonConjugate as e:
      self.value(e.rv_nc)
      self.marginalize(rv)
    
  ########################################################################

  def parents(self, rv: RandomVar) -> List[RandomVar]:
    return self.distr(rv).rvs()
    
  def score(self, rv: RandomVar[T], v: T) -> float:
    self.hoist_and_eval(rv)
    return self.distr(rv).score(v)

  def draw(self, rv: RandomVar) -> Any:
    self.hoist_and_eval(rv)
    return self.distr(rv).draw(self.rng)

  def intervene(self, rv: RandomVar[T], v: Delta[T]) -> None:
    self.set_distr(rv, v)

  def rv_depends_on_transitive(self, expr: RandomVar, rv: RandomVar) -> bool:
    match self.distr(expr):
      case Normal(mu, var):
        return self.depends_on(mu, rv, True) or self.depends_on(var, rv, True)
      case Bernoulli(p):
        return self.depends_on(p, rv, True)
      case Beta(a, b):
        return self.depends_on(a, rv, True) or self.depends_on(b, rv, True)
      case Binomial(n, p):
        return self.depends_on(n, rv, True) or self.depends_on(p, rv, True)
      case BetaBinomial(n, a, b):
        return self.depends_on(n, rv, True) or self.depends_on(a, rv, True) or self.depends_on(b, rv, True)
      case NegativeBinomial(n, p):
        return self.depends_on(n, rv, True) or self.depends_on(p, rv, True)
      case Gamma(a, b):
        return self.depends_on(a, rv, True) or self.depends_on(b, rv, True)
      case Poisson(l):
        return self.depends_on(l, rv, True)
      case StudentT(mu, tau2, nu):
        return self.depends_on(mu, rv, True) or self.depends_on(tau2, rv, True) or self.depends_on(nu, rv, True)
      case Categorical(lower, upper, probs):
        return self.depends_on(lower, rv, True) or self.depends_on(upper, rv, True) or self.depends_on(probs, rv, True)
      case Delta(v, _):
        return self.depends_on(v, rv, True)
      case _:
        raise ValueError(self.distr(expr))

  def depends_on(self, expr: SymExpr, rv: RandomVar, transitive: bool) -> bool:
    match expr:
      case Const(_):
        return False
      case RandomVar(_):
        if expr == rv:
          return True
        else:
          if not transitive:
            return False
          else:
            return self.rv_depends_on_transitive(expr, rv)
      case Add(e1, e2):
        return self.depends_on(e1, rv, transitive) or self.depends_on(e2, rv, transitive)
      case Mul(e1, e2):
        return self.depends_on(e1, rv, transitive) or self.depends_on(e2, rv, transitive)
      case Div(e1, e2):
        return self.depends_on(e1, rv, transitive) or self.depends_on(e2, rv, transitive)
      case Ite(cond, true, false):
        return self.depends_on(cond, rv, transitive) or self.depends_on(true, rv, transitive) or self.depends_on(false, rv, transitive)
      case Eq(e1, e2):
        return self.depends_on(e1, rv, transitive) or self.depends_on(e2, rv, transitive)
      case Lt(e1, e2):
        return self.depends_on(e1, rv, transitive) or self.depends_on(e2, rv, transitive)
      case Lst(es):
        return any(self.depends_on(e, rv, transitive) for e in es)
      case Pair(e1, e2):
        return self.depends_on(e1, rv, transitive) or self.depends_on(e2, rv, transitive)
      case _:
        raise ValueError(expr)

  def hoist(self, rv: RandomVar) -> None:
    def _topo_sort(rvs: List[RandomVar]) -> List[RandomVar]:
      sorted_nodes = []

      def _visit(rv: RandomVar) -> None:
        parents = self.parents(rv)

        for parent in parents:
          _visit(parent)

        if rv not in sorted_nodes:
          sorted_nodes.append(rv)

      for rv in rvs:
        _visit(rv)

      nodes = []
      for node in sorted_nodes:
        if node in rvs:
          nodes.append(node)
      
      return nodes

    def _can_swap(rv_par: RandomVar, rv_child: RandomVar) -> bool:
      def _has_other_deps_on_par(expr: SymExpr) -> bool:
        match expr:
          case Const(_):
            return False
          case RandomVar(_):
            if expr == rv_par:
              return False
            else:
              return self.rv_depends_on_transitive(expr, rv_par)
          case Add(e1, e2):
            return _has_other_deps_on_par(e1) or _has_other_deps_on_par(e2)
          case Mul(e1, e2):
            return _has_other_deps_on_par(e1) or _has_other_deps_on_par(e2)
          case Div(e1, e2):
            return _has_other_deps_on_par(e1) or _has_other_deps_on_par(e2)
          case Ite(cond, true, false):
            return _has_other_deps_on_par(cond) or _has_other_deps_on_par(true) or _has_other_deps_on_par(false)
          case Eq(e1, e2):
            return _has_other_deps_on_par(e1) or _has_other_deps_on_par(e2)
          case Lt(e1, e2):
            return _has_other_deps_on_par(e1) or _has_other_deps_on_par(e2)
          case Lst(es):
            return any(_has_other_deps_on_par(e) for e in es)
          case Pair(e1, e2):
            return _has_other_deps_on_par(e1) or _has_other_deps_on_par(e2)
          case _:
            raise ValueError(expr)

      match self.distr(rv_child):
        case Normal(mu, var):
          return (self.depends_on(mu, rv_par, False) or self.depends_on(var, rv_par, False)) \
            and (not _has_other_deps_on_par(mu)) and (not _has_other_deps_on_par(var))
        case Bernoulli(p):
          return self.depends_on(p, rv_par, False) and (not _has_other_deps_on_par(p))
        case Beta(a, b):
          return (self.depends_on(a, rv_par, False) or self.depends_on(b, rv_par, False)) \
            and (not _has_other_deps_on_par(a)) and (not _has_other_deps_on_par(b))
        case Binomial(n, p):
          return (self.depends_on(n, rv_par, False) or self.depends_on(p, rv_par, False)) \
            and (not _has_other_deps_on_par(n)) and (not _has_other_deps_on_par(p))
        case BetaBinomial(n, a, b):
          return (self.depends_on(n, rv_par, False) or self.depends_on(a, rv_par, False) or self.depends_on(b, rv_par, False)) \
            and (not _has_other_deps_on_par(n)) and (not _has_other_deps_on_par(a)) and (not _has_other_deps_on_par(b))
        case NegativeBinomial(n, p):
          return (self.depends_on(n, rv_par, False) or self.depends_on(p, rv_par, False)) \
            and (not _has_other_deps_on_par(n)) and (not _has_other_deps_on_par(p))
        case Gamma(a, b):
          return (self.depends_on(a, rv_par, False) or self.depends_on(b, rv_par, False)) \
            and (not _has_other_deps_on_par(a)) and (not _has_other_deps_on_par(b))
        case Poisson(l):
          return self.depends_on(l, rv_par, False) and (not _has_other_deps_on_par(l))
        case StudentT(mu, tau2, nu):
          return (self.depends_on(mu, rv_par, False) or self.depends_on(tau2, rv_par, False) or self.depends_on(nu, rv_par, False)) \
            and (not _has_other_deps_on_par(mu)) and (not _has_other_deps_on_par(tau2)) and (not _has_other_deps_on_par(nu))
        case Categorical(lower, upper, probs):
          return (self.depends_on(lower, rv_par, False) or self.depends_on(upper, rv_par, False) or self.depends_on(probs, rv_par, False)) \
            and (not _has_other_deps_on_par(lower)) and (not _has_other_deps_on_par(upper)) and (not _has_other_deps_on_par(probs))
        case Delta(v, _):
          return self.depends_on(v, rv_par, False) and (not _has_other_deps_on_par(v))
        case _:
          raise ValueError(self.distr(rv_child))

    def _swap(rv_par: RandomVar, rv_child: RandomVar) -> bool:
      def _update(marginal_posterior: Optional[Tuple[SymDistr, SymDistr]]) -> bool:
        if marginal_posterior is None:
          return False
        
        marginal, posterior = marginal_posterior
        self.set_distr(rv_par, posterior)
        self.set_distr(rv_child, marginal)
        return True

      match self.distr(rv_par), self.distr(rv_child):
        case Normal(_), Normal(_):
          return _update(conj.gaussian_conjugate(self, rv_par, rv_child))
        case Bernoulli(_), Bernoulli(_):
          return _update(conj.bernoulli_conjugate(self, rv_par, rv_child))
        case Beta(_), Bernoulli(_):
          return _update(conj.beta_bernoulli_conjugate(self, rv_par, rv_child))
        case Beta(_), Binomial(_):
          return _update(conj.beta_binomial_conjugate(self, rv_par, rv_child))
        case Gamma(_), Poisson(_):
          return _update(conj.gamma_poisson_conjugate(self, rv_par, rv_child))
        case Gamma(_), Normal(_):
          return _update(conj.gamma_normal_conjugate(self,rv_par, rv_child))
        case _:
          return False

    def _hoist_inner(rv_cur: RandomVar, ghost_roots: Set[RandomVar]) -> None:
      # Hoist parents
      parents = _topo_sort(self.parents(rv_cur))[::-1]
      for rv_par in parents:
        if rv_par not in ghost_roots:
          _hoist_inner(rv_par, ghost_roots)
        else:
          ghost_roots.add(rv_par)

      # Hoist current node
      for rv_par in parents[::-1]:
        if rv_par not in ghost_roots:
          if not _can_swap(rv_par, rv_cur):
            raise ValueError(f'Cannot swap {rv_par} and {rv_cur}')
          
          if not _swap(rv_par, rv_cur):
            raise NonConjugate(f'Nonconjugate {rv_par} and {rv_cur}', rv_par)

    _hoist_inner(rv, set())

  def hoist_and_eval(self, rv: RandomVar) -> None:
    self.set_distr(rv, self.eval_distr(self.distr(rv)))
    self.hoist(rv)
    self.set_distr(rv, self.eval_distr(self.distr(rv)))
