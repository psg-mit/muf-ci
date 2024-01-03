from typing import Dict, Set, Tuple, Optional, Any
from copy import copy, deepcopy

from siren.inference_plan import InferencePlan, DistrEnc
from siren.grammar import *
from siren.utils import is_abs_pair, is_abs_lst, get_abs_pair, get_abs_lst, fast_copy, match_rvs

ED = TypeVar('ED', bound=AbsSymExpr | AbsSymDistr)
OTHERSTATE = TypeVar('OTHERSTATE', bound='AbsSymState')

class AnalysisViolatedAnnotationError(Exception):
  pass

class AbsSymState(object):

  ### Shared functions ###
  # These should not need to be overridden
  def __init__(self) -> None:
    super().__init__()
    self.state: Dict[AbsRandomVar, Dict[str, Any]] = {}
    self.plan: InferencePlan = InferencePlan()
    self.ctx: AbsContext = AbsContext()
    self.counter: int = 0
    self.annotations: Dict[Identifier, Annotation] = {}

  def __copy__(self):
    new_state = type(self)()
    new_state.state = fast_copy(self.state)
    new_state.ctx = copy(self.ctx)
    new_state.counter = self.counter
    new_state.annotations = self.annotations
    new_state.plan = copy(self.plan)
    return new_state
  
  def __len__(self) -> int:
    return len(self.state)

  def __iter__(self):
    return iter(self.state)
  
  def __str__(self):
    return f"SymState({', '.join(map(str, self.state.items()))})"
  
  def __eq__(self, other: 'AbsSymState') -> bool:
    self_vars = self.vars()
    other_vars = other.vars()
    for rv1 in self_vars:
      if rv1 not in other_vars:
        return False
      for key, item in self.state[rv1].items():
        if key not in other.state[rv1] or \
          item != other.state[rv1][key]:
          return False

    for rv2 in other_vars:
      if rv2 not in self_vars:
        return False
      for key, item in other.state[rv2].items():
        if key not in self.state[rv2] or \
          item != self.state[rv2][key]:
          return False

    return True

  def new_var(self, min_counter: Optional[int] = None) -> AbsRandomVar:
    if min_counter is not None:
      self.counter = max(self.counter, min_counter)
    self.counter += 1
    return AbsRandomVar(f"rv{self.counter}")
  
  def vars(self) -> Set[AbsRandomVar]:
    return set(self.state.keys())
    
  def get_entry(self, rv: AbsRandomVar, key: str) -> Any:
    if rv not in self.state:
      raise ValueError(f"{rv} not in state")
    if key not in self.state[rv]:
      raise ValueError(f"{key} not in {rv}")
    return self.state[rv][key]
  
  def set_entry(self, variable: AbsRandomVar, **kwargs) -> None:
    if variable not in self.state:
      self.state[variable] = {}

    for key, value in kwargs.items():
      self.state[variable][key] = value

    # Check if annotations violated
    if 'distribution' in kwargs:
      distribution = kwargs['distribution']
      if isinstance(distribution, AbsDelta):
        pv = self.get_entry(variable, 'pv')
        for pv in self.pv(variable):
          if pv in self.annotations and self.annotations[pv] == Annotation.symbolic \
            and distribution.sampled:
            raise AnalysisViolatedAnnotationError(
              f"{self.get_entry(variable, 'pv')} is annotated as symbolic but will be sampled")
        
  def is_sampled(self, variable: AbsRandomVar) -> bool:
    match self.get_entry(variable, 'distribution'):
      case AbsDelta(_, sampled):
        return sampled
      case _:
        return False
      
  # Removes unused variables from the state
  # Depends on referenced_vars and subst_rv, which may need to be overridden
  def clean(self, other: 'AbsSymState', expr: Optional[AbsSymExpr]=None) -> None:
    expr_vars = set() if expr is None else set(expr.rvs())
    # other_vars = other.vars()
    used_vars = expr_vars | set().union(*(expr.rvs() for expr in self.ctx.context.values()))
    # get referenced vars in the distribution of each ctx variable
    while True:
      new_used_vars = used_vars | self.entry_referenced_rvs(used_vars)
      if new_used_vars == used_vars:
        used_vars = new_used_vars
        break
      else:
        used_vars = new_used_vars

    # remove unused variables
    for rv in self.vars():
      if rv not in used_vars:
        # subst deltas out
        d = self.distr(rv)
        if isinstance(d, AbsDelta):
          for rv2 in self.vars():
            self.set_distr(rv2, self.distr(rv2).subst_rv(rv, d.v))
          
          for x, e in self.ctx.context.items():
            self.ctx.context[x] = e.subst_rv(rv, d.v)

        del self.state[rv]

  # Renames old rv to new rv in the state, ctx, and given expr
  # Depends on rename_rv, which may need to be overridden
  def rename(self, expr: ED, old: AbsRandomVar, new: AbsRandomVar) -> ED:
    if new == old:
      return expr
    # rename each distribution
    for rv in self.vars():
      # rename references in parameters
      self.entry_rename_rv(rv, old, new)

      # use new name as entry key
      if rv == old:
        self.state[new] = self.state[rv]
        del self.state[rv]

    # rename entries in context
    self.ctx.rename(old, new)

    return expr.rename(old, new)
  
  def unify(self, e1: AbsSymExpr, e2: AbsSymExpr, other: OTHERSTATE) -> Tuple[AbsSymExpr, AbsSymExpr, OTHERSTATE]:
    vars2 = set(e2.rvs())
    
    capture_avoiding_mapping : Dict[AbsRandomVar, AbsRandomVar] = {}

    unify_mapping = match_rvs({}, e1, e2)

    # Get canonical variable for for each e2_var from set of e1_vars
    map2temp = {}
    # use temp vars to avoid collision
    map2canonical = {}
    for e2_var, e1_vars in unify_mapping.items():
      canon_var = min(e1_vars, key=lambda v: v.rv)

      # capture avoiding substitution to avoid mapping a variable 
      # in e2 to an irrelevant variable in e2 or a variable referenced
      # by that old variable
      # note: this doesn't happen for e1 vars because they will be 
      # mapped to one of the vars in the mapped-to set, which means
      # they will never be mapped to something that isn't used in e1
      if (canon_var in other.vars() and canon_var not in vars2) \
        or canon_var in other.entry_referenced_rvs({e2_var}):
        # capture avoiding substitution
        new_var = self.new_var(other.counter)
        capture_avoiding_mapping[canon_var] = new_var

      temp_var = self.new_var(other.counter)
      map2temp[e2_var] = temp_var
      map2canonical[temp_var] = canon_var

    for old_var, new_var in capture_avoiding_mapping.items():
      e2 = other.rename(e2, old_var, new_var)

    # rename other vars in e1_vars to temp_var in e1 and self
    for e2_var, e1_vars in unify_mapping.items():
      temp_var = map2temp[e2_var]

      for e1_var in e1_vars:
        e1 = self.rename(e1, e1_var, temp_var)

    # rename e2_var to temp_var in e2 and other
    for e2_var, e1_vars in unify_mapping.items():
      temp_var = map2temp[e2_var]

      e2 = other.rename(e2, e2_var, temp_var)

    # rename temp vars to canon vars
    for temp_var, canon_var in map2canonical.items():
      e2 = other.rename(e2, temp_var, canon_var)
      e1 = self.rename(e1, temp_var, canon_var)

    return (e1, e2, other)
  
  # Joins the two states
  # Depends on join_entry and copy_entry, which may need to be overridden
  def join(self, other: 'AbsSymState') -> None:
    self_vars = self.vars()
    other_vars = other.vars()

    for rv in self_vars | other_vars:
      if rv in self_vars and rv in other_vars:
        self.entry_join(rv, other)
      elif rv in self_vars:
        # keep distribution
        pass
      elif rv in other_vars:
        # copy entry
        # copy pv first
        self.set_pv(rv, copy(other.pv(rv)))
        for key, value in other.state[rv].items():
          self.set_entry(rv, **{key: copy(value)})
      else:
        raise ValueError(rv)
      
    # set counter to max of both states
    self.counter = max(self.counter, other.counter)

  def narrow_join_expr(self, e1: AbsSymExpr, e2: AbsSymExpr, other: 'AbsSymState') -> AbsSymExpr:
    e1, e2, other = self.unify(e1, e2, other)
    
    e = join_expr(e1, e2)
    self.join(other)

    return e
  
  def ex_add(self, e1: AbsSymExpr[Number], e2: AbsSymExpr[Number]) -> AbsSymExpr[Number]:
    match e1, e2:
      case AbsConst(v1), AbsConst(v2):
        if isinstance(v1, UnkC) or isinstance(v2, UnkC):
          return AbsConst(UnkC())
        return AbsConst(v1 + v2)
      case AbsConst(v1), AbsAdd(AbsConst(v2), e3):
        if isinstance(v1, UnkC) or isinstance(v2, UnkC):
          s = AbsConst(UnkC())
        else:
          s = AbsConst(v1 + v2)
        return self.ex_add(s, e3)
      case AbsConst(v1), AbsAdd(e2, AbsConst(v3)):
        if isinstance(v1, UnkC) or isinstance(v3, UnkC):
          s = AbsConst(UnkC())
        else:
          s = AbsConst(v1 + v3)
        return self.ex_add(s, e2)
      case AbsAdd(AbsConst(v1), e2), e3:
        return self.ex_add(AbsConst(v1), self.ex_add(e2, e3))
      case AbsConst(0), e2:
        return e2
      case e1, AbsConst(0):
        return e1
      case _:
        return AbsAdd(e1, e2)

  def ex_mul(self, e1: AbsSymExpr[Number], e2: AbsSymExpr[Number]) -> AbsSymExpr[Number]:
    match e1, e2:
      case AbsConst(v1), AbsConst(v2):
        if isinstance(v1, UnkC) or isinstance(v2, UnkC):
          return AbsConst(UnkC())
        return AbsConst(v1 * v2)
      case AbsConst(v1), AbsMul(AbsConst(v2), e3):
        if isinstance(v1, UnkC) or isinstance(v2, UnkC):
          s = AbsConst(UnkC())
        else:
          s = AbsConst(v1 * v2)
        return self.ex_mul(s, e3)
      case AbsConst(v1), AbsMul(e2, AbsConst(v3)):
        if isinstance(v1, UnkC) or isinstance(v3, UnkC):
          s = AbsConst(UnkC())
        else:
          s = AbsConst(v1 * v3)
        return self.ex_mul(s, e2)
      case AbsConst(v1), AbsAdd(AbsConst(v2), e3):
        if isinstance(v1, UnkC) or isinstance(v2, UnkC):
          s = AbsConst(UnkC())
        else:
          s = AbsConst(v1 * v2)
        return self.ex_add(s, self.ex_mul(AbsConst(v1), e3))
      case AbsConst(0), _:
        return AbsConst(0)
      case _, AbsConst(0):
        return AbsConst(0)
      case _:
        return AbsMul(e1, e2)

  def ex_div(self, e1: AbsSymExpr[Number], e2: AbsSymExpr[Number]) -> AbsSymExpr[Number]:
    match e1, e2:
      case AbsConst(v1), AbsConst(v2):
        if isinstance(v1, UnkC) or isinstance(v2, UnkC):
          return AbsConst(UnkC())
        return AbsConst(v1 / v2)
      case e1, AbsConst(1):
        return e1
      case _:
        return AbsDiv(e1, e2)

  def ex_ite(self, cond: AbsSymExpr[bool], true: AbsSymExpr[T], false: AbsSymExpr[T]) -> AbsSymExpr[T]:
    def _is_const(expr: AbsSymExpr) -> bool:
      match expr:
        case AbsConst(_):
          return True
        case AbsRandomVar(_):
          match self.distr(expr):
            case AbsDelta(v, _):
              return _is_const(v)
            case _:
              return False
        case AbsAdd(e1, e2):
          return _is_const(e1) and _is_const(e2)
        case AbsMul(e1, e2):
          return _is_const(e1) and _is_const(e2)
        case AbsDiv(e1, e2):
          return _is_const(e1) and _is_const(e2)
        case AbsIte(cond, true, false):
          return _is_const(cond) and _is_const(true) and _is_const(false)
        case AbsEq(e1, e2):
          return _is_const(e1) and _is_const(e2)
        case AbsLt(e1, e2):
          return _is_const(e1) and _is_const(e2)
        case AbsLst(es):
          return all(_is_const(e) for e in es)
        case AbsPair(e1, e2):
          return _is_const(e1) and _is_const(e2)
        case UnkE(parents):
          return len(parents) == 0
        case _:
          raise ValueError(expr)

    if isinstance(cond, AbsConst) and not isinstance(cond.v, UnkC):
      return true if cond.v else false
    
    if not _is_const(cond):
      return AbsIte(cond, true, false)
    else:
      e = self.narrow_join_expr(true, false, copy(self))
      return e

  def ex_eq(self, e1: AbsSymExpr[T], e2: AbsSymExpr[T]) -> AbsSymExpr[bool]:
    match e1, e2:
      case AbsConst(v1), AbsConst(v2):
        if isinstance(v1, UnkC) or isinstance(v2, UnkC):
          return AbsConst(UnkC())
        return AbsConst(v1 == v2)
      case _:
        return AbsEq(e1, e2)

  def ex_lt(self, e1: AbsSymExpr[Number], e2: AbsSymExpr[Number]) -> AbsSymExpr[bool]:
    match e1, e2:
      case AbsConst(v1), AbsConst(v2):
        if isinstance(v1, UnkC) or isinstance(v2, UnkC):
          return AbsConst(UnkC())
        return AbsConst(v1 < v2)
      case _:
        return AbsLt(e1, e2)
      
  def eval(self, expr: AbsSymExpr) -> AbsSymExpr:
    def _const_list(es: List[AbsSymExpr]) -> Optional[AbsConst[List[AbsSymExpr]]]:
      consts = []
      for e in es:
        if not isinstance(e, AbsConst):
          return None
        consts.append(e.v)

      return AbsConst(consts)
    
    def _eval(expr: AbsSymExpr) -> AbsSymExpr:
      match expr:
        case AbsConst(_):
          return expr
        case AbsRandomVar(_):
          match self.distr(expr):
            case AbsDelta(v, _):
              return _eval(v)
            case _:
              self.set_distr(expr, self.eval_distr(self.distr(expr)))
              return expr
        case AbsAdd(e1, e2):
          return self.ex_add(_eval(e1), _eval(e2))
        case AbsMul(e1, e2):
          return self.ex_mul(_eval(e1), _eval(e2))
        case AbsDiv(e1, e2):
          return self.ex_div(_eval(e1), _eval(e2))
        case AbsIte(cond, true, false):
          return self.ex_ite(_eval(cond), _eval(true), _eval(false))
        case AbsEq(e1, e2):
          return self.ex_eq(_eval(e1), _eval(e2))
        case AbsLt(e1, e2):
          return self.ex_lt(_eval(e1), _eval(e2))
        case AbsLst(es):
          es = [self.eval(e) for e in es]
          const_list = _const_list(es)
          return const_list if const_list is not None else AbsLst(es)
        case AbsPair(e1, e2):
          e1, e2 = self.eval(e1), self.eval(e2)
          match e1, e2:
            case AbsConst(_), AbsConst(_):
              return AbsConst((e1.v, e2.v))
            case _:
              return AbsPair(e1, e2)
        case UnkE(s):
          parents = []
          for var in s:
            match self.distr(var):
              case AbsDelta(v, _):
                continue
              case _:
                if var not in parents:
                  parents.append(var)
          # If no parents, then it's a constant
          if len(parents) == 0:
            return AbsConst(UnkC())
          return UnkE(parents)
        case _:
          raise ValueError(expr)

    return _eval(expr)

  def eval_distr(self, distr: AbsSymDistr) -> AbsSymDistr:
    match distr:
      case AbsNormal(mu, var):
        return AbsNormal(self.eval(mu), self.eval(var))
      case AbsBernoulli(p):
        return AbsBernoulli(self.eval(p))
      case AbsBeta(a, b):
        return AbsBeta(self.eval(a), self.eval(b))
      case AbsBinomial(n, p):
        return AbsBinomial(self.eval(n), self.eval(p))
      case AbsBetaBinomial(n, a, b):
        return AbsBetaBinomial(self.eval(n), self.eval(a), self.eval(b))
      case AbsNegativeBinomial(n, p):
        return AbsNegativeBinomial(self.eval(n), self.eval(p))
      case AbsGamma(a, b):
        return AbsGamma(self.eval(a), self.eval(b))
      case AbsPoisson(l):
        return AbsPoisson(self.eval(l))
      case AbsStudentT(mu, tau2, nu):
        return AbsStudentT(self.eval(mu), self.eval(tau2), self.eval(nu))
      case AbsCategorical(lower, upper, probs):
        return AbsCategorical(self.eval(lower), self.eval(upper), self.eval(probs))
      case AbsDelta(v, sampled):
        return AbsDelta(self.eval(v), sampled)
      case UnkD(s):
        parents = []
        for var in s:
          match self.distr(var):
            case AbsDelta(v, _):
              continue
            case _:
              if var not in parents:
                parents.append(var)
        return UnkD(parents)
      case _:
        raise ValueError(distr)
      
  def mean(self, expr: AbsSymExpr) -> None:
    expr = self.eval(expr)

    match expr:
      case AbsConst(_):
        return
      case AbsRandomVar(_):
        self.marginalize(expr)
      case AbsAdd(left, right):
        self.mean(left)
        self.mean(right)
      # case Sub(left, right):
      #   return mean(left, state) - mean(right, state)
      case AbsMul(left, right):
        self.mean(left)
        self.mean(right)
      case AbsDiv(left, right):
        self.mean(left)
        self.mean(right)
      case AbsIte(cond, true, false):
        cond = self.mean(cond)
        true = self.mean(true)
        false = self.mean(false)
      case _:
        raise ValueError(expr)
  
  def marginalize(self, rv: AbsSymExpr) -> None:
    raise NotImplementedError()

  def value_expr(self, expr: AbsSymExpr) -> AbsConst:
    match expr:
      case AbsConst(_):
        return expr
      case AbsRandomVar(_):
        return self.value(expr)
      case AbsAdd(fst, snd):
        return AbsConst(self.value_expr(fst).v + self.value_expr(snd).v)
      # case Sub(fst, snd):
      #   fst = value(fst, state).v
      #   snd = value(snd, state).v
      #   if (isinstance(fst, float) and isinstance(snd, float)) or \
      #     isinstance(fst, int) and isinstance(snd, int):
      #     return Const(fst - snd)
      #   else:
      #     raise ValueError(fst, snd)
      case AbsMul(fst, snd):
        return AbsConst(self.value_expr(fst).v * self.value_expr(snd).v)
      case AbsDiv(fst, snd):
        return AbsConst(self.value_expr(fst).v / self.value_expr(snd).v)
      case AbsIte(cond, true, false):
        cond = self.value_expr(cond).v
        if isinstance(cond, bool):
          if cond:
            return self.value_expr(true)
          else:
            return self.value_expr(false)
        else:
          raise ValueError(cond)
      case AbsEq(fst, snd):
        fst = self.value_expr(fst).v
        snd = self.value_expr(snd).v
        if isinstance(fst, UnkC) or isinstance(snd, UnkC):
          return AbsConst(UnkC())
        return AbsConst(fst == snd)
      case Lt(fst, snd):
        fst = self.value_expr(fst).v
        snd = self.value_expr(snd).v
        if isinstance(fst, UnkC) or isinstance(snd, UnkC):
          return AbsConst(UnkC())
        return AbsConst(fst < snd)
      case AbsLst(es):
        return AbsConst([self.value_expr(e).v for e in es])
      case AbsPair(fst, snd):
        return AbsConst((self.value_expr(fst).v, self.value_expr(snd).v))
      case UnkE(_):
        return AbsConst(UnkC())
      case _:
        raise ValueError(expr)
      
  # Depends on value_impl, which needs to be implemented by subclasses
  def value(self, rv: AbsRandomVar[T]) -> AbsConst[T]:
    for pv in self.pv(rv):
      self.plan[pv] = DistrEnc.sample

    return self.value_impl(rv)
        
  ### Abstract functions ###
  # These may need be overridden by subclasses, if the subclass has additional
  # state entries
  # If overridden, the subclass should call the superclass method
  
  ## Accessors
  # Returns the specified entry for the given variable
  # Base symbolic state only has 'distribution' and 'pv' entries
  def pv(self, rv: AbsRandomVar) -> Set[Identifier]:
    return self.get_entry(rv, 'pv')

  def distr(self, rv: AbsRandomVar) -> AbsSymDistr:
    distribution = self.get_entry(rv, 'distribution')
    return distribution
  
  ## Mutators
  # Sets the specified entry for the given variable
  # Base symbolic state only has 'distribution' and 'pv' entries
  def set_distr(self, rv: AbsRandomVar, distribution: AbsSymDistr) -> None:
    self.set_entry(rv, distribution=distribution)

  def set_pv(self, rv: AbsRandomVar, pv: Set[Identifier]) -> None:
    self.set_entry(rv, pv=pv)
      
  # Returns the set of random variables referenced in the entries of each variable
  # Base symbolic state only needs care about distribution of each variable
  def entry_referenced_rvs(self, rvs: Set[AbsRandomVar]) -> Set[AbsRandomVar]:
    return set().union(*(self.eval_distr(self.distr(rv)).rvs() for rv in rvs))

  # Renames rv to new in the entries of symbolic state
  # Base symbolic state only needs care about distribution of each variable
  def entry_rename_rv(self, rv: AbsRandomVar, old: AbsRandomVar, new: AbsRandomVar) -> None:
    self.set_distr(rv, self.distr(rv).rename(old, new))

  # Joins the entries to the same variable in the two states
  # Base symbolic state only holds 'distribution' and 'pv' entries
  def entry_join(self, rv: AbsRandomVar, other: 'AbsSymState') -> None:
    self.set_distr(rv, join_distr(self.distr(rv), other.distr(rv)))
    self.set_pv(rv, self.pv(rv) | other.pv(rv))

  # Sets the inferred distribution encoding as dynamic
  # May need to be overridden by subclasses
  def set_dynamic(self, variable: AbsRandomVar) -> None:
    for pv in self.pv(variable):
      if pv in self.annotations:
        raise AnalysisViolatedAnnotationError(
          f"{self.get_entry(variable, 'pv')} is annotated as {self.annotations[pv]} but cannot be determined")
    
    for pv in self.pv(variable):
      self.plan[pv] = DistrEnc.dynamic

  ### Symbolic Interface ###
  # These need to be implemented by subclasses
  def assume(self, name: Optional[Identifier], annotation: Optional[Annotation], distribution: AbsSymDistr[T]) -> AbsRandomVar[T]:
    raise NotImplementedError()

  def observe(self, rv: AbsRandomVar[T], value: AbsConst[T]) -> float:
    raise NotImplementedError()
  
  def value_impl(self, rv: AbsRandomVar[T]) -> AbsConst[T]:
    raise NotImplementedError()

# Domain Operations

# TODO: would be neater as methods in each abstract expression
      
def join_expr(e1: AbsSymExpr, e2: AbsSymExpr) -> AbsSymExpr:
  match e1, e2:
    case AbsConst(v1), AbsConst(v2):
      eq = v1 == v2
      if isinstance(eq, UnkC):
        return AbsConst(UnkC())
      elif eq:
        return AbsConst(v1)
      else:
        return AbsConst(UnkC())
    case AbsConst(_), AbsRandomVar(_):
      return e2
    case AbsRandomVar(_), AbsConst(_):
      return e1
    case AbsRandomVar(_), AbsRandomVar(_):
      return e1 if e1 == e2 else UnkE([e1, e2])
    case AbsAdd(e11, e12), AbsAdd(e21, e22):
      return AbsAdd(join_expr(e11, e21), join_expr(e12, e22))
    case AbsMul(e11, e12), AbsMul(e21, e22):
      return AbsMul(join_expr(e11, e21), join_expr(e12, e22))
    case AbsDiv(e11, e12), AbsDiv(e21, e22):
      return AbsDiv(join_expr(e11, e21), join_expr(e12, e22))
    case AbsIte(cond1, true1, false1), AbsIte(cond2, true2, false2):
      return AbsIte(join_expr(cond1, cond2), 
                    join_expr(true1, true2),
                    join_expr(false1, false2))
    case AbsEq(e11, e12), AbsEq(e21, e22):
      return AbsEq(join_expr(e11, e21), join_expr(e12, e22))
    case AbsLt(e11, e12), AbsLt(e21, e22):
      return AbsLt(join_expr(e11, e21), join_expr(e12, e22))
    case AbsConst(c), AbsLst(es):
      if isinstance(c, UnkC) or isinstance(c, list):
        return join_expr(AbsLst([]), e2)
      else:
        raise ValueError(c)
    case AbsLst(es), AbsConst(c):
      if isinstance(c, UnkC) or isinstance(c, list):
        return join_expr(e1, AbsLst([]))
      else:
        raise ValueError(c)
    case AbsLst(es1), AbsLst(es2):
      es = []
      max_len = max(len(es1), len(es2))
      for i in range(max_len):
        if i < len(es1) and i < len(es2):
          es.append(join_expr(es1[i], es2[i]))
        else:
          if i < len(es1):
            rest_parents = AbsLst(es1[i:]).rvs()
          else: # i < len(es2)
            rest_parents = AbsLst(es2[i:]).rvs()

          # Collapse with the last element if it's just UnkE
          if len(es) > 0:
            match es[-1]:
              case UnkE(parents):
                new_parents = []
                for p in parents + rest_parents:
                  if p in new_parents:
                    continue
                  new_parents.append(p)
                es[-1] = UnkE(new_parents)
              case _:
                es.append(UnkE(rest_parents))
            break

      return AbsLst(es)     
    case AbsConst(c), AbsPair(e21, e22):
      if isinstance(c, UnkC):
        return join_expr(AbsPair(AbsConst(UnkC()), AbsConst(UnkC())), e2)
      elif isinstance(c, tuple):
        return join_expr(AbsPair(AbsConst(c[0]), AbsConst(c[1])), e2)
      else:
        raise ValueError(c)
    case AbsPair(e11, e12), AbsConst(c):
      if isinstance(c, UnkC):
        return join_expr(e1, AbsPair(AbsConst(UnkC()), AbsConst(UnkC())))
      elif isinstance(c, tuple):
        return join_expr(e1, AbsPair(AbsConst(c[0]), AbsConst(c[1])))
      else:
        raise ValueError(c)
    case AbsPair(e11, e12), AbsPair(e21, e22):
      return AbsPair(join_expr(e11, e21), join_expr(e12, e22))
    case _, _:
      parents = []
      for p in e1.rvs() + e2.rvs():
        if p in parents:
          continue
        parents.append(p)
      return UnkE(parents)
 
def join_distr(d1: AbsSymDistr, d2: AbsSymDistr) -> AbsSymDistr:
  match d1, d2:
    case AbsNormal(mu1, var1), AbsNormal(mu2, var2):
      return AbsNormal(join_expr(mu1, mu2), join_expr(var1, var2))
    case AbsBernoulli(p1), AbsBernoulli(p2):
      return AbsBernoulli(join_expr(p1, p2))
    case AbsBeta(a1, b1), AbsBeta(a2, b2):
      return AbsBeta(join_expr(a1, a2), join_expr(b1, b2))
    case AbsBinomial(n1, p1), AbsBinomial(n2, p2):
      return AbsBinomial(join_expr(n1, n2), join_expr(p1, p2))
    case AbsBetaBinomial(n1, a1, b1), AbsBetaBinomial(n2, a2, b2):
      return AbsBetaBinomial(join_expr(n1, n2), join_expr(a1, a2), join_expr(b1, b2))
    case AbsNegativeBinomial(n1, p1), AbsNegativeBinomial(n2, p2):
      return AbsNegativeBinomial(join_expr(n1, n2), join_expr(p1, p2))
    case AbsGamma(a1, b1), AbsGamma(a2, b2):
      return AbsGamma(join_expr(a1, a2), join_expr(b1, b2))
    case AbsPoisson(l1), AbsPoisson(l2):
      return AbsPoisson(join_expr(l1, l2))
    case AbsStudentT(mu1, tau21, nu1), AbsStudentT(mu2, tau22, nu2):
      return AbsStudentT(join_expr(mu1, mu2), join_expr(tau21, tau22), join_expr(nu1, nu2))
    case AbsCategorical(lower1, upper1, probs1), AbsCategorical(lower2, upper2, probs2):
      return AbsCategorical(join_expr(lower1, lower2), join_expr(upper1, upper2), join_expr(probs1, probs2))
    case AbsDelta(v1, sampled1), AbsDelta(v2, sampled2):
      if sampled1 == sampled2:
        return AbsDelta(join_expr(v1, v2), sampled1)
      else:
        # Once a delta is sampled, it is always sampled
        return AbsDelta(join_expr(v1, v2), sampled=True)
    case UnkD(parents1), UnkD(parents2):
      return UnkD(parents1 + parents2)
    case _, _:
      parents = []
      for p in d1.rvs() + d2.rvs():
        if p in parents:
          continue
        parents.append(p)
      return UnkD(parents)
    
# Abstract prob states 
# TODO: Basically the same as concrete version, so can refractor somehow
class AbsContext(object):
  def __init__(self, init={}) -> None:
    super().__init__()
    self.context: Dict[Identifier, AbsSymExpr] = init

  def __getitem__(self, identifier: Identifier) -> AbsSymExpr:
    return self.context[identifier]

  def __setitem__(self, identifier: Identifier, value: AbsSymExpr) -> None:
    self.context[identifier] = value

  def __len__(self) -> int:
    return len(self.context)

  def __iter__(self):
    return iter(self.context)

  def __or__(self, other: 'AbsContext') -> 'AbsContext':
    return AbsContext({**self.context, **other.context})

  def __str__(self) -> str:
    return f"AbsContext({', '.join(map(str, self.context.items()))})"
  
  def temp_var(self, name: str="x") -> Identifier:
    i = 0
    while True:
      identifier = Identifier(None, f"{name}_{i}")
      if identifier not in self:
        return identifier
      i += 1

  def rename(self, old: AbsRandomVar, new: AbsRandomVar) -> None:
    for k, v in self.context.items():
      self.context[k] = v.rename(old, new)

class AbsParticle(object):
  def __init__(self, cont: Expr[AbsSymExpr], state: AbsSymState = AbsSymState()) -> None:
    super().__init__()
    self.cont: Expr[AbsSymExpr] = cont
    self.state: AbsSymState = state

  def __copy__(self) -> 'AbsParticle':
    return AbsParticle(self.cont, copy(self.state))

  @property
  def final_expr(self) -> AbsSymExpr:
    assert isinstance(self.cont, AbsSymExpr)
    return self.cont
    
  def update(self, cont: Optional[Expr] = None,
              state: Optional[AbsSymState] = None) -> 'AbsParticle':
    if cont is not None:
      self.cont = cont
    if state is not None:
      self.state = state
    return self

  def copy(self, cont: Optional[Expr] = None,
             state: Optional[AbsSymState] = None) -> 'AbsParticle':
    return AbsParticle(
        self.cont if cont is None else cont,
        copy(self.state) if state is None else copy(state),
    )

  def __str__(self):
    return f"AbsParticle({self.cont}, {self.state})"
  
class AbsMixture(object):
  def __init__(self, mixture: Tuple[AbsSymExpr, AbsSymState]):
    super().__init__()
    if len(mixture) == 0:
      raise ValueError("Empty distribution")
    self.mixture: Tuple[AbsSymExpr, AbsSymState] = mixture

  @property
  def is_pair_mixture(self) -> bool:
    return is_abs_pair(self.mixture[0])

  def get_pair_mixture(self) -> Tuple['AbsMixture', 'AbsMixture']:
    expr, state = self.mixture
    f, s = get_abs_pair(expr)
    return AbsMixture((f, state)), AbsMixture((s, state))

  @property
  def is_lst_mixture(self) -> bool:
    return is_abs_lst(self.mixture[0])

  def get_lst_mixture(self) -> List['AbsMixture'] | UnkE:
    expr, state = self.mixture
    lst = get_abs_lst(expr)
    match lst:
      case UnkE(_):
        return lst
      case _:
        return [AbsMixture((e, state)) for e in lst]
      
  def mean(self) -> None:
    expr, state = self.mixture
    state.mean(expr)

class AbsProbState(object):
  def __init__(self, cont: Expr, method: type[AbsSymState]) -> None:
    super().__init__()
    self.particles : AbsParticle = AbsParticle(cont, method())

  def __str__(self) -> str:
    return f"{self.particles}"
  



  def mixture(self) -> AbsMixture:
    return AbsMixture((self.particles.final_expr, self.particles.state))

  def result(self) -> None:
    mixture = self.mixture()
    
    def _get_mean(res: AbsMixture) -> None:
      if res.is_pair_mixture:
        fst, snd = res.get_pair_mixture()
        if not isinstance(fst.mixture[0], UnkE):
          fst = _get_mean(fst)
        if not isinstance(snd.mixture[0], UnkE):
          snd = _get_mean(snd)
      elif res.is_lst_mixture:
        lst = res.get_lst_mixture()
        if not isinstance(lst, UnkE):
          list(map(lambda x: _get_mean(x), lst))
      else:
        res.mean()
      
    return _get_mean(mixture)