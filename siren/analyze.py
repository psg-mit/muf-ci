from typing import Any, Optional, List, Dict, Tuple
import numpy as np
from copy import copy

from siren.grammar import *
from siren.utils import get_abs_pair, get_abs_lst
from siren.analysis.interface import *
from siren.inference_plan import DistrEnc

def assume(name: Identifier, annotation: Optional[Annotation], distribution: AbsSymExpr, 
           state: AbsSymState) -> AbsConst | AbsRandomVar:
  assert isinstance(distribution, AbsSymDistr)
  rv = state.assume(name, annotation, distribution)
  state.plan[name] = DistrEnc.symbolic
  if annotation is Annotation.sample:
    return state.value(rv)
  return rv

def observe(distribution: AbsSymExpr, v: AbsSymExpr, state: AbsSymState) -> None:
  assert isinstance(distribution, AbsSymDistr)
  rv = state.assume(None, None, distribution)
  v = state.value_expr(v)
  state.observe(rv, v)
  return
    
# true if expr does not contain resample and observe
def pure(expr: Expr[AbsSymExpr], functions: Dict[Identifier, Function]) -> bool:
  def _pure(expr: Expr[AbsSymExpr]) -> bool:
    if isinstance(expr, AbsSymExpr):
      return True
    
    match expr:
      case Const(_): # a bit hacky, but Const counts has external grammar as well
        return True
      case Resample():
        return False
      case Observe(_, _):
        return False
      case Identifier(_, _):
        return True
      case GenericOp(op, args):
        return all(_pure(arg) for arg in args)
      case Fold(func, init, acc):
        return _pure(functions[func].body) and _pure(init) and _pure(acc)
      case Apply(func, args):
        return all(_pure(arg) for arg in args) and _pure(functions[func].body)
      case AbsLst(exprs):
        return all(_pure(expr) for expr in exprs)
      case AbsPair(fst, snd):
        return _pure(fst) and _pure(snd)
      case IfElse(cond, true, false):
        return _pure(cond) and _pure(true) and _pure(false)
      case Let(_, value, body):
        return _pure(value) and _pure(body)
      case LetRV(_, _, distribution, expression):
        return _pure(distribution) and _pure(expression)
      case _:
        raise ValueError(expr)
      
  return _pure(expr)

def match_pattern(pattern: List[Any], expr: AbsSymExpr) -> AbsContext:
    if len(pattern) == 0:
      return AbsContext()
    elif len(pattern) == 1:
      return AbsContext({pattern[0]: expr})
    else:
      try:
        fst, snd = get_abs_pair(expr)
      except ValueError:
        raise ValueError(pattern, expr)
      if len(pattern) == 2:
        return match_pattern(pattern[0], fst) | match_pattern(pattern[1], snd)
      else:
        return match_pattern(pattern[0], fst) | match_pattern(pattern[1:], snd)

def evaluate_particle(particle: AbsParticle, functions: Dict[Identifier, Function[AbsSymExpr]]) -> AbsParticle:
  def _evaluate_args(particle: AbsParticle, 
                     args: List[Expr[AbsSymExpr]], 
                     new_args: List[AbsSymExpr]) -> Tuple[AbsParticle, List[Expr[AbsSymExpr]], List[AbsSymExpr]]:
    if len(args) == 0:
      return particle, args, new_args
    p1 = _evaluate(particle.update(cont=args[0]))
    new_args.append(p1.final_expr)
    return _evaluate_args(p1, args[1:], new_args)

  def _evaluate_ops(particle: AbsParticle, op: Operator, args: AbsSymExpr) -> AbsParticle:
    def _evaluate_unops(particle: AbsParticle, constructor: Any, args: AbsSymExpr) -> AbsParticle:
      return particle.update(cont=constructor(args))

    def _evaluate_binops(particle: AbsParticle, constructor: Any, args: AbsSymExpr) -> AbsParticle:
      fst, snd = get_abs_pair(args)
      return particle.update(cont=constructor(fst, snd))
  
    def _evaluate_triops(particle: AbsParticle, constructor: Any, args: AbsSymExpr) -> AbsParticle:
      fst, args2 = get_abs_pair(args)
      snd, trd = get_abs_pair(args2)
      return particle.update(cont=constructor(fst, snd, trd))
        
    match op.name:
      case "add":
        return _evaluate_binops(particle, AbsAdd, args)
      case "sub":
        return _evaluate_binops(particle, lambda fst,snd: AbsAdd(fst, AbsMul(AbsConst(-1), snd)), args)
      case "mul":
        return _evaluate_binops(particle, AbsMul, args)
      case "div":
        return _evaluate_binops(particle, AbsDiv, args)
      case "eq":
        return _evaluate_binops(particle, AbsEq, args)
      case "lt":
        return _evaluate_binops(particle, AbsLt, args)
      case "cons":
        def _make_cons(fst, snd):
          l = get_abs_lst(snd)
          if isinstance(l, UnkE):
            parents = []
            for p in fst.rvs() + l.rvs():
              parents += [p]
            return UnkE(parents)
          return AbsLst([fst] + l)
        return _evaluate_binops(particle, _make_cons, args)
      case "lst":
        def _make_list(x):
          if isinstance(x, AbsConst):
            if x.v is None:
              return AbsConst([])
          return AbsLst([x])
        return _evaluate_unops(particle, _make_list, args)
      case "pair":
        return _evaluate_binops(particle, AbsPair, args)
      case "gaussian":
        return _evaluate_binops(particle, AbsNormal, args)
      case "beta":
        return _evaluate_binops(particle, AbsBeta, args)
      case "bernoulli":
        return _evaluate_unops(particle, AbsBernoulli, args)
      case "binomial":
        return _evaluate_binops(particle, AbsBinomial, args)
      case "beta_binomial":
        return _evaluate_triops(particle, AbsBetaBinomial, args)
      case "negative_binomial":
        return _evaluate_binops(particle, AbsNegativeBinomial, args)
      case "exponential":
        return _evaluate_binops(particle, AbsGamma, AbsPair(AbsConst(1.0), args))
      case "gamma":
        return _evaluate_binops(particle, AbsGamma, args)
      case "poisson":
        return _evaluate_unops(particle, AbsPoisson, args)
      case "delta":
        return _evaluate_unops(particle, AbsDelta, args)
      case "categorical":
        return _evaluate_triops(particle, AbsCategorical, args)
      case "uniform_int":
        a, b = get_abs_pair(args)
        # For now, uniform only takes constants
        # [a, b]
        a, b = particle.state.eval(a), particle.state.eval(b)
        match (a, b):
          case (AbsConst(a), AbsConst(b)):
            if isinstance(a, UnkC) or isinstance(b, UnkC):
              return _evaluate_ops(particle, Operator.categorical, 
                                  AbsPair(AbsConst(a), AbsPair(AbsConst(b), AbsConst(UnkC()))))
            assert isinstance(a, Number) and isinstance(b, Number)\
              and round(a) == a and round(b) == b and a <= b
            a, b = int(a), int(b)
            probs = AbsConst(list(np.ones(b - a + 1) / (b - a + 1)))
            return _evaluate_ops(particle, Operator.categorical, 
                                  AbsPair(AbsConst(a), AbsPair(AbsConst(b), probs)))
          case _:
            raise ValueError(args)
      case "student_t":
        return _evaluate_triops(particle, AbsStudentT, args)
      case _:
        raise ValueError(op.name)
      
  def _convert_args(args: List[AbsSymExpr]) -> AbsSymExpr:
    if len(args) == 0:
      return AbsConst(None)
    elif len(args) == 1:
      return args[0]
    else:
      return AbsPair(args[0], _convert_args(args[1:]))
    
  def _evaluate_list(particle: AbsParticle, func: Identifier, args: List[Expr[AbsSymExpr]]) -> AbsParticle:
    assert func.module == 'List'
    match func.name:
      case 'hd':
        (p1, old_args, new_args) = _evaluate_args(particle, args, [])
        assert len(old_args) == 0
        new_args = _convert_args(new_args)
        
        exprs = get_abs_lst(new_args)
        if isinstance(exprs, UnkE):
          return p1.update(cont=exprs)
        if len(exprs) == 0:
          raise ValueError(new_args)
        return p1.update(cont=exprs[0])
      case 'tl':
        (p1, old_args, new_args) = _evaluate_args(particle, args, [])
        assert len(old_args) == 0
        new_args = _convert_args(new_args)
        
        exprs = get_abs_lst(new_args)
        if isinstance(exprs, UnkE):
          return p1.update(cont=exprs)
        if len(exprs) == 0:
          raise ValueError(new_args)
        return p1.update(cont=AbsLst(exprs[1:]))
      case 'len':
        (p1, old_args, new_args) = _evaluate_args(particle, args, [])
        assert len(old_args) == 0
        new_args = _convert_args(new_args)
        
        exprs = get_abs_lst(new_args)
        if isinstance(exprs, UnkE):
          return p1.update(cont=AbsConst(UnkC()))
        return p1.update(cont=AbsConst(len(exprs)))
      case 'range':
        (p1, old_args, new_args) = _evaluate_args(particle, args, [])
        assert len(old_args) == 0
        new_args = _convert_args(new_args)
        
        a, b = get_abs_pair(new_args)
        if isinstance(a, UnkE) or isinstance(b, UnkE):
          return p1.update(cont=UnkE([]))
        match a, b:
          case AbsConst(a), AbsConst(b):
            assert isinstance(a, int) and isinstance(b, int) and a <= b
            l : List[AbsSymExpr] = list(map(AbsConst, range(a, b)))
            return p1.update(cont=AbsLst(l))
          case _:
            raise ValueError(new_args)
      case 'rev':
        (p1, old_args, new_args) = _evaluate_args(particle, args, [])
        assert len(old_args) == 0
        new_args = _convert_args(new_args)
        
        exprs = get_abs_lst(new_args)
        if isinstance(exprs, UnkE):
          return p1.update(cont=exprs)
        l: List[AbsSymExpr] = exprs[::-1]
        return p1.update(cont=AbsLst(l))
      case 'map':
        map_func = args[0]
        assert isinstance(map_func, Identifier)

        (p1, old_args, new_args) = _evaluate_args(particle, args[1:], [])
        assert len(old_args) == 0
        new_args = _convert_args(new_args)
        
        exprs = get_abs_lst(new_args)
        if isinstance(exprs, UnkE):
          return p1.update(cont=exprs)
        new_e = AbsLst([])
        for e in exprs[::-1]:
          new_e = GenericOp(Operator.cons, [
            Apply(map_func, [e]),
            new_e,
          ])
        return _evaluate(p1.update(cont=new_e))
      case _:
        raise ValueError(func)
      
  def _evaluate_file(particle: AbsParticle, func: Identifier, args: List[Expr[AbsSymExpr]]) -> AbsParticle:
    assert func.module == 'File'
    match func.name:
      case 'read':
        (p1, old_args, new_args) = _evaluate_args(particle, args, [])
        assert len(old_args) == 0
        new_args = _convert_args(new_args)
        
        match new_args:
          case AbsConst(_):
            return p1.update(cont=UnkE([]))
          case _:
            raise ValueError(new_args)
      case _:
        raise ValueError(func)
    
  def _evaluate(particle: AbsParticle) -> AbsParticle:
    if isinstance(particle.cont, AbsSymExpr):
      return particle
    match particle.cont:
      case Const(v): # a bit hacky, but Const counts has external grammar as well
        return particle.update(cont=AbsConst(v))
      case Identifier(_, _):
        return particle.update(cont=particle.state.ctx[particle.cont])
      case GenericOp(op, args):
        (p1, old_args, new_args) = _evaluate_args(particle, args, [])
        assert len(old_args) == 0
        new_args = _convert_args(new_args)
        return _evaluate_ops(p1, op, new_args)
      case Fold(func, lst, acc):
        p1 = _evaluate(particle.update(cont=lst))
        lst_val = p1.final_expr

        p2 = _evaluate(p1.update(cont=acc))
        acc_val = p2.final_expr
        match lst_val:
          case AbsLst(exprs):
            if len(exprs) == 0:
              return p2.update(cont=acc_val)
            else:
              hd, tl = exprs[0], exprs[1:]
              tempvar = p2.state.ctx.temp_var()
              e = Let([tempvar], 
                      Apply(func, [hd, acc_val]), 
                      Fold(func, AbsLst(tl), tempvar))
              return _evaluate(p2.update(cont=e))
          case UnkE(_):
            state_old = copy(p2.state)
            p3 = _evaluate(p2.copy(cont=Apply(func, [lst_val, acc_val])))
            acc_new = p2.state.narrow_join_expr(acc_val, p3.final_expr, p3.state)
            p2.state.clean(state_old, acc_new)
            if acc_val == acc_new and state_old == p2.state:
              return p3.update(cont=acc_new, state=state_old)
            else:
              return _evaluate(p3.update(cont=Fold(func, lst_val, acc_new), state=p2.state))
          case _:
            raise ValueError(lst_val)
      case Apply(func, args):
        if func.module is not None:
          # use lib functions
          match func.module:
            case "List":
              return _evaluate_list(particle, func, args)
            case "File":
              return _evaluate_file(particle, func, args)
            case _:
              raise ValueError(func.module)
        else:
          (p1, old_args, new_args) = _evaluate_args(particle, args, [])
          assert len(old_args) == 0
          new_args = _convert_args(new_args)
          function = functions[func]
          ctx = copy(p1.state.ctx)
          p1.state.ctx = match_pattern(function.args, new_args)
          p2 = _evaluate(p1.update(cont=function.body))
          p2.state.ctx = ctx
          return p2
      case IfElse(cond, true, false):
        p1 = _evaluate(particle.update(cont=cond))
        cond_val = p1.final_expr

        if pure(true, functions) and pure(false, functions):
          p2 = _evaluate(p1.update(cont=true))
          then_val = p2.final_expr
          p3 = _evaluate(p2.update(cont=false))
          return p3.update(cont=AbsIte(cond_val, then_val, p3.final_expr))
        else:
          cond_value = p1.state.value_expr(p1.final_expr)
          match cond_value:
            case AbsConst(v):
              if isinstance(v, UnkC):
                p2 = _evaluate(p1.copy(cont=true))
                then_val = p2.final_expr
                p3 = _evaluate(p1.copy(cont=false))
                else_val = p3.final_expr
                e = p2.state.narrow_join_expr(then_val, else_val, p3.state)
                p2.state.plan |= p3.state.plan
                return p3.update(cont=e, state=p2.state)
              elif v:
                return _evaluate(p1.update(cont=true))
              else:
                return _evaluate(p1.update(cont=false))
            case _:
              raise ValueError(cond_value)
      case Let(pattern, v, body):
        ctx = copy(particle.state.ctx)
        p1 = _evaluate(particle.update(cont=v))
        p1.state.ctx |= match_pattern(pattern, p1.final_expr)
        p2 = _evaluate(p1.update(cont=body))
        p2.state.ctx = ctx
        return p2
      case LetRV(identifier, annotation, distribution, expression):
        p1 = _evaluate(particle.update(cont=distribution))
        assert isinstance(p1.cont, Op) # should still be an Op
        assert identifier.name is not None # RVs should always be named
        rv = assume(identifier, annotation, p1.final_expr, p1.state)
        return _evaluate(p1.update(cont=Let([identifier], rv, expression)))
      case Observe(expression, v):
        p1 = _evaluate(particle.update(cont=expression))
        assert isinstance(p1.cont, Op) # should still be an Op
        d = p1.final_expr
        p2 = _evaluate(p1.update(cont=v))
        observe(d, p2.final_expr, p2.state)
        return p2
      case Resample():
        return particle.update(cont=AbsConst(None))
      case _:
        raise ValueError(particle.cont)
      
  return _evaluate(particle)

def evaluate(program: Program, method: type[AbsSymState]) -> AbsProbState:
  functions, expression = program.functions, program.main

  # Make lookup for functions
  functions = {f.name: f for f in functions}

  probstate = AbsProbState(expression, method)

  probstate.particles = evaluate_particle(probstate.particles, functions)

  return probstate

def analyze(program: Program, method: type[AbsSymState]) -> InferencePlan:
  prob = evaluate(program, method)

  inferred_plan = prob.particles.state.plan

  return inferred_plan

