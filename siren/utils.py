from typing import Dict, List, Tuple

from siren.grammar import *

def get_lst(l: Expr[SymExpr]) -> List[SymExpr]:
  match l:
    case Lst(exprs):
      return exprs
    case Const(exprs):
      if isinstance(exprs, list):
        return [Const(e) for e in exprs]
      else:
        raise ValueError(exprs)
    case _:
      raise ValueError(l)
    
def is_lst(expr: SymExpr) -> bool:
  match expr:
    case Lst(_):
      return True
    case Const(value):
      return isinstance(value, list)
    case _:
      return False
    
def get_pair(p: Expr[SymExpr]) -> Tuple[SymExpr, SymExpr]:
  match p:
    case Pair(fst, snd):
      return fst, snd
    case Const(exprs):
      if isinstance(exprs, Tuple):
        a, b = exprs
        return Const(a), Const(b)
      else:
        raise ValueError(exprs)
    case _:
      raise ValueError(p)

def is_pair(expr: SymExpr) -> bool:
  match expr:
    case Pair(_, _):
      return True
    case Const(value):
      return isinstance(value, tuple)
    case _:
      return False

def get_abs_lst(l: Expr[AbsSymExpr]) -> List[AbsSymExpr] | UnkE:
  match l:
    case AbsLst(exprs):
      return exprs
    case AbsConst(exprs):
      if isinstance(exprs, list):
        return [AbsConst(e) for e in exprs]
      else:
        raise ValueError(exprs)
    case UnkE(parents):
      return UnkE(parents)
    case _:
      raise ValueError(l)
    
def get_abs_pair(p: Expr[AbsSymExpr]) -> Tuple[AbsSymExpr, AbsSymExpr]:
  match p:
    case AbsPair(fst, snd):
      return fst, snd
    case AbsConst(exprs):
      if isinstance(exprs, Tuple):
        a, b = exprs
        return AbsConst(a), AbsConst(b)
      else:
        raise ValueError(exprs)
    case UnkE(parents):
      return UnkE(parents), UnkE(parents)
    case _:
      raise ValueError(p)