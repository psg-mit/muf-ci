from typing import Optional, Tuple

from siren.grammar import *
from siren.inference.interface import SymState

def is_affine(state: SymState, expr: SymExpr, rv: RandomVar) -> Optional[Tuple[SymExpr, SymExpr]]:
  match expr:
    case Const(_):
      return (Const(0), expr)
    case RandomVar(_):
      if expr == rv:
        return (Const(1), Const(0))
      else:
        return (Const(0), expr)
    case Add(e1, e2):
      coefs1 = is_affine(state, e1, rv)
      coefs2 = is_affine(state, e2, rv)
      if coefs1 is None or coefs2 is None:
        return None
      else:
        a1, b1 = coefs1
        a2, b2 = coefs2
        return (state.ex_add(a1, a2), state.ex_add(b1, b2))
    case Mul(e1, e2):
      coefs1 = is_affine(state, e1, rv)
      coefs2 = is_affine(state, e2, rv)
      if coefs1 is None or coefs2 is None:
        return None
      else:
        a1, b1 = coefs1
        a2, b2 = coefs2
        match state.eval(a1), state.eval(a2):
          case Const(0), Const(0):
            return (Const(0), state.ex_mul(b1, b2))
          case a1, Const(0):
            return (state.ex_mul(a1, b2), state.ex_mul(b1, b2))
          case Const(0), a2:
            return (state.ex_mul(b1, a2), state.ex_mul(b1, b2))
          case _:
            return None
    case Div(e1, e2):
      coefs1 = is_affine(state, e1, rv)
      coefs2 = is_affine(state, e2, rv)
      if coefs1 is None or coefs2 is None:
        return None
      else:
        a1, b1 = coefs1
        a2, b2 = coefs2
        match state.eval(a2):
          case Const(0):
            return (state.ex_div(a1, b2), state.ex_div(b1, b2))
          case _:
            return None
    case Ite(_):
      return None
    case Eq(_):
      return None
    case Lt(_):
      return None
    case Lst(_):
      return  None
    case Pair(_):
      return None
    case _:
      raise ValueError(expr)
    
# Returns (marginal, posterior) distributions
def gaussian_conjugate(state: SymState, rv_par: RandomVar, rv_child: RandomVar) -> Optional[Tuple[Normal, Normal]]:
  prior, likelihood = state.distr(rv_par), state.distr(rv_child)
  match prior, likelihood:
    case Normal(mu0, var0), Normal(mu, var):
      coefs = is_affine(state, mu, rv_par)
      if coefs is None:
        return None
      else:
        a, b = coefs
        if not mu0.depends_on(rv_child, True) \
          and not var0.depends_on(rv_child, True) \
          and not var.depends_on(rv_par, True):

          mu01 = state.ex_add(state.ex_mul(a, mu0), b)
          var01 = state.ex_mul(state.ex_mul(a, a), var0)

          denom = state.ex_add(state.ex_div(Const(1), var01), state.ex_div(Const(1), var))
          var02 = state.ex_div(Const(1), denom)

          sum1 = state.ex_add(state.ex_div(mu01, var01), state.ex_div(rv_child, var))
          mu02 = state.ex_mul(sum1, var02)

          mu1_new = state.ex_div(state.ex_add(mu02, state.ex_mul(Const(-1), b)), a)
          var1_new = state.ex_div(var02, state.ex_mul(a, a))

          mu0_new = mu01
          var0_new = state.ex_add(var01, var)

          return (Normal(mu0_new, var0_new), Normal(mu1_new, var1_new))
        else:
          return None
    case _:
      return None
    
def bernoulli_conjugate(state: SymState, rv_par: RandomVar, rv_child: RandomVar) -> Optional[Tuple[Bernoulli, Bernoulli]]:
  prior, likelihood = state.distr(rv_par), state.distr(rv_child)
  match prior, likelihood:
    case Bernoulli(p1), Bernoulli(p2):
      if p2.depends_on(rv_par, False) and \
        not p1.depends_on(rv_child, True):

        p2_new = state.ex_add(state.ex_mul(p1, p2.subst_rv(rv_par, Const(True))),
                              state.ex_mul(state.ex_add(Const(1), state.ex_mul(Const(-1), p1)),
                                          p2.subst_rv(rv_par, Const(False))))
        
        p1_num_sub = state.ex_ite(rv_child, p2, state.ex_add(Const(1), state.ex_mul(Const(-1), p2)))
        p1_num = state.ex_mul(p1, p1_num_sub.subst_rv(rv_par, Const(True)))
        p1_denom = state.ex_ite(rv_child, p2_new, state.ex_add(Const(1), state.ex_mul(Const(-1), p2_new)))
        p1_new = state.ex_div(p1_num, p1_denom)

        return (Bernoulli(p2_new), Bernoulli(p1_new))
      else:
        return None
    case _:
      return None
    
def beta_bernoulli_conjugate(state: SymState, rv_par: RandomVar, rv_child: RandomVar) -> Optional[Tuple[Bernoulli, Beta]]:
  prior, likelihood = state.distr(rv_par), state.distr(rv_child)
  match prior, likelihood:
    case Beta(a, b), Bernoulli(p):
      if rv_par == p \
        and not a.depends_on(rv_child, True) \
        and not b.depends_on(rv_child, True):

        p_new = state.ex_div(a, state.ex_add(a, b))

        a_new = state.ex_add(a, state.ex_ite(rv_child, Const(1), Const(0)))
        b_new = state.ex_add(b, state.ex_ite(rv_child, Const(0), Const(1)))

        return (Bernoulli(p_new), Beta(a_new, b_new))
      else:
        return None
    case _:
      return None
    
def beta_binomial_conjugate(state: SymState, rv_par: RandomVar, rv_child: RandomVar) -> Optional[Tuple[BetaBinomial, Beta]]:
  prior, likelihood = state.distr(rv_par), state.distr(rv_child)
  match prior, likelihood:
    case Beta(a, b), Binomial(n, p):
      if isinstance(n, Const) \
        and isinstance(a, Const)\
        and rv_par == p \
        and not a.depends_on(rv_child, True) \
        and not b.depends_on(rv_child, True):

        a_new = state.ex_add(a, rv_child)
        b_new = state.ex_add(b, state.ex_add(Const(float(n.v)), state.ex_mul(Const(-1), rv_child)))

        return (BetaBinomial(n, a, b), Beta(a_new, b_new))
      else:
        return None
    case _:
      return None
    
def gamma_poisson_conjugate(state: SymState, rv_par: RandomVar, rv_child: RandomVar) -> Optional[Tuple[NegativeBinomial, Gamma]]:
  prior, likelihood = state.distr(rv_par), state.distr(rv_child)
  match prior, likelihood:
    case Gamma(a, b), Poisson(l):
      # a is an integer 
      if isinstance(a, Const) \
        and np.isclose(round(a.v), a.v) \
        and rv_par == l \
        and not b.depends_on(rv_child, True):

        n_new = Const(int(a.v))
        p_new = state.ex_div(b, state.ex_add(Const(1), b))

        a_new = state.ex_add(a, rv_child)
        b_new = state.ex_add(b, Const(1))

        return (NegativeBinomial(n_new, p_new), Gamma(a_new, b_new))
      else:
        return None
    case _:
      return None
    
def gamma_normal_conjugate(state: SymState, rv_par: RandomVar, rv_child: RandomVar) -> Optional[Tuple[StudentT, Gamma]]:
  prior, likelihood = state.distr(rv_par), state.distr(rv_child)
  match prior, likelihood:
    case Gamma(a, b), Normal(mu, var):
      if isinstance(mu, Const) \
        and var == state.ex_div(Const(1), rv_par) \
        and not a.depends_on(rv_child, True) \
        and not b.depends_on(rv_child, True):

        tau2 = state.ex_div(b, a)
        nu = state.ex_mul(Const(2), a)

        a_new = state.ex_add(a, Const(0.5))
        b_inner = state.ex_add(rv_child, Const(-mu.v))
        b_new = state.ex_add(b, state.ex_mul(Const(0.5), 
                                            state.ex_mul(b_inner, b_inner)))
        
        return (StudentT(mu, tau2, nu), Gamma(a_new, b_new))
      else:
        return None
    case _:
      return None
    