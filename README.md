# Controllable Inference Muf 

## Requirements
- Probzelus (https://github.com/psg-mit/probzelus-ci) `master`

## Todos
- auto splitting pairs
```ml
let (outlier_prob, xs) = List.fold_resample(step, data, (outlier_prob, x)) in
(outlier_prob, xs)
```
This doesn't work because the pattern matching doensn't split.
- CPS evaluates first for ite, so can have errors (`List.hd` on an empty list)
- global declaration parsing error (currently disabled)
- Simplify compiled code by removing this pattern
```ml
(fun v1 ->
    let x = v1 in
    ...)
```
- extend SSI module to keep changes local to muf
- SSI MCMC
