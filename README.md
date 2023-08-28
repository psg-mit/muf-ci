# Siren

## Requirements
- Probzelus (https://github.com/IBM/probzelus) `master`

## Todos
- auto splitting pairs
```ml
let (outlier_prob, xs) = List.fold_resample(step, data, (outlier_prob, x)) in
(outlier_prob, xs)
```
This doesn't work because the pattern matching doensn't split.
- global declaration parsing error (currently disabled)
- Simplify compiled code by removing this pattern
```ml
(fun v1 ->
    let x = v1 in
    ...)
```
- SSI MCMC
