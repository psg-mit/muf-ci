# Controllable Inference Muf 

## Requirements
- Probzelus (https://github.com/psg-mit/probzelus-ci) `master`

## Todos
- global declaration parsing error (currently disabled)
- Simplify compiled code by removing this pattern
```ml
(fun v1 ->
    let x = v1 in
    ...)
```
- extend SSI module to keep changes local to muf
- SSI MCMC
