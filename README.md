# Siren

## Benchmarking
Inside `benchmarks` folder,
```bash
python gen_plans.py
python harness.py check
python harness.py analyze
python harness.py baseline
python harness.py run --accuracy -n 100
python visualize.py
```

## Todos + Nice to haves
- Given $X_i \leftarrow Gamma(a_i, b)$ and X_i iid., then $\sum X_i \leftarrow Gamma(\sum a_i, b)$
- Given $X \leftarrow Gamma(a, b)$ and $c$ a constant, then $cX \leftarrow Gamma(a, b/c)$
- If X ~ Gamma(α, θ) and Y ~ Gamma(β, θ) are independently distributed, then X/(X + Y) has a beta distribution with parameters α and β, and X/(X + Y) is independent of X + Y, which is Gamma(α + β, θ)-distributed.
- SSI MCMC
- Ocaml implementation has a bug where when marginalizing at the end of inference, the engine always samples the resulting RVs because it thinks it has a parent of the mixture distribution formed. Likely a result of the Mixture random variable hack. 