# Mixture model with label switching

A two-component Gaussian mixture model demonstrating the label switching
equivalence problem.

## Format

A list with 4 elements:

- y:

  Numeric vector of 200 observations from a mixture

- spec:

  A model_spec for the mixture model

- true_params:

  True parameter values (mu1=0, mu2=3, sigma=1, pi=0.4)

- swapped_params:

  Label-swapped equivalent parameters

## Details

In mixture models, swapping component labels produces an equivalent
model. If (mu1, mu2, pi) are the parameters, then (mu2, mu1, 1-pi) gives
exactly the same likelihood for all data.

This is a classic example of practical non-identifiability that affects
MCMC sampling and optimization.

## Examples

``` r
data(mixture_model)

# Both parameter sets give same likelihood
ll1 <- loglik(mixture_model$spec, mixture_model$y, mixture_model$true_params)
ll2 <- loglik(mixture_model$spec, mixture_model$y, mixture_model$swapped_params)
abs(ll1 - ll2) < 1e-10  # TRUE
```
