# Fit a model specification via MLE

Find maximum likelihood estimates for a model_spec.

## Usage

``` r
fit_model(spec, y, start = NULL, method = "L-BFGS-B", ...)
```

## Arguments

- spec:

  A `model_spec` object

- y:

  Numeric vector of observed data

- start:

  Optional starting values. If NULL, uses center of bounds.

- method:

  Optimization method passed to
  [`optim()`](https://rdrr.io/r/stats/optim.html)

- ...:

  Additional arguments passed to
  [`optim()`](https://rdrr.io/r/stats/optim.html)

## Value

A list with:

- par:

  MLE parameter estimates

- loglik:

  Log-likelihood at MLE

- convergence:

  Convergence code from optim

- hessian:

  Hessian matrix at MLE (if computed)

- spec:

  The original model_spec

- y:

  The data

## Examples

``` r
spec <- model_spec_normal()
y <- rnorm(100, mean = 5, sd = 2)
fit <- fit_model(spec, y)
fit$par
```
