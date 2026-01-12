# Evaluate log-likelihood

Evaluate the log-likelihood function for a model specification at given
parameter values.

## Usage

``` r
loglik(object, y, par, ...)

# S3 method for class 'model_spec'
loglik(object, y, par, ...)
```

## Arguments

- object:

  A model specification object

- y:

  Numeric vector of observed data

- par:

  Named numeric vector of parameter values

- ...:

  Additional arguments passed to methods

## Value

Numeric scalar log-likelihood value

## Examples

``` r
spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(1e-6, Inf))
)
y <- rexp(100, rate = 2)
loglik(spec, y, par = c(lambda = 2))
#> [1] -16.63916
```
