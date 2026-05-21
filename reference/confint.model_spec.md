# S3 method for confint

Compute confidence intervals for model parameters using profile
likelihood.

## Usage

``` r
# S3 method for class 'model_spec'
confint(object, parm, level = 0.95, y, par, ...)
```

## Arguments

- object:

  A `model_spec` object

- parm:

  Parameter names (character vector). If missing, all parameters.

- level:

  Confidence level (default 0.95)

- y:

  Numeric vector of observed data

- par:

  Named numeric vector of parameter values (typically MLE)

- ...:

  Additional arguments (ignored)

## Value

A matrix with columns for lower and upper bounds

## Examples

``` r
spec <- model_spec(
  loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
  par_names = c("mu", "sigma"),
  par_bounds = list(sigma = c(1e-6, Inf))
)
set.seed(123)
y <- rnorm(100, mean = 5, sd = 2)
confint(spec, y = y, par = c(mu = 5, sigma = 2))
```
