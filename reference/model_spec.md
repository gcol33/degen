# Create a model specification

Define a parametric model through its log-likelihood function. This is
the foundational object for equivalence and identifiability analysis.

## Usage

``` r
model_spec(
  loglik_fn,
  par_names,
  par_bounds = NULL,
  name = NULL,
  validate = TRUE
)
```

## Arguments

- loglik_fn:

  A function with signature `function(y, ...)` where `y` is the data and
  `...` are named parameters. Must return a scalar log-likelihood.

- par_names:

  Character vector of parameter names. Must match the argument names in
  `loglik_fn` (excluding `y`).

- par_bounds:

  Optional named list of parameter bounds. Each element should be a
  length-2 numeric vector `c(lower, upper)`. Parameters not specified
  default to `c(-Inf, Inf)`.

- name:

  Optional character string naming the model. If `NULL`, a name is
  generated from the parameter names.

- validate:

  Logical; if `TRUE` (default), validate inputs on construction. Set to
  `FALSE` for performance in tight loops.

## Value

An S3 object of class `model_spec` with components:

- loglik_fn:

  The log-likelihood function

- par_names:

  Character vector of parameter names

- par_bounds:

  Named list of bounds for each parameter

- n_par:

  Number of parameters

- name:

  Model name

## Examples

``` r
# Exponential model
exp_spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(1e-6, Inf)),
  name = "Exponential"
)

# Normal model with two parameters
norm_spec <- model_spec(
  loglik_fn = function(y, mu, sigma) {
    sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
  },
  par_names = c("mu", "sigma"),
  par_bounds = list(sigma = c(1e-6, Inf))
)
```
