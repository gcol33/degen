# Simulate data from a model specification

Generate random data from a model_spec that has a simulation function
attached via
[`add_simulator()`](https://gcol33.github.io/degen/reference/add_simulator.md).

## Usage

``` r
# S3 method for class 'model_spec'
simulate(object, nsim = 1, seed = NULL, n, par, ...)
```

## Arguments

- object:

  A `model_spec` object with simulation capability

- nsim:

  Number of simulations (datasets) to generate

- seed:

  Random seed for reproducibility

- n:

  Number of observations per simulation

- par:

  Named numeric vector of parameter values

- ...:

  Additional arguments (ignored)

## Value

If `nsim = 1`, a numeric vector. If `nsim > 1`, a list of vectors.

## Examples

``` r
spec <- model_spec_normal()
spec <- add_simulator(spec, function(n, mu, sigma) rnorm(n, mu, sigma))
y <- simulate(spec, n = 100, par = c(mu = 5, sigma = 2))
hist(y)
```
