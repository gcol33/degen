# Add simulation function to model_spec

Attach a data-generating function to an existing model_spec. This
enables the [`simulate()`](https://rdrr.io/r/stats/simulate.html) method
for parametric bootstrap and power analysis.

## Usage

``` r
add_simulator(spec, simulate_fn)
```

## Arguments

- spec:

  A `model_spec` object

- simulate_fn:

  A function with signature `function(n, ...)` where `...` are the model
  parameters. Should return a numeric vector of length `n`.

## Value

A modified `model_spec` with simulation capability

## Examples

``` r
spec <- model_spec_normal()
spec <- add_simulator(spec, function(n, mu, sigma) rnorm(n, mu, sigma))
simulate(spec, n = 10, par = c(mu = 5, sigma = 2))
```
