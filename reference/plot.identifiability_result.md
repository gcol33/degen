# Plot identifiability diagnostics

Visualize parameter identifiability status and Fisher information
diagnostics.

## Usage

``` r
# S3 method for class 'identifiability_result'
plot(x, type = c("status", "profile", "both"), ...)
```

## Arguments

- x:

  An `identifiability_result` object

- type:

  Type of plot: "status" (default), "profile", or "both"

- ...:

  Additional arguments passed to plotting functions

## Value

Invisible `x`

## Examples

``` r
spec <- model_spec(
  loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
  par_names = c("mu", "sigma"),
  par_bounds = list(sigma = c(1e-6, Inf))
)
y <- rnorm(100, mean = 5, sd = 2)
result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))
plot(result)
```
