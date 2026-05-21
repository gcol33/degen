# Plot surface comparison results

Visualize the likelihood discrepancies from a surface comparison.

## Usage

``` r
# S3 method for class 'surface_comparison'
plot(x, type = c("discrepancy", "histogram"), ...)
```

## Arguments

- x:

  A `surface_comparison` object

- type:

  Type of plot: "discrepancy" (default) or "histogram"

- ...:

  Additional arguments passed to plotting functions

## Value

Invisible `x`

## Examples

``` r
exp_spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(1e-6, 100))
)
gamma_spec <- model_spec(
  loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
  par_names = "rate",
  par_bounds = list(rate = c(1e-6, 100))
)
pair <- equivalence_pair(exp_spec, gamma_spec)
y <- rexp(50, rate = 2)
result <- compare_surfaces(pair, y, n_points = 20)
plot(result)
```
