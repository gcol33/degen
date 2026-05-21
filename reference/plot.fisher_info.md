# Plot Fisher information diagnostics

Visualize the eigenvalue spectrum and parameter loadings of a Fisher
information matrix.

## Usage

``` r
# S3 method for class 'fisher_info'
plot(x, type = c("eigenvalues", "loadings", "both"), ...)
```

## Arguments

- x:

  A `fisher_info` object

- type:

  Type of plot: "eigenvalues" (default), "loadings", or "both"

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
info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))
plot(info)
plot(info, type = "loadings")
```
