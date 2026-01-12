# Compute Fisher information matrix

Compute the observed Fisher information matrix (negative Hessian of
log-likelihood) for a model specification at given parameter values.

## Usage

``` r
fisher_information(
  spec,
  y,
  par,
  type = c("observed", "expected"),
  method = c("hessian")
)
```

## Arguments

- spec:

  A `model_spec` object

- y:

  Numeric vector of observed data

- par:

  Named numeric vector of parameter values at which to evaluate

- type:

  Type of information: "observed" (default) uses the negative Hessian at
  the data; "expected" would use theoretical expectation (not yet
  implemented)

- method:

  Computation method: "hessian" (default) uses numerical differentiation

## Value

An S3 object of class `fisher_info` containing:

- matrix:

  The Fisher information matrix

- eigenvalues:

  Eigenvalues (sorted decreasing)

- eigenvectors:

  Corresponding eigenvectors

- condition:

  Condition number

- rank:

  Numerical rank

- par:

  Parameter values used

- par_names:

  Parameter names

## Details

The Fisher information matrix characterizes the curvature of the
log-likelihood surface. Key properties:

- Positive definite: all parameters locally identifiable

- Singular (rank deficient): some parameters not identifiable

- Near-singular (high condition number): some parameters weakly
  identified

## Examples

``` r
spec <- model_spec(
  loglik_fn = function(y, mu, sigma) {
    sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
  },
  par_names = c("mu", "sigma"),
  par_bounds = list(sigma = c(1e-6, Inf))
)

set.seed(123)
y <- rnorm(100, mean = 5, sd = 2)
info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))
print(info)
#> <fisher_info> at par = (mu=5, sigma=2)
#> Condition number: 1.7
#> Rank: 2 / 2 (full)
#> Eigenvalues: 38.9, 23.5 
```
