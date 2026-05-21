# Check numerical gradient against analytical

Compare numerically computed gradient with a user-supplied analytical
gradient function.

## Usage

``` r
check_gradient(spec, y, par, grad_fn, tol = 1e-04)
```

## Arguments

- spec:

  A `model_spec` object

- y:

  Numeric vector of data

- par:

  Named numeric vector of parameter values

- grad_fn:

  Analytical gradient function with signature `function(y, ...)`

- tol:

  Tolerance for comparison (default 1e-4)

## Value

Logical indicating whether gradients match

## Examples

``` r
# For normal distribution, gradient w.r.t. mu is sum(y - mu) / sigma^2
spec <- model_spec_normal()
y <- rnorm(100, 5, 2)
grad_fn <- function(y, mu, sigma) {
  c(mu = sum(y - mu) / sigma^2,
    sigma = -length(y) / sigma + sum((y - mu)^2) / sigma^3)
}
check_gradient(spec, y, c(mu = 5, sigma = 2), grad_fn)
```
