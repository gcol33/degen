# Compute variance-covariance from Fisher information

For a model_spec (not necessarily fitted), compute the asymptotic
variance-covariance matrix from the inverse Fisher information.

## Usage

``` r
model_vcov(spec, y, par, type = "observed")
```

## Arguments

- spec:

  A `model_spec` object

- y:

  Numeric vector of data

- par:

  Parameter values at which to evaluate

- type:

  Type of Fisher information: "observed" or "expected"

## Value

Variance-covariance matrix

## Examples

``` r
spec <- model_spec_normal()
y <- rnorm(100, mean = 5, sd = 2)
model_vcov(spec, y, c(mu = 5, sigma = 2))
```
