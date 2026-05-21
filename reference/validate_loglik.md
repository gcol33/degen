# Validate a log-likelihood function

Check that a log-likelihood function behaves correctly across its
parameter space. Useful for debugging user-defined likelihood functions.

## Usage

``` r
validate_loglik(spec, y, n_test = 20, verbose = TRUE)
```

## Arguments

- spec:

  A `model_spec` object

- y:

  Numeric vector of test data

- n_test:

  Number of random parameter points to test (default 20)

- verbose:

  Logical; print detailed output

## Value

A list with validation results (invisible)

## Examples

``` r
spec <- model_spec_normal()
y <- rnorm(50)
validate_loglik(spec, y)
```
