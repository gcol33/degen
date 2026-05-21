# Find all models equivalent to a reference

Search a list of models to find those equivalent to a specified
reference.

## Usage

``` r
find_equivalent_to(
  reference,
  candidates,
  y,
  n_points = 50,
  tol = 1e-06,
  verbose = FALSE,
  cl = NULL
)
```

## Arguments

- reference:

  A `model_spec` object (the reference model)

- candidates:

  Named list of `model_spec` objects to test

- y:

  Numeric vector of observed data

- n_points:

  Number of parameter points to sample per comparison

- tol:

  Tolerance for likelihood equality

- verbose:

  Logical; print progress information

- cl:

  Optional parallel cluster

## Value

A list with:

- equivalent:

  Names of equivalent models

- not_equivalent:

  Names of non-equivalent models

- comparisons:

  List of comparison results

## Examples

``` r
ref <- model_spec_exponential()
candidates <- list(
  gamma1 = model_spec_gamma(par = "rate", known_shape = 1),
  gamma2 = model_spec_gamma(par = "rate", known_shape = 2),
  weibull = model_spec_weibull()
)
y <- rexp(50, rate = 2)
result <- find_equivalent_to(ref, candidates, y, n_points = 10)
result$equivalent
```
