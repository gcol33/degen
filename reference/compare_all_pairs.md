# Compare all pairs of models

Perform pairwise comparisons between all models in a list.

## Usage

``` r
compare_all_pairs(
  models,
  y,
  n_points = 50,
  tol = 1e-06,
  verbose = FALSE,
  cl = NULL
)
```

## Arguments

- models:

  Named list of `model_spec` objects

- y:

  Numeric vector of observed data

- n_points:

  Number of parameter points to sample per comparison

- tol:

  Tolerance for likelihood equality

- verbose:

  Logical; print progress information

- cl:

  Optional parallel cluster from
  [`setup_cluster()`](https://gcol33.github.io/degen/reference/setup_cluster.md)

## Value

A list with:

- comparisons:

  List of `surface_comparison` results

- matrix:

  Logical matrix of pairwise equivalence

- n_equivalent:

  Number of equivalent pairs

- n_total:

  Total number of comparisons

## Examples

``` r
exp_spec <- model_spec_exponential()
gamma1_spec <- model_spec_gamma(par = "rate", known_shape = 1)
gamma2_spec <- model_spec_gamma(par = "rate", known_shape = 2)
models <- list(exp = exp_spec, gamma1 = gamma1_spec, gamma2 = gamma2_spec)
y <- rexp(50, rate = 2)
result <- compare_all_pairs(models, y, n_points = 10)
result$matrix
```
