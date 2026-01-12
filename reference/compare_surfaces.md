# Compare likelihood surfaces between two models

Test whether two model specifications are observationally equivalent by
comparing their likelihood surfaces. For each parameter configuration of
model A, the algorithm searches for a configuration of model B that
produces the same likelihood.

## Usage

``` r
compare_surfaces(
  pair,
  y,
  n_points = 100,
  method = c("grid", "optimization"),
  tol = 1e-06,
  verbose = FALSE
)
```

## Arguments

- pair:

  An `equivalence_pair` object

- y:

  Numeric vector of observed data

- n_points:

  Number of parameter points to sample (default 100)

- method:

  Comparison method: "grid" samples parameter space, "optimization"
  searches for counterexamples

- tol:

  Tolerance for likelihood equality (default 1e-6)

- verbose:

  Logical; print progress information

## Value

An S3 object of class `surface_comparison` containing:

- equivalent:

  Logical; overall equivalence conclusion

- max_discrepancy:

  Maximum likelihood discrepancy found

- n_tested:

  Number of parameter points tested

- evidence:

  Data frame of tested points and discrepancies

- tol:

  Tolerance used

- method:

  Method used

## Details

The comparison is performed in both directions (A to B and B to A) to
ensure the equivalence relation is symmetric.

Note that this provides numerical evidence, not mathematical proof. A
conclusion of equivalence means no counterexample was found within the
tested region of parameter space.

## Examples

``` r
# Compare exponential and gamma(shape=1) - these are equivalent
exp_spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(1e-6, Inf)),
  name = "Exponential"
)

gamma_spec <- model_spec(
  loglik_fn = function(y, rate) {
    sum(dgamma(y, shape = 1, rate = rate, log = TRUE))
  },
  par_names = "rate",
  par_bounds = list(rate = c(1e-6, Inf)),
  name = "Gamma(1)"
)

pair <- equivalence_pair(exp_spec, gamma_spec)
set.seed(123)
y <- rexp(100, rate = 2)
result <- compare_surfaces(pair, y, n_points = 20)
print(result)
#> <surface_comparison>
#> Conclusion: Models are NOT EQUIVALENT
#> Points tested: 40
#> Max discrepancy: 3.24e-06
#> Tolerance: 1.00e-06
#> Method: grid
```
