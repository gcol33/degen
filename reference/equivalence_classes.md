# Find equivalence classes among multiple models

Given a list of model specifications, determine which models are
observationally equivalent and group them into equivalence classes.

## Usage

``` r
equivalence_classes(specs, y, n_points = 50, tol = 1e-06, verbose = FALSE)
```

## Arguments

- specs:

  A named list of `model_spec` objects

- y:

  Numeric vector of observed data

- n_points:

  Number of parameter points to sample for each comparison

- tol:

  Tolerance for equivalence (default 1e-6)

- verbose:

  Logical; print progress

## Value

An S3 object of class `equiv_classes` containing:

- classes:

  List of equivalence classes (vectors of model names)

- n_classes:

  Number of distinct classes

- membership:

  Named vector mapping model names to class indices

- pairwise:

  Matrix of pairwise equivalence results

- discrepancies:

  Matrix of pairwise discrepancies

## Details

The algorithm performs pairwise comparisons between all models and uses
union-find to group equivalent models. Two models are considered
equivalent if their maximum likelihood discrepancy is below the
tolerance.

## Examples

``` r
# Define several models
exp_spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(1e-6, 100)),
  name = "Exponential"
)

gamma1_spec <- model_spec(
  loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
  par_names = "rate",
  par_bounds = list(rate = c(1e-6, 100)),
  name = "Gamma(1)"
)

gamma2_spec <- model_spec(
  loglik_fn = function(y, rate) sum(dgamma(y, shape = 2, rate = rate, log = TRUE)),
  par_names = "rate",
  par_bounds = list(rate = c(1e-6, 100)),
  name = "Gamma(2)"
)

models <- list(exp = exp_spec, gamma1 = gamma1_spec, gamma2 = gamma2_spec)

set.seed(123)
y <- rexp(50, rate = 2)
classes <- equivalence_classes(models, y, n_points = 10)
print(classes)
#> <equiv_classes>
#> 3 models -> 3 equivalence classes
#> 
#> Class 1: exp
#> Class 2: gamma1
#> Class 3: gamma2
```
