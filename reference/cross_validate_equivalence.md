# Cross-validation test for equivalence

Split data into training and test sets to assess whether equivalence
generalizes to held-out data.

## Usage

``` r
cross_validate_equivalence(
  pair,
  y,
  n_folds = 5,
  n_points = 50,
  tol = 1e-06,
  method = c("grid", "optimization"),
  progress = interactive()
)
```

## Arguments

- pair:

  An `equivalence_pair` object

- y:

  Numeric vector of observed data

- n_folds:

  Number of cross-validation folds (default 5)

- n_points:

  Number of parameter points per comparison

- tol:

  Tolerance for equivalence

- method:

  Comparison method

- progress:

  Logical; show progress bar

## Value

An S3 object of class `cv_equiv` containing:

- fold_results:

  Data frame with results for each fold

- prop_equivalent:

  Proportion of folds showing equivalence

- mean_discrepancy:

  Mean max discrepancy across folds

- n_folds:

  Number of folds

## Details

For each fold, a portion of the data is held out and the equivalence
test is performed on the remaining data. Consistent results across folds
suggest the conclusion generalizes.

## Examples

``` r
# \donttest{
exp_spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(1e-6, 100)),
  name = "Exponential"
)

gamma_spec <- model_spec(
  loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
  par_names = "rate",
  par_bounds = list(rate = c(1e-6, 100)),
  name = "Gamma(1)"
)

pair <- equivalence_pair(exp_spec, gamma_spec)
set.seed(123)
y <- rexp(100, rate = 2)

# Cross-validation test
cv <- cross_validate_equivalence(pair, y, n_folds = 5, n_points = 20)
print(cv)
# }
```
