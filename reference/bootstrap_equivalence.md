# Bootstrap test for equivalence

Use bootstrap resampling to assess the reliability of an equivalence
conclusion. This helps determine whether the conclusion is robust to
sampling variability.

## Usage

``` r
bootstrap_equivalence(
  pair,
  y,
  n_boot = 100,
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

- n_boot:

  Number of bootstrap replicates (default 100)

- n_points:

  Number of parameter points per comparison

- tol:

  Tolerance for equivalence

- method:

  Comparison method

- progress:

  Logical; show progress bar

## Value

An S3 object of class `bootstrap_equiv` containing:

- prop_equivalent:

  Proportion of bootstrap samples showing equivalence

- boot_discrepancies:

  Vector of max discrepancies from each replicate

- original_result:

  Result from original (non-bootstrapped) data

- ci_discrepancy:

  95% CI for max discrepancy

- n_boot:

  Number of bootstrap replicates

## Details

For each bootstrap replicate, the data are resampled with replacement
and the equivalence test is re-run. The proportion of replicates showing
equivalence provides a measure of confidence in the original conclusion.

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
y <- rexp(50, rate = 2)

# Bootstrap test (use more replicates in practice)
boot <- bootstrap_equivalence(pair, y, n_boot = 20, n_points = 20)
print(boot)
# }
```
