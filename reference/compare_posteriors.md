# Compare posterior distributions

Test whether two models produce equivalent posterior predictive
distributions using MCMC samples.

## Usage

``` r
compare_posteriors(
  spec_a,
  spec_b,
  y,
  samples_a,
  samples_b,
  n_compare = 100,
  tol = 1e-06,
  progress = interactive()
)
```

## Arguments

- spec_a:

  A `model_spec` object for model A

- spec_b:

  A `model_spec` object for model B

- y:

  Numeric vector of observed data

- samples_a:

  Matrix or data frame of posterior samples for model A. Rows are
  samples, columns are parameters (named to match spec_a).

- samples_b:

  Matrix or data frame of posterior samples for model B. Rows are
  samples, columns are parameters (named to match spec_b).

- n_compare:

  Number of sample pairs to compare (default 100)

- tol:

  Tolerance for likelihood equivalence

- progress:

  Logical; show progress bar

## Value

An S3 object of class `posterior_comparison` containing:

- prop_matched:

  Proportion of A samples that could be matched by B

- discrepancies:

  Vector of likelihood discrepancies

- equivalent:

  Logical; overall equivalence conclusion

- summary:

  Summary statistics of comparison

## Details

For each posterior sample from model A, this function finds the closest
matching posterior sample from model B (in terms of likelihood) and
computes the discrepancy. If models are equivalent, posterior samples
from different parameterizations should produce similar likelihoods.

This approach does not require the models to have the same
parameterization or even the same number of parameters.

## Examples

``` r
# \donttest{
# Two equivalent models
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

# Simulate "posterior" samples (in practice, use actual MCMC output)
set.seed(123)
y <- rexp(50, rate = 2)

# Simple posterior approximation (replace with real MCMC samples)
samples_a <- data.frame(lambda = rgamma(200, shape = 50, rate = 25))
samples_b <- data.frame(rate = rgamma(200, shape = 50, rate = 25))

result <- compare_posteriors(exp_spec, gamma_spec, y, samples_a, samples_b,
                             n_compare = 50)
print(result)
# }
```
