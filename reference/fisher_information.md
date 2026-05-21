# Compute Fisher information matrix

Compute the Fisher information matrix for a model specification at given
parameter values. Supports both observed information (negative Hessian)
and expected information (outer product of gradients estimator).

## Usage

``` r
fisher_information(
  spec,
  y,
  par,
  type = c("observed", "expected"),
  method = c("hessian"),
  cl = NULL
)
```

## Arguments

- spec:

  A `model_spec` object

- y:

  Numeric vector of observed data

- par:

  Named numeric vector of parameter values at which to evaluate

- type:

  Type of information: "observed" (default) uses the negative Hessian at
  the data; "expected" uses the outer product of gradients (OPG)
  estimator computed from per-observation score contributions

- method:

  Computation method: "hessian" (default) uses numerical differentiation

- cl:

  Optional parallel cluster from
  [`setup_cluster()`](https://gcol33.github.io/degen/reference/setup_cluster.md).
  Only used for `type = "expected"` to parallelize per-observation score
  computation.

## Value

An S3 object of class `fisher_info` containing:

- matrix:

  The Fisher information matrix

- eigenvalues:

  Eigenvalues (sorted decreasing)

- eigenvectors:

  Corresponding eigenvectors

- condition:

  Condition number

- rank:

  Numerical rank

- par:

  Parameter values used

- par_names:

  Parameter names

- type:

  Type of information computed

## Details

The Fisher information matrix characterizes the curvature of the
log-likelihood surface. Key properties:

- Positive definite: all parameters locally identifiable

- Singular (rank deficient): some parameters not identifiable

- Near-singular (high condition number): some parameters weakly
  identified

Two types of information are available:

- **Observed information** (`type = "observed"`): The negative Hessian
  of the log-likelihood. This is the default and most common choice.

- **Expected information** (`type = "expected"`): Estimated using the
  outer product of gradients (OPG), where scores are summed over
  observations. The score is the gradient of the log-likelihood for each
  observation. Under regularity conditions, this converges to the same
  limit as observed information but may differ in finite samples.
  Requires the log-likelihood to be additive over observations.

## Examples

``` r
spec <- model_spec(
  loglik_fn = function(y, mu, sigma) {
    sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
  },
  par_names = c("mu", "sigma"),
  par_bounds = list(sigma = c(1e-6, Inf))
)

set.seed(123)
y <- rnorm(100, mean = 5, sd = 2)

# Observed information (negative Hessian)
info_obs <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))
print(info_obs)

# Expected information (OPG estimator)
info_exp <- fisher_information(spec, y, par = c(mu = 5, sigma = 2),
                               type = "expected")
print(info_exp)
```
