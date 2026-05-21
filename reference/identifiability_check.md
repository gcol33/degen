# Check model identifiability

Perform comprehensive identifiability analysis for a model
specification. Combines Fisher information analysis with profile
likelihood diagnostics.

## Usage

``` r
identifiability_check(
  spec,
  y,
  par = NULL,
  level = c("local", "profile"),
  threshold = 0.01,
  verbose = FALSE,
  cl = NULL
)
```

## Arguments

- spec:

  A `model_spec` object

- y:

  Numeric vector of observed data

- par:

  Named numeric vector of parameter values. If NULL, attempts to find
  MLE first.

- level:

  Analysis level: "local" uses Fisher information only, "profile" adds
  profile likelihood analysis

- threshold:

  Eigenvalue threshold for non-identifiability (default 0.01)

- verbose:

  Logical; print progress information

- cl:

  Optional parallel cluster from
  [`setup_cluster()`](https://gcol33.github.io/degen/reference/setup_cluster.md).
  If provided, profile likelihood computations run in parallel.

## Value

An S3 object of class `identifiability_result` containing:

- status:

  Named character vector of identifiability status per parameter

- fisher_info:

  The Fisher information analysis

- identified_functions:

  Character vector of identified parameter combinations

- non_identified:

  Character vector of non-identified directions

- condition:

  Condition number

- rank:

  Numerical rank

- recommendations:

  Suggested actions

## Examples

``` r
# Well-identified model
norm_spec <- model_spec(
  loglik_fn = function(y, mu, sigma) {
    sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
  },
  par_names = c("mu", "sigma"),
  par_bounds = list(sigma = c(1e-6, Inf))
)

set.seed(123)
y <- rnorm(100, mean = 5, sd = 2)
result <- identifiability_check(norm_spec, y, par = c(mu = 5, sigma = 2))
print(result)

# Non-identifiable model
nonid_spec <- model_spec(
  loglik_fn = function(y, a, b) {
    sum(dnorm(y, mean = a + b, sd = 1, log = TRUE))
  },
  par_names = c("a", "b")
)

result2 <- identifiability_check(nonid_spec, y, par = c(a = 2, b = 3))
print(result2)
```
