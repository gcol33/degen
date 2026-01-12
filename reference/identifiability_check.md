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
  verbose = FALSE
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
#> <identifiability_result>
#> Overall: Model appears WELL-IDENTIFIED
#> 
#> Parameter status:
#>   mu: identified
#>   sigma: identified
#> 
#> Condition number: 1.7
#> Rank: 2 / 2

# Non-identifiable model
nonid_spec <- model_spec(
  loglik_fn = function(y, a, b) {
    sum(dnorm(y, mean = a + b, sd = 1, log = TRUE))
  },
  par_names = c("a", "b")
)

result2 <- identifiability_check(nonid_spec, y, par = c(a = 2, b = 3))
print(result2)
#> <identifiability_result>
#> Overall: Model has NON-IDENTIFIABLE parameters
#> 
#> Parameter status:
#>   a: NON-IDENTIFIABLE
#>   b: NON-IDENTIFIABLE
#> 
#> Non-identified directions:
#>   0.71*a -0.71*b
#> 
#> Identified functions:
#>   -0.71*a -0.71*b
#> 
#> Condition number: 3e+12
#> Rank: 1 / 2
```
