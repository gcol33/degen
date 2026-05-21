# Check for exact parameter equivalence

Test whether two models are exactly equivalent with a direct parameter
mapping (identity or simple transformation).

## Usage

``` r
check_exact_equivalence(spec_a, spec_b, y, transformations)
```

## Arguments

- spec_a:

  First `model_spec` object

- spec_b:

  Second `model_spec` object

- y:

  Numeric vector of observed data

- transformations:

  List of candidate transformations to test. Each element should be a
  function that takes parameters from A and returns parameters for B.

## Value

An S3 object indicating whether exact equivalence was found and which
transformation achieves it.

## Examples

``` r
exp_spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(0.1, 10)),
  name = "Exponential"
)

gamma_spec <- model_spec(
  loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
  par_names = "rate",
  par_bounds = list(rate = c(0.1, 10)),
  name = "Gamma(1)"
)

set.seed(123)
y <- rexp(50, rate = 2)

# Test identity transformation (lambda -> rate directly)
result <- check_exact_equivalence(exp_spec, gamma_spec, y,
  transformations = list(
    identity = function(p) c(rate = p["lambda"])
  ))
print(result)
```
