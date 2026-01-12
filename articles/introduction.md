# Introduction to degen

``` r
library(degen)
```

## What is observational equivalence?

Two statistical models are **observationally equivalent** if they assign
the same probability to every possible dataset. When this occurs, no
amount of data can distinguish between them.

This matters because:

- Different model formulations may encode different mechanistic stories
- If those formulations are observationally equivalent, the data cannot
  tell us which story is correct
- Conclusions drawn from one formulation versus another are assumptions,
  not results

## What is identifiability?

A parameter is **identifiable** if its value can be uniquely determined
from the likelihood function. Non-identifiable parameters exhibit flat
likelihood surfaces in some direction, meaning the data provide no
information for distinguishing values along that direction.

## The degen package

`degen` provides tools to:

1.  **Detect observational equivalence** between model specifications
2.  **Diagnose identifiability problems** within a single model
3.  **Characterize equivalence classes** among multiple competing models

## Quick start

### Defining models

Models are defined through their log-likelihood functions using
[`model_spec()`](https://gcol33.github.io/degen/reference/model_spec.md):

``` r
# Exponential model
exp_spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(1e-6, Inf)),
  name = "Exponential"
)

# Gamma model with shape = 1 (equivalent to exponential)
gamma1_spec <- model_spec(
  loglik_fn = function(y, rate) {
    sum(dgamma(y, shape = 1, rate = rate, log = TRUE))
  },
  par_names = "rate",
  par_bounds = list(rate = c(1e-6, Inf)),
  name = "Gamma(1)"
)

# Gamma model with shape = 2 (not equivalent)
gamma2_spec <- model_spec(
  loglik_fn = function(y, rate) {
    sum(dgamma(y, shape = 2, rate = rate, log = TRUE))
  },
  par_names = "rate",
  par_bounds = list(rate = c(1e-6, Inf)),
  name = "Gamma(2)"
)
```

### Comparing two models

Create an equivalence pair and compare their likelihood surfaces:

``` r
pair <- equivalence_pair(exp_spec, gamma1_spec)

set.seed(123)
y <- rexp(100, rate = 2)

result <- compare_surfaces(pair, y, n_points = 30, tol = 1e-4)
print(result)
#> <surface_comparison>
#> Conclusion: Models appear EQUIVALENT
#> Points tested: 60
#> Max discrepancy: 4.65e-06
#> Tolerance: 1.00e-04
#> Method: grid
```

### Checking identifiability

For a single model, check whether all parameters are identifiable:

``` r
# A model where only a + b is identified
nonid_spec <- model_spec(
  loglik_fn = function(y, a, b) {
    sum(dnorm(y, mean = a + b, sd = 1, log = TRUE))
  },
  par_names = c("a", "b"),
  name = "Non-identifiable"
)

set.seed(123)
y_norm <- rnorm(100, mean = 5, sd = 1)

id_result <- identifiability_check(nonid_spec, y_norm, par = c(a = 2, b = 3))
print(id_result)
#> <identifiability_result>
#> Overall: Model has NON-IDENTIFIABLE parameters
#> 
#> Parameter status:
#>   a: NON-IDENTIFIABLE
#>   b: NON-IDENTIFIABLE
#> 
#> Non-identified directions:
#>   -0.71*a + 0.71*b
#> 
#> Identified functions:
#>   0.71*a + 0.71*b
#> 
#> Condition number: 3.1e+12
#> Rank: 1 / 2
```

### Finding equivalence classes

When you have multiple candidate models, group them by equivalence:

``` r
models <- list(
  exp = exp_spec,
  gamma1 = gamma1_spec,
  gamma2 = gamma2_spec
)

set.seed(123)
y <- rexp(50, rate = 2)

classes <- equivalence_classes(models, y, n_points = 15, tol = 1e-4)
print(classes)
#> <equiv_classes>
#> 3 models -> 2 equivalence classes
#> 
#> Class 1: exp, gamma1 (equivalent)
#> Class 2: gamma2
```

## Key concepts

### Model specification

A `model_spec` contains:

- `loglik_fn`: A function with signature `function(y, ...)` returning
  the log-likelihood
- `par_names`: Names of the parameters
- `par_bounds`: Optional bounds for each parameter

### Equivalence detection

[`compare_surfaces()`](https://gcol33.github.io/degen/reference/compare_surfaces.md)
samples parameter space and checks whether each parameter configuration
of model A can be matched by some configuration of model B (and vice
versa).

### Identifiability analysis

[`identifiability_check()`](https://gcol33.github.io/degen/reference/identifiability_check.md)
uses:

- **Fisher information**: The curvature of the log-likelihood surface
- **Eigenvalue analysis**: Near-zero eigenvalues indicate
  non-identifiable directions
- **Profile likelihood**: Optional deeper analysis of individual
  parameters

## When to use this package

This package is designed for researchers who:

- Work with models where structural assumptions matter
- Need to assess whether competing mechanistic hypotheses are
  empirically distinguishable
- Want to diagnose why certain parameters are poorly estimated

## Limitations

- This provides numerical evidence, not mathematical proof
- Conclusions depend on the tolerance and number of test points
- High-dimensional parameter spaces are computationally challenging

## Further reading

- See `vignette("comparing-models")` for detailed comparison workflows
- See `vignette("identifiability-diagnostics")` for in-depth
  identifiability analysis
