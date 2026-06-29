# Introduction to degen

## Installation

``` r

# Install from CRAN (when available)
install.packages("degen")

# Or install development version from GitHub
# install.packages("pak")
pak::pak("gcol33/degen")
```

**Dependencies**:

- **Required**: `Rcpp`, `numDeriv`

- **Suggested** (for extended functionality): `brms`, `rstan`,
  `rstanarm` (Bayesian model extraction), `jsonlite` (JSON export)

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
#> Max discrepancy: 4.83e-06
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

## Quick Reference

### Core Functions

| Function | Purpose |
|----|----|
| [`model_spec()`](https://gcol33.github.io/degen/reference/model_spec.md) | Define a model via log-likelihood function |
| [`equivalence_pair()`](https://gcol33.github.io/degen/reference/equivalence_pair.md) | Pair two models for comparison |
| [`compare_surfaces()`](https://gcol33.github.io/degen/reference/compare_surfaces.md) | Test observational equivalence |
| [`fisher_information()`](https://gcol33.github.io/degen/reference/fisher_information.md) | Compute Fisher information matrix |
| [`identifiability_check()`](https://gcol33.github.io/degen/reference/identifiability_check.md) | Diagnose parameter identifiability |
| [`equivalence_classes()`](https://gcol33.github.io/degen/reference/equivalence_classes.md) | Group multiple models by equivalence |

### model_spec()

``` r

model_spec(loglik_fn, par_names, par_bounds = NULL, name = NULL)
```

| Parameter | Description |
|----|----|
| `loglik_fn` | Function with signature `function(y, ...)` returning log-likelihood |
| `par_names` | Character vector of parameter names |
| `par_bounds` | Named list of `c(lower, upper)` bounds per parameter |
| `name` | Optional model name for display |

### compare_surfaces()

``` r

compare_surfaces(pair, y, n_points = 50, tol = 1e-6, method = "grid")
```

| Parameter  | Description                  | Default    |
|------------|------------------------------|------------|
| `pair`     | `equivalence_pair` object    | *required* |
| `y`        | Data vector/matrix           | *required* |
| `n_points` | Number of test points        | `50`       |
| `tol`      | Equivalence tolerance        | `1e-6`     |
| `method`   | `"grid"` or `"optimization"` | `"grid"`   |

### Distribution Helpers

Pre-built `model_spec` constructors for common distributions:

- [`model_spec_normal()`](https://gcol33.github.io/degen/reference/model_spec_normal.md),
  [`model_spec_exponential()`](https://gcol33.github.io/degen/reference/model_spec_exponential.md),
  [`model_spec_gamma()`](https://gcol33.github.io/degen/reference/model_spec_gamma.md)

- [`model_spec_poisson()`](https://gcol33.github.io/degen/reference/model_spec_poisson.md),
  [`model_spec_binomial()`](https://gcol33.github.io/degen/reference/model_spec_binomial.md),
  [`model_spec_beta()`](https://gcol33.github.io/degen/reference/model_spec_beta.md)

- [`model_spec_lognormal()`](https://gcol33.github.io/degen/reference/model_spec_lognormal.md),
  [`model_spec_weibull()`](https://gcol33.github.io/degen/reference/model_spec_weibull.md),
  [`model_spec_negbinom()`](https://gcol33.github.io/degen/reference/model_spec_negbinom.md)

- [`model_spec_uniform()`](https://gcol33.github.io/degen/reference/model_spec_uniform.md)

## Troubleshooting

**“Parameter names don’t match function arguments”**

- Ensure `par_names` exactly match the argument names in `loglik_fn`
  (excluding `y`)

- Example: if `loglik_fn = function(y, lambda) ...`, then
  `par_names = "lambda"`

**Large discrepancies between equivalent models**

- Increase `n_points` for finer sampling

- Check that `par_bounds` cover the relevant parameter region

- Tighten tolerance with `tol = 1e-8` for stricter comparison

**Fisher information returns singular matrix**

- This indicates non-identifiability; use
  [`identifiability_check()`](https://gcol33.github.io/degen/reference/identifiability_check.md)
  for diagnosis

- Consider reparameterization (see
  [`vignette("identifiability-diagnostics")`](https://gcol33.github.io/degen/articles/identifiability-diagnostics.md))

**Slow performance with many parameters**

- Reduce `n_points` for initial exploration

- Use `method = "optimization"` for targeted search

- Enable parallel processing with `parallel = TRUE` (requires cluster
  setup)

## See Also

- [`vignette("model-specification")`](https://gcol33.github.io/degen/articles/model-specification.md) -
  How to write log-likelihood functions

- [`vignette("comparing-models")`](https://gcol33.github.io/degen/articles/comparing-models.md) -
  Detailed comparison workflows

- [`vignette("identifiability-diagnostics")`](https://gcol33.github.io/degen/articles/identifiability-diagnostics.md) -
  In-depth identifiability analysis

- [`vignette("case-studies")`](https://gcol33.github.io/degen/articles/case-studies.md) -
  Real-world examples

## Session Info

``` r

sessionInfo()
#> R version 4.6.1 (2026-06-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] degen_0.15.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.39       desc_1.4.3          R6_2.6.1           
#>  [4] numDeriv_2016.8-1.1 fastmap_1.2.0       xfun_0.59          
#>  [7] cachem_1.1.0        knitr_1.51          htmltools_0.5.9    
#> [10] rmarkdown_2.31      lifecycle_1.0.5     cli_3.6.6          
#> [13] svglite_2.2.2       sass_0.4.10         pkgdown_2.2.0      
#> [16] textshaping_1.0.5   jquerylib_0.1.4     systemfonts_1.3.2  
#> [19] compiler_4.6.1      tools_4.6.1         bslib_0.11.0       
#> [22] evaluate_1.0.5      Rcpp_1.1.1-1.1      yaml_2.3.12        
#> [25] otel_0.2.0          jsonlite_2.0.0      rlang_1.2.0        
#> [28] fs_2.1.0            htmlwidgets_1.6.4
```
