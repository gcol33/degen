# Writing Model Specifications

## The model_spec object

The `model_spec` is the fundamental building block in degen. It
encapsulates a statistical model through its log-likelihood function,
allowing the package to perform equivalence testing and identifiability
analysis.

### Basic structure

A model_spec requires three key components:

1.  **loglik_fn**: A function computing the log-likelihood

2.  **par_names**: Names of the model parameters

3.  **par_bounds**: (Optional) Bounds on parameter values

``` r

# A simple normal model
norm_spec <- model_spec(
  loglik_fn = function(y, mu, sigma) {
    sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
  },
  par_names = c("mu", "sigma"),
  par_bounds = list(mu = c(-Inf, Inf), sigma = c(1e-6, Inf)),
  name = "Normal"
)

print(norm_spec)
#> <model_spec> Normal 
#> Parameters: mu, sigma (2)
#> Bounds:
#>   sigma in (1e-06, Inf)
```

## Writing log-likelihood functions

### The function signature

The log-likelihood function must have `y` as its first argument (the
data), followed by named parameters:

``` r

# Correct: y comes first, then named parameters
good_fn <- function(y, lambda) {
  sum(dexp(y, rate = lambda, log = TRUE))
}

# The parameter names must match those declared in par_names
exp_spec <- model_spec(
  loglik_fn = good_fn,
  par_names = "lambda",
  par_bounds = list(lambda = c(1e-6, Inf)),
  name = "Exponential"
)
```

### Returning a scalar

The function must return a single numeric value (the log-likelihood):

``` r

# Correct: returns sum (a scalar)
correct_ll <- function(y, mu) {
  sum(dnorm(y, mean = mu, sd = 1, log = TRUE))
}

# Wrong: returns a vector
wrong_ll <- function(y, mu) {
  dnorm(y, mean = mu, sd = 1, log = TRUE)  # Missing sum()!
}
```

### Handling edge cases

Write defensive code that handles problematic parameter values:

``` r

# A robust mixture model likelihood
mixture_ll <- function(y, mu1, mu2, sigma, pi) {
  # Guard against invalid parameters
  if (pi <= 0 || pi >= 1) return(-Inf)
  if (sigma <= 0) return(-Inf)

  # Compute likelihood with numerical stability
  ll <- log(pi * dnorm(y, mu1, sigma) + (1 - pi) * dnorm(y, mu2, sigma))
  sum(ll)
}
```

## Using helper functions

For common distributions, degen provides helper functions that create
properly-specified models:

``` r

# Instead of writing the likelihood manually...
norm_manual <- model_spec(
  loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
  par_names = c("mu", "sigma"),
  par_bounds = list(mu = c(-Inf, Inf), sigma = c(1e-6, Inf)),
  name = "Normal"
)

# ...use the helper
norm_helper <- model_spec_normal()

# Both work the same way
y <- rnorm(100, mean = 5, sd = 2)
loglik(norm_manual, y, c(mu = 5, sigma = 2))
#> [1] -216.127
loglik(norm_helper, y, c(mu = 5, sigma = 2))
#> [1] -216.127
```

Available helpers include:

- [`model_spec_normal()`](https://gcol33.github.io/degen/reference/model_spec_normal.md) -
  Normal distribution

- [`model_spec_exponential()`](https://gcol33.github.io/degen/reference/model_spec_exponential.md) -
  Exponential distribution

- [`model_spec_gamma()`](https://gcol33.github.io/degen/reference/model_spec_gamma.md) -
  Gamma distribution

- [`model_spec_poisson()`](https://gcol33.github.io/degen/reference/model_spec_poisson.md) -
  Poisson distribution

- [`model_spec_binomial()`](https://gcol33.github.io/degen/reference/model_spec_binomial.md) -
  Binomial distribution

- [`model_spec_beta()`](https://gcol33.github.io/degen/reference/model_spec_beta.md) -
  Beta distribution

- [`model_spec_lognormal()`](https://gcol33.github.io/degen/reference/model_spec_lognormal.md) -
  Log-normal distribution

- [`model_spec_weibull()`](https://gcol33.github.io/degen/reference/model_spec_weibull.md) -
  Weibull distribution

- [`model_spec_negbinom()`](https://gcol33.github.io/degen/reference/model_spec_negbinom.md) -
  Negative binomial distribution

- [`model_spec_uniform()`](https://gcol33.github.io/degen/reference/model_spec_uniform.md) -
  Uniform distribution

## Parameter bounds

Bounds constrain where the package searches for equivalent parameters.

### Why bounds matter

1.  **Numerical stability**: Prevent evaluation at invalid values

2.  **Efficiency**: Focus search on meaningful regions

3.  **Physical constraints**: Enforce domain knowledge (e.g., rates must
    be positive)

``` r

# A gamma model with proper bounds
gamma_spec <- model_spec(
  loglik_fn = function(y, shape, rate) {
    sum(dgamma(y, shape = shape, rate = rate, log = TRUE))
  },
  par_names = c("shape", "rate"),
  par_bounds = list(
    shape = c(1e-6, 100),   # Shape must be positive
    rate = c(1e-6, 100)      # Rate must be positive
  ),
  name = "Gamma"
)
```

### Default bounds

If you don’t specify bounds, degen uses `c(-Inf, Inf)`:

``` r

# No bounds specified - defaults to unbounded
unbounded_spec <- model_spec(
  loglik_fn = function(y, theta) sum(dnorm(y, theta, 1, log = TRUE)),
  par_names = "theta",
  name = "Location model"
)

par_bounds(unbounded_spec)
#> $theta
#> [1] -Inf  Inf
```

### Effective bounds

For equivalence testing, degen proposes “effective bounds” based on the
data to focus the search:

``` r

y <- rexp(100, rate = 2)
spec <- model_spec_exponential()

# Internal function proposes data-driven bounds
# degen:::propose_bounds(spec, y)
```

## Validation and diagnostics

### Checking your specification

Use
[`validate_loglik()`](https://gcol33.github.io/degen/reference/validate_loglik.md)
to test your likelihood function:

``` r

spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(1e-6, 100)),
  name = "Exponential"
)

y <- rexp(50, rate = 2)
validate_loglik(spec, y, n_test = 10)
#> Validating log-likelihood function...
#> 
#> 1. Checking return type... OK
#> 2. Checking for NA/NaN values... OK
#> 3. Checking for +Inf values... OK
#> 4. Checking gradient computation... OK
#> 5. Checking likelihood ordering... OK
#> 
#> All checks passed.
```

### Full diagnostics

For comprehensive checking, use
[`diagnose_model()`](https://gcol33.github.io/degen/reference/diagnose_model.md):

``` r

# diagnose_model() runs multiple checks
diag <- diagnose_model(spec, y, par = c(lambda = 2))
print(diag)
```

## Common pitfalls

### Pitfall 1: Mismatched parameter names

``` r

# This will fail - 'rate' in function but 'lambda' in par_names
model_spec(
  loglik_fn = function(y, rate) sum(dexp(y, rate = rate, log = TRUE)),
  par_names = "lambda"  # Mismatch!
)
```

### Pitfall 2: Returning non-finite values

``` r

# Bad: can return NaN for negative sigma
bad_ll <- function(y, mu, sigma) {
  sum(dnorm(y, mu, sigma, log = TRUE))  # NaN if sigma < 0
}

# Better: return -Inf for invalid regions
better_ll <- function(y, mu, sigma) {
  if (sigma <= 0) return(-Inf)
  sum(dnorm(y, mu, sigma, log = TRUE))
}
```

### Pitfall 3: Using vectorized parameters

``` r

# Wrong: trying to use a vector parameter
bad_spec <- model_spec(
  loglik_fn = function(y, beta) {
    # beta is expected to be length 2
    sum(dnorm(y, mean = beta[1], sd = beta[2], log = TRUE))
  },
  par_names = "beta"  # Only one name but expects vector!
)

# Correct: use separate named parameters
good_spec <- model_spec(
  loglik_fn = function(y, beta1, beta2) {
    sum(dnorm(y, mean = beta1, sd = beta2, log = TRUE))
  },
  par_names = c("beta1", "beta2")
)
```

## Advanced usage

### Adding simulation capability

Attach a simulation function for bootstrap or power analysis:

``` r

spec <- model_spec_normal()
spec <- add_simulator(spec, function(n, mu, sigma) rnorm(n, mu, sigma))

# Now you can simulate data
can_simulate(spec)
#> [1] TRUE
simulated_y <- simulate(spec, n = 100, par = c(mu = 5, sigma = 2))
head(simulated_y)
#> [1] 6.350489 7.306752 1.626991 3.194370 7.635267 7.200379
```

### Extracting from fitted models

Create a model_spec from an existing fitted model:

``` r

# Fit a linear model
data <- data.frame(x = 1:50, y = 2 + 3 * (1:50) + rnorm(50))
fit <- lm(y ~ x, data = data)

# Extract as model_spec
spec <- model_spec_from_fit(fit)
print(spec)
#> <model_spec> LM: y ~ x 
#> Parameters: X_Intercept_, x, sigma (3)
#> Bounds:
#>   sigma in (1e-10, Inf)
```

## See Also

- [`vignette("introduction")`](https://gcol33.github.io/degen/articles/introduction.md) -
  Package overview and quick start

- [`vignette("comparing-models")`](https://gcol33.github.io/degen/articles/comparing-models.md) -
  Using models for equivalence testing

- [`vignette("identifiability-diagnostics")`](https://gcol33.github.io/degen/articles/identifiability-diagnostics.md) -
  Diagnosing parameter issues

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
