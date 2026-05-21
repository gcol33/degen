# Compute profile likelihood confidence intervals

Extract confidence intervals from profile likelihood curves using the
likelihood ratio test criterion.

## Usage

``` r
profile_ci(spec, y, par, level = 0.95, n_points = 50, which_par = NULL)
```

## Arguments

- spec:

  A `model_spec` object

- y:

  Numeric vector of observed data

- par:

  Named numeric vector of parameter values (typically MLE)

- level:

  Confidence level (default 0.95)

- n_points:

  Number of points for profile likelihood (default 50)

- which_par:

  Optional character vector of parameter names to compute CIs for. If
  NULL (default), computes CIs for all parameters.

## Value

A data frame with columns:

- parameter:

  Parameter name

- estimate:

  Point estimate (from `par`)

- lower:

  Lower confidence bound

- upper:

  Upper confidence bound

- level:

  Confidence level

## Details

The confidence interval is based on the likelihood ratio test. For a
given confidence level, the interval includes all parameter values where
the profile log-likelihood is within `qchisq(level, df=1)/2` of the
maximum.

## Examples

``` r
spec <- model_spec(
  loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
  par_names = c("mu", "sigma"),
  par_bounds = list(sigma = c(1e-6, Inf))
)
set.seed(123)
y <- rnorm(100, mean = 5, sd = 2)
ci <- profile_ci(spec, y, par = c(mu = 5, sigma = 2))
print(ci)
```
