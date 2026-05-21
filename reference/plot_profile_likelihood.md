# Plot profile likelihood for a parameter

Plot the profile likelihood curve for a single parameter with confidence
interval bounds.

## Usage

``` r
plot_profile_likelihood(profile, ci_level = 0.95, mle = NULL, ...)
```

## Arguments

- profile:

  A data frame from
  [`profile_likelihood()`](https://gcol33.github.io/degen/reference/profile_likelihood.md)
  or parameter name

- ci_level:

  Confidence level for interval (default 0.95)

- mle:

  Optional MLE value to mark on plot

- ...:

  Additional arguments passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

## Value

Invisible profile data

## Examples

``` r
spec <- model_spec(
  loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
  par_names = c("mu", "sigma"),
  par_bounds = list(sigma = c(1e-6, Inf))
)
y <- rnorm(100, mean = 5, sd = 2)
prof <- profile_likelihood(spec, y, par = c(mu = 5, sigma = 2), which_par = "mu")
plot_profile_likelihood(prof, mle = 5)
```
