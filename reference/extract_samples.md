# Extract posterior samples from fitted objects

Helper function to extract posterior samples from common Bayesian
modeling packages.

## Usage

``` r
extract_samples(fit, pars = NULL)
```

## Arguments

- fit:

  A fitted model object from rstan, brms, rstanarm, or similar

- pars:

  Optional character vector of parameter names to extract

## Value

A matrix of posterior samples (rows = samples, columns = parameters)

## Details

This function provides a unified interface for extracting posterior
samples from various Bayesian modeling packages. Currently supports:

- stanfit objects (rstan)

- brmsfit objects (brms)

- stanreg objects (rstanarm)

- matrix/data.frame (returned as-is)

## Examples

``` r
# With a matrix of samples (manual specification)
samples <- matrix(rnorm(200), ncol = 2)
colnames(samples) <- c("mu", "sigma")
extracted <- extract_samples(samples)
```
