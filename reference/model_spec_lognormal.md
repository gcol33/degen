# Create model_spec for Log-normal distribution

Create model_spec for Log-normal distribution

## Usage

``` r
model_spec_lognormal(name = "LogNormal")
```

## Arguments

- name:

  Optional model name

## Value

A `model_spec` object

## Examples

``` r
spec <- model_spec_lognormal()
y <- rlnorm(100, meanlog = 1, sdlog = 0.5)
loglik(spec, y, c(meanlog = 1, sdlog = 0.5))
```
