# Create model_spec for Poisson distribution

Create model_spec for Poisson distribution

## Usage

``` r
model_spec_poisson(name = "Poisson")
```

## Arguments

- name:

  Optional model name

## Value

A `model_spec` object

## Examples

``` r
spec <- model_spec_poisson()
y <- rpois(100, lambda = 5)
loglik(spec, y, c(lambda = 5))
```
