# Create model_spec for Weibull distribution

Create model_spec for Weibull distribution

## Usage

``` r
model_spec_weibull(name = "Weibull")
```

## Arguments

- name:

  Optional model name

## Value

A `model_spec` object

## Examples

``` r
spec <- model_spec_weibull()
y <- rweibull(100, shape = 2, scale = 1)
loglik(spec, y, c(shape = 2, scale = 1))
```
