# Create model_spec for Exponential distribution

Create model_spec for Exponential distribution

## Usage

``` r
model_spec_exponential(name = "Exponential")
```

## Arguments

- name:

  Optional model name

## Value

A `model_spec` object

## Examples

``` r
spec <- model_spec_exponential()
y <- rexp(100, rate = 2)
loglik(spec, y, c(rate = 2))
```
