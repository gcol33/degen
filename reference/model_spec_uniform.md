# Create model_spec for Uniform distribution

Create model_spec for Uniform distribution

## Usage

``` r
model_spec_uniform(name = "Uniform")
```

## Arguments

- name:

  Optional model name

## Value

A `model_spec` object

## Examples

``` r
spec <- model_spec_uniform()
y <- runif(100, min = 2, max = 8)
loglik(spec, y, c(min = 2, max = 8))
```
