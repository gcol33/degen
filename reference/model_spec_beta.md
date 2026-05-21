# Create model_spec for Beta distribution

Create model_spec for Beta distribution

## Usage

``` r
model_spec_beta(name = "Beta")
```

## Arguments

- name:

  Optional model name

## Value

A `model_spec` object

## Examples

``` r
spec <- model_spec_beta()
y <- rbeta(100, shape1 = 2, shape2 = 5)
loglik(spec, y, c(shape1 = 2, shape2 = 5))
```
