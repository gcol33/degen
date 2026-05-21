# Create model_spec for Gamma distribution

Create model_spec for Gamma distribution

## Usage

``` r
model_spec_gamma(
  par = c("shape_rate", "shape_scale", "rate", "scale"),
  known_shape = NULL,
  name = "Gamma"
)
```

## Arguments

- par:

  Parameterization: "shape_rate" (default) or "shape_scale"

- known_shape:

  Fixed shape value (if estimating only rate/scale)

- name:

  Optional model name

## Value

A `model_spec` object

## Examples

``` r
spec <- model_spec_gamma()
y <- rgamma(100, shape = 2, rate = 1)
loglik(spec, y, c(shape = 2, rate = 1))
```
