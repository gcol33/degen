# Extract model_spec from fitted R objects

Create a model_spec from common fitted model objects.

## Usage

``` r
model_spec_from_fit(object, ...)
```

## Arguments

- object:

  A fitted model object (lm, glm, nls, etc.)

- ...:

  Additional arguments (currently unused)

## Value

A `model_spec` object

## Examples

``` r
# From linear model
fit <- lm(mpg ~ wt, data = mtcars)
spec <- model_spec_from_fit(fit)
print(spec)
```
