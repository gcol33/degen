# Convert equiv_classes to data frame

Extract equivalence class membership as a data frame.

## Usage

``` r
# S3 method for class 'equiv_classes'
as.data.frame(x, ...)
```

## Arguments

- x:

  An `equiv_classes` object

- ...:

  Additional arguments (ignored)

## Value

A data frame with columns: model, class

## Examples

``` r
exp_spec <- model_spec_exponential()
gamma1_spec <- model_spec_gamma(par = "rate", known_shape = 1)
gamma2_spec <- model_spec_gamma(par = "rate", known_shape = 2)
models <- list(exp = exp_spec, gamma1 = gamma1_spec, gamma2 = gamma2_spec)
y <- rexp(50, rate = 2)
classes <- equivalence_classes(models, y, n_points = 10)
as.data.frame(classes)
```
