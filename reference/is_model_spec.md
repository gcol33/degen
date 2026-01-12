# Test if object is a model_spec

Test if object is a model_spec

## Usage

``` r
is_model_spec(x)
```

## Arguments

- x:

  Object to test

## Value

Logical indicating whether `x` is a `model_spec` object

## Examples

``` r
spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda"
)
is_model_spec(spec)  # TRUE
#> [1] TRUE
is_model_spec(list())  # FALSE
#> [1] FALSE
```
