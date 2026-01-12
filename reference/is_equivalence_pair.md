# Test if object is an equivalence_pair

Test if object is an equivalence_pair

## Usage

``` r
is_equivalence_pair(x)
```

## Arguments

- x:

  Object to test

## Value

Logical indicating whether `x` is an `equivalence_pair` object

## Examples

``` r
exp_spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda"
)
pair <- equivalence_pair(exp_spec, exp_spec)
is_equivalence_pair(pair)  # TRUE
#> [1] TRUE
is_equivalence_pair(list())
#> [1] FALSE
is_equivalence_pair(list())  # FALSE
#> [1] FALSE
```
