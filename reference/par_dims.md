# Get parameter dimensions for both models

Get parameter dimensions for both models

## Usage

``` r
par_dims(pair)
```

## Arguments

- pair:

  An `equivalence_pair` object

## Value

Named integer vector with elements `a` and `b`

## Examples

``` r
exp_spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda"
)
norm_spec <- model_spec(
  loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
  par_names = c("mu", "sigma")
)
pair <- equivalence_pair(exp_spec, norm_spec)
par_dims(pair)  # c(a = 1, b = 2)
#> a b 
#> 1 2 
```
