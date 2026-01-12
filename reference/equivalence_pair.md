# Create an equivalence pair for model comparison

Pairs two model specifications for equivalence comparison. The pair
represents a question: "Are these two models observationally
equivalent?"

## Usage

``` r
equivalence_pair(spec_a, spec_b, name = NULL)
```

## Arguments

- spec_a:

  A `model_spec` object (first model)

- spec_b:

  A `model_spec` object (second model)

- name:

  Optional character string naming this comparison. If `NULL`, a name is
  generated from the model names.

## Value

An S3 object of class `equivalence_pair` with components:

- spec_a:

  First model specification

- spec_b:

  Second model specification

- name:

  Comparison name

- n_par_a:

  Number of parameters in model A

- n_par_b:

  Number of parameters in model B

## Examples

``` r
# Compare exponential and gamma(shape=1) models
exp_spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(1e-6, Inf)),
  name = "Exponential"
)

gamma_spec <- model_spec(
  loglik_fn = function(y, rate) {
    sum(dgamma(y, shape = 1, rate = rate, log = TRUE))
  },
  par_names = "rate",
  par_bounds = list(rate = c(1e-6, Inf)),
  name = "Gamma(1)"
)

pair <- equivalence_pair(exp_spec, gamma_spec)
print(pair)
#> <equivalence_pair> Exponential vs Gamma(1) 
#> Model A: Exponential (1 parameter)
#> Model B: Gamma(1) (1 parameter)
```
