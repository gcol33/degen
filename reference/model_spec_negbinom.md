# Create model_spec for Negative Binomial distribution

Create model_spec for Negative Binomial distribution

## Usage

``` r
model_spec_negbinom(par = c("size_prob", "size_mu"), name = "NegBinomial")
```

## Arguments

- par:

  Parameterization: "size_prob" or "size_mu" (mean parameterization)

- name:

  Optional model name

## Value

A `model_spec` object

## Examples

``` r
spec <- model_spec_negbinom()
y <- rnbinom(100, size = 5, prob = 0.3)
loglik(spec, y, c(size = 5, prob = 0.3))
```
