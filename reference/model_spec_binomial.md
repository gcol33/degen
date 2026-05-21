# Create model_spec for Binomial distribution

Create model_spec for Binomial distribution

## Usage

``` r
model_spec_binomial(size, name = "Binomial")
```

## Arguments

- size:

  Number of trials (known)

- name:

  Optional model name

## Value

A `model_spec` object

## Examples

``` r
spec <- model_spec_binomial(size = 10)
y <- rbinom(100, size = 10, prob = 0.3)
loglik(spec, y, c(prob = 0.3))
```
