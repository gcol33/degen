# Equivalent model pairs

A list of model specification pairs that are mathematically equivalent.
Useful for testing and demonstrating equivalence detection.

## Format

A list with 3 elements, each containing:

- model_a:

  First model specification

- model_b:

  Second model specification (equivalent to model_a)

- description:

  Character string describing the equivalence

## Details

The pairs include:

1.  Exponential vs Gamma(shape=1): Rate parameterizations

2.  Normal(mu, sigma) vs Normal(mu, sigma^2): Variance parameterizations

3.  Weibull(shape=1) vs Exponential: Special case equivalence

## Examples

``` r
data(equivalent_models)
names(equivalent_models)

# Test the first pair
pair1 <- equivalent_models[[1]]
y <- rexp(50, rate = 2)
ep <- equivalence_pair(pair1$model_a, pair1$model_b)
result <- compare_surfaces(ep, y, n_points = 20)
print(result)
```
