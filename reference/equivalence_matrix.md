# Create equivalence matrix visualization

Generate a visual matrix showing pairwise equivalence relationships.

## Usage

``` r
equivalence_matrix(x, ...)
```

## Arguments

- x:

  Result from
  [`compare_all_pairs()`](https://gcol33.github.io/degen/reference/compare_all_pairs.md)
  or
  [`equivalence_classes()`](https://gcol33.github.io/degen/reference/equivalence_classes.md)

- ...:

  Additional arguments passed to
  [`image()`](https://rdrr.io/r/graphics/image.html)

## Value

Invisible equivalence matrix

## Examples

``` r
exp_spec <- model_spec_exponential()
gamma1_spec <- model_spec_gamma(par = "rate", known_shape = 1)
gamma2_spec <- model_spec_gamma(par = "rate", known_shape = 2)
models <- list(exp = exp_spec, gamma1 = gamma1_spec, gamma2 = gamma2_spec)
y <- rexp(50, rate = 2)
result <- compare_all_pairs(models, y, n_points = 10)
equivalence_matrix(result)
```
