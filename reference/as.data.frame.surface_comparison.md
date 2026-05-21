# Convert surface_comparison to data frame

Extract the evidence from a surface comparison as a tidy data frame.

## Usage

``` r
# S3 method for class 'surface_comparison'
as.data.frame(x, ...)
```

## Arguments

- x:

  A `surface_comparison` object

- ...:

  Additional arguments (ignored)

## Value

A data frame with columns for parameters, source likelihood,
discrepancy, and direction

## Examples

``` r
exp_spec <- model_spec_exponential()
gamma_spec <- model_spec_gamma(par = "rate", known_shape = 1)
pair <- equivalence_pair(exp_spec, gamma_spec)
y <- rexp(50, rate = 2)
result <- compare_surfaces(pair, y, n_points = 10)
df <- as.data.frame(result)
head(df)
```
