# Convert identifiability_result to data frame

Extract parameter identifiability status as a data frame.

## Usage

``` r
# S3 method for class 'identifiability_result'
as.data.frame(x, ...)
```

## Arguments

- x:

  An `identifiability_result` object

- ...:

  Additional arguments (ignored)

## Value

A data frame with columns: parameter, status, eigenvalue

## Examples

``` r
spec <- model_spec_normal()
y <- rnorm(100, mean = 5, sd = 2)
result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))
as.data.frame(result)
```
