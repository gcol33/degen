# Convert fisher_info to data frame

Extract Fisher information matrix as a data frame.

## Usage

``` r
# S3 method for class 'fisher_info'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  what = c("matrix", "eigenvalues", "summary"),
  ...
)
```

## Arguments

- x:

  A `fisher_info` object

- row.names:

  Ignored

- optional:

  Ignored

- what:

  What to extract: "matrix" (default), "eigenvalues", or "summary"

- ...:

  Additional arguments (ignored)

## Value

A data frame

## Examples

``` r
spec <- model_spec_normal()
y <- rnorm(100, mean = 5, sd = 2)
info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))
as.data.frame(info)
as.data.frame(info, what = "eigenvalues")
```
