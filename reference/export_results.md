# Export results to file

Write degen results to CSV or JSON format.

## Usage

``` r
export_results(x, file, ...)
```

## Arguments

- x:

  A degen result object (surface_comparison, identifiability_result,
  equiv_classes, or fisher_info)

- file:

  Output file path. Extension determines format (.csv or .json)

- ...:

  Additional arguments passed to write functions

## Value

Invisible file path

## Examples

``` r
spec <- model_spec_normal()
y <- rnorm(100, mean = 5, sd = 2)
result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))
tmp <- tempfile(fileext = ".csv")
export_results(result, tmp)
unlink(tmp)
```
