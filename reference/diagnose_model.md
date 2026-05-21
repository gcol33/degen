# Diagnose model specification issues

Comprehensive health check for a model_spec, including likelihood
evaluation, gradient computation, and identifiability diagnostics.

## Usage

``` r
diagnose_model(spec, y, par = NULL)
```

## Arguments

- spec:

  A `model_spec` object

- y:

  Numeric vector of data

- par:

  Optional starting parameter values. If NULL, uses center of bounds.

## Value

A list with diagnostic results

## Examples

``` r
spec <- model_spec_normal()
y <- rnorm(100, 5, 2)
diagnose_model(spec, y)
```
