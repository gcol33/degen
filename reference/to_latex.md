# Format results as LaTeX table

Generate LaTeX table code for degen results.

## Usage

``` r
to_latex(x, caption = NULL, label = NULL, ...)
```

## Arguments

- x:

  A degen result object

- caption:

  Optional table caption

- label:

  Optional LaTeX label

- ...:

  Additional arguments passed to as.data.frame

## Value

Character string containing LaTeX code

## Examples

``` r
spec <- model_spec_normal()
y <- rnorm(100, mean = 5, sd = 2)
result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))
cat(to_latex(result))
```
