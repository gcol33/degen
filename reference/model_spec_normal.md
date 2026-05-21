# Create model_spec for Normal distribution

Create model_spec for Normal distribution

## Usage

``` r
model_spec_normal(
  par = c("both", "mean", "sd"),
  known_mean = NULL,
  known_sd = NULL,
  name = "Normal"
)
```

## Arguments

- par:

  Which parameters to estimate: "both" (default), "mean", or "sd"

- known_mean:

  Fixed mean value (if par = "sd")

- known_sd:

  Fixed sd value (if par = "mean")

- name:

  Optional model name

## Value

A `model_spec` object

## Examples

``` r
spec <- model_spec_normal()
y <- rnorm(100, mean = 5, sd = 2)
loglik(spec, y, c(mu = 5, sigma = 2))
```
