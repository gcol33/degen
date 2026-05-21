# Plot likelihood surface

Create a 2D contour plot of the log-likelihood surface for two-parameter
models. Useful for visualizing parameter identifiability and
correlation.

## Usage

``` r
plot_likelihood_surface(
  spec,
  y,
  par,
  which_par = NULL,
  n_grid = 50,
  range_mult = 2,
  type = c("contour", "filled", "both"),
  show_mle = TRUE,
  ...
)
```

## Arguments

- spec:

  A `model_spec` object with exactly 2 parameters

- y:

  Numeric vector of observed data

- par:

  Named numeric vector of parameter values (used as center point)

- which_par:

  Optional character vector of length 2 specifying which parameters to
  plot (for models with \>2 parameters)

- n_grid:

  Number of grid points per dimension (default 50)

- range_mult:

  Multiplier for parameter range around center (default 2)

- type:

  Plot type: "contour" (default), "filled", or "both"

- show_mle:

  Logical; show MLE point (default TRUE)

- ...:

  Additional arguments passed to plotting functions

## Value

Invisible list with grid coordinates and likelihood values

## Examples

``` r
spec <- model_spec_normal()
y <- rnorm(100, mean = 5, sd = 2)
plot_likelihood_surface(spec, y, par = c(mu = 5, sigma = 2))
plot_likelihood_surface(spec, y, par = c(mu = 5, sigma = 2), type = "filled")
```
