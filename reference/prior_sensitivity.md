# Prior sensitivity analysis

Test how different prior specifications affect identifiability and
posterior concentration.

## Usage

``` r
prior_sensitivity(spec, y, par, prior_scales = NULL)
```

## Arguments

- spec:

  A `model_spec` object

- y:

  Numeric vector of observed data

- par:

  Numeric vector of parameter values (point estimate or posterior mean)

- prior_scales:

  Named list of scale factors for each parameter. Each element should be
  a numeric vector of scales to test. Scale 1 means use the original
  bounds; larger scales mean wider priors.

## Value

An S3 object of class `prior_sensitivity` containing:

- results:

  Data frame of identifiability results for each prior setting

- baseline:

  Identifiability result with original bounds

- most_informative:

  Prior setting with best identifiability

## Details

This function examines how the effective prior (parameter bounds)
affects the Fisher information and identifiability. Wider bounds (larger
scales) represent more diffuse priors, while narrower bounds represent
more informative priors.

## Examples

``` r
spec <- model_spec(
  loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
  par_names = c("mu", "sigma"),
  par_bounds = list(mu = c(-10, 10), sigma = c(0.1, 10)),
  name = "Normal"
)

set.seed(123)
y <- rnorm(30, mean = 5, sd = 2)

sens <- prior_sensitivity(spec, y, par = c(mu = 5, sigma = 2),
  prior_scales = list(mu = c(0.5, 1, 2), sigma = c(0.5, 1, 2)))
print(sens)
```
