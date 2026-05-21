# Detect linear reparameterization

Check whether two model specifications are related by a linear
reparameterization (affine transformation of parameters).

## Usage

``` r
detect_linear_reparam(spec_a, spec_b, y, n_test = 50, tol = 1e-04)
```

## Arguments

- spec_a:

  First `model_spec` object

- spec_b:

  Second `model_spec` object

- y:

  Numeric vector of observed data

- n_test:

  Number of parameter points to test (default 50)

- tol:

  Tolerance for detecting linear relationship

## Value

An S3 object of class `linear_reparam` containing:

- is_linear:

  Logical; whether a linear relationship was detected

- transformation:

  If linear, the estimated transformation matrix and offset

- r_squared:

  R-squared value of the linear fit

- residuals:

  Residuals from the linear fit

## Details

This function tests whether there exists a linear transformation theta_B
= A \* theta_A + b such that the two models produce the same likelihood
for all data.

Examples of linear reparameterizations:

- Normal(mu, sigma) vs Normal(mu, sigma^2): not linear (square
  transformation)

- Exponential(rate) vs Exponential(mean): linear (mean = 1/rate is not
  linear)

- Location-scale: linear in location, but log-linear in scale

## Examples

``` r
# Two models with different parameterizations
exp_spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(0.1, 10)),
  name = "Exponential"
)

gamma_spec <- model_spec(
  loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
  par_names = "rate",
  par_bounds = list(rate = c(0.1, 10)),
  name = "Gamma(1)"
)

set.seed(123)
y <- rexp(50, rate = 2)

result <- detect_linear_reparam(exp_spec, gamma_spec, y)
print(result)
```
