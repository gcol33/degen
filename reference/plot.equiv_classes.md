# Plot equivalence classes

Visualize equivalence relationships between models as a network diagram.

## Usage

``` r
# S3 method for class 'equiv_classes'
plot(x, layout = c("circle", "grid", "spring"), ...)
```

## Arguments

- x:

  An `equiv_classes` object

- layout:

  Layout algorithm: "circle" (default), "grid", or "spring"

- ...:

  Additional arguments passed to plotting functions

## Value

Invisible `x`

## Examples

``` r
exp_spec <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(1e-6, 100))
)
gamma1_spec <- model_spec(
  loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
  par_names = "rate",
  par_bounds = list(rate = c(1e-6, 100))
)
gamma2_spec <- model_spec(
  loglik_fn = function(y, rate) sum(dgamma(y, shape = 2, rate = rate, log = TRUE)),
  par_names = "rate",
  par_bounds = list(rate = c(1e-6, 100))
)
models <- list(exp = exp_spec, gamma1 = gamma1_spec, gamma2 = gamma2_spec)
y <- rexp(50, rate = 2)
classes <- equivalence_classes(models, y, n_points = 10)
plot(classes)
```
