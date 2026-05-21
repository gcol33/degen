# Ecological population models

Three competing models for population growth data, demonstrating how to
compare alternative ecological hypotheses.

## Format

A list with 4 elements:

- y:

  Numeric vector of 50 population count observations

- models:

  Named list of 3 model_spec objects

- true_model:

  Name of the model used to generate data ("gompertz")

- true_params:

  True parameter values used in simulation

## Details

The three models are:

1.  Exponential growth: y ~ Poisson(lambda \* exp(r))

2.  Logistic growth: y ~ Poisson(K / (1 + exp(-r)))

3.  Gompertz growth: y ~ Poisson(K \* exp(-exp(-r)))

These models make different assumptions about density dependence and are
generally not equivalent except in limiting cases.

## Examples

``` r
data(ecological_models)

# Compare all three models
classes <- equivalence_classes(
  ecological_models$models,
  ecological_models$y,
  n_points = 20
)
print(classes)
```
