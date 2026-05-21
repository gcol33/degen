# Symbolic Fisher information

Compute Fisher information using closed-form analytical formulas for
standard distributions. Falls back to numerical computation if no
analytical formula is available.

## Usage

``` r
symbolic_fisher(spec, y, par, method = c("auto", "analytical", "numerical"))
```

## Arguments

- spec:

  A `model_spec` object

- y:

  Numeric vector of observed data (used for numerical fallback and n)

- par:

  Named numeric vector of parameter values

- method:

  Character; "auto" tries analytical first, "analytical" requires
  closed-form, "numerical" forces numerical computation

## Value

A `fisher_info` object containing the Fisher information matrix

## Details

Analytical formulas are available for:

- Normal (mu, sigma): I = diag(n/sigma^2, 2n/sigma^2)

- Exponential (rate): I = n/rate^2

- Poisson (lambda): I = n/lambda

- Gamma (shape, rate): Closed-form involving digamma/trigamma

- Binomial (prob): I = n/(prob \* (1-prob))

- Beta (shape1, shape2): Closed-form involving trigamma

- Log-normal (mu, sigma): Similar to normal on log scale

The function attempts to detect the distribution from the model name.

## Examples

``` r
spec <- model_spec_normal()
y <- rnorm(100, mean = 5, sd = 2)

# Analytical computation
info <- symbolic_fisher(spec, y, par = c(mu = 5, sigma = 2))
print(info)
```
