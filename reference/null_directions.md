# Find directions of non-identifiability

Returns eigenvectors corresponding to zero or near-zero eigenvalues.
These represent directions in parameter space along which the likelihood
is flat.

## Usage

``` r
null_directions(x, tol = 1e-06)
```

## Arguments

- x:

  A `fisher_info` object

- tol:

  Tolerance for considering an eigenvalue as zero

## Value

Matrix where each column is a null direction, or NULL if none

## Examples

``` r
# Model where only a+b is identified, not a and b individually
spec <- model_spec(
  loglik_fn = function(y, a, b) {
    sum(dnorm(y, mean = a + b, sd = 1, log = TRUE))
  },
  par_names = c("a", "b")
)

set.seed(123)
y <- rnorm(100, mean = 5, sd = 1)
info <- fisher_information(spec, y, par = c(a = 2, b = 3))
null_directions(info)
#>       null_1
#> a -0.7071068
#> b  0.7071068
```
