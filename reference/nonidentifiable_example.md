# Non-identifiable model example

A dataset demonstrating parameter non-identifiability, where the sum of
two parameters is identifiable but individual parameters are not.

## Format

A list with 3 elements:

- y:

  Numeric vector of 100 simulated observations

- spec:

  A model_spec object with non-identifiable parameters

- true_sum:

  The true value of a + b used in simulation (5)

## Details

The model is: y ~ Normal(a + b, 1)

Only the sum (a + b) affects the likelihood, so infinitely many
combinations of (a, b) produce the same likelihood. The Fisher
information matrix is singular with rank 1.

## Examples

``` r
data(nonidentifiable_example)

# Check identifiability
result <- identifiability_check(
  nonidentifiable_example$spec,
  nonidentifiable_example$y,
  par = c(a = 2, b = 3)
)
print(result)

# Fisher information shows rank deficiency
info <- fisher_information(
  nonidentifiable_example$spec,
  nonidentifiable_example$y,
  par = c(a = 2, b = 3)
)
print(info)
```
