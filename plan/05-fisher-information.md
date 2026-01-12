# Step 5: Fisher Information

**Version target**: 0.5.0
**Scope**: Fisher information matrix computation and analysis

## Objective

Implement tools for computing the Fisher information matrix and related quantities. The Fisher information characterizes local identifiability; singular or near-singular information matrices indicate non-identifiability.

## Conceptual background

The Fisher information matrix is:

$$I(\theta)_{ij} = -E\left[\frac{\partial^2 \log L}{\partial \theta_i \partial \theta_j}\right]$$

For observed data, we use the observed information (negative Hessian of log-likelihood). Properties:

- Positive definite → locally identifiable
- Singular → non-identifiable (rank deficient)
- Near-singular → weakly identified (practically problematic)

## API

### Core function

```r
fisher_information(
  spec,
  y,
  par,
  type = c("observed", "expected"),
  method = c("hessian", "score")
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `spec` | model_spec | Model specification |
| `y` | numeric | Observed data |
| `par` | numeric | Parameter values at which to evaluate |
| `type` | character | "observed" (data-specific) or "expected" (theoretical) |
| `method` | character | Computation method |

### Return value

An S3 object of class `fisher_info` containing:

- `matrix`: The information matrix
- `eigenvalues`: Eigenvalue decomposition
- `condition`: Condition number
- `rank`: Numerical rank
- `par`: Parameters at which computed
- `spec`: The model_spec

## Tasks

### 5.1 Hessian computation

Create `R/fisher_information.R`:

- [ ] `fisher_information()` - main function
- [ ] Use `numDeriv::hessian()` for numerical second derivatives
- [ ] Negate Hessian to get observed information
- [ ] Handle parameters at bounds (one-sided derivatives)

### 5.2 Score-based method (outer product)

Alternative computation using score function:

$$I(\theta) \approx \sum_i s_i(\theta) s_i(\theta)^T$$

where $s_i$ is the score contribution from observation $i$.

- [ ] `score_information()` - score-based computation
- [ ] Requires per-observation log-likelihood (or assumes iid)

### 5.3 Expected information

For common distributions, compute theoretical expected information:

- [ ] `expected_information()` - generic for model_spec
- [ ] User can provide analytical formula or use simulation

### 5.4 Information matrix analysis

Create `R/fisher_info.R`:

- [ ] `fisher_info` S3 class
- [ ] `print.fisher_info()` - summary display
- [ ] `summary.fisher_info()` - detailed analysis
- [ ] `plot.fisher_info()` - eigenvalue visualization

### 5.5 Diagnostic functions

- [ ] `info_eigenvalues()` - extract eigenvalues
- [ ] `info_condition()` - condition number
- [ ] `info_rank()` - numerical rank (with tolerance)
- [ ] `null_directions()` - eigenvectors of near-zero eigenvalues

## Example usage

```r
spec <- model_spec(
  loglik_fn = function(y, mu, sigma) {
    sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
  },
  par_names = c("mu", "sigma"),
  par_bounds = list(mu = c(-Inf, Inf), sigma = c(1e-6, Inf))
)

y <- rnorm(100, mean = 5, sd = 2)
info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))

print(info)
# <fisher_info> at par = (mu=5, sigma=2)
# Condition number: 2.1
# Rank: 2 (full)
# Eigenvalues: 50.2, 24.1

summary(info)
# Detailed eigenvalue analysis
# No near-zero eigenvalues detected
# Model appears locally identifiable
```

## Tests to write

```r
# tests/testthat/test-fisher_information.R

test_that("fisher_information returns correct dimensions", {...})

test_that("normal model gives known Fisher information", {
  # I(mu, sigma) is known analytically for normal
})

test_that("singular models have rank-deficient information", {
  # Create intentionally non-identifiable model
})

test_that("condition number detects near-singularity", {...})
test_that("null_directions identifies flat directions", {...})
```

## Files created

```
R/
├── fisher_information.R
├── fisher_info.R  # S3 class and methods
└── info_diagnostics.R  # eigenvalue analysis functions
tests/testthat/
└── test-fisher_information.R
```

## Edge cases

1. Single-parameter models (1x1 matrix)
2. Very flat likelihoods (all eigenvalues small)
3. Parameters at boundary (Hessian may be unreliable)
4. High-dimensional parameter spaces (>10 parameters)
5. Numerical instability in Hessian computation

## Numerical considerations

- Use `numDeriv::hessian()` with appropriate step sizes
- Check symmetry of computed Hessian (should be symmetric)
- Use eigendecomposition from `eigen()` with `symmetric = TRUE`
- Define rank tolerance based on machine precision and matrix size

## Interpretation guidance

| Condition number | Interpretation |
|------------------|----------------|
| < 10 | Well-conditioned, all parameters identifiable |
| 10-100 | Moderate conditioning, check eigenvalues |
| 100-1000 | Poor conditioning, some directions weakly identified |
| > 1000 | Near-singular, likely identifiability issues |

## Notes

- Observed information depends on data; expected information is a population quantity
- For small samples, observed and expected information can differ substantially
- Consider adding confidence intervals for eigenvalues via bootstrap
- May want to scale parameters to improve numerical stability
