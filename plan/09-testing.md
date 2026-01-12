# Step 9: Testing

**Version target**: 0.9.0
**Scope**: Comprehensive test suite with high coverage

## Objective

Build a robust test suite that validates correctness, catches regressions, and documents expected behavior. Tests should cover both happy paths and edge cases.

## Testing philosophy

1. **Known results**: Test against analytically known results where possible
2. **Invariants**: Test properties that must hold (e.g., equivalence is symmetric)
3. **Edge cases**: Explicitly test boundary conditions
4. **Error handling**: Verify informative errors for invalid inputs
5. **Integration**: Test complete workflows, not just individual functions

## Test organization

```
tests/
├── testthat.R
└── testthat/
    ├── test-model_spec.R
    ├── test-equivalence_pair.R
    ├── test-compare_surfaces.R
    ├── test-fisher_information.R
    ├── test-identifiability.R
    ├── test-equivalence_classes.R
    ├── test-integration.R
    └── helper-fixtures.R
```

## Tasks

### 9.1 Test fixtures

Create `tests/testthat/helper-fixtures.R`:

- [ ] `make_exp_spec()` - Exponential model
- [ ] `make_normal_spec()` - Normal model (mu, sigma)
- [ ] `make_gamma_spec()` - Gamma model (shape, rate)
- [ ] `make_nonid_spec()` - Intentionally non-identifiable model
- [ ] `make_equivalent_pair()` - Pair of known-equivalent models
- [ ] `make_different_pair()` - Pair of known-different models

### 9.2 Unit tests by module

#### `test-model_spec.R`

```r
test_that("model_spec creates valid object", {
  spec <- make_exp_spec()
  expect_s3_class(spec, "model_spec")
  expect_equal(n_par(spec), 1)
  expect_equal(par_names(spec), "lambda")
})

test_that("model_spec validates loglik_fn signature", {
  expect_error(
    model_spec(loglik_fn = function(x) x, par_names = "a"),
    "must have 'y' as first argument"
  )
})

test_that("model_spec rejects invalid bounds", {
  expect_error(
    model_spec(
      loglik_fn = function(y, a) sum(y * a),
      par_names = "a",
      par_bounds = list(a = c(5, 1))  # lower > upper
    ),
    "lower bound must be less than upper"
  )
})

test_that("loglik evaluates correctly", {
  spec <- make_exp_spec()
  y <- c(1, 2, 3)
  expected <- sum(dexp(y, rate = 2, log = TRUE))
  expect_equal(loglik(spec, y, par = c(lambda = 2)), expected)
})
```

#### `test-compare_surfaces.R`

```r
test_that("equivalent models are detected", {
  pair <- make_equivalent_pair()  # Exp vs Gamma(shape=1)
  y <- rexp(100, rate = 2)
  result <- compare_surfaces(pair, y, n_points = 20)
  expect_true(result$equivalent)
})

test_that("non-equivalent models are detected", {
  pair <- make_different_pair()  # Exp vs Normal
  y <- rexp(100, rate = 2)
  result <- compare_surfaces(pair, y, n_points = 20)
  expect_false(result$equivalent)
})

test_that("comparison is symmetric", {
  spec_a <- make_exp_spec()
  spec_b <- make_gamma_spec(shape = 1)
  y <- rexp(50, rate = 2)

  pair_ab <- equivalence_pair(spec_a, spec_b)
  pair_ba <- equivalence_pair(spec_b, spec_a)

  result_ab <- compare_surfaces(pair_ab, y)
  result_ba <- compare_surfaces(pair_ba, y)

  expect_equal(result_ab$equivalent, result_ba$equivalent)
})
```

#### `test-fisher_information.R`

```r
test_that("normal model gives known Fisher information", {
  spec <- make_normal_spec()
  y <- rnorm(1000, mean = 5, sd = 2)
  info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))

  # Known result: I_mu = n/sigma^2, I_sigma = 2n/sigma^2
  n <- length(y)
  expected_I_mu <- n / 4
  expected_I_sigma <- 2 * n / 4

  expect_equal(info$matrix[1, 1], expected_I_mu, tolerance = 0.1)
  expect_equal(info$matrix[2, 2], expected_I_sigma, tolerance = 0.1)
})

test_that("singular models have rank-deficient information", {
  spec <- make_nonid_spec()  # a + b model
  y <- rnorm(100, mean = 5)
  info <- fisher_information(spec, y, par = c(a = 2, b = 3))

  expect_lt(info$rank, 2)  # Should be rank 1, not 2
})
```

#### `test-identifiability.R`

```r
test_that("identifiable model passes check", {
  spec <- make_normal_spec()
  y <- rnorm(100, mean = 5, sd = 2)
  result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))

  expect_equal(result$status["mu"], "identified")
  expect_equal(result$status["sigma"], "identified")
})

test_that("non-identifiable parameters are detected", {
  spec <- make_nonid_spec()
  y <- rnorm(100, mean = 5)
  result <- identifiability_check(spec, y, par = c(a = 2, b = 3))

  expect_equal(result$status["a"], "non_identifiable")
  expect_equal(result$status["b"], "non_identifiable")
})

test_that("identified functions are found", {
  spec <- make_nonid_spec()
  y <- rnorm(100, mean = 5)
  result <- identifiability_check(spec, y, par = c(a = 2, b = 3))

  # a + b should be identified
  expect_true("a + b" %in% result$identified_functions)
})
```

### 9.3 Integration tests

Create `tests/testthat/test-integration.R`:

```r
test_that("complete workflow: spec -> pair -> compare", {
  # Define models
  spec_a <- model_spec(...)
  spec_b <- model_spec(...)

  # Create pair
  pair <- equivalence_pair(spec_a, spec_b)

  # Compare
  y <- simulate_data()
  result <- compare_surfaces(pair, y)

  # Check complete result structure
  expect_s3_class(result, "surface_comparison")
  expect_true(is.logical(result$equivalent))
})

test_that("complete workflow: spec -> fisher -> identifiability", {
  spec <- model_spec(...)
  y <- simulate_data()

  info <- fisher_information(spec, y, par = initial_par())
  result <- identifiability_check(spec, y, par = initial_par())

  # Results should be consistent
  if (info$rank < n_par(spec)) {
    expect_true(any(result$status == "non_identifiable"))
  }
})

test_that("equivalence_classes groups correctly", {
  models <- list(
    exp = make_exp_spec(),
    gamma1 = make_gamma_spec(shape = 1),
    gamma2 = make_gamma_spec(shape = 2)
  )

  y <- rexp(100, rate = 2)
  classes <- equivalence_classes(models, y)

  # exp and gamma1 should be in same class
  expect_equal(classes$membership["exp"], classes$membership["gamma1"])
  # gamma2 should be different
  expect_false(classes$membership["gamma2"] == classes$membership["exp"])
})
```

### 9.4 Edge case tests

- [ ] Single-parameter models
- [ ] Empty data (y = numeric(0))
- [ ] Single observation (y = 5)
- [ ] Parameters at bounds
- [ ] Very large datasets
- [ ] High-dimensional parameter spaces
- [ ] Flat likelihood surfaces
- [ ] Multimodal likelihoods

### 9.5 Error handling tests

```r
test_that("informative errors for invalid inputs", {
  expect_error(model_spec(loglik_fn = "not a function"), "must be a function")
  expect_error(equivalence_pair("not a spec", make_exp_spec()), "must be model_spec")
  expect_error(compare_surfaces(make_equivalent_pair(), y = "not numeric"), "must be numeric")
})
```

### 9.6 Snapshot tests

For complex outputs (print methods, summaries):

```r
test_that("print.model_spec output is stable", {
  spec <- make_exp_spec()
  expect_snapshot(print(spec))
})

test_that("summary.identifiability_result output is stable", {
  # ...
  expect_snapshot(summary(result))
})
```

## Coverage targets

| Module | Target |
|--------|--------|
| model_spec.R | 95% |
| equivalence_pair.R | 95% |
| compare_surfaces.R | 85% |
| fisher_information.R | 90% |
| identifiability.R | 85% |
| equivalence_classes.R | 85% |
| **Overall** | **90%** |

## Tasks checklist

- [ ] Write helper-fixtures.R
- [ ] Unit tests for model_spec
- [ ] Unit tests for equivalence_pair
- [ ] Unit tests for compare_surfaces
- [ ] Unit tests for fisher_information
- [ ] Unit tests for identifiability
- [ ] Unit tests for equivalence_classes
- [ ] Integration tests
- [ ] Edge case tests
- [ ] Error handling tests
- [ ] Snapshot tests
- [ ] Achieve 90% coverage

## Notes

- Use `set.seed()` for reproducibility in stochastic tests
- Use `skip_on_cran()` for slow tests
- Consider `testthat::local_seed()` for test isolation
- Add `expect_no_error()` for smoke tests
- Document known limitations in test comments
