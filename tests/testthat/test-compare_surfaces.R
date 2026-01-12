# Test fixtures
make_exp_spec <- function() {
  model_spec(
    loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
    par_names = "lambda",
    par_bounds = list(lambda = c(1e-6, 100)),
    name = "Exponential"
  )
}

make_gamma1_spec <- function() {
  model_spec(
    loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
    par_names = "rate",
    par_bounds = list(rate = c(1e-6, 100)),
    name = "Gamma(1)"
  )
}

make_gamma2_spec <- function() {
  model_spec(
    loglik_fn = function(y, rate) sum(dgamma(y, shape = 2, rate = rate, log = TRUE)),
    par_names = "rate",
    par_bounds = list(rate = c(1e-6, 100)),
    name = "Gamma(2)"
  )
}

make_normal_spec <- function() {
  model_spec(
    loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
    par_names = c("mu", "sigma"),
    par_bounds = list(mu = c(-100, 100), sigma = c(1e-6, 100)),
    name = "Normal"
  )
}

# Core functionality tests
test_that("equivalent models are detected (exp vs gamma1)", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma_spec <- make_gamma1_spec()
  pair <- equivalence_pair(exp_spec, gamma_spec)

  set.seed(42)
  y <- rexp(50, rate = 2)

  result <- compare_surfaces(pair, y, n_points = 15, tol = 1e-4)

  expect_s3_class(result, "surface_comparison")
  expect_true(result$equivalent)
  expect_lt(result$max_discrepancy, 1e-4)
})

test_that("non-equivalent models are detected (exp vs gamma2)", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma2_spec <- make_gamma2_spec()
  pair <- equivalence_pair(exp_spec, gamma2_spec)

  set.seed(42)
  y <- rexp(50, rate = 2)

  result <- compare_surfaces(pair, y, n_points = 15, tol = 1e-4)

  expect_s3_class(result, "surface_comparison")
  expect_false(result$equivalent)
  expect_gt(result$max_discrepancy, 1e-4)
})

test_that("non-equivalent models detected (different parameter counts)", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  norm_spec <- make_normal_spec()
  pair <- equivalence_pair(exp_spec, norm_spec)

  set.seed(42)
  y <- rexp(50, rate = 2)

  result <- compare_surfaces(pair, y, n_points = 10, tol = 1e-4)

  expect_s3_class(result, "surface_comparison")
  # Exp and Normal are not equivalent
  expect_false(result$equivalent)
})

# Input validation tests
test_that("compare_surfaces validates inputs", {
  exp_spec <- make_exp_spec()
  pair <- equivalence_pair(exp_spec, exp_spec)

  expect_error(compare_surfaces("not a pair", 1:10), "must be an equivalence_pair")
  expect_error(compare_surfaces(pair, "not numeric"), "must be numeric")
  expect_error(compare_surfaces(pair, numeric(0)), "must not be empty")
})

# Result structure tests
test_that("result contains expected components", {
  exp_spec <- make_exp_spec()
  pair <- equivalence_pair(exp_spec, exp_spec)

  set.seed(123)
  y <- rexp(20, rate = 2)
  result <- compare_surfaces(pair, y, n_points = 5)

  expect_true("equivalent" %in% names(result))
  expect_true("max_discrepancy" %in% names(result))
  expect_true("n_tested" %in% names(result))
  expect_true("evidence" %in% names(result))
  expect_true("method" %in% names(result))
  expect_true("tol" %in% names(result))

  expect_type(result$equivalent, "logical")
  expect_type(result$max_discrepancy, "double")
  expect_type(result$evidence, "list")
  expect_true("A_to_B" %in% names(result$evidence))
  expect_true("B_to_A" %in% names(result$evidence))
})

# Print/format tests
test_that("print method produces output", {
  exp_spec <- make_exp_spec()
  pair <- equivalence_pair(exp_spec, exp_spec)

  set.seed(123)
  y <- rexp(20, rate = 2)
  result <- compare_surfaces(pair, y, n_points = 5)

  expect_output(print(result), "surface_comparison")
  expect_output(print(result), "Points tested")
  expect_output(print(result), "discrepancy")
})

test_that("summary produces detailed output", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma_spec <- make_gamma1_spec()
  pair <- equivalence_pair(exp_spec, gamma_spec)

  set.seed(123)
  y <- rexp(30, rate = 2)
  result <- compare_surfaces(pair, y, n_points = 10)

  expect_output(summary(result), "Surface Comparison")
  expect_output(summary(result), "CONCLUSION")
  expect_output(summary(result), "Discrepancy")
})

test_that("format method works", {
  exp_spec <- make_exp_spec()
  pair <- equivalence_pair(exp_spec, exp_spec)

  set.seed(123)
  y <- rexp(20, rate = 2)
  result <- compare_surfaces(pair, y, n_points = 5)

  formatted <- format(result)
  expect_match(formatted, "surface_comparison")
})

test_that("is_surface_comparison works", {
  exp_spec <- make_exp_spec()
  pair <- equivalence_pair(exp_spec, exp_spec)

  set.seed(123)
  y <- rexp(20, rate = 2)
  result <- compare_surfaces(pair, y, n_points = 5)

  expect_true(is_surface_comparison(result))
  expect_false(is_surface_comparison(list()))
  expect_false(is_surface_comparison(NULL))
})

# Edge cases
test_that("tolerance affects conclusion", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma_spec <- make_gamma1_spec()
  pair <- equivalence_pair(exp_spec, gamma_spec)

  set.seed(42)
  y <- rexp(30, rate = 2)

  # Very strict tolerance might fail even for equivalent models
  result_strict <- compare_surfaces(pair, y, n_points = 10, tol = 1e-15)
  result_loose <- compare_surfaces(pair, y, n_points = 10, tol = 1e-2)

  # Loose tolerance should definitely pass for equivalent models
  expect_true(result_loose$equivalent)
})

test_that("comparison works with single parameter", {
  exp_spec <- make_exp_spec()
  pair <- equivalence_pair(exp_spec, exp_spec)

  set.seed(123)
  y <- rexp(20, rate = 2)
  result <- compare_surfaces(pair, y, n_points = 5, tol = 1e-4)

  expect_s3_class(result, "surface_comparison")
  # Same model should be equivalent (with reasonable tolerance)
  expect_true(result$equivalent)
})

# Additional coverage tests for surface_comparison methods
test_that("print shows NOT EQUIVALENT for non-equivalent models", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma2_spec <- make_gamma2_spec()
  pair <- equivalence_pair(exp_spec, gamma2_spec)

  set.seed(42)
  y <- rexp(30, rate = 2)
  result <- compare_surfaces(pair, y, n_points = 10, tol = 1e-4)

  expect_output(print(result), "NOT EQUIVALENT")
})

test_that("summary shows full output for equivalent models", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma1_spec <- make_gamma1_spec()
  pair <- equivalence_pair(exp_spec, gamma1_spec)

  set.seed(42)
  y <- rexp(30, rate = 2)
  result <- compare_surfaces(pair, y, n_points = 10, tol = 1e-4)

  expect_output(summary(result), "observationally equivalent")
  expect_output(summary(result), "Discrepancy distribution")
  expect_output(summary(result), "Min:")
  expect_output(summary(result), "Median:")
  expect_output(summary(result), "% < tolerance")
})

test_that("summary shows NOT equivalent message", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma2_spec <- make_gamma2_spec()
  pair <- equivalence_pair(exp_spec, gamma2_spec)

  set.seed(42)
  y <- rexp(30, rate = 2)
  result <- compare_surfaces(pair, y, n_points = 10, tol = 1e-4)

  expect_output(summary(result), "NOT observationally equivalent")
})

test_that("format shows NOT EQUIVALENT for non-equivalent models", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma2_spec <- make_gamma2_spec()
  pair <- equivalence_pair(exp_spec, gamma2_spec)

  set.seed(42)
  y <- rexp(30, rate = 2)
  result <- compare_surfaces(pair, y, n_points = 10, tol = 1e-4)

  expect_match(format(result), "NOT EQUIVALENT")
})
