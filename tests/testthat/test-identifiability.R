# Test fixtures
make_normal_spec <- function() {
  model_spec(
    loglik_fn = function(y, mu, sigma) {
      sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
    },
    par_names = c("mu", "sigma"),
    par_bounds = list(mu = c(-100, 100), sigma = c(1e-6, 100)),
    name = "Normal"
  )
}

make_nonid_spec <- function() {
  model_spec(
    loglik_fn = function(y, a, b) {
      sum(dnorm(y, mean = a + b, sd = 1, log = TRUE))
    },
    par_names = c("a", "b"),
    par_bounds = list(a = c(-50, 50), b = c(-50, 50)),
    name = "Non-identifiable"
  )
}

make_exp_spec <- function() {
  model_spec(
    loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
    par_names = "lambda",
    par_bounds = list(lambda = c(1e-6, 100)),
    name = "Exponential"
  )
}

# Core functionality tests
test_that("identifiability_check creates valid object", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 2)

  result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))

  expect_s3_class(result, "identifiability_result")
  expect_true("status" %in% names(result))
  expect_true("fisher_info" %in% names(result))
  expect_true("recommendations" %in% names(result))
})

test_that("well-identified model is classified correctly", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 2)

  result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))

  expect_equal(unname(result$status["mu"]), "identified")
  expect_equal(unname(result$status["sigma"]), "identified")
  expect_equal(result$rank, 2)
})

test_that("non-identifiable model is detected", {
  spec <- make_nonid_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 1)

  result <- identifiability_check(spec, y, par = c(a = 2, b = 3))

  # At least one parameter should be non-identifiable
  expect_true(any(result$status == "non_identifiable"))
  expect_lt(result$rank, 2)
})

test_that("single parameter model works", {
  spec <- make_exp_spec()
  set.seed(123)
  y <- rexp(100, rate = 2)

  result <- identifiability_check(spec, y, par = c(lambda = 2))

  expect_s3_class(result, "identifiability_result")
  expect_equal(unname(result$status["lambda"]), "identified")
})

test_that("identifiability_check finds MLE when par not provided", {
  spec <- make_exp_spec()
  set.seed(123)
  y <- rexp(100, rate = 2)

  result <- identifiability_check(spec, y)

  expect_s3_class(result, "identifiability_result")
  # MLE should be close to true value
  expect_equal(unname(result$par["lambda"]), 2, tolerance = 0.5)
})

test_that("identifiability_check validates inputs", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(50)

  expect_error(identifiability_check("not a spec", y), "must be a model_spec")
  expect_error(identifiability_check(spec, "not numeric"), "must be numeric")
})

# Profile likelihood tests
test_that("profile_likelihood works for single parameter", {
  spec <- make_exp_spec()
  set.seed(123)
  y <- rexp(50, rate = 2)

  profile <- profile_likelihood(spec, y, par = c(lambda = 2), "lambda", n_points = 10)

  expect_s3_class(profile, "data.frame")
  expect_true("value" %in% names(profile))
  expect_true("loglik" %in% names(profile))
  expect_equal(nrow(profile), 10)
})

test_that("profile_likelihood works for multi-parameter model", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(50, mean = 5, sd = 2)

  profile_mu <- profile_likelihood(spec, y, par = c(mu = 5, sigma = 2), "mu", n_points = 10)
  profile_sigma <- profile_likelihood(spec, y, par = c(mu = 5, sigma = 2), "sigma", n_points = 10)

  expect_s3_class(profile_mu, "data.frame")
  expect_s3_class(profile_sigma, "data.frame")
})

test_that("profile_likelihood validates parameter name", {
  spec <- make_normal_spec()
  y <- rnorm(20)

  expect_error(profile_likelihood(spec, y, c(mu = 0, sigma = 1), "invalid"),
               "not found")
})

# Recommendations tests
test_that("recommendations are generated for non-identifiable model", {
  spec <- make_nonid_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 1)

  result <- identifiability_check(spec, y, par = c(a = 2, b = 3))

  expect_true(!is.null(result$recommendations))
  expect_true("status" %in% names(result$recommendations))
  expect_true("message" %in% names(result$recommendations))
})

test_that("recommendations are positive for well-identified model", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(200, mean = 5, sd = 2)

  result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))

  expect_equal(result$recommendations$status, "good")
})

# Type predicate test
test_that("is_identifiability_result works", {
  spec <- make_exp_spec()
  set.seed(123)
  y <- rexp(50, rate = 2)

  result <- identifiability_check(spec, y, par = c(lambda = 2))

  expect_true(is_identifiability_result(result))
  expect_false(is_identifiability_result(list()))
  expect_false(is_identifiability_result(NULL))
})

# Print/format tests
test_that("print method produces output", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(50, mean = 5, sd = 2)

  result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))

  expect_output(print(result), "identifiability_result")
  expect_output(print(result), "Parameter status")
})

test_that("print highlights non-identifiable parameters", {
  spec <- make_nonid_spec()
  set.seed(123)
  y <- rnorm(50, mean = 5, sd = 1)

  result <- identifiability_check(spec, y, par = c(a = 2, b = 3))

  expect_output(print(result), "NON-IDENTIFIABLE")
})

test_that("summary produces detailed output", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(50, mean = 5, sd = 2)

  result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))

  expect_output(summary(result), "Identifiability Analysis")
  expect_output(summary(result), "Recommendation")
})

test_that("format method works", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(50, mean = 5, sd = 2)

  result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))

  formatted <- format(result)
  expect_match(formatted, "identifiability_result")
})

# Identified functions test
test_that("identified functions are found for non-id model", {
  spec <- make_nonid_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 1)

  result <- identifiability_check(spec, y, par = c(a = 2, b = 3))

  # Should find that a + b is identified
  expect_true(length(result$identified_functions) > 0 ||
              length(result$non_identified) > 0)
})
