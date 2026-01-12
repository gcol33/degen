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

# Core functionality tests
test_that("equivalence_classes creates valid object", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma1_spec <- make_gamma1_spec()

  models <- list(exp = exp_spec, gamma1 = gamma1_spec)

  set.seed(42)
  y <- rexp(30, rate = 2)

  result <- equivalence_classes(models, y, n_points = 10, tol = 1e-4)

  expect_s3_class(result, "equiv_classes")
  expect_true("classes" %in% names(result))
  expect_true("membership" %in% names(result))
  expect_true("pairwise" %in% names(result))
})

test_that("equivalent models are grouped together", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma1_spec <- make_gamma1_spec()

  models <- list(exp = exp_spec, gamma1 = gamma1_spec)

  set.seed(42)
  y <- rexp(30, rate = 2)

  result <- equivalence_classes(models, y, n_points = 10, tol = 1e-4)

  # Exp and Gamma(1) should be in same class
  expect_equal(result$n_classes, 1)
  expect_true(are_equivalent(result, "exp", "gamma1"))
})

test_that("non-equivalent models are in different classes", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma2_spec <- make_gamma2_spec()

  models <- list(exp = exp_spec, gamma2 = gamma2_spec)

  set.seed(42)
  y <- rexp(30, rate = 2)

  result <- equivalence_classes(models, y, n_points = 10, tol = 1e-4)

  # Exp and Gamma(2) should be in different classes
  expect_equal(result$n_classes, 2)
  expect_false(are_equivalent(result, "exp", "gamma2"))
})

test_that("three models with two equivalent grouped correctly", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma1_spec <- make_gamma1_spec()
  gamma2_spec <- make_gamma2_spec()

  models <- list(exp = exp_spec, gamma1 = gamma1_spec, gamma2 = gamma2_spec)

  set.seed(42)
  y <- rexp(30, rate = 2)

  result <- equivalence_classes(models, y, n_points = 10, tol = 1e-4)

  # Should have 2 classes: {exp, gamma1} and {gamma2}
  expect_equal(result$n_classes, 2)
  expect_true(are_equivalent(result, "exp", "gamma1"))
  expect_false(are_equivalent(result, "exp", "gamma2"))
})

# Input validation tests
test_that("equivalence_classes validates inputs", {
  exp_spec <- make_exp_spec()
  y <- rexp(20, rate = 2)

  expect_error(equivalence_classes("not a list", y), "must be a list")
  expect_error(equivalence_classes(list(exp_spec), y), "at least 2")
  expect_error(equivalence_classes(list("not spec", exp_spec), y), "not a model_spec")
})

test_that("equivalence_classes generates names if missing", {
  exp_spec <- make_exp_spec()
  gamma1_spec <- make_gamma1_spec()

  models <- list(exp_spec, gamma1_spec)  # unnamed

  set.seed(42)
  y <- rexp(20, rate = 2)

  result <- equivalence_classes(models, y, n_points = 5, tol = 1e-3)

  expect_true(all(c("model_1", "model_2") %in% result$model_names))
})

# Accessor tests
test_that("n_classes returns correct count", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma1_spec <- make_gamma1_spec()

  models <- list(exp = exp_spec, gamma1 = gamma1_spec)

  set.seed(42)
  y <- rexp(30, rate = 2)

  result <- equivalence_classes(models, y, n_points = 10, tol = 1e-4)

  expect_equal(n_classes(result), 1)
})

test_that("n_classes validates input", {
  expect_error(n_classes(list()), "must be an equiv_classes")
})

test_that("class_members returns correct members", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma1_spec <- make_gamma1_spec()

  models <- list(exp = exp_spec, gamma1 = gamma1_spec)

  set.seed(42)
  y <- rexp(30, rate = 2)

  result <- equivalence_classes(models, y, n_points = 10, tol = 1e-4)

  members <- class_members(result, 1)
  expect_true("exp" %in% members)
  expect_true("gamma1" %in% members)
})

test_that("class_members validates inputs", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma1_spec <- make_gamma1_spec()

  models <- list(exp = exp_spec, gamma1 = gamma1_spec)

  set.seed(42)
  y <- rexp(20, rate = 2)

  result <- equivalence_classes(models, y, n_points = 5, tol = 1e-3)

  expect_error(class_members(list(), 1), "must be an equiv_classes")
  expect_error(class_members(result, 0), "must be between")
  expect_error(class_members(result, 10), "must be between")
})

test_that("are_equivalent validates inputs", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma1_spec <- make_gamma1_spec()

  models <- list(exp = exp_spec, gamma1 = gamma1_spec)

  set.seed(42)
  y <- rexp(20, rate = 2)

  result <- equivalence_classes(models, y, n_points = 5, tol = 1e-3)

  expect_error(are_equivalent(list(), "exp", "gamma1"), "must be an equiv_classes")
  expect_error(are_equivalent(result, "invalid", "gamma1"), "not found")
  expect_error(are_equivalent(result, "exp", "invalid"), "not found")
})

# Type predicate test
test_that("is_equiv_classes works", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma1_spec <- make_gamma1_spec()

  models <- list(exp = exp_spec, gamma1 = gamma1_spec)

  set.seed(42)
  y <- rexp(20, rate = 2)

  result <- equivalence_classes(models, y, n_points = 5, tol = 1e-3)

  expect_true(is_equiv_classes(result))
  expect_false(is_equiv_classes(list()))
  expect_false(is_equiv_classes(NULL))
})

# Print/format tests
test_that("print method produces output", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma1_spec <- make_gamma1_spec()

  models <- list(exp = exp_spec, gamma1 = gamma1_spec)

  set.seed(42)
  y <- rexp(20, rate = 2)

  result <- equivalence_classes(models, y, n_points = 5, tol = 1e-3)

  expect_output(print(result), "equiv_classes")
  expect_output(print(result), "Class")
})

test_that("summary produces detailed output", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma1_spec <- make_gamma1_spec()

  models <- list(exp = exp_spec, gamma1 = gamma1_spec)

  set.seed(42)
  y <- rexp(20, rate = 2)

  result <- equivalence_classes(models, y, n_points = 5, tol = 1e-3)

  expect_output(summary(result), "Equivalence Class Analysis")
  expect_output(summary(result), "Pairwise")
})

test_that("format method works", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma1_spec <- make_gamma1_spec()

  models <- list(exp = exp_spec, gamma1 = gamma1_spec)

  set.seed(42)
  y <- rexp(20, rate = 2)

  result <- equivalence_classes(models, y, n_points = 5, tol = 1e-3)

  formatted <- format(result)
  expect_match(formatted, "equiv_classes")
  expect_match(formatted, "2 models")
})
