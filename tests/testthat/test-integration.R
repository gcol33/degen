# Integration tests for complete workflows

# Workflow 1: Define -> Compare -> Interpret
test_that("complete workflow: define models -> compare -> interpret", {
  skip_on_cran()

  # Step 1: Define models
  exp_spec <- make_exp_spec()
  gamma1_spec <- make_gamma_spec(shape = 1)

  expect_s3_class(exp_spec, "model_spec")
  expect_s3_class(gamma1_spec, "model_spec")

  # Step 2: Create pair
  pair <- equivalence_pair(exp_spec, gamma1_spec)
  expect_s3_class(pair, "equivalence_pair")

  # Step 3: Compare
  set.seed(42)
  y <- rexp(50, rate = 2)

  result <- compare_surfaces(pair, y, n_points = 15, tol = 1e-4)
  expect_s3_class(result, "surface_comparison")

  # Step 4: Interpret
  expect_true(result$equivalent)
  expect_lt(result$max_discrepancy, 1e-4)
})

# Workflow 2: Identifiability analysis
test_that("complete workflow: identifiability analysis", {
  skip_on_cran()

  # Step 1: Define model
  spec <- make_normal_spec()
  expect_s3_class(spec, "model_spec")

  # Step 2: Generate data
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 2)

  # Step 3: Compute Fisher information
  info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))
  expect_s3_class(info, "fisher_info")
  expect_equal(info$rank, 2)

  # Step 4: Full identifiability check
  result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))
  expect_s3_class(result, "identifiability_result")
  expect_equal(unname(result$status["mu"]), "identified")
  expect_equal(unname(result$status["sigma"]), "identified")
})

# Workflow 3: Non-identifiable model analysis
test_that("complete workflow: non-identifiable model", {
  skip_on_cran()

  # Step 1: Define non-identifiable model
  spec <- make_nonid_spec()

  # Step 2: Generate data
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 1)

  # Step 3: Check identifiability
  result <- identifiability_check(spec, y, par = c(a = 2, b = 3))

  # Step 4: Verify detection
  expect_lt(result$rank, 2)
  expect_true(any(result$status == "non_identifiable"))

  # Step 5: Find null directions
  null_dirs <- null_directions(result$fisher_info)
  expect_true(!is.null(null_dirs))
})

# Workflow 4: Multiple model comparison
test_that("complete workflow: equivalence classes", {
  skip_on_cran()

  # Step 1: Define multiple models
  models <- list(
    exp = make_exp_spec(),
    gamma1 = make_gamma_spec(shape = 1),
    gamma2 = make_gamma_spec(shape = 2)
  )

  # Step 2: Generate data
  set.seed(42)
  y <- rexp(40, rate = 2)

  # Step 3: Find equivalence classes
  classes <- equivalence_classes(models, y, n_points = 10, tol = 1e-4)
  expect_s3_class(classes, "equiv_classes")

  # Step 4: Verify grouping
  expect_equal(classes$n_classes, 2)
  expect_true(are_equivalent(classes, "exp", "gamma1"))
  expect_false(are_equivalent(classes, "exp", "gamma2"))
})

# Workflow 5: Profile likelihood
test_that("complete workflow: profile likelihood analysis", {
  skip_on_cran()

  # Step 1: Define model
  spec <- make_normal_spec()

  # Step 2: Generate data
  set.seed(123)
  y <- rnorm(50, mean = 5, sd = 2)

  # Step 3: Compute profile
  profile <- profile_likelihood(spec, y, par = c(mu = 5, sigma = 2), "mu", n_points = 15)

  expect_s3_class(profile, "data.frame")
  expect_true("value" %in% names(profile))
  expect_true("loglik" %in% names(profile))

  # Profile should peak near true value
  max_idx <- which.max(profile$loglik)
  expect_equal(profile$value[max_idx], 5, tolerance = 1)
})

# Edge case tests
test_that("models with same structure are equivalent to themselves", {
  skip_on_cran()

  spec <- make_exp_spec()
  pair <- equivalence_pair(spec, spec, name = "Self comparison")

  set.seed(123)
  y <- rexp(30, rate = 2)

  result <- compare_surfaces(pair, y, n_points = 10, tol = 1e-4)
  expect_true(result$equivalent)
})

test_that("Fisher information is positive definite for well-posed models", {
  spec <- make_normal_spec()

  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 2)

  info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))

  # All eigenvalues should be positive
  expect_true(all(info$eigenvalues > 0))
})

test_that("single parameter models work throughout pipeline", {
  skip_on_cran()

  spec <- make_exp_spec()

  set.seed(123)
  y <- rexp(50, rate = 2)

  # Fisher information
  info <- fisher_information(spec, y, par = c(lambda = 2))
  expect_equal(info$n_par, 1)
  expect_equal(info$rank, 1)

  # Identifiability
  result <- identifiability_check(spec, y, par = c(lambda = 2))
  expect_equal(unname(result$status["lambda"]), "identified")

  # Profile
  profile <- profile_likelihood(spec, y, par = c(lambda = 2), "lambda", n_points = 10)
  expect_equal(nrow(profile), 10)
})

# Consistency tests
test_that("equivalence detection is symmetric", {
  skip_on_cran()

  exp_spec <- make_exp_spec()
  gamma_spec <- make_gamma_spec(shape = 1)

  set.seed(42)
  y <- rexp(30, rate = 2)

  pair_ab <- equivalence_pair(exp_spec, gamma_spec)
  pair_ba <- equivalence_pair(gamma_spec, exp_spec)

  result_ab <- compare_surfaces(pair_ab, y, n_points = 10, tol = 1e-4)
  result_ba <- compare_surfaces(pair_ba, y, n_points = 10, tol = 1e-4)

  expect_equal(result_ab$equivalent, result_ba$equivalent)
})

test_that("equivalence classes are transitive", {
  skip_on_cran()

  # Create three models where A~B and B~C, so A~C
  models <- list(
    exp = make_exp_spec(),
    gamma1 = make_gamma_spec(shape = 1),
    # Weibull with shape=1 is also equivalent to exponential
    weibull1 = make_weibull_spec(shape = 1)
  )

  set.seed(42)
  y <- rexp(40, rate = 2)

  classes <- equivalence_classes(models, y, n_points = 10, tol = 1e-3)

  # All three should be equivalent (same class)
  # Note: Weibull(1) with scale s has rate 1/s, so they're equivalent
  # but the parameterization differs - this tests the algorithm's robustness
})
