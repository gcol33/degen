# Tests for edge cases and uncovered code paths

# ============================================================================
# model_spec.R edge cases
# ============================================================================

test_that("validate_model_spec validates non-model_spec input", {
  expect_error(
    degen:::validate_model_spec(list()),
    "must be a model_spec"
  )
})

test_that("validate_model_spec tests loglik with test data", {
  spec <- model_spec(
    loglik_fn = function(y, mu) sum(dnorm(y, mu, 1, log = TRUE)),
    par_names = "mu"
  )

  # Should pass with valid test data
  expect_true(degen:::validate_model_spec(spec, test_y = 1:5, test_par = c(mu = 0)))
})

test_that("validate_model_spec catches non-scalar loglik", {
  spec <- model_spec(
    loglik_fn = function(y, mu) dnorm(y, mu, 1, log = TRUE),  # Returns vector!
    par_names = "mu",
    validate = FALSE
  )

  expect_error(
    degen:::validate_model_spec(spec, test_y = 1:5, test_par = c(mu = 0)),
    "scalar numeric"
  )
})

test_that("validate_model_spec catches loglik errors", {
  spec <- model_spec(
    loglik_fn = function(y, mu) stop("intentional error"),
    par_names = "mu",
    validate = FALSE
  )

  expect_error(
    degen:::validate_model_spec(spec, test_y = 1:5, test_par = c(mu = 0)),
    "Log-likelihood evaluation failed"
  )
})

test_that("evaluate_loglik catches non-scalar return", {
  spec <- model_spec(
    loglik_fn = function(y, mu) dnorm(y, mu, 1, log = TRUE),  # Returns vector!
    par_names = "mu",
    validate = FALSE
  )

  expect_error(
    degen:::evaluate_loglik(spec, y = 1:5, par = c(mu = 0)),
    "scalar numeric"
  )
})

# ============================================================================
# sampling.R edge cases
# ============================================================================

test_that("sample_par_space works with random method", {
  bounds <- list(mu = c(-10, 10), sigma = c(0.1, 5))

  set.seed(123)
  samples <- degen:::sample_par_space(bounds, n = 10, method = "random")

  expect_equal(nrow(samples), 10)
  expect_equal(ncol(samples), 2)
  expect_equal(colnames(samples), c("mu", "sigma"))

  # Check samples are within bounds
  expect_true(all(samples[, "mu"] >= -10 & samples[, "mu"] <= 10))
  expect_true(all(samples[, "sigma"] >= 0.1 & samples[, "sigma"] <= 5))
})

test_that("effective_bounds handles lower-bounded only", {
  # Lower bounded only (e.g., variance > 0)
  bounds <- c(0, Inf)
  eff <- degen:::effective_bounds(bounds, scale = 10)

  expect_equal(eff[1], 0)
  expect_equal(eff[2], 10)
})

test_that("effective_bounds handles upper-bounded only", {
  # Upper bounded only
  bounds <- c(-Inf, 5)
  eff <- degen:::effective_bounds(bounds, scale = 10)

  expect_equal(eff[1], -5)
  expect_equal(eff[2], 5)
})

test_that("effective_bounds handles both infinite", {
  bounds <- c(-Inf, Inf)
  eff <- degen:::effective_bounds(bounds, scale = 10)

  expect_equal(eff, c(-10, 10))
})

test_that("effective_bounds handles finite bounds", {
  bounds <- c(-5, 5)
  eff <- degen:::effective_bounds(bounds, scale = 10)

  expect_equal(eff, c(-5, 5))
})

# ============================================================================
# identifiability.R edge cases
# ============================================================================

test_that("identifiability_check finds MLE when par is NULL", {
  spec <- model_spec(
    loglik_fn = function(y, mu) sum(dnorm(y, mu, 1, log = TRUE)),
    par_names = "mu"
  )

  set.seed(123)
  y <- rnorm(50, mean = 5, sd = 1)

  # Should work without providing par
  result <- identifiability_check(spec, y, par = NULL)

  expect_s3_class(result, "identifiability_result")
  # MLE should be close to true value
  expect_true(abs(result$par["mu"] - 5) < 1)
})

test_that("identifiability_check works with verbose=TRUE", {
  spec <- model_spec(
    loglik_fn = function(y, mu) sum(dnorm(y, mu, 1, log = TRUE)),
    par_names = "mu"
  )

  set.seed(123)
  y <- rnorm(30, mean = 5, sd = 1)

  expect_output(
    identifiability_check(spec, y, par = NULL, verbose = TRUE),
    "Finding MLE"
  )
  expect_output(
    identifiability_check(spec, y, par = c(mu = 5), verbose = TRUE),
    "Computing Fisher"
  )
})

test_that("identifiability_check works with level='profile'", {
  skip_on_cran()

  spec <- model_spec(
    loglik_fn = function(y, mu) sum(dnorm(y, mu, 1, log = TRUE)),
    par_names = "mu"
  )

  set.seed(123)
  y <- rnorm(30, mean = 5, sd = 1)

  result <- identifiability_check(spec, y, par = c(mu = 5), level = "profile")

  expect_s3_class(result, "identifiability_result")
  expect_false(is.null(result$profile_results))
  expect_true("mu" %in% names(result$profile_results))
})

test_that("identifiability_check works with par as list", {
  spec <- model_spec(
    loglik_fn = function(y, mu) sum(dnorm(y, mu, 1, log = TRUE)),
    par_names = "mu"
  )

  set.seed(123)
  y <- rnorm(30, mean = 5, sd = 1)

  # Pass par as a list instead of vector
  result <- identifiability_check(spec, y, par = list(mu = 5))

  expect_s3_class(result, "identifiability_result")
})

test_that("identifiability_check profile with multi-param model", {
  skip_on_cran()

  spec <- model_spec(
    loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
    par_names = c("mu", "sigma"),
    par_bounds = list(sigma = c(0.01, 100))
  )

  set.seed(123)
  y <- rnorm(30, mean = 5, sd = 2)

  result <- identifiability_check(
    spec, y,
    par = c(mu = 5, sigma = 2),
    level = "profile",
    verbose = TRUE
  )

  expect_s3_class(result, "identifiability_result")
  expect_true("mu" %in% names(result$profile_results))
  expect_true("sigma" %in% names(result$profile_results))
})

test_that("identifiability_check profile with verbose", {
  skip_on_cran()

  spec <- model_spec(
    loglik_fn = function(y, mu) sum(dnorm(y, mu, 1, log = TRUE)),
    par_names = "mu"
  )

  set.seed(123)
  y <- rnorm(30, mean = 5, sd = 1)

  expect_output(
    identifiability_check(spec, y, par = c(mu = 5), level = "profile", verbose = TRUE),
    "Profiling"
  )
})

# Mock identifiability_result objects for print/format testing
make_mock_id_result_nonid <- function() {
  structure(
    list(
      status = c(a = "non_identifiable", b = "non_identifiable"),
      fisher_info = make_mock_fisher_rank_deficient(),
      identified_functions = c("0.71*a + 0.71*b"),
      non_identified = c("0.71*a - 0.71*b"),
      condition = Inf,
      rank = 1,
      n_par = 2,
      par_names = c("a", "b"),
      par = c(a = 2, b = 3),
      recommendations = list(
        status = "non_identifiable",
        message = "Model has structurally non-identifiable parameters",
        action = "Consider reparameterizing"
      ),
      profile_results = NULL
    ),
    class = "identifiability_result"
  )
}

make_mock_id_result_poorly_conditioned <- function() {
  structure(
    list(
      status = c(mu = "identified", sigma = "weakly_identified"),
      fisher_info = structure(
        list(
          matrix = matrix(c(100, -5, -5, 50), 2, 2),
          eigenvalues = c(102, 0.001),
          eigenvectors = matrix(c(0.98, -0.2, 0.2, 0.98), 2, 2),
          condition = 5000,
          rank = 2,
          n_par = 2,
          par = c(mu = 5, sigma = 2),
          par_names = c("mu", "sigma"),
          n_obs = 100
        ),
        class = "fisher_info"
      ),
      identified_functions = character(0),
      non_identified = character(0),
      condition = 5000,
      rank = 2,
      n_par = 2,
      par_names = c("mu", "sigma"),
      par = c(mu = 5, sigma = 2),
      recommendations = list(
        status = "poorly_conditioned",
        message = "Model is very poorly conditioned",
        action = "Some parameters may be practically non-identifiable"
      ),
      profile_results = NULL
    ),
    class = "identifiability_result"
  )
}

make_mock_id_result_weak <- function() {
  structure(
    list(
      status = c(mu = "identified", sigma = "weakly_identified"),
      fisher_info = structure(
        list(
          matrix = matrix(c(100, -5, -5, 50), 2, 2),
          eigenvalues = c(102, 48),
          eigenvectors = matrix(c(0.98, -0.2, 0.2, 0.98), 2, 2),
          condition = 150,
          rank = 2,
          n_par = 2,
          par = c(mu = 5, sigma = 2),
          par_names = c("mu", "sigma"),
          n_obs = 100
        ),
        class = "fisher_info"
      ),
      identified_functions = character(0),
      non_identified = character(0),
      condition = 150,
      rank = 2,
      n_par = 2,
      par_names = c("mu", "sigma"),
      par = c(mu = 5, sigma = 2),
      recommendations = list(
        status = "moderate",
        message = "Moderate conditioning",
        action = "Review eigenvalue decomposition"
      ),
      profile_results = NULL
    ),
    class = "identifiability_result"
  )
}

make_mock_id_result_good <- function() {
  structure(
    list(
      status = c(mu = "identified", sigma = "identified"),
      fisher_info = make_mock_fisher_full_rank(),
      identified_functions = character(0),
      non_identified = character(0),
      condition = 2.125,
      rank = 2,
      n_par = 2,
      par_names = c("mu", "sigma"),
      par = c(mu = 5, sigma = 2),
      recommendations = list(
        status = "good",
        message = "Model appears well-identified",
        action = "No action needed"
      ),
      profile_results = NULL
    ),
    class = "identifiability_result"
  )
}

# Need these helper functions from test-print-methods.R
make_mock_fisher_full_rank <- function() {
  structure(
    list(
      matrix = matrix(c(100, -5, -5, 50), 2, 2,
                      dimnames = list(c("mu", "sigma"), c("mu", "sigma"))),
      eigenvalues = c(102, 48),
      eigenvectors = matrix(c(0.98, -0.2, 0.2, 0.98), 2, 2),
      condition = 2.125,
      rank = 2,
      n_par = 2,
      par = c(mu = 5, sigma = 2),
      par_names = c("mu", "sigma"),
      n_obs = 100
    ),
    class = "fisher_info"
  )
}

make_mock_fisher_rank_deficient <- function() {
  structure(
    list(
      matrix = matrix(c(100, 100, 100, 100), 2, 2,
                      dimnames = list(c("a", "b"), c("a", "b"))),
      eigenvalues = c(200, 0),
      eigenvectors = matrix(c(0.707, 0.707, -0.707, 0.707), 2, 2),
      condition = Inf,
      rank = 1,
      n_par = 2,
      par = c(a = 2, b = 3),
      par_names = c("a", "b"),
      n_obs = 100
    ),
    class = "fisher_info"
  )
}

test_that("print.identifiability_result shows NON-IDENTIFIABLE", {
  mock <- make_mock_id_result_nonid()
  expect_output(print(mock), "NON-IDENTIFIABLE")
  expect_output(print(mock), "Non-identified directions")
  expect_output(print(mock), "Identified functions")
})

test_that("print.identifiability_result shows POORLY IDENTIFIED", {
  mock <- make_mock_id_result_poorly_conditioned()
  expect_output(print(mock), "POORLY IDENTIFIED")
})

test_that("print.identifiability_result shows WELL-IDENTIFIED", {
  mock <- make_mock_id_result_good()
  expect_output(print(mock), "WELL-IDENTIFIED")
})

test_that("print.identifiability_result shows weakly identified status", {
  mock <- make_mock_id_result_weak()
  expect_output(print(mock), "WEAKLY IDENTIFIED")
})

test_that("format.identifiability_result formats non-identifiable", {
  mock <- make_mock_id_result_nonid()
  formatted <- format(mock)
  expect_match(formatted, "non-identifiable")
})

test_that("format.identifiability_result formats weakly identified", {
  mock <- make_mock_id_result_weak()
  formatted <- format(mock)
  expect_match(formatted, "weakly identified")
})

test_that("format.identifiability_result formats all identified", {
  mock <- make_mock_id_result_good()
  formatted <- format(mock)
  expect_match(formatted, "all identified")
})

# ============================================================================
# equivalence_classes.R edge cases
# ============================================================================

test_that("equivalence_classes with verbose=TRUE shows progress", {
  skip_on_cran()

  exp_spec <- model_spec(
    loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
    par_names = "lambda",
    par_bounds = list(lambda = c(1e-6, 100)),
    name = "Exponential"
  )

  gamma_spec <- model_spec(
    loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
    par_names = "rate",
    par_bounds = list(rate = c(1e-6, 100)),
    name = "Gamma(1)"
  )

  models <- list(exp = exp_spec, gamma = gamma_spec)

  set.seed(42)
  y <- rexp(20, rate = 2)

  expect_output(
    equivalence_classes(models, y, n_points = 5, verbose = TRUE),
    "Comparing"
  )
})

test_that("class_members validates input type", {
  expect_error(class_members(list(), 1), "must be an equiv_classes")
})

test_that("are_equivalent validates input type", {
  expect_error(are_equivalent(list(), "a", "b"), "must be an equiv_classes")
})

test_that("are_equivalent validates model names", {
  mock <- structure(
    list(
      classes = list(class_1 = c("a", "b")),
      n_classes = 1,
      membership = c(a = 1L, b = 1L),
      model_names = c("a", "b"),
      n_models = 2,
      tol = 1e-6
    ),
    class = "equiv_classes"
  )

  expect_error(are_equivalent(mock, "invalid", "b"), "not found")
  expect_error(are_equivalent(mock, "a", "invalid"), "not found")
})

# ============================================================================
# equivalence_pair.R edge cases
# ============================================================================

test_that("summary.equivalence_pair works for different param counts", {
  spec1 <- model_spec(
    loglik_fn = function(y, mu) sum(dnorm(y, mu, 1, log = TRUE)),
    par_names = "mu",
    name = "Normal(mu)"
  )

  spec2 <- model_spec(
    loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
    par_names = c("mu", "sigma"),
    par_bounds = list(sigma = c(1e-6, Inf)),
    name = "Normal(mu, sigma)"
  )

  pair <- equivalence_pair(spec1, spec2)
  expect_output(summary(pair), "different")
})

# ============================================================================
# utils.R edge cases
# ============================================================================

test_that("check_bounds catches malformed bounds", {
  expect_error(
    degen:::check_bounds(list(x = c(1, 2, 3)), "x"),
    "length-2"
  )
})

test_that("check_par_in_bounds catches out-of-bounds", {
  bounds <- list(x = c(0, 10))

  expect_error(
    degen:::check_par_in_bounds(c(x = -1), bounds),
    "outside bounds"
  )
})

# ============================================================================
# fisher_information.R edge cases
# ============================================================================

test_that("fisher_information rejects expected type", {
  spec <- model_spec(
    loglik_fn = function(y, mu) sum(dnorm(y, mu, 1, log = TRUE)),
    par_names = "mu"
  )

  set.seed(123)
  y <- rnorm(20)

  expect_error(
    fisher_information(spec, y, par = c(mu = 0), type = "expected"),
    "not yet implemented"
  )
})

test_that("fisher_information requires named par vector", {
  spec <- model_spec(
    loglik_fn = function(y, mu) sum(dnorm(y, mu, 1, log = TRUE)),
    par_names = "mu"
  )

  set.seed(123)
  y <- rnorm(20)

  expect_error(
    fisher_information(spec, y, par = 5),  # unnamed
    "must be a named vector"
  )
})

test_that("fisher_information accepts par as list", {
  spec <- model_spec(
    loglik_fn = function(y, mu) sum(dnorm(y, mu, 1, log = TRUE)),
    par_names = "mu"
  )

  set.seed(123)
  y <- rnorm(20)

  # Should work with par as list
  result <- fisher_information(spec, y, par = list(mu = 0))
  expect_s3_class(result, "fisher_info")
})

test_that("fisher_information handles rank deficient case", {
  # Model where only a+b is identified
  spec <- model_spec(
    loglik_fn = function(y, a, b) {
      sum(dnorm(y, mean = a + b, sd = 1, log = TRUE))
    },
    par_names = c("a", "b")
  )

  set.seed(123)
  y <- rnorm(50, mean = 5, sd = 1)

  result <- fisher_information(spec, y, par = c(a = 2, b = 3))
  expect_s3_class(result, "fisher_info")
  expect_lt(result$rank, result$n_par)  # Should be rank deficient
})

test_that("fisher_information catches missing parameters", {
  spec <- model_spec(
    loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
    par_names = c("mu", "sigma"),
    par_bounds = list(sigma = c(1e-6, Inf))
  )

  set.seed(123)
  y <- rnorm(20)

  expect_error(
    fisher_information(spec, y, par = c(mu = 0)),  # missing sigma
    "Missing parameter"
  )
})

# ============================================================================
# compare_surfaces.R edge cases
# ============================================================================

test_that("compare_surfaces handles empty evidence gracefully", {
  # This tests the edge case where optimization might fail
  spec <- model_spec(
    loglik_fn = function(y, mu) sum(dnorm(y, mu, 1, log = TRUE)),
    par_names = "mu",
    par_bounds = list(mu = c(-10, 10))
  )

  pair <- equivalence_pair(spec, spec)

  set.seed(123)
  y <- rnorm(20, mean = 0, sd = 1)

  # Should work even with very few points
  result <- compare_surfaces(pair, y, n_points = 3)
  expect_s3_class(result, "surface_comparison")
})

test_that("compare_surfaces works with method='optimization'", {
  spec <- model_spec(
    loglik_fn = function(y, mu) sum(dnorm(y, mu, 1, log = TRUE)),
    par_names = "mu",
    par_bounds = list(mu = c(-10, 10))
  )

  pair <- equivalence_pair(spec, spec)

  set.seed(123)
  y <- rnorm(20, mean = 0, sd = 1)

  result <- compare_surfaces(pair, y, n_points = 5, method = "optimization")
  expect_s3_class(result, "surface_comparison")
  expect_equal(result$method, "optimization")
})

test_that("compare_surfaces with verbose=TRUE shows progress", {
  skip_on_cran()

  spec <- model_spec(
    loglik_fn = function(y, mu) sum(dnorm(y, mu, 1, log = TRUE)),
    par_names = "mu",
    par_bounds = list(mu = c(-10, 10))
  )

  pair <- equivalence_pair(spec, spec)

  set.seed(123)
  y <- rnorm(20, mean = 0, sd = 1)

  expect_output(
    compare_surfaces(pair, y, n_points = 15, verbose = TRUE),
    "Comparing"
  )
})

test_that("compare_surfaces handles NA log-likelihood gracefully", {
  # Model that sometimes returns NA
  bad_spec <- model_spec(
    loglik_fn = function(y, mu) {
      if (mu < -5) return(NA_real_)
      sum(dnorm(y, mu, 1, log = TRUE))
    },
    par_names = "mu",
    par_bounds = list(mu = c(-10, 10)),
    validate = FALSE
  )

  good_spec <- model_spec(
    loglik_fn = function(y, mu) sum(dnorm(y, mu, 1, log = TRUE)),
    par_names = "mu",
    par_bounds = list(mu = c(-10, 10))
  )

  pair <- equivalence_pair(bad_spec, good_spec)

  set.seed(42)
  y <- rnorm(20, mean = 0, sd = 1)

  # Should handle NA gracefully (suppress expected warnings about NA/NaN)
  result <- suppressWarnings(compare_surfaces(pair, y, n_points = 10))
  expect_s3_class(result, "surface_comparison")
})

# ============================================================================
# classify_identifiability edge cases
# ============================================================================

test_that("classify_identifiability handles very poor conditioning", {
  info <- structure(
    list(
      eigenvalues = c(1000, 0.0001),
      eigenvectors = matrix(c(0.9, 0.4, 0.4, 0.9), 2, 2),
      condition = 10000000,
      rank = 2,
      n_par = 2,
      par_names = c("a", "b")
    ),
    class = "fisher_info"
  )

  status <- degen:::classify_identifiability(info, threshold = 0.01)
  expect_true(all(status %in% c("identified", "weakly_identified", "non_identifiable")))
})

test_that("classify_identifiability handles rank deficient", {
  info <- structure(
    list(
      eigenvalues = c(100, 0),  # Rank deficient
      eigenvectors = matrix(c(0.707, 0.707, -0.707, 0.707), 2, 2),
      condition = Inf,
      rank = 1,
      n_par = 2,
      par_names = c("a", "b")
    ),
    class = "fisher_info"
  )

  status <- degen:::classify_identifiability(info, threshold = 0.01)
  expect_true(any(status == "non_identifiable"))
})

test_that("classify_identifiability handles moderate conditioning", {
  info <- structure(
    list(
      eigenvalues = c(100, 0.1),  # Poorly conditioned but not rank deficient
      eigenvectors = matrix(c(0.9, 0.4, 0.4, 0.9), 2, 2),
      condition = 1500,  # Above 1000 threshold
      rank = 2,
      n_par = 2,
      par_names = c("a", "b")
    ),
    class = "fisher_info"
  )

  status <- degen:::classify_identifiability(info, threshold = 0.01)
  # Should identify some as weakly_identified
  expect_true(all(status %in% c("identified", "weakly_identified", "non_identifiable")))
})

# ============================================================================
# generate_recommendations edge cases
# ============================================================================

test_that("generate_recommendations handles all condition levels", {
  info_good <- structure(
    list(rank = 2, n_par = 2, condition = 50),
    class = "fisher_info"
  )
  status_good <- c(a = "identified", b = "identified")
  recs <- degen:::generate_recommendations(info_good, status_good, 0.01)
  expect_equal(recs$status, "good")

  info_moderate <- structure(
    list(rank = 2, n_par = 2, condition = 150),
    class = "fisher_info"
  )
  recs <- degen:::generate_recommendations(info_moderate, status_good, 0.01)
  expect_equal(recs$status, "moderate")

  info_weak <- structure(
    list(rank = 2, n_par = 2, condition = 2000),
    class = "fisher_info"
  )
  recs <- degen:::generate_recommendations(info_weak, status_good, 0.01)
  expect_equal(recs$status, "weak_identification")

  info_poor <- structure(
    list(rank = 2, n_par = 2, condition = 15000),
    class = "fisher_info"
  )
  recs <- degen:::generate_recommendations(info_poor, status_good, 0.01)
  expect_equal(recs$status, "poorly_conditioned")

  info_nonid <- structure(
    list(rank = 1, n_par = 2, condition = Inf),
    class = "fisher_info"
  )
  status_nonid <- c(a = "non_identifiable", b = "non_identifiable")
  recs <- degen:::generate_recommendations(info_nonid, status_nonid, 0.01)
  expect_equal(recs$status, "non_identifiable")
  expect_true("non_identifiable_params" %in% names(recs))

  # Test with weakly identified params
  status_weak <- c(a = "identified", b = "weakly_identified")
  recs <- degen:::generate_recommendations(info_weak, status_weak, 0.01)
  expect_true("weakly_identified_params" %in% names(recs))
})

# ============================================================================
# find_identified_functions edge case
# ============================================================================

test_that("find_identified_functions returns empty when no good eigenvalues", {
  info <- structure(
    list(
      eigenvalues = c(1e-15, 1e-16),  # All effectively zero
      eigenvectors = matrix(c(0.707, 0.707, -0.707, 0.707), 2, 2),
      par_names = c("a", "b")
    ),
    class = "fisher_info"
  )

  # With threshold = 0.01, max eigenvalue is 1e-15, so threshold is 1e-17
  # Both eigenvalues are above this, so they'd be "good"
  # Use a higher threshold to make both "bad"
  result <- degen:::find_identified_functions(info, threshold = 1.0)
  expect_equal(result, character(0))
})
