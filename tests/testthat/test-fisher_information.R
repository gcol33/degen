# Test fixtures
make_normal_spec <- function() {
  model_spec(
    loglik_fn = function(y, mu, sigma) {
      sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
    },
    par_names = c("mu", "sigma"),
    par_bounds = list(sigma = c(1e-6, Inf)),
    name = "Normal"
  )
}

make_nonid_spec <- function() {
  # Only a + b is identified, not a and b individually
  model_spec(
    loglik_fn = function(y, a, b) {
      sum(dnorm(y, mean = a + b, sd = 1, log = TRUE))
    },
    par_names = c("a", "b"),
    name = "Non-identifiable"
  )
}

make_exp_spec <- function() {
  model_spec(
    loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
    par_names = "lambda",
    par_bounds = list(lambda = c(1e-6, Inf)),
    name = "Exponential"
  )
}

# Constructor tests
test_that("fisher_information creates valid object", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 2)

  info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))

  expect_s3_class(info, "fisher_info")
  expect_true(is.matrix(info$matrix))
  expect_equal(nrow(info$matrix), 2)
  expect_equal(ncol(info$matrix), 2)
  expect_equal(length(info$eigenvalues), 2)
})

test_that("fisher_information validates inputs", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(50)

  expect_error(fisher_information("not a spec", y, c(mu = 0, sigma = 1)),
               "must be a model_spec")
  expect_error(fisher_information(spec, "not numeric", c(mu = 0, sigma = 1)),
               "must be numeric")
  expect_error(fisher_information(spec, numeric(0), c(mu = 0, sigma = 1)),
               "must not be empty")
  expect_error(fisher_information(spec, y, c(0, 1)),
               "must be a named")
  expect_error(fisher_information(spec, y, c(mu = 0)),
               "Missing parameter")
})

test_that("information matrix is symmetric", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 2)

  info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))

  expect_equal(info$matrix, t(info$matrix), tolerance = 1e-10)
})

test_that("normal model gives approximately correct Fisher information", {
  spec <- make_normal_spec()
  n <- 500
  set.seed(42)
  y <- rnorm(n, mean = 5, sd = 2)

  info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))

  # Known theoretical results:
  # I_mu,mu = n / sigma^2 = 500 / 4 = 125
  # I_sigma,sigma = 2n / sigma^2 = 1000 / 4 = 250
  # I_mu,sigma = 0

  expect_equal(info$matrix[1, 1], 125, tolerance = 20)  # I_mu,mu
  expect_equal(info$matrix[2, 2], 250, tolerance = 40)  # I_sigma,sigma
  expect_equal(info$matrix[1, 2], 0, tolerance = 10)    # I_mu,sigma
})

test_that("identifiable model has full rank", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 2)

  info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))

  expect_equal(info$rank, 2)
  expect_true(all(info$eigenvalues > 0))
})

test_that("non-identifiable model has rank deficiency", {
  spec <- make_nonid_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 1)

  info <- fisher_information(spec, y, par = c(a = 2, b = 3))

  expect_lt(info$rank, 2)
  expect_true(any(abs(info$eigenvalues) < 1e-6))
})

test_that("single parameter model works", {
  spec <- make_exp_spec()
  set.seed(123)
  y <- rexp(100, rate = 2)

  info <- fisher_information(spec, y, par = c(lambda = 2))

  expect_s3_class(info, "fisher_info")
  expect_equal(info$n_par, 1)
  expect_equal(info$rank, 1)
  expect_length(info$eigenvalues, 1)
})

# Accessor tests
test_that("info_eigenvalues works", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 2)
  info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))

  eigs <- info_eigenvalues(info)
  expect_length(eigs, 2)
  expect_true(all(eigs > 0))
  expect_equal(eigs, info$eigenvalues)
})

test_that("info_condition works", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 2)
  info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))

  cond <- info_condition(info)
  expect_type(cond, "double")
  expect_gt(cond, 1)
})

test_that("info_rank works", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 2)
  info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))

  rank <- info_rank(info)
  expect_equal(rank, 2)
})

test_that("accessors validate input", {
  expect_error(info_eigenvalues(list()), "must be a fisher_info")
  expect_error(info_condition(list()), "must be a fisher_info")
  expect_error(info_rank(list()), "must be a fisher_info")
})

test_that("null_directions returns NULL for identifiable model", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 2)
  info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))

  nulls <- null_directions(info)
  expect_null(nulls)
})

test_that("null_directions finds flat directions", {
  spec <- make_nonid_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 1)
  info <- fisher_information(spec, y, par = c(a = 2, b = 3))

  nulls <- null_directions(info)
  expect_true(is.matrix(nulls))
  expect_equal(nrow(nulls), 2)
  expect_equal(ncol(nulls), 1)

  # The null direction should be (1, -1) or (-1, 1) normalized
  # because moving a up and b down keeps a+b constant
  null_vec <- nulls[, 1]
  # Check that signs are opposite
  expect_true(sign(null_vec[1]) != sign(null_vec[2]))
})

test_that("null_directions validates input", {
  expect_error(null_directions(list()), "must be a fisher_info")
})

# Type predicate test
test_that("is_fisher_info works", {
  spec <- make_exp_spec()
  set.seed(123)
  y <- rexp(50, rate = 2)
  info <- fisher_information(spec, y, par = c(lambda = 2))

  expect_true(is_fisher_info(info))
  expect_false(is_fisher_info(list()))
  expect_false(is_fisher_info(NULL))
})

# Print/format tests
test_that("print method produces output", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(50, mean = 5, sd = 2)
  info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))

  expect_output(print(info), "fisher_info")
  expect_output(print(info), "Condition")
  expect_output(print(info), "Rank")
  expect_output(print(info), "Eigenvalues")
})

test_that("summary method produces detailed output", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(50, mean = 5, sd = 2)
  info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))

  expect_output(summary(info), "Fisher Information")
  expect_output(summary(info), "Information Matrix")
  expect_output(summary(info), "Eigenvalue")
  expect_output(summary(info), "Interpretation")
})

test_that("summary warns about non-identifiability", {
  spec <- make_nonid_spec()
  set.seed(123)
  y <- rnorm(50, mean = 5, sd = 1)
  info <- fisher_information(spec, y, par = c(a = 2, b = 3))

  expect_output(summary(info), "NON-IDENTIFIABLE")
})

test_that("format method works", {
  spec <- make_exp_spec()
  set.seed(123)
  y <- rexp(50, rate = 2)
  info <- fisher_information(spec, y, par = c(lambda = 2))

  formatted <- format(info)
  expect_match(formatted, "fisher_info")
  expect_match(formatted, "rank")
})
