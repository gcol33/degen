# Test fixtures
make_exp_spec <- function() {
  model_spec(
    loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
    par_names = "lambda",
    par_bounds = list(lambda = c(1e-6, Inf)),
    name = "Exponential"
  )
}

make_gamma1_spec <- function() {
  model_spec(
    loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
    par_names = "rate",
    par_bounds = list(rate = c(1e-6, Inf)),
    name = "Gamma(1)"
  )
}

make_normal_spec <- function() {
  model_spec(
    loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
    par_names = c("mu", "sigma"),
    par_bounds = list(sigma = c(1e-6, Inf)),
    name = "Normal"
  )
}

# Constructor tests
test_that("equivalence_pair creates valid object", {
  exp_spec <- make_exp_spec()
  gamma_spec <- make_gamma1_spec()

  pair <- equivalence_pair(exp_spec, gamma_spec)

  expect_s3_class(pair, "equivalence_pair")
  expect_equal(pair$n_par_a, 1)
  expect_equal(pair$n_par_b, 1)
})

test_that("equivalence_pair requires model_spec inputs", {
  exp_spec <- make_exp_spec()

  expect_error(
    equivalence_pair("not a spec", exp_spec),
    "must be a model_spec"
  )

  expect_error(
    equivalence_pair(exp_spec, list()),
    "must be a model_spec"
  )
})

test_that("equivalence_pair generates default name", {
  exp_spec <- make_exp_spec()
  gamma_spec <- make_gamma1_spec()

  pair <- equivalence_pair(exp_spec, gamma_spec)

  expect_equal(pair$name, "Exponential vs Gamma(1)")
})

test_that("equivalence_pair accepts custom name", {
  exp_spec <- make_exp_spec()
  gamma_spec <- make_gamma1_spec()

  pair <- equivalence_pair(exp_spec, gamma_spec, name = "Custom Name")

  expect_equal(pair$name, "Custom Name")
})

test_that("equivalence_pair validates name argument", {
  exp_spec <- make_exp_spec()

  expect_error(
    equivalence_pair(exp_spec, exp_spec, name = c("a", "b")),
    "single character"
  )
})

# Accessor tests
test_that("spec_a returns first model", {
  exp_spec <- make_exp_spec()
  gamma_spec <- make_gamma1_spec()
  pair <- equivalence_pair(exp_spec, gamma_spec)

  result <- spec_a(pair)
  expect_identical(result, exp_spec)
})

test_that("spec_b returns second model", {
  exp_spec <- make_exp_spec()
  gamma_spec <- make_gamma1_spec()
  pair <- equivalence_pair(exp_spec, gamma_spec)

  result <- spec_b(pair)
  expect_identical(result, gamma_spec)
})

test_that("accessors require equivalence_pair input", {
  expect_error(spec_a(list()), "must be an equivalence_pair")
  expect_error(spec_b(list()), "must be an equivalence_pair")
})

test_that("par_dims returns correct dimensions", {
  exp_spec <- make_exp_spec()
  norm_spec <- make_normal_spec()
  pair <- equivalence_pair(exp_spec, norm_spec)

  dims <- par_dims(pair)
  expect_equal(dims, c(a = 1, b = 2))
})

test_that("par_dims requires equivalence_pair", {
  expect_error(par_dims(list()), "must be an equivalence_pair")
})

test_that("same_par_count works correctly", {
  exp_spec <- make_exp_spec()
  gamma_spec <- make_gamma1_spec()
  norm_spec <- make_normal_spec()

  pair_equal <- equivalence_pair(exp_spec, gamma_spec)
  pair_different <- equivalence_pair(exp_spec, norm_spec)

  expect_true(same_par_count(pair_equal))
  expect_false(same_par_count(pair_different))
})

test_that("same_par_count requires equivalence_pair", {
  expect_error(same_par_count(list()), "must be an equivalence_pair")
})

# Type predicate tests
test_that("is_equivalence_pair works correctly", {
  exp_spec <- make_exp_spec()
  pair <- equivalence_pair(exp_spec, exp_spec)

  expect_true(is_equivalence_pair(pair))
  expect_false(is_equivalence_pair(list()))
  expect_false(is_equivalence_pair(exp_spec))
  expect_false(is_equivalence_pair(NULL))
})

# Print/format tests
test_that("print method shows both models", {
  exp_spec <- make_exp_spec()
  gamma_spec <- make_gamma1_spec()
  pair <- equivalence_pair(exp_spec, gamma_spec)

  expect_output(print(pair), "equivalence_pair")
  expect_output(print(pair), "Model A:")
  expect_output(print(pair), "Model B:")
  expect_output(print(pair), "Exponential")
  expect_output(print(pair), "Gamma")
})

test_that("format method produces expected string", {
  exp_spec <- make_exp_spec()
  norm_spec <- make_normal_spec()
  pair <- equivalence_pair(exp_spec, norm_spec)

  formatted <- format(pair)
  expect_match(formatted, "equivalence_pair")
  expect_match(formatted, "1 vs 2")
})

test_that("summary method shows detailed information", {
  exp_spec <- make_exp_spec()
  gamma_spec <- make_gamma1_spec()
  pair <- equivalence_pair(exp_spec, gamma_spec)

  expect_output(summary(pair), "Equivalence Pair")
  expect_output(summary(pair), "lambda")
  expect_output(summary(pair), "rate")
})
