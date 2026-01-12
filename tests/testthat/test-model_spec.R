# Test fixtures
make_exp_spec <- function() {
  model_spec(
    loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
    par_names = "lambda",
    par_bounds = list(lambda = c(1e-6, Inf)),
    name = "Exponential"
  )
}

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

# Constructor tests
test_that("model_spec creates valid object", {
  spec <- make_exp_spec()

  expect_s3_class(spec, "model_spec")
  expect_equal(spec$n_par, 1)
  expect_equal(spec$par_names, "lambda")
  expect_equal(spec$name, "Exponential")
  expect_true(is.function(spec$loglik_fn))
})

test_that("model_spec fills default bounds", {
  spec <- model_spec(
    loglik_fn = function(y, a, b) sum(y * a + b),
    par_names = c("a", "b")
  )

  expect_equal(spec$par_bounds$a, c(-Inf, Inf))
  expect_equal(spec$par_bounds$b, c(-Inf, Inf))
})

test_that("model_spec generates default name", {
  spec <- model_spec(
    loglik_fn = function(y, alpha, beta) sum(y),
    par_names = c("alpha", "beta")
  )

  expect_equal(spec$name, "model(alpha, beta)")
})

test_that("model_spec validates loglik_fn signature", {
  # Missing y argument
  expect_error(
    model_spec(
      loglik_fn = function(x, lambda) sum(x),
      par_names = "lambda"
    ),
    "must have argument"
  )

  # Not a function
  expect_error(
    model_spec(
      loglik_fn = "not a function",
      par_names = "lambda"
    ),
    "must be a function"
  )
})

test_that("model_spec validates par_names", {
  # Empty par_names
  expect_error(
    model_spec(
      loglik_fn = function(y) sum(y),
      par_names = character(0)
    ),
    "non-empty character"
  )

  # Duplicates
  expect_error(
    model_spec(
      loglik_fn = function(y, a) sum(y * a),
      par_names = c("a", "a")
    ),
    "duplicates"
  )
})

test_that("par_names must match loglik_fn arguments", {
  # Parameter not in function
  expect_error(
    model_spec(
      loglik_fn = function(y, a) sum(y * a),
      par_names = c("a", "b")
    ),
    "not found in"
  )

  # Extra argument in function
  expect_error(
    model_spec(
      loglik_fn = function(y, a, b) sum(y * a * b),
      par_names = "a"
    ),
    "extra arguments"
  )
})

test_that("model_spec rejects invalid bounds", {
  expect_error(
    model_spec(
      loglik_fn = function(y, a) sum(y * a),
      par_names = "a",
      par_bounds = list(a = c(5, 1))  # lower > upper
    ),
    "Lower bound must be less"
  )

  expect_error(
    model_spec(
      loglik_fn = function(y, a) sum(y * a),
      par_names = "a",
      par_bounds = list(b = c(0, 1))  # unknown parameter
    ),
    "Unknown parameter"
  )
})

test_that("model_spec validates name argument", {
  expect_error(
    model_spec(
      loglik_fn = function(y, a) sum(y),
      par_names = "a",
      name = c("name1", "name2")  # not single string
    ),
    "single character"
  )
})

# Accessor tests
test_that("par_names returns correct values", {
  spec <- make_normal_spec()
  expect_equal(par_names(spec), c("mu", "sigma"))
})

test_that("par_bounds returns correct values", {
  spec <- make_normal_spec()
  bounds <- par_bounds(spec)

  expect_type(bounds, "list")
  expect_equal(names(bounds), c("mu", "sigma"))
  expect_equal(bounds$mu, c(-Inf, Inf))
  expect_equal(bounds$sigma, c(1e-6, Inf))
})

test_that("n_par returns correct count", {
  expect_equal(n_par(make_exp_spec()), 1)
  expect_equal(n_par(make_normal_spec()), 2)
})

test_that("is_model_spec works correctly", {
  expect_true(is_model_spec(make_exp_spec()))
  expect_false(is_model_spec(list()))
  expect_false(is_model_spec(NULL))
  expect_false(is_model_spec("string"))
})

# Log-likelihood evaluation tests
test_that("loglik evaluates correctly for exponential", {
  spec <- make_exp_spec()
  set.seed(123)
  y <- rexp(100, rate = 2)

  result <- loglik(spec, y, par = c(lambda = 2))
  expected <- sum(dexp(y, rate = 2, log = TRUE))

  expect_equal(result, expected)
})

test_that("loglik evaluates correctly for normal", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 2)

  result <- loglik(spec, y, par = c(mu = 5, sigma = 2))
  expected <- sum(dnorm(y, mean = 5, sd = 2, log = TRUE))

  expect_equal(result, expected)
})

test_that("loglik requires named parameters", {
  spec <- make_exp_spec()
  y <- rexp(10)

  expect_error(
    loglik(spec, y, par = 2),  # unnamed
    "named vector"
  )
})

test_that("loglik errors on missing parameters", {
  spec <- make_normal_spec()
  y <- rnorm(10)

  expect_error(
    loglik(spec, y, par = c(mu = 5)),  # missing sigma
    "Missing parameter"
  )
})

test_that("loglik checks parameter bounds", {
  spec <- make_exp_spec()
  y <- rexp(10)

  expect_error(
    loglik(spec, y, par = c(lambda = -1)),  # below lower bound
    "outside bounds"
  )
})

test_that("loglik accepts list parameters", {
  spec <- make_normal_spec()
  set.seed(123)
  y <- rnorm(100, mean = 5, sd = 2)

  result <- loglik(spec, y, par = list(mu = 5, sigma = 2))
  expected <- sum(dnorm(y, mean = 5, sd = 2, log = TRUE))

  expect_equal(result, expected)
})

# Print/format tests
test_that("print method produces output", {
  spec <- make_exp_spec()
  expect_output(print(spec), "model_spec")
  expect_output(print(spec), "Exponential")
  expect_output(print(spec), "lambda")
})

test_that("format method produces expected string", {
  spec <- make_exp_spec()
  expect_match(format(spec), "model_spec")
  expect_match(format(spec), "1 parameters")
})

test_that("summary method produces detailed output", {
  spec <- make_normal_spec()
  expect_output(summary(spec), "Model Specification")
  expect_output(summary(spec), "mu")
  expect_output(summary(spec), "sigma")
})

# Edge cases
test_that("model_spec works with single parameter", {
  spec <- make_exp_spec()
  expect_equal(n_par(spec), 1)

  set.seed(1)
  y <- rexp(50, rate = 3)
  expect_no_error(loglik(spec, y, par = c(lambda = 3)))
})

test_that("model_spec works with many parameters", {
  spec <- model_spec(
    loglik_fn = function(y, a, b, c, d, e) sum(y * (a + b + c + d + e)),
    par_names = c("a", "b", "c", "d", "e")
  )

  expect_equal(n_par(spec), 5)
  expect_equal(length(par_bounds(spec)), 5)
})

test_that("validate = FALSE skips validation", {
  # This would normally error due to mismatched par_names
  # but with validate = FALSE it proceeds
  expect_no_error(
    model_spec(
      loglik_fn = function(y, a) sum(y),
      par_names = "a",
      validate = FALSE
    )
  )
})
