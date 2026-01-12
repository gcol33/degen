# Common test fixtures for degen tests
# These are automatically loaded before tests run

#' Create exponential model specification
#' @noRd
make_exp_spec <- function(bounds = list(lambda = c(1e-6, 100))) {
  model_spec(
    loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
    par_names = "lambda",
    par_bounds = bounds,
    name = "Exponential"
  )
}

#' Create normal model specification
#' @noRd
make_normal_spec <- function(bounds = list(mu = c(-100, 100), sigma = c(1e-6, 100))) {
  model_spec(
    loglik_fn = function(y, mu, sigma) {
      sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
    },
    par_names = c("mu", "sigma"),
    par_bounds = bounds,
    name = "Normal"
  )
}

#' Create gamma model specification with fixed shape
#' @noRd
make_gamma_spec <- function(shape = 1, bounds = list(rate = c(1e-6, 100))) {
  model_spec(
    loglik_fn = function(y, rate) {
      sum(dgamma(y, shape = shape, rate = rate, log = TRUE))
    },
    par_names = "rate",
    par_bounds = bounds,
    name = sprintf("Gamma(%g)", shape)
  )
}

#' Create non-identifiable model (only sum is identified)
#' @noRd
make_nonid_spec <- function(bounds = list(a = c(-50, 50), b = c(-50, 50))) {
  model_spec(
    loglik_fn = function(y, a, b) {
      sum(dnorm(y, mean = a + b, sd = 1, log = TRUE))
    },
    par_names = c("a", "b"),
    par_bounds = bounds,
    name = "Non-identifiable"
  )
}

#' Create Weibull model specification with fixed shape
#' @noRd
make_weibull_spec <- function(shape = 1, bounds = list(scale = c(1e-6, 100))) {
  model_spec(
    loglik_fn = function(y, scale) {
      sum(dweibull(y, shape = shape, scale = scale, log = TRUE))
    },
    par_names = "scale",
    par_bounds = bounds,
    name = sprintf("Weibull(%g)", shape)
  )
}

#' Create pair of known-equivalent models
#' @noRd
make_equivalent_pair <- function() {
  exp_spec <- make_exp_spec()
  gamma_spec <- make_gamma_spec(shape = 1)
  equivalence_pair(exp_spec, gamma_spec, name = "Exp vs Gamma(1)")
}

#' Create pair of known-different models
#' @noRd
make_different_pair <- function() {
  exp_spec <- make_exp_spec()
  gamma_spec <- make_gamma_spec(shape = 2)
  equivalence_pair(exp_spec, gamma_spec, name = "Exp vs Gamma(2)")
}
