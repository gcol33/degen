# Simulation methods for model_spec objects

#' Add simulation function to model_spec
#'
#' Attach a data-generating function to an existing model_spec. This enables
#' the `simulate()` method for parametric bootstrap and power analysis.
#'
#' @param spec A `model_spec` object
#' @param simulate_fn A function with signature `function(n, ...)` where `...`
#'   are the model parameters. Should return a numeric vector of length `n`.
#'
#' @return A modified `model_spec` with simulation capability
#' @export
#'
#' @examples
#' spec <- model_spec_normal()
#' spec <- add_simulator(spec, function(n, mu, sigma) rnorm(n, mu, sigma))
#' simulate(spec, n = 10, par = c(mu = 5, sigma = 2))
add_simulator <- function(spec, simulate_fn) {
  if (!is_model_spec(spec)) {
    stop("`spec` must be a model_spec object", call. = FALSE)
  }
  if (!is.function(simulate_fn)) {
    stop("`simulate_fn` must be a function", call. = FALSE)
  }
  spec$simulate_fn <- simulate_fn
  spec
}

#' Simulate data from a model specification
#'
#' Generate random data from a model_spec that has a simulation function
#' attached via `add_simulator()`.
#'
#' @param object A `model_spec` object with simulation capability
#' @param nsim Number of simulations (datasets) to generate
#' @param seed Random seed for reproducibility
#' @param n Number of observations per simulation
#' @param par Named numeric vector of parameter values
#' @param ... Additional arguments (ignored)
#'
#' @return If `nsim = 1`, a numeric vector. If `nsim > 1`, a list of vectors.
#' @importFrom stats simulate
#' @export
#'
#' @examples
#' spec <- model_spec_normal()
#' spec <- add_simulator(spec, function(n, mu, sigma) rnorm(n, mu, sigma))
#' y <- simulate(spec, n = 100, par = c(mu = 5, sigma = 2))
#' hist(y)
simulate.model_spec <- function(object, nsim = 1, seed = NULL, n, par, ...) {
  if (is.null(object$simulate_fn)) {
    stop("No simulation function attached. Use add_simulator() first.", call. = FALSE)
  }
  if (missing(n)) {
    stop("`n` (number of observations) is required", call. = FALSE)
  }
  if (missing(par)) {
    stop("`par` (parameter values) is required", call. = FALSE)
  }

  if (!is.null(seed)) set.seed(seed)

  # Convert par to list for do.call
  par_list <- as.list(par)

  if (nsim == 1) {
    do.call(object$simulate_fn, c(list(n = n), par_list))
  } else {
    replicate(nsim, do.call(object$simulate_fn, c(list(n = n), par_list)),
              simplify = FALSE)
  }
}

#' Check if model_spec can simulate
#'
#' @param spec A `model_spec` object
#'
#' @return Logical
#' @export
can_simulate <- function(spec) {
  is_model_spec(spec) && !is.null(spec$simulate_fn)
}


# Pre-built simulators for helper functions
# These are automatically added when using model_spec_* helpers

#' @noRd
add_default_simulators <- function() {
  # This function documents the simulation functions used by helpers
}

# Update model_spec helpers to include simulators
