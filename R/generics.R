#' Get parameter names
#'
#' @param x An object containing parameter information
#' @param ... Additional arguments passed to methods
#'
#' @return Character vector of parameter names
#' @export
par_names <- function(x, ...) {

UseMethod("par_names")
}

#' Get parameter bounds
#'
#' @param x An object containing parameter bounds
#' @param ... Additional arguments passed to methods
#'
#' @return Named list of parameter bounds
#' @export
par_bounds <- function(x, ...) {
UseMethod("par_bounds")
}

#' Get number of parameters
#'
#' @param x An object containing parameters
#' @param ... Additional arguments passed to methods
#'
#' @return Integer number of parameters
#' @export
n_par <- function(x, ...) {
UseMethod("n_par")
}

#' Evaluate log-likelihood
#'
#' Evaluate the log-likelihood function for a model specification at given
#' parameter values.
#'
#' @param object A model specification object
#' @param y Numeric vector of observed data
#' @param par Named numeric vector of parameter values
#' @param ... Additional arguments passed to methods
#'
#' @return Numeric scalar log-likelihood value
#' @export
loglik <- function(object, y, par, ...) {
UseMethod("loglik")
}
