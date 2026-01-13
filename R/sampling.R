#' Sample from parameter space
#'
#' Generate samples from a parameter space respecting bounds.
#'
#' @param bounds Named list of parameter bounds
#' @param n Number of samples
#' @param method Sampling method: "lhs" (Latin hypercube) or "random"
#'
#' @return Matrix with n rows and length(bounds) columns
#' @noRd
sample_par_space <- function(bounds, n, method = c("lhs", "random")) {
  method <- match.arg(method)
  par_names <- names(bounds)
  n_par <- length(par_names)

  # Extract lower and upper bounds
  lower <- vapply(bounds, `[`, numeric(1), 1)
  upper <- vapply(bounds, `[`, numeric(1), 2)

  # Call C++ backend
  result <- .sample_par_space_cpp(lower, upper, n, lhs = (method == "lhs"))
  colnames(result) <- par_names

  result
}

#' Latin hypercube sampling
#'
#' @param n Number of samples
#' @param k Number of dimensions
#'
#' @return Matrix of samples in unit hypercube
#' @noRd
lhs_sample <- function(n, k) {
  .lhs_sample_cpp(n, k)
}

#' Get effective bounds for sampling
#'
#' Convert infinite bounds to reasonable finite values for sampling.
#'
#' @param bounds Length-2 vector of bounds
#' @param scale Scale factor for unbounded directions
#'
#' @return Length-2 vector of finite bounds
#' @noRd
effective_bounds <- function(bounds, scale = 10) {
  .effective_bounds_cpp(bounds[1], bounds[2], scale)
}

#' Propose reasonable bounds for unbounded parameters
#'
#' @param spec A model_spec object
#' @param y Data vector (used to estimate reasonable scale)
#'
#' @return Named list of bounds
#' @noRd
propose_bounds <- function(spec, y) {
  bounds <- spec$par_bounds

  # Use data scale to inform bound proposals
  data_scale <- max(abs(range(y, na.rm = TRUE)), 1)

  lapply(bounds, function(b) {
    effective_bounds(b, scale = data_scale * 5)
  })
}
