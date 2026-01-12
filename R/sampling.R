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

  # Get effective bounds (handle infinite)
  eff_bounds <- lapply(bounds, effective_bounds)

  if (method == "lhs") {
    # Latin hypercube sampling
    samples <- lhs_sample(n, n_par)
  } else {
    # Simple random sampling
    samples <- matrix(stats::runif(n * n_par), nrow = n, ncol = n_par)
  }

  # Transform to parameter space
  result <- matrix(0, nrow = n, ncol = n_par)
  colnames(result) <- par_names

  for (j in seq_len(n_par)) {
    lower <- eff_bounds[[j]][1]
    upper <- eff_bounds[[j]][2]
    result[, j] <- lower + samples[, j] * (upper - lower)
  }

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
  result <- matrix(0, nrow = n, ncol = k)

  for (j in seq_len(k)) {
    # Create stratified samples
    intervals <- seq(0, 1, length.out = n + 1)
    lower <- intervals[1:n]
    upper <- intervals[2:(n + 1)]

    # Sample within each stratum and shuffle
    samples <- stats::runif(n, lower, upper)
    result[, j] <- sample(samples)
  }

  result
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
  lower <- bounds[1]
  upper <- bounds[2]

  # Handle various cases
if (is.finite(lower) && is.finite(upper)) {
    return(c(lower, upper))
  }

  if (is.finite(lower) && !is.finite(upper)) {
    # Lower bounded only (e.g., variance > 0)
    return(c(lower, lower + scale))
  }

  if (!is.finite(lower) && is.finite(upper)) {
    # Upper bounded only
    return(c(upper - scale, upper))
  }

  # Both infinite
  c(-scale, scale)
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
