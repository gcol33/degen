# Analytical Fisher information for standard distributions
#
# These functions compute Fisher information using closed-form expressions
# rather than numerical differentiation, providing faster and more accurate
# results for common distributions.

#' Compute analytical Fisher information
#'
#' For standard distributions, compute the Fisher information matrix using
#' known closed-form expressions. Falls back to numerical computation for
#' unknown distributions.
#'
#' @param spec A `model_spec` object
#' @param y Numeric vector of observed data
#' @param par Named numeric vector of parameter values
#' @param distribution Optional distribution name. If NULL, attempts to detect
#'   from spec$name. Supported: "normal", "exponential", "poisson", "gamma",
#'   "binomial", "beta"
#'
#' @return A `fisher_info` object
#' @export
#'
#' @examples
#' spec <- model_spec_normal()
#' y <- rnorm(100, mean = 5, sd = 2)
#' info <- fisher_analytical(spec, y, c(mu = 5, sigma = 2), distribution = "normal")
#' print(info)
fisher_analytical <- function(spec, y, par, distribution = NULL) {
  if (!is_model_spec(spec)) {
    stop("`spec` must be a model_spec object", call. = FALSE)
  }
  check_numeric(y, "y")

  n <- length(y)

  # Try to detect distribution from name
  if (is.null(distribution) && !is.null(spec$name)) {
    distribution <- detect_distribution(spec$name)
  }

  if (is.null(distribution)) {
    message("Distribution not recognized. Falling back to numerical computation.")
    return(fisher_information(spec, y, par, type = "observed"))
  }

  # Compute analytical information matrix
  info_matrix <- switch(tolower(distribution),
    "normal" = fisher_normal(n, par),
    "exponential" = fisher_exponential(n, par),
    "poisson" = fisher_poisson(n, par),
    "gamma" = fisher_gamma(n, par),
    "binomial" = fisher_binomial(n, par),
    "beta" = fisher_beta(n, par),
    "lognormal" = fisher_lognormal(n, par),
    {
      message(sprintf("No analytical formula for '%s'. Using numerical.", distribution))
      return(fisher_information(spec, y, par, type = "observed"))
    }
  )

  # Build fisher_info object
  rownames(info_matrix) <- spec$par_names
  colnames(info_matrix) <- spec$par_names

  eig <- eigen(info_matrix, symmetric = TRUE)

  eig_abs <- abs(eig$values)
  condition <- if (min(eig_abs) > .Machine$double.eps) {
    max(eig_abs) / min(eig_abs)
  } else {
    Inf
  }

  rank_tol <- max(dim(info_matrix)) * max(eig_abs) * sqrt(.Machine$double.eps)
  rank <- sum(eig_abs > rank_tol)

  structure(
    list(
      matrix = info_matrix,
      eigenvalues = eig$values,
      eigenvectors = eig$vectors,
      condition = condition,
      rank = rank,
      n_par = length(par),
      par = par,
      par_names = spec$par_names,
      n_obs = n,
      type = "analytical"
    ),
    class = "fisher_info"
  )
}

#' Detect distribution from name
#' @noRd
detect_distribution <- function(name) {
  name_lower <- tolower(name)

  patterns <- list(
    "normal" = c("normal", "gaussian"),
    "exponential" = c("exponential", "exp"),
    "poisson" = c("poisson", "pois"),
    "gamma" = c("gamma"),
    "binomial" = c("binomial", "binom"),
    "beta" = c("beta"),
    "lognormal" = c("lognormal", "log-normal", "log normal")
  )

  for (dist in names(patterns)) {
    for (pattern in patterns[[dist]]) {
      if (grepl(pattern, name_lower, fixed = TRUE)) {
        return(dist)
      }
    }
  }

  NULL
}

#' Fisher information for Normal distribution
#' @noRd
fisher_normal <- function(n, par) {
  if (!all(c("mu", "sigma") %in% names(par))) {
    stop("Normal distribution requires parameters 'mu' and 'sigma'", call. = FALSE)
  }
  sigma <- par["sigma"]

  # I(mu, sigma) = diag(n/sigma^2, 2n/sigma^2)
  matrix(c(
    n / sigma^2,  0,
    0,            2 * n / sigma^2
  ), nrow = 2, byrow = TRUE)
}

#' Fisher information for Exponential distribution
#' @noRd
fisher_exponential <- function(n, par) {
  if (!"rate" %in% names(par)) {
    stop("Exponential distribution requires parameter 'rate'", call. = FALSE)
  }
  rate <- par["rate"]

  # I(rate) = n / rate^2
  matrix(n / rate^2, nrow = 1)
}

#' Fisher information for Poisson distribution
#' @noRd
fisher_poisson <- function(n, par) {
  if (!"lambda" %in% names(par)) {
    stop("Poisson distribution requires parameter 'lambda'", call. = FALSE)
  }
  lambda <- par["lambda"]

  # I(lambda) = n / lambda
  matrix(n / lambda, nrow = 1)
}

#' Fisher information for Gamma distribution (shape-rate parameterization)
#' @noRd
fisher_gamma <- function(n, par) {
  if (!all(c("shape", "rate") %in% names(par))) {
    stop("Gamma distribution requires parameters 'shape' and 'rate'", call. = FALSE)
  }
  shape <- par["shape"]
  rate <- par["rate"]

  # I(shape, rate) matrix
  # I_11 = n * trigamma(shape)
  # I_12 = I_21 = -n / rate
  # I_22 = n * shape / rate^2
  matrix(c(
    n * trigamma(shape),  -n / rate,
    -n / rate,            n * shape / rate^2
  ), nrow = 2, byrow = TRUE)
}

#' Fisher information for Binomial distribution
#' @noRd
fisher_binomial <- function(n, par) {
  if (!"prob" %in% names(par)) {
    stop("Binomial distribution requires parameter 'prob'", call. = FALSE)
  }
  # Note: n here is number of observations, not number of trials
  # Trials should be encoded in the spec
  prob <- par["prob"]

  # I(prob) = n * size / (prob * (1 - prob))
  # Since we don't have 'size' here, we estimate from data
  # For simplicity, assume size = 1 (Bernoulli)
  matrix(n / (prob * (1 - prob)), nrow = 1)
}

#' Fisher information for Beta distribution
#' @noRd
fisher_beta <- function(n, par) {
  if (!all(c("shape1", "shape2") %in% names(par))) {
    stop("Beta distribution requires parameters 'shape1' and 'shape2'", call. = FALSE)
  }
  a <- par["shape1"]
  b <- par["shape2"]

  # I(a, b) matrix using trigamma function
  tri_a <- trigamma(a)
  tri_b <- trigamma(b)
  tri_ab <- trigamma(a + b)

  matrix(c(
    n * (tri_a - tri_ab),   -n * tri_ab,
    -n * tri_ab,            n * (tri_b - tri_ab)
  ), nrow = 2, byrow = TRUE)
}

#' Fisher information for Log-normal distribution
#' @noRd
fisher_lognormal <- function(n, par) {
  if (!all(c("meanlog", "sdlog") %in% names(par))) {
    stop("Log-normal distribution requires parameters 'meanlog' and 'sdlog'", call. = FALSE)
  }
  sdlog <- par["sdlog"]

  # Same structure as normal but on log scale
  matrix(c(
    n / sdlog^2,  0,
    0,            2 * n / sdlog^2
  ), nrow = 2, byrow = TRUE)
}
