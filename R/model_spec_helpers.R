# Model specification helpers for common distributions
#
# These functions create model_spec objects for standard statistical
# distributions, reducing boilerplate and ensuring correct parameterization.

#' Create model_spec for Normal distribution
#'
#' @param par Which parameters to estimate: "both" (default), "mean", or "sd"
#' @param known_mean Fixed mean value (if par = "sd")
#' @param known_sd Fixed sd value (if par = "mean")
#' @param name Optional model name
#'
#' @return A `model_spec` object
#' @export
#'
#' @examples
#' spec <- model_spec_normal()
#' y <- rnorm(100, mean = 5, sd = 2)
#' loglik(spec, y, c(mu = 5, sigma = 2))
model_spec_normal <- function(par = c("both", "mean", "sd"),
                              known_mean = NULL,
                              known_sd = NULL,
                              name = "Normal") {
  par <- match.arg(par)

  if (par == "both") {
    model_spec(
      loglik_fn = function(y, mu, sigma) {
        sum(stats::dnorm(y, mean = mu, sd = sigma, log = TRUE))
      },
      par_names = c("mu", "sigma"),
      par_bounds = list(mu = c(-Inf, Inf), sigma = c(1e-10, Inf)),
      name = name
    )
  } else if (par == "mean") {
    if (is.null(known_sd)) stop("known_sd required when par = 'mean'", call. = FALSE)
    sd_val <- known_sd
    model_spec(
      loglik_fn = function(y, mu) {
        sum(stats::dnorm(y, mean = mu, sd = sd_val, log = TRUE))
      },
      par_names = "mu",
      par_bounds = list(mu = c(-Inf, Inf)),
      name = paste0(name, "(sd=", known_sd, ")")
    )
  } else {
    if (is.null(known_mean)) stop("known_mean required when par = 'sd'", call. = FALSE)
    mean_val <- known_mean
    model_spec(
      loglik_fn = function(y, sigma) {
        sum(stats::dnorm(y, mean = mean_val, sd = sigma, log = TRUE))
      },
      par_names = "sigma",
      par_bounds = list(sigma = c(1e-10, Inf)),
      name = paste0(name, "(mu=", known_mean, ")")
    )
  }
}

#' Create model_spec for Exponential distribution
#'
#' @param name Optional model name
#'
#' @return A `model_spec` object
#' @export
#'
#' @examples
#' spec <- model_spec_exponential()
#' y <- rexp(100, rate = 2)
#' loglik(spec, y, c(rate = 2))
model_spec_exponential <- function(name = "Exponential") {
  model_spec(
    loglik_fn = function(y, rate) {
      sum(stats::dexp(y, rate = rate, log = TRUE))
    },
    par_names = "rate",
    par_bounds = list(rate = c(1e-10, Inf)),
    name = name
  )
}

#' Create model_spec for Gamma distribution
#'
#' @param par Parameterization: "shape_rate" (default) or "shape_scale"
#' @param known_shape Fixed shape value (if estimating only rate/scale)
#' @param name Optional model name
#'
#' @return A `model_spec` object
#' @export
#'
#' @examples
#' spec <- model_spec_gamma()
#' y <- rgamma(100, shape = 2, rate = 1)
#' loglik(spec, y, c(shape = 2, rate = 1))
model_spec_gamma <- function(par = c("shape_rate", "shape_scale", "rate", "scale"),
                             known_shape = NULL,
                             name = "Gamma") {
  par <- match.arg(par)

  if (par == "shape_rate") {
    model_spec(
      loglik_fn = function(y, shape, rate) {
        sum(stats::dgamma(y, shape = shape, rate = rate, log = TRUE))
      },
      par_names = c("shape", "rate"),
      par_bounds = list(shape = c(1e-10, Inf), rate = c(1e-10, Inf)),
      name = name
    )
  } else if (par == "shape_scale") {
    model_spec(
      loglik_fn = function(y, shape, scale) {
        sum(stats::dgamma(y, shape = shape, scale = scale, log = TRUE))
      },
      par_names = c("shape", "scale"),
      par_bounds = list(shape = c(1e-10, Inf), scale = c(1e-10, Inf)),
      name = paste0(name, "(scale)")
    )
  } else if (par == "rate") {
    if (is.null(known_shape)) stop("known_shape required when par = 'rate'", call. = FALSE)
    shape_val <- known_shape
    model_spec(
      loglik_fn = function(y, rate) {
        sum(stats::dgamma(y, shape = shape_val, rate = rate, log = TRUE))
      },
      par_names = "rate",
      par_bounds = list(rate = c(1e-10, Inf)),
      name = paste0(name, "(shape=", known_shape, ")")
    )
  } else {
    if (is.null(known_shape)) stop("known_shape required when par = 'scale'", call. = FALSE)
    shape_val <- known_shape
    model_spec(
      loglik_fn = function(y, scale) {
        sum(stats::dgamma(y, shape = shape_val, scale = scale, log = TRUE))
      },
      par_names = "scale",
      par_bounds = list(scale = c(1e-10, Inf)),
      name = paste0(name, "(shape=", known_shape, ")")
    )
  }
}

#' Create model_spec for Poisson distribution
#'
#' @param name Optional model name
#'
#' @return A `model_spec` object
#' @export
#'
#' @examples
#' spec <- model_spec_poisson()
#' y <- rpois(100, lambda = 5)
#' loglik(spec, y, c(lambda = 5))
model_spec_poisson <- function(name = "Poisson") {
  model_spec(
    loglik_fn = function(y, lambda) {
      sum(stats::dpois(y, lambda = lambda, log = TRUE))
    },
    par_names = "lambda",
    par_bounds = list(lambda = c(1e-10, Inf)),
    name = name
  )
}

#' Create model_spec for Binomial distribution
#'
#' @param size Number of trials (known)
#' @param name Optional model name
#'
#' @return A `model_spec` object
#' @export
#'
#' @examples
#' spec <- model_spec_binomial(size = 10)
#' y <- rbinom(100, size = 10, prob = 0.3)
#' loglik(spec, y, c(prob = 0.3))
model_spec_binomial <- function(size, name = "Binomial") {
  if (missing(size)) stop("`size` (number of trials) is required", call. = FALSE)
  n <- size
  model_spec(
    loglik_fn = function(y, prob) {
      sum(stats::dbinom(y, size = n, prob = prob, log = TRUE))
    },
    par_names = "prob",
    par_bounds = list(prob = c(1e-10, 1 - 1e-10)),
    name = paste0(name, "(n=", size, ")")
  )
}

#' Create model_spec for Beta distribution
#'
#' @param name Optional model name
#'
#' @return A `model_spec` object
#' @export
#'
#' @examples
#' spec <- model_spec_beta()
#' y <- rbeta(100, shape1 = 2, shape2 = 5)
#' loglik(spec, y, c(shape1 = 2, shape2 = 5))
model_spec_beta <- function(name = "Beta") {
  model_spec(
    loglik_fn = function(y, shape1, shape2) {
      sum(stats::dbeta(y, shape1 = shape1, shape2 = shape2, log = TRUE))
    },
    par_names = c("shape1", "shape2"),
    par_bounds = list(shape1 = c(1e-10, Inf), shape2 = c(1e-10, Inf)),
    name = name
  )
}

#' Create model_spec for Log-normal distribution
#'
#' @param name Optional model name
#'
#' @return A `model_spec` object
#' @export
#'
#' @examples
#' spec <- model_spec_lognormal()
#' y <- rlnorm(100, meanlog = 1, sdlog = 0.5)
#' loglik(spec, y, c(meanlog = 1, sdlog = 0.5))
model_spec_lognormal <- function(name = "LogNormal") {
  model_spec(
    loglik_fn = function(y, meanlog, sdlog) {
      sum(stats::dlnorm(y, meanlog = meanlog, sdlog = sdlog, log = TRUE))
    },
    par_names = c("meanlog", "sdlog"),
    par_bounds = list(meanlog = c(-Inf, Inf), sdlog = c(1e-10, Inf)),
    name = name
  )
}

#' Create model_spec for Weibull distribution
#'
#' @param name Optional model name
#'
#' @return A `model_spec` object
#' @export
#'
#' @examples
#' spec <- model_spec_weibull()
#' y <- rweibull(100, shape = 2, scale = 1)
#' loglik(spec, y, c(shape = 2, scale = 1))
model_spec_weibull <- function(name = "Weibull") {
  model_spec(
    loglik_fn = function(y, shape, scale) {
      sum(stats::dweibull(y, shape = shape, scale = scale, log = TRUE))
    },
    par_names = c("shape", "scale"),
    par_bounds = list(shape = c(1e-10, Inf), scale = c(1e-10, Inf)),
    name = name
  )
}

#' Create model_spec for Negative Binomial distribution
#'
#' @param par Parameterization: "size_prob" or "size_mu" (mean parameterization)
#' @param name Optional model name
#'
#' @return A `model_spec` object
#' @export
#'
#' @examples
#' spec <- model_spec_negbinom()
#' y <- rnbinom(100, size = 5, prob = 0.3)
#' loglik(spec, y, c(size = 5, prob = 0.3))
model_spec_negbinom <- function(par = c("size_prob", "size_mu"),
                                name = "NegBinomial") {
  par <- match.arg(par)

  if (par == "size_prob") {
    model_spec(
      loglik_fn = function(y, size, prob) {
        sum(stats::dnbinom(y, size = size, prob = prob, log = TRUE))
      },
      par_names = c("size", "prob"),
      par_bounds = list(size = c(1e-10, Inf), prob = c(1e-10, 1 - 1e-10)),
      name = name
    )
  } else {
    model_spec(
      loglik_fn = function(y, size, mu) {
        sum(stats::dnbinom(y, size = size, mu = mu, log = TRUE))
      },
      par_names = c("size", "mu"),
      par_bounds = list(size = c(1e-10, Inf), mu = c(1e-10, Inf)),
      name = paste0(name, "(mu)")
    )
  }
}

#' Create model_spec for Uniform distribution
#'
#' @param name Optional model name
#'
#' @return A `model_spec` object
#' @export
#'
#' @examples
#' spec <- model_spec_uniform()
#' y <- runif(100, min = 2, max = 8)
#' loglik(spec, y, c(min = 2, max = 8))
model_spec_uniform <- function(name = "Uniform") {
  model_spec(
    loglik_fn = function(y, min, max) {
      if (min >= max) return(-Inf)
      if (any(y < min) || any(y > max)) return(-Inf)
      sum(stats::dunif(y, min = min, max = max, log = TRUE))
    },
    par_names = c("min", "max"),
    par_bounds = list(min = c(-Inf, Inf), max = c(-Inf, Inf)),
    name = name
  )
}
