# Model fitting integration for degen
#
# Functions for fitting models and integrating with R's modeling ecosystem.

#' Fit a model specification via MLE
#'
#' Find maximum likelihood estimates for a model_spec.
#'
#' @param spec A `model_spec` object
#' @param y Numeric vector of observed data
#' @param start Optional starting values. If NULL, uses center of bounds.
#' @param method Optimization method passed to `optim()`
#' @param ... Additional arguments passed to `optim()`
#'
#' @return A list with:
#' \describe{
#'   \item{par}{MLE parameter estimates}
#'   \item{loglik}{Log-likelihood at MLE}
#'   \item{convergence}{Convergence code from optim}
#'   \item{hessian}{Hessian matrix at MLE (if computed)}
#'   \item{spec}{The original model_spec}
#'   \item{y}{The data}
#' }
#'
#' @export
#'
#' @examples
#' spec <- model_spec_normal()
#' y <- rnorm(100, mean = 5, sd = 2)
#' fit <- fit_model(spec, y)
#' fit$par
fit_model <- function(spec, y, start = NULL, method = "L-BFGS-B", ...) {
  if (!is_model_spec(spec)) {
    stop("`spec` must be a model_spec object", call. = FALSE)
  }
  check_numeric(y, "y")

  # Get bounds
  bounds <- propose_bounds(spec, y)
  lower <- vapply(bounds, `[`, numeric(1), 1)
  upper <- vapply(bounds, `[`, numeric(1), 2)

  # Starting values
  if (is.null(start)) {
    # Use data-driven starting values where possible
    start <- vapply(bounds, function(b) {
      # If bounds span 0, use a small positive value for potentially positive-only params
      if (b[1] > 0) {
        mean(c(b[1], min(b[2], b[1] * 10)))
      } else if (is.finite(b[1]) && is.finite(b[2])) {
        mean(b)
      } else {
        1  # Safe default
      }
    }, numeric(1))
    names(start) <- spec$par_names

    # For normal distribution, use sample statistics
    if (!is.null(spec$name) && grepl("normal", tolower(spec$name))) {
      if ("mu" %in% spec$par_names) start["mu"] <- mean(y)
      if ("sigma" %in% spec$par_names) start["sigma"] <- stats::sd(y)
    }
  }

  # Objective function (negative log-likelihood for minimization)
  objective <- function(par) {
    names(par) <- spec$par_names
    ll <- tryCatch(
      loglik(spec, y, par),
      error = function(e) -Inf
    )
    if (!is.finite(ll)) return(1e10)  # Large penalty for invalid regions
    -ll
  }

  # Optimize
  result <- stats::optim(
    par = start,
    fn = objective,
    method = method,
    lower = lower,
    upper = upper,
    hessian = TRUE,
    ...
  )

  # Extract MLE
  mle <- result$par
  names(mle) <- spec$par_names

  structure(
    list(
      par = mle,
      loglik = -result$value,
      convergence = result$convergence,
      hessian = result$hessian,
      se = tryCatch(sqrt(diag(solve(result$hessian))), error = function(e) rep(NA, length(mle))),
      spec = spec,
      y = y,
      n = length(y)
    ),
    class = "fitted_model_spec"
  )
}


#' @export
print.fitted_model_spec <- function(x, ...) {
  cat("<fitted_model_spec>\n")
  if (!is.null(x$spec$name)) {
    cat("Model:", x$spec$name, "\n")
  }
  cat("Log-likelihood:", round(x$loglik, 4), "\n")
  cat("Parameters:\n")
  for (i in seq_along(x$par)) {
    se_str <- if (!is.na(x$se[i])) sprintf(" (SE: %.4g)", x$se[i]) else ""
    cat(sprintf("  %s = %.4g%s\n", names(x$par)[i], x$par[i], se_str))
  }
  if (x$convergence != 0) {
    cat("Warning: optimization may not have converged (code:", x$convergence, ")\n")
  }
  invisible(x)
}


#' Extract coefficients from fitted model_spec
#'
#' @param object A `fitted_model_spec` object
#' @param ... Additional arguments (ignored)
#'
#' @return Named numeric vector of parameter estimates
#' @export
coef.fitted_model_spec <- function(object, ...) {
  object$par
}


#' Extract variance-covariance matrix
#'
#' Compute variance-covariance matrix from the inverse Hessian.
#'
#' @param object A `fitted_model_spec` object
#' @param ... Additional arguments (ignored)
#'
#' @return Variance-covariance matrix
#' @export
vcov.fitted_model_spec <- function(object, ...) {
  if (is.null(object$hessian)) {
    stop("No Hessian available. Refit with hessian = TRUE", call. = FALSE)
  }

  vcov_mat <- tryCatch(
    solve(object$hessian),
    error = function(e) {
      warning("Could not invert Hessian. Model may be poorly identified.")
      matrix(NA, nrow = length(object$par), ncol = length(object$par))
    }
  )

  rownames(vcov_mat) <- names(object$par)
  colnames(vcov_mat) <- names(object$par)
  vcov_mat
}


#' Compute variance-covariance from Fisher information
#'
#' For a model_spec (not necessarily fitted), compute the asymptotic
#' variance-covariance matrix from the inverse Fisher information.
#'
#' @param spec A `model_spec` object
#' @param y Numeric vector of data
#' @param par Parameter values at which to evaluate
#' @param type Type of Fisher information: "observed" or "expected"
#'
#' @return Variance-covariance matrix
#' @export
#'
#' @examples
#' spec <- model_spec_normal()
#' y <- rnorm(100, mean = 5, sd = 2)
#' model_vcov(spec, y, c(mu = 5, sigma = 2))
model_vcov <- function(spec, y, par, type = "observed") {
  info <- fisher_information(spec, y, par, type = type)

  vcov_mat <- tryCatch(
    solve(info$matrix),
    error = function(e) {
      warning("Could not invert Fisher information. Model may be non-identifiable.")
      matrix(NA, nrow = info$n_par, ncol = info$n_par)
    }
  )

  rownames(vcov_mat) <- info$par_names
  colnames(vcov_mat) <- info$par_names
  vcov_mat
}


#' Extract model_spec from fitted R objects
#'
#' Create a model_spec from common fitted model objects.
#'
#' @param object A fitted model object (lm, glm, nls, etc.)
#' @param ... Additional arguments (currently unused)
#'
#' @return A `model_spec` object
#' @export
#'
#' @examples
#' # From linear model
#' fit <- lm(mpg ~ wt, data = mtcars)
#' spec <- model_spec_from_fit(fit)
#' print(spec)
model_spec_from_fit <- function(object, ...) {
  UseMethod("model_spec_from_fit")
}


#' @export
model_spec_from_fit.lm <- function(object, ...) {
  # Extract components
  y <- stats::model.response(stats::model.frame(object))
  X <- stats::model.matrix(object)
  n <- length(y)
  p <- ncol(X)

  # Sanitize parameter names (replace special chars)
  orig_coef_names <- names(stats::coef(object))
  coef_names <- gsub("[^[:alnum:]_]", "_", orig_coef_names)
  coef_names <- make.names(coef_names, unique = TRUE)
  par_names <- c(coef_names, "sigma")

  # Create the loglik function with sanitized names
  loglik_fn <- function(y, ...) {
    args <- list(...)
    beta <- unlist(args[coef_names])
    sigma <- args[["sigma"]]

    mu <- X %*% beta
    sum(stats::dnorm(y, mean = mu, sd = sigma, log = TRUE))
  }
  # Add formal arguments
  formals(loglik_fn) <- c(alist(y =), stats::setNames(rep(list(NULL), length(par_names)), par_names))

  model_spec(
    loglik_fn = loglik_fn,
    par_names = par_names,
    par_bounds = c(
      stats::setNames(rep(list(c(-Inf, Inf)), p), coef_names),
      list(sigma = c(1e-10, Inf))
    ),
    name = paste("LM:", deparse(stats::formula(object)))
  )
}


#' @export
model_spec_from_fit.glm <- function(object, ...) {
  family <- stats::family(object)
  y <- stats::model.response(stats::model.frame(object))
  X <- stats::model.matrix(object)
  n <- length(y)
  p <- ncol(X)

  # Sanitize parameter names
  orig_coef_names <- names(stats::coef(object))
  coef_names <- gsub("[^[:alnum:]_]", "_", orig_coef_names)
  coef_names <- make.names(coef_names, unique = TRUE)

  # Build log-likelihood based on family
  if (family$family == "gaussian") {
    par_names <- c(coef_names, "sigma")

    loglik_fn <- function(y, ...) {
      args <- list(...)
      beta <- unlist(args[coef_names])
      sigma <- args[["sigma"]]

      eta <- X %*% beta
      mu <- family$linkinv(eta)
      sum(stats::dnorm(y, mean = mu, sd = sigma, log = TRUE))
    }
    formals(loglik_fn) <- c(alist(y =), stats::setNames(rep(list(NULL), length(par_names)), par_names))

    model_spec(
      loglik_fn = loglik_fn,
      par_names = par_names,
      par_bounds = c(
        stats::setNames(rep(list(c(-Inf, Inf)), p), coef_names),
        list(sigma = c(1e-10, Inf))
      ),
      name = paste("GLM (gaussian):", deparse(stats::formula(object)))
    )
  } else if (family$family == "binomial") {
    loglik_fn <- function(y, ...) {
      args <- list(...)
      beta <- unlist(args[coef_names])

      eta <- X %*% beta
      prob <- family$linkinv(eta)
      sum(stats::dbinom(y, size = 1, prob = prob, log = TRUE))
    }
    formals(loglik_fn) <- c(alist(y =), stats::setNames(rep(list(NULL), length(coef_names)), coef_names))

    model_spec(
      loglik_fn = loglik_fn,
      par_names = coef_names,
      par_bounds = stats::setNames(rep(list(c(-Inf, Inf)), p), coef_names),
      name = paste("GLM (binomial):", deparse(stats::formula(object)))
    )
  } else if (family$family == "poisson") {
    loglik_fn <- function(y, ...) {
      args <- list(...)
      beta <- unlist(args[coef_names])

      eta <- X %*% beta
      lambda <- family$linkinv(eta)
      sum(stats::dpois(y, lambda = lambda, log = TRUE))
    }
    formals(loglik_fn) <- c(alist(y =), stats::setNames(rep(list(NULL), length(coef_names)), coef_names))

    model_spec(
      loglik_fn = loglik_fn,
      par_names = coef_names,
      par_bounds = stats::setNames(rep(list(c(-Inf, Inf)), p), coef_names),
      name = paste("GLM (poisson):", deparse(stats::formula(object)))
    )
  } else {
    stop("Unsupported GLM family: ", family$family, call. = FALSE)
  }
}


#' @export
model_spec_from_fit.default <- function(object, ...) {
  stop("model_spec_from_fit not implemented for class: ",
       paste(class(object), collapse = ", "), call. = FALSE)
}
