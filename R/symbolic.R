# Symbolic analysis tools
#
# Analytical Fisher information and reparameterization detection.

#' Symbolic Fisher information
#'
#' Compute Fisher information using closed-form analytical formulas for
#' standard distributions. Falls back to numerical computation if no
#' analytical formula is available.
#'
#' @param spec A `model_spec` object
#' @param y Numeric vector of observed data (used for numerical fallback and n)
#' @param par Named numeric vector of parameter values
#' @param method Character; "auto" tries analytical first, "analytical" requires
#'   closed-form, "numerical" forces numerical computation
#'
#' @return A `fisher_info` object containing the Fisher information matrix
#'
#' @details
#' Analytical formulas are available for:
#' - Normal (mu, sigma): I = diag(n/sigma^2, 2n/sigma^2)
#' - Exponential (rate): I = n/rate^2
#' - Poisson (lambda): I = n/lambda
#' - Gamma (shape, rate): Closed-form involving digamma/trigamma
#' - Binomial (prob): I = n/(prob * (1-prob))
#' - Beta (shape1, shape2): Closed-form involving trigamma
#' - Log-normal (mu, sigma): Similar to normal on log scale
#'
#' The function attempts to detect the distribution from the model name.
#'
#' @export
#'
#' @examples
#' spec <- model_spec_normal()
#' y <- rnorm(100, mean = 5, sd = 2)
#'
#' # Analytical computation
#' info <- symbolic_fisher(spec, y, par = c(mu = 5, sigma = 2))
#' print(info)
symbolic_fisher <- function(spec,
                             y,
                             par,
                             method = c("auto", "analytical", "numerical")) {
  if (!is_model_spec(spec)) {
    stop("`spec` must be a model_spec object", call. = FALSE)
  }

  method <- match.arg(method)
  n <- length(y)

  # Try to detect distribution from name
  dist <- detect_distribution(spec$name)

  if (method == "numerical") {
    return(fisher_information(spec, y, par))
  }

  # Try analytical computation
  info_mat <- NULL

  if (!is.null(dist)) {
    info_mat <- tryCatch(
      compute_analytical_fisher(dist, par, n),
      error = function(e) NULL
    )
  }

  if (is.null(info_mat)) {
    if (method == "analytical") {
      stop("No analytical formula available for this distribution", call. = FALSE)
    }
    # Fallback to numerical
    return(fisher_information(spec, y, par))
  }

  # Build fisher_info object
  rownames(info_mat) <- spec$par_names
  colnames(info_mat) <- spec$par_names

  eigendecomp <- eigen(info_mat, symmetric = TRUE)

  # Compute rank and condition
  tol <- .Machine$double.eps * max(dim(info_mat)) * max(eigendecomp$values)
  rank <- sum(eigendecomp$values > tol)
  condition <- if (min(eigendecomp$values) > tol) {
    max(eigendecomp$values) / min(eigendecomp$values)
  } else {
    Inf
  }

  structure(
    list(
      matrix = info_mat,
      eigenvalues = eigendecomp$values,
      eigenvectors = eigendecomp$vectors,
      par_names = spec$par_names,
      par = par,
      n_par = length(par),
      rank = rank,
      condition = condition,
      type = "analytical",
      distribution = dist
    ),
    class = "fisher_info"
  )
}

#' Detect distribution from model name
#' @noRd
detect_distribution <- function(name) {
  name_lower <- tolower(name)

  # Patterns ordered by specificity (more specific first)
  patterns <- c(
    lognormal = "lognormal|log-normal|log_normal",
    normal = "normal|gaussian",
    exponential = "exponential|exp",
    poisson = "poisson",
    gamma = "gamma",
    binomial = "binomial|binom",
    beta = "beta",
    weibull = "weibull"
  )

  for (dist in names(patterns)) {
    if (grepl(patterns[dist], name_lower)) {
      # Exclusions for ambiguous patterns
      if (dist == "beta" && grepl("negbin|negative", name_lower)) next
      return(dist)
    }
  }

  NULL
}

#' Compute analytical Fisher information for known distributions
#' @noRd
compute_analytical_fisher <- function(dist, par, n) {
  switch(dist,
    normal = {
      mu <- par["mu"]
      sigma <- par["sigma"]
      if (is.na(sigma)) sigma <- sqrt(par["var"])
      matrix(c(n / sigma^2, 0, 0, 2 * n / sigma^2), nrow = 2)
    },
    exponential = {
      rate <- par["rate"]
      if (is.na(rate)) rate <- par["lambda"]
      matrix(n / rate^2, nrow = 1)
    },
    poisson = {
      lambda <- par["lambda"]
      if (is.na(lambda)) lambda <- par["rate"]
      matrix(n / lambda, nrow = 1)
    },
    gamma = {
      shape <- par["shape"]
      rate <- par["rate"]
      # Fisher info for gamma(shape, rate)
      # I_11 = n * trigamma(shape)
      # I_22 = n * shape / rate^2
      # I_12 = -n / rate
      I_11 <- n * trigamma(shape)
      I_22 <- n * shape / rate^2
      I_12 <- -n / rate
      matrix(c(I_11, I_12, I_12, I_22), nrow = 2)
    },
    binomial = {
      prob <- par["prob"]
      if (is.na(prob)) prob <- par["p"]
      matrix(n / (prob * (1 - prob)), nrow = 1)
    },
    beta = {
      a <- par["shape1"]
      b <- par["shape2"]
      if (is.na(a)) a <- par["alpha"]
      if (is.na(b)) b <- par["beta"]
      # Fisher info for beta(a, b)
      tri_sum <- trigamma(a + b)
      I_11 <- n * (trigamma(a) - tri_sum)
      I_22 <- n * (trigamma(b) - tri_sum)
      I_12 <- -n * tri_sum
      matrix(c(I_11, I_12, I_12, I_22), nrow = 2)
    },
    lognormal = {
      mu <- par["mu"]
      sigma <- par["sigma"]
      # Same as normal on the log scale
      matrix(c(n / sigma^2, 0, 0, 2 * n / sigma^2), nrow = 2)
    },
    weibull = {
      # Weibull Fisher info is complex; return NULL to trigger fallback
      NULL
    },
    NULL
  )
}


#' Detect linear reparameterization
#'
#' Check whether two model specifications are related by a linear
#' reparameterization (affine transformation of parameters).
#'
#' @param spec_a First `model_spec` object
#' @param spec_b Second `model_spec` object
#' @param y Numeric vector of observed data
#' @param n_test Number of parameter points to test (default 50)
#' @param tol Tolerance for detecting linear relationship
#'
#' @return An S3 object of class `linear_reparam` containing:
#' \describe{
#'   \item{is_linear}{Logical; whether a linear relationship was detected}
#'   \item{transformation}{If linear, the estimated transformation matrix and offset}
#'   \item{r_squared}{R-squared value of the linear fit}
#'   \item{residuals}{Residuals from the linear fit}
#' }
#'
#' @details
#' This function tests whether there exists a linear transformation
#' theta_B = A * theta_A + b such that the two models produce the same
#' likelihood for all data.
#'
#' Examples of linear reparameterizations:
#' - Normal(mu, sigma) vs Normal(mu, sigma^2): not linear (square transformation)
#' - Exponential(rate) vs Exponential(mean): linear (mean = 1/rate is not linear)
#' - Location-scale: linear in location, but log-linear in scale
#'
#' @export
#'
#' @examples
#' # Two models with different parameterizations
#' exp_spec <- model_spec(
#'   loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
#'   par_names = "lambda",
#'   par_bounds = list(lambda = c(0.1, 10)),
#'   name = "Exponential"
#' )
#'
#' gamma_spec <- model_spec(
#'   loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
#'   par_names = "rate",
#'   par_bounds = list(rate = c(0.1, 10)),
#'   name = "Gamma(1)"
#' )
#'
#' set.seed(123)
#' y <- rexp(50, rate = 2)
#'
#' result <- detect_linear_reparam(exp_spec, gamma_spec, y)
#' print(result)
detect_linear_reparam <- function(spec_a,
                                   spec_b,
                                   y,
                                   n_test = 50,
                                   tol = 1e-4) {
  if (!is_model_spec(spec_a) || !is_model_spec(spec_b)) {
    stop("Both spec_a and spec_b must be model_spec objects", call. = FALSE)
  }

  # Sample parameter space for A
  bounds_a <- propose_bounds(spec_a, y)
  bounds_b <- propose_bounds(spec_b, y)

  samples_a <- sample_par_space(bounds_a, n_test, method = "lhs")

  # For each A sample, find matching B parameters
  par_pairs <- vector("list", n_test)
  valid <- logical(n_test)

  for (i in seq_len(n_test)) {
    par_a <- samples_a[i, ]
    names(par_a) <- colnames(samples_a)

    ll_a <- tryCatch(
      loglik(spec_a, y, par_a),
      error = function(e) NA
    )

    if (is.na(ll_a) || !is.finite(ll_a)) {
      valid[i] <- FALSE
      next
    }

    # Find matching B parameters
    match_result <- find_equivalent_par(spec_b, y, ll_a, bounds_b)

    if (match_result$discrepancy < tol) {
      par_pairs[[i]] <- list(a = par_a, b = match_result$par)
      valid[i] <- TRUE
    } else {
      valid[i] <- FALSE
    }
  }

  # Extract valid pairs
  valid_pairs <- par_pairs[valid]
  n_valid <- length(valid_pairs)

  if (n_valid < 5) {
    return(structure(
      list(
        is_linear = FALSE,
        reason = "Insufficient valid parameter pairs found",
        n_valid = n_valid
      ),
      class = "linear_reparam"
    ))
  }

  # Build matrices for regression
  A_mat <- do.call(rbind, lapply(valid_pairs, function(p) p$a))
  B_mat <- do.call(rbind, lapply(valid_pairs, function(p) p$b))

  # Test for linear relationship: B = A * M + c
  # Use linear regression for each B parameter
  n_par_a <- ncol(A_mat)
  n_par_b <- ncol(B_mat)

  # Fit linear model: each column of B ~ columns of A
  fits <- vector("list", n_par_b)
  r_squared <- numeric(n_par_b)

  A_df <- as.data.frame(A_mat)
  colnames(A_df) <- paste0("a", seq_len(n_par_a))

  for (j in seq_len(n_par_b)) {
    b_j <- B_mat[, j]
    fit <- stats::lm(b_j ~ ., data = A_df)
    fits[[j]] <- fit
    r_squared[j] <- summary(fit)$r.squared
  }

  # Check if all relationships are approximately linear
  is_linear <- all(r_squared > 0.99)

  # Extract transformation
  if (is_linear) {
    # Build transformation matrix
    trans_mat <- matrix(0, nrow = n_par_b, ncol = n_par_a)
    offset <- numeric(n_par_b)

    for (j in seq_len(n_par_b)) {
      coefs <- stats::coef(fits[[j]])
      offset[j] <- coefs[1]  # Intercept
      trans_mat[j, ] <- coefs[-1]  # Slopes
    }

    transformation <- list(
      matrix = trans_mat,
      offset = offset,
      par_names_a = spec_a$par_names,
      par_names_b = spec_b$par_names
    )
  } else {
    transformation <- NULL
  }

  structure(
    list(
      is_linear = is_linear,
      transformation = transformation,
      r_squared = r_squared,
      n_valid = n_valid,
      spec_a = spec_a,
      spec_b = spec_b
    ),
    class = "linear_reparam"
  )
}

#' @export
print.linear_reparam <- function(x, ...) {
  cat("<linear_reparam>\n")

  if (!is.null(x$spec_a)) {
    cat(sprintf("Models: %s -> %s\n", x$spec_a$name, x$spec_b$name))
  }

  cat(sprintf("Valid pairs tested: %d\n", x$n_valid))
  cat(sprintf("Linear relationship: %s\n\n", x$is_linear))

  if (x$is_linear && !is.null(x$transformation)) {
    cat("Transformation: B = A * M + c\n")
    cat("\nMatrix M:\n")
    print(round(x$transformation$matrix, 4))
    cat("\nOffset c:\n")
    print(round(x$transformation$offset, 4))
  }

  cat("\nR-squared for each B parameter:\n")
  print(round(x$r_squared, 4))

  invisible(x)
}


#' Check for exact parameter equivalence
#'
#' Test whether two models are exactly equivalent with a direct
#' parameter mapping (identity or simple transformation).
#'
#' @param spec_a First `model_spec` object
#' @param spec_b Second `model_spec` object
#' @param y Numeric vector of observed data
#' @param transformations List of candidate transformations to test.
#'   Each element should be a function that takes parameters from A
#'   and returns parameters for B.
#'
#' @return An S3 object indicating whether exact equivalence was found
#'   and which transformation achieves it.
#'
#' @export
#'
#' @examples
#' exp_spec <- model_spec(
#'   loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
#'   par_names = "lambda",
#'   par_bounds = list(lambda = c(0.1, 10)),
#'   name = "Exponential"
#' )
#'
#' gamma_spec <- model_spec(
#'   loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
#'   par_names = "rate",
#'   par_bounds = list(rate = c(0.1, 10)),
#'   name = "Gamma(1)"
#' )
#'
#' set.seed(123)
#' y <- rexp(50, rate = 2)
#'
#' # Test identity transformation (lambda -> rate directly)
#' result <- check_exact_equivalence(exp_spec, gamma_spec, y,
#'   transformations = list(
#'     identity = function(p) c(rate = p["lambda"])
#'   ))
#' print(result)
check_exact_equivalence <- function(spec_a,
                                     spec_b,
                                     y,
                                     transformations) {
  if (!is_model_spec(spec_a) || !is_model_spec(spec_b)) {
    stop("Both spec_a and spec_b must be model_spec objects", call. = FALSE)
  }

  # Test each transformation
  results <- vector("list", length(transformations))
  names(results) <- names(transformations)

  bounds_a <- propose_bounds(spec_a, y)
  test_points <- sample_par_space(bounds_a, 20, method = "lhs")

  for (trans_name in names(transformations)) {
    trans_fn <- transformations[[trans_name]]
    discrepancies <- numeric(nrow(test_points))

    for (i in seq_len(nrow(test_points))) {
      par_a <- test_points[i, ]
      names(par_a) <- colnames(test_points)

      # Apply transformation
      par_b <- tryCatch(
        trans_fn(par_a),
        error = function(e) NULL
      )

      if (is.null(par_b)) {
        discrepancies[i] <- Inf
        next
      }

      # Compute likelihoods
      ll_a <- tryCatch(loglik(spec_a, y, par_a), error = function(e) NA)
      ll_b <- tryCatch(loglik(spec_b, y, par_b), error = function(e) NA)

      if (is.na(ll_a) || is.na(ll_b)) {
        discrepancies[i] <- NA
      } else {
        discrepancies[i] <- abs(ll_a - ll_b)
      }
    }

    valid_d <- discrepancies[is.finite(discrepancies)]
    results[[trans_name]] <- list(
      max_discrepancy = max(valid_d, na.rm = TRUE),
      mean_discrepancy = mean(valid_d, na.rm = TRUE),
      is_exact = max(valid_d, na.rm = TRUE) < 1e-10
    )
  }

  # Find best transformation
  exact_trans <- names(results)[vapply(results, function(r) r$is_exact, logical(1))]

  structure(
    list(
      results = results,
      exact_transformations = exact_trans,
      has_exact = length(exact_trans) > 0,
      spec_a = spec_a,
      spec_b = spec_b
    ),
    class = "exact_equiv"
  )
}

#' @export
print.exact_equiv <- function(x, ...) {
  cat("<exact_equiv>\n")
  cat(sprintf("Models: %s -> %s\n", x$spec_a$name, x$spec_b$name))
  cat(sprintf("Transformations tested: %d\n\n", length(x$results)))

  if (x$has_exact) {
    cat("Exact equivalence found with:\n")
    for (trans in x$exact_transformations) {
      cat(sprintf("  - %s\n", trans))
    }
  } else {
    cat("No exact equivalence found\n")
  }

  cat("\nTransformation results:\n")
  for (name in names(x$results)) {
    r <- x$results[[name]]
    cat(sprintf("  %s: max_discrepancy = %.2e %s\n",
                name, r$max_discrepancy,
                if (r$is_exact) "(EXACT)" else ""))
  }

  invisible(x)
}
