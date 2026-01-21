#' Check model identifiability
#'
#' Perform comprehensive identifiability analysis for a model specification.
#' Combines Fisher information analysis with profile likelihood diagnostics.
#'
#' @param spec A `model_spec` object
#' @param y Numeric vector of observed data
#' @param par Named numeric vector of parameter values. If NULL, attempts to
#'   find MLE first.
#' @param level Analysis level: "local" uses Fisher information only,
#'   "profile" adds profile likelihood analysis
#' @param threshold Eigenvalue threshold for non-identifiability (default 0.01)
#' @param verbose Logical; print progress information
#' @param cl Optional parallel cluster from `setup_cluster()`. If provided,
#'   profile likelihood computations run in parallel.
#'
#' @return An S3 object of class `identifiability_result` containing:
#' \describe{
#'   \item{status}{Named character vector of identifiability status per parameter}
#'   \item{fisher_info}{The Fisher information analysis}
#'   \item{identified_functions}{Character vector of identified parameter combinations}
#'   \item{non_identified}{Character vector of non-identified directions}
#'   \item{condition}{Condition number}
#'   \item{rank}{Numerical rank}
#'   \item{recommendations}{Suggested actions}
#' }
#'
#' @export
#'
#' @examples
#' # Well-identified model
#' norm_spec <- model_spec(
#'   loglik_fn = function(y, mu, sigma) {
#'     sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
#'   },
#'   par_names = c("mu", "sigma"),
#'   par_bounds = list(sigma = c(1e-6, Inf))
#' )
#'
#' set.seed(123)
#' y <- rnorm(100, mean = 5, sd = 2)
#' result <- identifiability_check(norm_spec, y, par = c(mu = 5, sigma = 2))
#' print(result)
#'
#' # Non-identifiable model
#' nonid_spec <- model_spec(
#'   loglik_fn = function(y, a, b) {
#'     sum(dnorm(y, mean = a + b, sd = 1, log = TRUE))
#'   },
#'   par_names = c("a", "b")
#' )
#'
#' result2 <- identifiability_check(nonid_spec, y, par = c(a = 2, b = 3))
#' print(result2)
identifiability_check <- function(spec,
                                  y,
                                  par = NULL,
                                  level = c("local", "profile"),
                                  threshold = 0.01,
                                  verbose = FALSE,
                                  cl = NULL) {
  # Validate inputs
  if (!is_model_spec(spec)) {
    stop("`spec` must be a model_spec object", call. = FALSE)
  }
  check_numeric(y, "y")
  level <- match.arg(level)

  # If par not provided, try to find MLE
  if (is.null(par)) {
    if (verbose) cat("Finding MLE...\n")
    par <- find_mle(spec, y)
  }

  if (is.list(par)) par <- unlist(par)

  # Compute Fisher information
  if (verbose) cat("Computing Fisher information...\n")
  info <- fisher_information(spec, y, par)

  # Classify parameters based on eigenvalue analysis
  status <- classify_identifiability(info, threshold)

  # Find identified functions
  identified_fns <- find_identified_functions(info, threshold)

  # Find non-identified directions
  non_id_dirs <- describe_null_directions(info, threshold)

  # Generate recommendations
  recommendations <- generate_recommendations(info, status, threshold)

  # Profile likelihood analysis if requested
  profile_results <- NULL
  if (level == "profile") {
    if (verbose) cat("Computing profile likelihoods...\n")
    profile_results <- profile_all(spec, y, par, verbose = verbose, cl = cl)
  }

  structure(
    list(
      status = status,
      fisher_info = info,
      identified_functions = identified_fns,
      non_identified = non_id_dirs,
      condition = info$condition,
      rank = info$rank,
      n_par = info$n_par,
      par_names = info$par_names,
      par = par,
      recommendations = recommendations,
      profile_results = profile_results
    ),
    class = "identifiability_result"
  )
}

#' Find MLE for a model
#' @noRd
find_mle <- function(spec, y) {
  bounds <- propose_bounds(spec, y)

  start <- vapply(bounds, mean, numeric(1))
  names(start) <- spec$par_names

  lower <- vapply(bounds, `[`, numeric(1), 1)
  upper <- vapply(bounds, `[`, numeric(1), 2)

  objective <- function(par) {
    names(par) <- spec$par_names
    -loglik(spec, y, par)
  }

  result <- tryCatch(
    stats::optim(
      par = start,
      fn = objective,
      method = if (length(start) == 1) "Brent" else "L-BFGS-B",
      lower = lower,
      upper = upper
    ),
    error = function(e) list(par = start)
  )

  mle <- result$par
  names(mle) <- spec$par_names
  mle
}

#' Classify parameter identifiability
#' @noRd
classify_identifiability <- function(info, threshold) {
  # Get eigenvalue contributions for each parameter
  eig_vals <- info$eigenvalues
  eig_vecs <- info$eigenvectors
  n_par <- info$n_par

  status <- character(n_par)
  names(status) <- info$par_names

  # Precompute invariants outside loop
  eig_vals_pos <- pmax(eig_vals, 0)
  max_abs_eig <- max(abs(eig_vals))
  null_threshold <- threshold * max_abs_eig
  null_idx <- which(abs(eig_vals) < null_threshold)
  has_null <- length(null_idx) > 0
  is_rank_deficient <- info$rank < n_par
  is_poorly_conditioned <- info$condition > 1000
  min_eig_idx <- which.min(abs(eig_vals))

  # For each parameter, check if it contributes to near-zero eigenvalues
  for (i in seq_len(n_par)) {
    # Get the parameter's loading on each eigenvector
    loadings <- abs(eig_vecs[i, ])

    # Check if parameter is dominant in any null direction
    if (has_null) {
      null_loading <- max(loadings[null_idx])
      if (null_loading > 0.3) {
        status[i] <- "non_identifiable"
        next
      }
    }

    # Classify based on condition number contribution
    if (is_rank_deficient) {
      # Model is rank deficient
      status[i] <- "non_identifiable"
    } else if (is_poorly_conditioned) {
      # Check if this parameter is poorly conditioned
      if (loadings[min_eig_idx] > 0.3) {
        status[i] <- "weakly_identified"
      } else {
        status[i] <- "identified"
      }
    } else {
      status[i] <- "identified"
    }
  }

  status
}

#' Find identified parameter functions
#' @noRd
find_identified_functions <- function(info, threshold) {
  eig_vals <- info$eigenvalues
  eig_vecs <- info$eigenvectors

  # Find eigenvectors with large eigenvalues (well-identified directions)
  good_idx <- which(abs(eig_vals) > threshold * max(abs(eig_vals)))

  if (length(good_idx) == 0) {
    return(character(0))
  }

  vapply(good_idx, function(idx) {
    format_eigenvector(eig_vecs[, idx], info$par_names)
  }, character(1))
}

#' Format eigenvector as human-readable linear combination
#' @noRd
format_eigenvector <- function(vec, par_names, coef_threshold = 0.1) {
  terms <- character(0)
  for (j in seq_along(vec)) {
    if (abs(vec[j]) > coef_threshold) {
      coef <- round(vec[j], 2)
      if (coef > 0 && length(terms) > 0) {
        terms <- c(terms, sprintf("+ %.2f*%s", coef, par_names[j]))
      } else {
        terms <- c(terms, sprintf("%.2f*%s", coef, par_names[j]))
      }
    }
  }
  paste(terms, collapse = " ")
}

#' Describe null directions
#' @noRd
describe_null_directions <- function(info, threshold) {
  null_vecs <- null_directions(info, tol = threshold * max(abs(info$eigenvalues)))

  if (is.null(null_vecs)) {
    return(character(0))
  }

  vapply(seq_len(ncol(null_vecs)), function(k) {
    format_eigenvector(null_vecs[, k], info$par_names)
  }, character(1))
}

#' Generate recommendations
#' @noRd
generate_recommendations <- function(info, status, threshold) {
  recs <- list()

  if (info$rank < info$n_par) {
    recs$status <- "non_identifiable"
    recs$message <- "Model has structurally non-identifiable parameters"
    recs$action <- "Consider reparameterizing to remove redundant parameters"
  } else if (info$condition > 10000) {
    recs$status <- "poorly_conditioned"
    recs$message <- "Model is very poorly conditioned"
    recs$action <- "Some parameters may be practically non-identifiable"
  } else if (info$condition > 1000) {
    recs$status <- "weak_identification"
    recs$message <- "Some parameters may be weakly identified"
    recs$action <- "Consider collecting more data or simplifying the model"
  } else if (info$condition > 100) {
    recs$status <- "moderate"
    recs$message <- "Moderate conditioning - check individual parameters"
    recs$action <- "Review eigenvalue decomposition for potential issues"
  } else {
    recs$status <- "good"
    recs$message <- "Model appears well-identified"
    recs$action <- "No action needed"
  }

  non_id <- names(status)[status == "non_identifiable"]
  if (length(non_id) > 0) {
    recs$non_identifiable_params <- non_id
  }

  weak <- names(status)[status == "weakly_identified"]
  if (length(weak) > 0) {
    recs$weakly_identified_params <- weak
  }

  recs
}

#' Compute profile likelihood for all parameters
#' @noRd
profile_all <- function(spec, y, par, n_points = 20, verbose = FALSE, cl = NULL) {
  par_names <- spec$par_names

  # Function to profile one parameter
  profile_one <- function(p) {
    profile_likelihood(spec, y, par, p, n_points)
  }

  # Run profiling (parallel if cluster provided)
  if (!is.null(cl)) {
    parallel::clusterExport(cl, c("spec", "y", "par", "n_points"),
                            envir = environment())
    result_list <- parallel::parLapply(cl, par_names, profile_one)
  } else {
    result_list <- vector("list", length(par_names))
    for (i in seq_along(par_names)) {
      if (verbose) cat(sprintf("  Profiling %s...\n", par_names[i]))
      result_list[[i]] <- profile_one(par_names[i])
    }
  }

  names(result_list) <- par_names
  result_list
}

#' Compute profile likelihood for one parameter
#'
#' @param spec A model_spec object
#' @param y Data
#' @param par Starting parameter values
#' @param which_par Name of parameter to profile
#' @param n_points Number of points in profile
#'
#' @return Data frame with profile likelihood values
#' @export
profile_likelihood <- function(spec, y, par, which_par, n_points = 20) {
  if (!which_par %in% spec$par_names) {
    stop(sprintf("Parameter '%s' not found in model", which_par), call. = FALSE)
  }

  # Get bounds for the parameter
  bounds <- spec$par_bounds[[which_par]]
  eff_bounds <- effective_bounds(bounds)

  # Center around current value
  center <- par[which_par]
  width <- min(abs(center - eff_bounds[1]), abs(eff_bounds[2] - center), abs(center) + 1)
  grid <- seq(center - width * 0.8, center + width * 0.8, length.out = n_points)
  grid <- grid[grid > eff_bounds[1] & grid < eff_bounds[2]]

  # Other parameters to optimize over
  other_pars <- setdiff(spec$par_names, which_par)

  profile_ll <- numeric(length(grid))

  for (i in seq_along(grid)) {
    fixed_val <- grid[i]

    if (length(other_pars) == 0) {
      # Single parameter model
      test_par <- fixed_val
      names(test_par) <- which_par
      profile_ll[i] <- tryCatch(
        loglik(spec, y, test_par),
        error = function(e) NA_real_
      )
    } else {
      # Optimize over other parameters
      other_bounds <- spec$par_bounds[other_pars]
      other_eff <- lapply(other_bounds, effective_bounds)

      start <- par[other_pars]
      lower <- vapply(other_eff, `[`, numeric(1), 1)
      upper <- vapply(other_eff, `[`, numeric(1), 2)

      objective <- function(theta) {
        full_par <- c(fixed_val, theta)
        names(full_par) <- c(which_par, other_pars)
        -loglik(spec, y, full_par)
      }

      opt_result <- tryCatch(
        stats::optim(
          par = start,
          fn = objective,
          method = if (length(other_pars) == 1) "Brent" else "L-BFGS-B",
          lower = lower,
          upper = upper
        ),
        error = function(e) list(value = Inf)
      )

      profile_ll[i] <- -opt_result$value
    }
  }

  data.frame(
    parameter = which_par,
    value = grid,
    loglik = profile_ll,
    stringsAsFactors = FALSE
  )
}

#' Compute profile likelihood confidence intervals
#'
#' Extract confidence intervals from profile likelihood curves using the
#' likelihood ratio test criterion.
#'
#' @param spec A `model_spec` object
#' @param y Numeric vector of observed data
#' @param par Named numeric vector of parameter values (typically MLE)
#' @param level Confidence level (default 0.95)
#' @param n_points Number of points for profile likelihood (default 50)
#' @param which_par Optional character vector of parameter names to compute CIs for.
#'   If NULL (default), computes CIs for all parameters.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{parameter}{Parameter name}
#'   \item{estimate}{Point estimate (from `par`)}
#'   \item{lower}{Lower confidence bound}
#'   \item{upper}{Upper confidence bound}
#'   \item{level}{Confidence level}
#' }
#'
#' @details
#' The confidence interval is based on the likelihood ratio test. For a given
#' confidence level, the interval includes all parameter values where the
#' profile log-likelihood is within `qchisq(level, df=1)/2` of the maximum.
#'
#' @export
#'
#' @examples
#' spec <- model_spec(
#'   loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
#'   par_names = c("mu", "sigma"),
#'   par_bounds = list(sigma = c(1e-6, Inf))
#' )
#' set.seed(123)
#' y <- rnorm(100, mean = 5, sd = 2)
#' ci <- profile_ci(spec, y, par = c(mu = 5, sigma = 2))
#' print(ci)
profile_ci <- function(spec, y, par, level = 0.95, n_points = 50, which_par = NULL) {
  if (!is_model_spec(spec)) {
    stop("`spec` must be a model_spec object", call. = FALSE)
  }

  if (is.null(which_par)) {
    which_par <- spec$par_names
  }

  # Chi-squared threshold for likelihood ratio
  threshold <- stats::qchisq(level, df = 1) / 2

  results <- vector("list", length(which_par))

  for (i in seq_along(which_par)) {
    p <- which_par[i]

    # Compute profile likelihood
    profile <- profile_likelihood(spec, y, par, p, n_points)

    # Normalize to max = 0
    ll <- profile$loglik
    ll_norm <- ll - max(ll, na.rm = TRUE)

    # Find where profile crosses threshold
    vals <- profile$value
    above_threshold <- which(ll_norm >= -threshold)

    if (length(above_threshold) == 0) {
      # Profile is too flat or didn't capture the CI
      lower <- NA_real_
      upper <- NA_real_
    } else {
      # Approximate CI bounds by interpolation
      lower <- find_ci_bound(vals, ll_norm, -threshold, "lower")
      upper <- find_ci_bound(vals, ll_norm, -threshold, "upper")
    }

    results[[i]] <- data.frame(
      parameter = p,
      estimate = par[p],
      lower = lower,
      upper = upper,
      level = level,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, results)
}

#' Find CI bound by interpolation
#' @noRd
find_ci_bound <- function(vals, ll_norm, threshold, side = c("lower", "upper")) {
  side <- match.arg(side)

  # Find where profile crosses threshold
  above <- ll_norm >= threshold
  n <- length(vals)

  if (side == "lower") {
    # Find first crossing from below
    for (i in seq_len(n - 1)) {
      if (!above[i] && above[i + 1]) {
        # Linear interpolation
        x1 <- vals[i]
        x2 <- vals[i + 1]
        y1 <- ll_norm[i]
        y2 <- ll_norm[i + 1]
        return(x1 + (threshold - y1) * (x2 - x1) / (y2 - y1))
      }
    }
    # If no crossing found, return the minimum value if above threshold
    if (above[1]) return(vals[1])
    return(NA_real_)
  } else {
    # Find last crossing from above
    for (i in seq(n - 1, 1)) {
      if (above[i] && !above[i + 1]) {
        # Linear interpolation
        x1 <- vals[i]
        x2 <- vals[i + 1]
        y1 <- ll_norm[i]
        y2 <- ll_norm[i + 1]
        return(x1 + (threshold - y1) * (x2 - x1) / (y2 - y1))
      }
    }
    # If no crossing found, return the maximum value if above threshold
    if (above[n]) return(vals[n])
    return(NA_real_)
  }
}

#' S3 method for confint
#'
#' Compute confidence intervals for model parameters using profile likelihood.
#'
#' @param object A `model_spec` object
#' @param parm Parameter names (character vector). If missing, all parameters.
#' @param level Confidence level (default 0.95)
#' @param y Numeric vector of observed data
#' @param par Named numeric vector of parameter values (typically MLE)
#' @param ... Additional arguments (ignored)
#'
#' @return A matrix with columns for lower and upper bounds
#' @export
#'
#' @examples
#' spec <- model_spec(
#'   loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
#'   par_names = c("mu", "sigma"),
#'   par_bounds = list(sigma = c(1e-6, Inf))
#' )
#' set.seed(123)
#' y <- rnorm(100, mean = 5, sd = 2)
#' confint(spec, y = y, par = c(mu = 5, sigma = 2))
confint.model_spec <- function(object, parm, level = 0.95, y, par, ...) {
  if (missing(y)) {
    stop("`y` (data) must be provided", call. = FALSE)
  }
  if (missing(par)) {
    stop("`par` (parameter values) must be provided", call. = FALSE)
  }

  if (missing(parm)) {
    parm <- object$par_names
  }

  ci <- profile_ci(object, y, par, level = level, which_par = parm)

  # Format as matrix like stats::confint
  result <- cbind(ci$lower, ci$upper)
  rownames(result) <- ci$parameter
  pct <- format_pct((1 - level) / 2)
  colnames(result) <- c(pct[1], pct[2])

  result
}

#' Format percentages for CI column names
#' @noRd
format_pct <- function(p) {
  paste0(format(100 * c(p, 1 - p), trim = TRUE, digits = 3), " %")
}

#' @export
print.identifiability_result <- function(x, ...) {
  cat("<identifiability_result>\n")

  # Overall status
  if (x$rank < x$n_par) {
    cat("Overall: Model has NON-IDENTIFIABLE parameters\n\n")
  } else if (x$condition > 1000) {
    cat("Overall: Model has POORLY IDENTIFIED parameters\n\n")
  } else {
    cat("Overall: Model appears WELL-IDENTIFIED\n\n")
  }

  cat("Parameter status:\n")
  for (p in x$par_names) {
    status_str <- switch(x$status[p],
      "identified" = "identified",
      "weakly_identified" = "WEAKLY IDENTIFIED",
      "non_identifiable" = "NON-IDENTIFIABLE",
      x$status[p]
    )
    cat(sprintf("  %s: %s\n", p, status_str))
  }

  if (length(x$non_identified) > 0) {
    cat("\nNon-identified directions:\n")
    for (d in x$non_identified) {
      cat(sprintf("  %s\n", d))
    }
  }

  if (length(x$identified_functions) > 0 && x$rank < x$n_par) {
    cat("\nIdentified functions:\n")
    for (f in x$identified_functions) {
      cat(sprintf("  %s\n", f))
    }
  }

  cat(sprintf("\nCondition number: %.2g\n", x$condition))
  cat(sprintf("Rank: %d / %d\n", x$rank, x$n_par))

  invisible(x)
}

#' @export
summary.identifiability_result <- function(object, ...) {
  cat("Identifiability Analysis Summary\n")
  cat(strrep("=", 50), "\n\n")

  cat("Model:", object$n_par, "parameters\n")
  cat("Evaluated at:", paste(sprintf("%s=%.3g", object$par_names, object$par),
                             collapse = ", "), "\n\n")

  # Status summary
  cat("Parameter Status:\n")
  for (p in object$par_names) {
    cat(sprintf("  %-15s %s\n", p, toupper(object$status[p])))
  }

  cat("\n")
  summary(object$fisher_info)

  if (!is.null(object$recommendations)) {
    cat("\nRecommendation:\n")
    cat("  ", object$recommendations$message, "\n")
    cat("  Action:", object$recommendations$action, "\n")
  }

  invisible(object)
}

#' @export
format.identifiability_result <- function(x, ...) {
  n_nonid <- sum(x$status == "non_identifiable")
  n_weak <- sum(x$status == "weakly_identified")

  if (n_nonid > 0) {
    sprintf("<identifiability_result> %d non-identifiable, %d weak",
            n_nonid, n_weak)
  } else if (n_weak > 0) {
    sprintf("<identifiability_result> %d weakly identified", n_weak)
  } else {
    "<identifiability_result> all identified"
  }
}

#' Test if object is an identifiability_result
#'
#' @param x Object to test
#'
#' @return Logical
#' @export
is_identifiability_result <- function(x) {
  inherits(x, "identifiability_result")
}
