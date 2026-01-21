# Bayesian model comparison
#
# Functions for comparing models using posterior samples from MCMC.

#' Compare posterior distributions
#'
#' Test whether two models produce equivalent posterior predictive distributions
#' using MCMC samples.
#'
#' @param spec_a A `model_spec` object for model A
#' @param spec_b A `model_spec` object for model B
#' @param y Numeric vector of observed data
#' @param samples_a Matrix or data frame of posterior samples for model A.
#'   Rows are samples, columns are parameters (named to match spec_a).
#' @param samples_b Matrix or data frame of posterior samples for model B.
#'   Rows are samples, columns are parameters (named to match spec_b).
#' @param n_compare Number of sample pairs to compare (default 100)
#' @param tol Tolerance for likelihood equivalence
#' @param progress Logical; show progress bar
#'
#' @return An S3 object of class `posterior_comparison` containing:
#' \describe{
#'   \item{prop_matched}{Proportion of A samples that could be matched by B}
#'   \item{discrepancies}{Vector of likelihood discrepancies}
#'   \item{equivalent}{Logical; overall equivalence conclusion}
#'   \item{summary}{Summary statistics of comparison}
#' }
#'
#' @details
#' For each posterior sample from model A, this function finds the closest
#' matching posterior sample from model B (in terms of likelihood) and
#' computes the discrepancy. If models are equivalent, posterior samples
#' from different parameterizations should produce similar likelihoods.
#'
#' This approach does not require the models to have the same parameterization
#' or even the same number of parameters.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Two equivalent models
#' exp_spec <- model_spec(
#'   loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
#'   par_names = "lambda",
#'   par_bounds = list(lambda = c(1e-6, 100)),
#'   name = "Exponential"
#' )
#'
#' gamma_spec <- model_spec(
#'   loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
#'   par_names = "rate",
#'   par_bounds = list(rate = c(1e-6, 100)),
#'   name = "Gamma(1)"
#' )
#'
#' # Simulate "posterior" samples (in practice, use actual MCMC output)
#' set.seed(123)
#' y <- rexp(50, rate = 2)
#'
#' # Simple posterior approximation (replace with real MCMC samples)
#' samples_a <- data.frame(lambda = rgamma(200, shape = 50, rate = 25))
#' samples_b <- data.frame(rate = rgamma(200, shape = 50, rate = 25))
#'
#' result <- compare_posteriors(exp_spec, gamma_spec, y, samples_a, samples_b,
#'                              n_compare = 50)
#' print(result)
#' }
compare_posteriors <- function(spec_a,
                                spec_b,
                                y,
                                samples_a,
                                samples_b,
                                n_compare = 100,
                                tol = 1e-6,
                                progress = interactive()) {
  # Validate inputs
  if (!is_model_spec(spec_a)) {
    stop("`spec_a` must be a model_spec object", call. = FALSE)
  }
  if (!is_model_spec(spec_b)) {
    stop("`spec_b` must be a model_spec object", call. = FALSE)
  }
  check_numeric(y, "y")

  # Convert to matrix if data frame
  samples_a <- as.matrix(samples_a)
  samples_b <- as.matrix(samples_b)

  # Validate samples have correct parameter names
  if (!all(spec_a$par_names %in% colnames(samples_a))) {
    stop("samples_a must have columns matching spec_a parameter names", call. = FALSE)
  }
  if (!all(spec_b$par_names %in% colnames(samples_b))) {
    stop("samples_b must have columns matching spec_b parameter names", call. = FALSE)
  }

  n_a <- nrow(samples_a)
  n_b <- nrow(samples_b)

  # Subsample if needed
  if (n_compare > n_a) n_compare <- n_a
  sample_idx <- sample.int(n_a, n_compare)

  # Pre-compute likelihoods for all B samples
  ll_b <- numeric(n_b)
  for (j in seq_len(n_b)) {
    par_b <- samples_b[j, spec_b$par_names]
    names(par_b) <- spec_b$par_names
    ll_b[j] <- tryCatch(
      loglik(spec_b, y, par_b),
      error = function(e) NA_real_
    )
  }

  # Compare each A sample to B samples
  discrepancies <- numeric(n_compare)
  matched <- logical(n_compare)

  prog <- create_timed_progress(n_compare, show = progress, label = "Comparing")

  for (i in seq_len(n_compare)) {
    idx <- sample_idx[i]
    par_a <- samples_a[idx, spec_a$par_names]
    names(par_a) <- spec_a$par_names

    # Compute likelihood for A sample
    ll_a <- tryCatch(
      loglik(spec_a, y, par_a),
      error = function(e) NA_real_
    )

    if (is.na(ll_a) || !is.finite(ll_a)) {
      discrepancies[i] <- NA
      matched[i] <- FALSE
    } else {
      # Find closest B sample
      diffs <- abs(ll_b - ll_a)
      min_diff <- min(diffs, na.rm = TRUE)
      discrepancies[i] <- min_diff
      matched[i] <- min_diff < tol
    }

    prog$update(i)
  }
  prog$close()

  # Summary statistics
  valid_discrep <- discrepancies[!is.na(discrepancies)]
  prop_matched <- mean(matched, na.rm = TRUE)
  equivalent <- prop_matched > 0.95 && max(valid_discrep) < tol

  structure(
    list(
      prop_matched = prop_matched,
      discrepancies = discrepancies,
      equivalent = equivalent,
      tol = tol,
      n_compare = n_compare,
      summary = list(
        mean_discrepancy = mean(valid_discrep),
        max_discrepancy = max(valid_discrep),
        median_discrepancy = stats::median(valid_discrep),
        n_valid = length(valid_discrep)
      ),
      spec_a = spec_a,
      spec_b = spec_b
    ),
    class = "posterior_comparison"
  )
}

#' @export
print.posterior_comparison <- function(x, ...) {
  cat("<posterior_comparison>\n")
  cat(sprintf("Models: %s vs %s\n", x$spec_a$name, x$spec_b$name))
  cat(sprintf("Samples compared: %d\n", x$n_compare))
  cat(sprintf("Tolerance: %.2e\n\n", x$tol))

  cat("Results:\n")
  cat(sprintf("  Proportion matched: %.1f%%\n", x$prop_matched * 100))
  cat(sprintf("  Mean discrepancy: %.2e\n", x$summary$mean_discrepancy))
  cat(sprintf("  Max discrepancy: %.2e\n", x$summary$max_discrepancy))
  cat(sprintf("  Equivalent: %s\n", x$equivalent))

  invisible(x)
}

#' @export
plot.posterior_comparison <- function(x, ...) {
  old_par <- graphics::par(mar = c(5, 4, 4, 2))
  on.exit(graphics::par(old_par))

  valid_d <- x$discrepancies[!is.na(x$discrepancies)]

  graphics::hist(
    log10(valid_d + 1e-16),
    main = sprintf("Posterior Comparison\n%s vs %s", x$spec_a$name, x$spec_b$name),
    xlab = "log10(likelihood discrepancy)",
    col = "lightblue",
    border = "white"
  )
  graphics::abline(v = log10(x$tol), col = "red", lty = 2, lwd = 2)
  graphics::legend("topright",
                   legend = sprintf("Tolerance (%.0e)", x$tol),
                   col = "red", lty = 2, lwd = 2)

  invisible(x)
}


#' Prior sensitivity analysis
#'
#' Test how different prior specifications affect identifiability and
#' posterior concentration.
#'
#' @param spec A `model_spec` object
#' @param y Numeric vector of observed data
#' @param par Numeric vector of parameter values (point estimate or posterior mean)
#' @param prior_scales Named list of scale factors for each parameter.
#'   Each element should be a numeric vector of scales to test.
#'   Scale 1 means use the original bounds; larger scales mean wider priors.
#'
#' @return An S3 object of class `prior_sensitivity` containing:
#' \describe{
#'   \item{results}{Data frame of identifiability results for each prior setting}
#'   \item{baseline}{Identifiability result with original bounds}
#'   \item{most_informative}{Prior setting with best identifiability}
#' }
#'
#' @details
#' This function examines how the effective prior (parameter bounds) affects
#' the Fisher information and identifiability. Wider bounds (larger scales)
#' represent more diffuse priors, while narrower bounds represent more
#' informative priors.
#'
#' @export
#'
#' @examples
#' spec <- model_spec(
#'   loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
#'   par_names = c("mu", "sigma"),
#'   par_bounds = list(mu = c(-10, 10), sigma = c(0.1, 10)),
#'   name = "Normal"
#' )
#'
#' set.seed(123)
#' y <- rnorm(30, mean = 5, sd = 2)
#'
#' sens <- prior_sensitivity(spec, y, par = c(mu = 5, sigma = 2),
#'   prior_scales = list(mu = c(0.5, 1, 2), sigma = c(0.5, 1, 2)))
#' print(sens)
prior_sensitivity <- function(spec,
                               y,
                               par,
                               prior_scales = NULL) {
  if (!is_model_spec(spec)) {
    stop("`spec` must be a model_spec object", call. = FALSE)
  }
  check_numeric(y, "y")

  # Default scales if not provided
  if (is.null(prior_scales)) {
    prior_scales <- lapply(spec$par_names, function(p) c(0.5, 1, 2))
    names(prior_scales) <- spec$par_names
  }

  # Baseline identifiability
  baseline <- identifiability_check(spec, y, par)

  # Build grid of scale combinations
  scale_grid <- expand.grid(prior_scales, stringsAsFactors = FALSE)
  n_settings <- nrow(scale_grid)

  results <- vector("list", n_settings)

  for (i in seq_len(n_settings)) {
    # Create modified spec with scaled bounds
    new_bounds <- spec$par_bounds
    for (p in spec$par_names) {
      scale <- scale_grid[i, p]
      orig_bounds <- spec$par_bounds[[p]]
      center <- mean(orig_bounds[is.finite(orig_bounds)])
      if (!is.finite(center)) center <- par[p]
      range <- diff(orig_bounds)
      if (!is.finite(range)) range <- abs(par[p]) * 4

      new_bounds[[p]] <- c(
        center - (range / 2) * scale,
        center + (range / 2) * scale
      )
    }

    # Create modified spec
    mod_spec <- model_spec(
      loglik_fn = spec$loglik_fn,
      par_names = spec$par_names,
      par_bounds = new_bounds,
      name = spec$name,
      validate = FALSE
    )

    # Check identifiability
    id_result <- tryCatch(
      identifiability_check(mod_spec, y, par),
      error = function(e) NULL
    )

    if (!is.null(id_result)) {
      # Build result row
      row_data <- data.frame(setting = i, stringsAsFactors = FALSE)
      for (p in spec$par_names) {
        row_data[[p]] <- scale_grid[i, p]
      }
      row_data$condition <- id_result$condition_number
      row_data$rank <- id_result$rank
      row_data$identified <- sum(id_result$classification == "identified")
      results[[i]] <- row_data
    }
  }

  results_df <- do.call(rbind, results[!vapply(results, is.null, logical(1))])

  # Find most informative setting (lowest condition number)
  if (nrow(results_df) > 0) {
    best_idx <- which.min(results_df$condition)
    most_informative <- results_df[best_idx, ]
  } else {
    most_informative <- NULL
  }

  structure(
    list(
      results = results_df,
      baseline = baseline,
      most_informative = most_informative,
      prior_scales = prior_scales,
      spec = spec
    ),
    class = "prior_sensitivity"
  )
}

#' @export
print.prior_sensitivity <- function(x, ...) {
  cat("<prior_sensitivity>\n")
  cat(sprintf("Model: %s\n", x$spec$name))
  cat(sprintf("Parameters: %s\n", paste(x$spec$par_names, collapse = ", ")))
  cat(sprintf("Settings tested: %d\n\n", nrow(x$results)))

  cat("Baseline (scale = 1):\n")
  cat(sprintf("  Condition number: %.2e\n", x$baseline$condition_number))
  cat(sprintf("  Rank: %d / %d\n", x$baseline$rank, length(x$spec$par_names)))

  if (!is.null(x$most_informative)) {
    cat("\nMost informative prior:\n")
    scales <- x$most_informative[x$spec$par_names]
    cat(sprintf("  Scales: %s\n",
                paste(names(scales), "=", scales, collapse = ", ")))
    cat(sprintf("  Condition number: %.2e\n", x$most_informative$condition))
  }

  cat("\nAll results:\n")
  print(x$results, row.names = FALSE)

  invisible(x)
}


#' Extract posterior samples from fitted objects
#'
#' Helper function to extract posterior samples from common Bayesian
#' modeling packages.
#'
#' @param fit A fitted model object from rstan, brms, rstanarm, or similar
#' @param pars Optional character vector of parameter names to extract
#'
#' @return A matrix of posterior samples (rows = samples, columns = parameters)
#'
#' @details
#' This function provides a unified interface for extracting posterior samples
#' from various Bayesian modeling packages. Currently supports:
#' - stanfit objects (rstan)
#' - brmsfit objects (brms)
#' - stanreg objects (rstanarm)
#' - matrix/data.frame (returned as-is)
#'
#' @export
#'
#' @examples
#' # With a matrix of samples (manual specification)
#' samples <- matrix(rnorm(200), ncol = 2)
#' colnames(samples) <- c("mu", "sigma")
#' extracted <- extract_samples(samples)
extract_samples <- function(fit, pars = NULL) {
  # Already a matrix or data frame
  if (is.matrix(fit) || is.data.frame(fit)) {
    samples <- as.matrix(fit)
    if (!is.null(pars)) {
      samples <- samples[, pars, drop = FALSE]
    }
    return(samples)
  }

  # stanfit object (rstan)
  if (inherits(fit, "stanfit")) {
    if (!requireNamespace("rstan", quietly = TRUE)) {
      stop("Package 'rstan' required to extract samples from stanfit objects",
           call. = FALSE)
    }
    samples <- rstan::extract(fit, pars = pars, permuted = TRUE)
    # Convert list to matrix
    samples <- do.call(cbind, samples)
    return(samples)
  }

  # brmsfit object (brms)
  if (inherits(fit, "brmsfit")) {
    if (!requireNamespace("brms", quietly = TRUE)) {
      stop("Package 'brms' required to extract samples from brmsfit objects",
           call. = FALSE)
    }
    samples <- brms::as_draws_matrix(fit)
    if (!is.null(pars)) {
      # brms uses different naming; try to match
      available <- colnames(samples)
      matched <- available[available %in% pars |
                           grepl(paste(pars, collapse = "|"), available)]
      samples <- samples[, matched, drop = FALSE]
    }
    return(as.matrix(samples))
  }

  # stanreg object (rstanarm)
  if (inherits(fit, "stanreg")) {
    if (!requireNamespace("rstanarm", quietly = TRUE)) {
      stop("Package 'rstanarm' required to extract samples from stanreg objects",
           call. = FALSE)
    }
    samples <- as.matrix(fit, pars = pars)
    return(samples)
  }

  # mcmc object (coda)
  if (inherits(fit, "mcmc") || inherits(fit, "mcmc.list")) {
    samples <- as.matrix(fit)
    if (!is.null(pars)) {
      samples <- samples[, pars, drop = FALSE]
    }
    return(samples)
  }

  stop("Unsupported fit object class: ", class(fit)[1], call. = FALSE)
}
