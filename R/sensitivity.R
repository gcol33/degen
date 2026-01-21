# Sensitivity analysis for equivalence testing
#
# Functions to assess robustness of equivalence conclusions.

#' Sensitivity analysis for equivalence testing
#'
#' Test how sensitive equivalence conclusions are to the choice of tolerance
#' and number of grid points.
#'
#' @param pair An `equivalence_pair` object
#' @param y Numeric vector of observed data
#' @param tol_range Numeric vector of tolerance values to test
#' @param n_points_range Integer vector of n_points values to test
#' @param method Comparison method: "grid" or "optimization"
#' @param progress Logical; show progress bar
#'
#' @return An S3 object of class `sensitivity_result` containing:
#' \describe{
#'   \item{results}{Data frame with columns: tol, n_points, equivalent, max_discrepancy}
#'   \item{pair}{The equivalence pair tested}
#'   \item{stable}{Logical; whether conclusion is stable across all settings}
#'   \item{critical_tol}{Smallest tolerance at which models appear equivalent}
#' }
#'
#' @export
#'
#' @examples
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
#' pair <- equivalence_pair(exp_spec, gamma_spec)
#' set.seed(123)
#' y <- rexp(50, rate = 2)
#'
#' # Test sensitivity to tolerance and grid size
#' sens <- sensitivity_analysis(pair, y,
#'   tol_range = c(1e-8, 1e-6, 1e-4),
#'   n_points_range = c(20, 50))
#' print(sens)
sensitivity_analysis <- function(pair,
                                  y,
                                  tol_range = c(1e-8, 1e-6, 1e-4, 1e-2),
                                  n_points_range = c(20, 50, 100),
                                  method = c("grid", "optimization"),
                                  progress = interactive()) {
  if (!is_equivalence_pair(pair)) {
    stop("`pair` must be an equivalence_pair object", call. = FALSE)
  }
  check_numeric(y, "y")
  method <- match.arg(method)

  # Build grid of settings to test
  settings <- expand.grid(
    tol = tol_range,
    n_points = n_points_range,
    stringsAsFactors = FALSE
  )
  n_settings <- nrow(settings)

  # Run comparisons
  results <- vector("list", n_settings)
  prog <- create_timed_progress(n_settings, show = progress, label = "Sensitivity")

  for (i in seq_len(n_settings)) {
    result <- compare_surfaces(
      pair, y,
      n_points = settings$n_points[i],
      method = method,
      tol = settings$tol[i],
      progress = FALSE
    )

    results[[i]] <- data.frame(
      tol = settings$tol[i],
      n_points = settings$n_points[i],
      equivalent = result$equivalent,
      max_discrepancy = result$max_discrepancy
    )

    prog$update(i)
  }
  prog$close()

  results_df <- do.call(rbind, results)

  # Determine stability
  all_equiv <- all(results_df$equivalent)
  none_equiv <- !any(results_df$equivalent)
  stable <- all_equiv || none_equiv

  # Find critical tolerance
  if (all_equiv) {
    critical_tol <- min(tol_range)
  } else if (none_equiv) {
    critical_tol <- NA
  } else {
    # Find smallest tol where equivalent is TRUE
    equiv_tols <- results_df$tol[results_df$equivalent]
    critical_tol <- if (length(equiv_tols) > 0) min(equiv_tols) else NA
  }

  structure(
    list(
      results = results_df,
      pair = pair,
      stable = stable,
      critical_tol = critical_tol,
      method = method
    ),
    class = "sensitivity_result"
  )
}

#' @export
print.sensitivity_result <- function(x, ...) {
  cat("<sensitivity_result>\n")
  cat(sprintf("Models: %s vs %s\n", x$pair$spec_a$name, x$pair$spec_b$name))
  cat(sprintf("Settings tested: %d\n", nrow(x$results)))
  cat(sprintf("Method: %s\n\n", x$method))

  if (x$stable) {
    if (all(x$results$equivalent)) {
      cat("Conclusion: STABLE - Models appear equivalent across all settings\n")
    } else {
      cat("Conclusion: STABLE - Models appear non-equivalent across all settings\n")
    }
  } else {
    cat("Conclusion: UNSTABLE - Equivalence depends on tolerance/grid settings\n")
    cat(sprintf("Critical tolerance: %.2e\n", x$critical_tol))
  }

  cat("\nResults:\n")
  print(x$results, row.names = FALSE)

  invisible(x)
}

#' @export
plot.sensitivity_result <- function(x, ...) {
  results <- x$results

  # Create heatmap-style visualization
  tols <- sort(unique(results$tol))
  n_pts <- sort(unique(results$n_points))

  # Pre-compute indices for each result row
  row_indices <- match(results$tol, tols)
  col_indices <- match(results$n_points, n_pts)

  # Build matrix
  mat <- matrix(NA, nrow = length(tols), ncol = length(n_pts))
  rownames(mat) <- format(tols, scientific = TRUE)
  colnames(mat) <- as.character(n_pts)

  for (i in seq_len(nrow(results))) {
    mat[row_indices[i], col_indices[i]] <- log10(results$max_discrepancy[i] + 1e-16)
  }

  # Plot
  old_par <- graphics::par(mar = c(5, 6, 4, 2))
  on.exit(graphics::par(old_par))

  graphics::image(
    x = seq_along(n_pts),
    y = seq_along(tols),
    z = t(mat),
    col = grDevices::hcl.colors(20, "YlOrRd", rev = TRUE),
    xlab = "n_points",
    ylab = "tolerance",
    axes = FALSE,
    main = "Sensitivity Analysis\n(log10 max discrepancy)"
  )

  graphics::axis(1, at = seq_along(n_pts), labels = n_pts)
  graphics::axis(2, at = seq_along(tols), labels = format(tols, scientific = TRUE), las = 2)

  # Add equivalence indicators
  for (i in seq_len(nrow(results))) {
    symbol <- if (results$equivalent[i]) "=" else "X"
    col <- if (results$equivalent[i]) "darkgreen" else "darkred"
    cex <- if (results$equivalent[i]) 1.5 else 1.2
    graphics::text(col_indices[i], row_indices[i], symbol, col = col, cex = cex, font = 2)
  }

  invisible(x)
}


#' Bootstrap test for equivalence
#'
#' Use bootstrap resampling to assess the reliability of an equivalence
#' conclusion. This helps determine whether the conclusion is robust to
#' sampling variability.
#'
#' @param pair An `equivalence_pair` object
#' @param y Numeric vector of observed data
#' @param n_boot Number of bootstrap replicates (default 100)
#' @param n_points Number of parameter points per comparison
#' @param tol Tolerance for equivalence
#' @param method Comparison method
#' @param progress Logical; show progress bar
#'
#' @return An S3 object of class `bootstrap_equiv` containing:
#' \describe{
#'   \item{prop_equivalent}{Proportion of bootstrap samples showing equivalence}
#'   \item{boot_discrepancies}{Vector of max discrepancies from each replicate}
#'   \item{original_result}{Result from original (non-bootstrapped) data}
#'   \item{ci_discrepancy}{95% CI for max discrepancy}
#'   \item{n_boot}{Number of bootstrap replicates}
#' }
#'
#' @details
#' For each bootstrap replicate, the data are resampled with replacement and
#' the equivalence test is re-run. The proportion of replicates showing
#' equivalence provides a measure of confidence in the original conclusion.
#'
#' @export
#'
#' @examples
#' \donttest{
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
#' pair <- equivalence_pair(exp_spec, gamma_spec)
#' set.seed(123)
#' y <- rexp(50, rate = 2)
#'
#' # Bootstrap test (use more replicates in practice)
#' boot <- bootstrap_equivalence(pair, y, n_boot = 20, n_points = 20)
#' print(boot)
#' }
bootstrap_equivalence <- function(pair,
                                   y,
                                   n_boot = 100,
                                   n_points = 50,
                                   tol = 1e-6,
                                   method = c("grid", "optimization"),
                                   progress = interactive()) {
  if (!is_equivalence_pair(pair)) {
    stop("`pair` must be an equivalence_pair object", call. = FALSE)
  }
  check_numeric(y, "y")
  method <- match.arg(method)
  n <- length(y)

 # Original comparison
  original <- compare_surfaces(pair, y, n_points = n_points,
                               method = method, tol = tol, progress = FALSE)

  # Bootstrap replicates
  boot_equiv <- logical(n_boot)
  boot_discrep <- numeric(n_boot)

  prog <- create_timed_progress(n_boot, show = progress, label = "Bootstrap")

  for (b in seq_len(n_boot)) {
    # Resample with replacement
    y_boot <- y[sample.int(n, n, replace = TRUE)]

    # Run comparison
    result <- compare_surfaces(pair, y_boot, n_points = n_points,
                               method = method, tol = tol, progress = FALSE)

    boot_equiv[b] <- result$equivalent
    boot_discrep[b] <- result$max_discrepancy

    prog$update(b)
  }
  prog$close()

  # Compute summary statistics
  prop_equiv <- mean(boot_equiv)
  ci_discrep <- stats::quantile(boot_discrep, probs = c(0.025, 0.975))

  structure(
    list(
      prop_equivalent = prop_equiv,
      boot_discrepancies = boot_discrep,
      boot_equivalent = boot_equiv,
      original_result = original,
      ci_discrepancy = ci_discrep,
      n_boot = n_boot,
      tol = tol,
      pair = pair
    ),
    class = "bootstrap_equiv"
  )
}

#' @export
print.bootstrap_equiv <- function(x, ...) {
  cat("<bootstrap_equiv>\n")
  cat(sprintf("Models: %s vs %s\n", x$pair$spec_a$name, x$pair$spec_b$name))
  cat(sprintf("Bootstrap replicates: %d\n", x$n_boot))
  cat(sprintf("Tolerance: %.2e\n\n", x$tol))

  cat("Original data result:\n")
  cat(sprintf("  Equivalent: %s\n", x$original_result$equivalent))
  cat(sprintf("  Max discrepancy: %.2e\n\n", x$original_result$max_discrepancy))

  cat("Bootstrap results:\n")
  cat(sprintf("  Proportion equivalent: %.1f%%\n", x$prop_equivalent * 100))
  cat(sprintf("  95%% CI for max discrepancy: [%.2e, %.2e]\n",
              x$ci_discrepancy[1], x$ci_discrepancy[2]))

  # Interpretation
  cat("\nInterpretation:\n")
  if (x$prop_equivalent > 0.95) {
    cat("  Strong evidence for equivalence (>95% of replicates)\n")
  } else if (x$prop_equivalent > 0.80) {
    cat("  Moderate evidence for equivalence (80-95% of replicates)\n")
  } else if (x$prop_equivalent > 0.50) {
    cat("  Weak/inconclusive evidence (50-80% of replicates)\n")
  } else if (x$prop_equivalent > 0.20) {
    cat("  Moderate evidence against equivalence (20-50% of replicates)\n")
  } else {
    cat("  Strong evidence against equivalence (<20% of replicates)\n")
  }

  invisible(x)
}

#' @export
plot.bootstrap_equiv <- function(x, ...) {
  old_par <- graphics::par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))
  on.exit(graphics::par(old_par))

  # Histogram of discrepancies
  graphics::hist(
    log10(x$boot_discrepancies + 1e-16),
    main = "Bootstrap Distribution\nof Max Discrepancy",
    xlab = "log10(max discrepancy)",
    col = "lightblue",
    border = "white"
  )
  graphics::abline(v = log10(x$tol), col = "red", lty = 2, lwd = 2)
  graphics::abline(v = log10(x$original_result$max_discrepancy), col = "blue", lwd = 2)
  graphics::legend("topright",
                   legend = c("Tolerance", "Original"),
                   col = c("red", "blue"),
                   lty = c(2, 1), lwd = 2, cex = 0.8)

  # Equivalence proportion
  props <- c(x$prop_equivalent, 1 - x$prop_equivalent)
  graphics::pie(
    props,
    labels = c(sprintf("Equiv\n%.0f%%", props[1] * 100),
               sprintf("Non-equiv\n%.0f%%", props[2] * 100)),
    col = c("lightgreen", "lightcoral"),
    main = "Bootstrap Equivalence\nDecisions"
  )

  invisible(x)
}


#' Cross-validation test for equivalence
#'
#' Split data into training and test sets to assess whether equivalence
#' generalizes to held-out data.
#'
#' @param pair An `equivalence_pair` object
#' @param y Numeric vector of observed data
#' @param n_folds Number of cross-validation folds (default 5)
#' @param n_points Number of parameter points per comparison
#' @param tol Tolerance for equivalence
#' @param method Comparison method
#' @param progress Logical; show progress bar
#'
#' @return An S3 object of class `cv_equiv` containing:
#' \describe{
#'   \item{fold_results}{Data frame with results for each fold}
#'   \item{prop_equivalent}{Proportion of folds showing equivalence}
#'   \item{mean_discrepancy}{Mean max discrepancy across folds}
#'   \item{n_folds}{Number of folds}
#' }
#'
#' @details
#' For each fold, a portion of the data is held out and the equivalence
#' test is performed on the remaining data. Consistent results across
#' folds suggest the conclusion generalizes.
#'
#' @export
#'
#' @examples
#' \donttest{
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
#' pair <- equivalence_pair(exp_spec, gamma_spec)
#' set.seed(123)
#' y <- rexp(100, rate = 2)
#'
#' # Cross-validation test
#' cv <- cross_validate_equivalence(pair, y, n_folds = 5, n_points = 20)
#' print(cv)
#' }
cross_validate_equivalence <- function(pair,
                                        y,
                                        n_folds = 5,
                                        n_points = 50,
                                        tol = 1e-6,
                                        method = c("grid", "optimization"),
                                        progress = interactive()) {
  if (!is_equivalence_pair(pair)) {
    stop("`pair` must be an equivalence_pair object", call. = FALSE)
  }
  check_numeric(y, "y")
  method <- match.arg(method)
  n <- length(y)

  if (n < n_folds * 10) {
    warning("Small sample size for cross-validation; results may be unstable",
            call. = FALSE)
  }

  # Create fold indices
  fold_ids <- rep(seq_len(n_folds), length.out = n)
  fold_ids <- sample(fold_ids)  # Shuffle

  # Run comparison for each fold
  fold_results <- vector("list", n_folds)
  prog <- create_timed_progress(n_folds, show = progress, label = "CV")

  for (k in seq_len(n_folds)) {
    # Training data (exclude fold k)
    y_train <- y[fold_ids != k]

    # Run comparison on training data
    result <- compare_surfaces(pair, y_train, n_points = n_points,
                               method = method, tol = tol, progress = FALSE)

    fold_results[[k]] <- data.frame(
      fold = k,
      n_train = length(y_train),
      equivalent = result$equivalent,
      max_discrepancy = result$max_discrepancy
    )

    prog$update(k)
  }
  prog$close()

  results_df <- do.call(rbind, fold_results)

  structure(
    list(
      fold_results = results_df,
      prop_equivalent = mean(results_df$equivalent),
      mean_discrepancy = mean(results_df$max_discrepancy),
      sd_discrepancy = stats::sd(results_df$max_discrepancy),
      n_folds = n_folds,
      tol = tol,
      pair = pair
    ),
    class = "cv_equiv"
  )
}

#' @export
print.cv_equiv <- function(x, ...) {
  cat("<cv_equiv>\n")
  cat(sprintf("Models: %s vs %s\n", x$pair$spec_a$name, x$pair$spec_b$name))
  cat(sprintf("Folds: %d\n", x$n_folds))
  cat(sprintf("Tolerance: %.2e\n\n", x$tol))

  cat("Cross-validation results:\n")
  cat(sprintf("  Proportion equivalent: %.0f%% (%d/%d folds)\n",
              x$prop_equivalent * 100,
              sum(x$fold_results$equivalent),
              x$n_folds))
  cat(sprintf("  Mean max discrepancy: %.2e (SD: %.2e)\n",
              x$mean_discrepancy, x$sd_discrepancy))

  cat("\nPer-fold results:\n")
  print(x$fold_results, row.names = FALSE)

  # Interpretation
  cat("\nInterpretation:\n")
  if (x$prop_equivalent == 1) {
    cat("  Equivalence consistent across all folds\n")
  } else if (x$prop_equivalent == 0) {
    cat("  Non-equivalence consistent across all folds\n")
  } else {
    cat("  Inconsistent results - conclusion may depend on data subset\n")
  }

  invisible(x)
}

#' @export
plot.cv_equiv <- function(x, ...) {
  old_par <- graphics::par(mar = c(5, 4, 4, 2))
  on.exit(graphics::par(old_par))

  results <- x$fold_results
  cols <- ifelse(results$equivalent, "lightgreen", "lightcoral")

  graphics::barplot(
    log10(results$max_discrepancy + 1e-16),
    names.arg = paste("Fold", results$fold),
    col = cols,
    border = "white",
    main = "Cross-Validation Results",
    ylab = "log10(max discrepancy)",
    las = 2
  )

  graphics::abline(h = log10(x$tol), col = "red", lty = 2, lwd = 2)
  graphics::legend("topright",
                   legend = c("Equivalent", "Non-equivalent", "Tolerance"),
                   fill = c("lightgreen", "lightcoral", NA),
                   border = c("white", "white", NA),
                   lty = c(NA, NA, 2),
                   col = c(NA, NA, "red"),
                   lwd = c(NA, NA, 2),
                   cex = 0.8)

  invisible(x)
}
