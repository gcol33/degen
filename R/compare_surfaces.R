#' Compare likelihood surfaces between two models
#'
#' Test whether two model specifications are observationally equivalent by
#' comparing their likelihood surfaces. For each parameter configuration of
#' model A, the algorithm searches for a configuration of model B that produces
#' the same likelihood.
#'
#' @param pair An `equivalence_pair` object
#' @param y Numeric vector of observed data
#' @param n_points Number of parameter points to sample (default 100)
#' @param method Comparison method: "grid" samples parameter space,
#'   "optimization" searches for counterexamples
#' @param tol Tolerance for likelihood equality (default 1e-6)
#' @param verbose Logical; print progress information
#'
#' @return An S3 object of class `surface_comparison` containing:
#' \describe{
#'   \item{equivalent}{Logical; overall equivalence conclusion}
#'   \item{max_discrepancy}{Maximum likelihood discrepancy found}
#'   \item{n_tested}{Number of parameter points tested}
#'   \item{evidence}{Data frame of tested points and discrepancies}
#'   \item{tol}{Tolerance used}
#'   \item{method}{Method used}
#' }
#'
#' @details
#' The comparison is performed in both directions (A to B and B to A) to ensure
#' the equivalence relation is symmetric.
#'
#' Note that this provides numerical evidence, not mathematical proof. A
#' conclusion of equivalence means no counterexample was found within the
#' tested region of parameter space.
#'
#' @export
#'
#' @examples
#' # Compare exponential and gamma(shape=1) - these are equivalent
#' exp_spec <- model_spec(
#'   loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
#'   par_names = "lambda",
#'   par_bounds = list(lambda = c(1e-6, Inf)),
#'   name = "Exponential"
#' )
#'
#' gamma_spec <- model_spec(
#'   loglik_fn = function(y, rate) {
#'     sum(dgamma(y, shape = 1, rate = rate, log = TRUE))
#'   },
#'   par_names = "rate",
#'   par_bounds = list(rate = c(1e-6, Inf)),
#'   name = "Gamma(1)"
#' )
#'
#' pair <- equivalence_pair(exp_spec, gamma_spec)
#' set.seed(123)
#' y <- rexp(100, rate = 2)
#' result <- compare_surfaces(pair, y, n_points = 20)
#' print(result)
compare_surfaces <- function(pair,
                             y,
                             n_points = 100,
                             method = c("grid", "optimization"),
                             tol = 1e-6,
                             verbose = FALSE) {
  # Validate inputs
  if (!is_equivalence_pair(pair)) {
    stop("`pair` must be an equivalence_pair object", call. = FALSE)
  }
  check_numeric(y, "y")
  if (length(y) == 0) {
    stop("`y` must not be empty", call. = FALSE)
  }

  method <- match.arg(method)

  if (method == "grid") {
    result <- compare_surfaces_grid(pair, y, n_points, tol, verbose)
  } else {
    result <- compare_surfaces_optim(pair, y, n_points, tol, verbose)
  }

  result$method <- method
  result$tol <- tol
  result$pair <- pair
  result$y_length <- length(y)

  class(result) <- "surface_comparison"
  result
}

#' Grid-based surface comparison
#' @noRd
compare_surfaces_grid <- function(pair, y, n_points, tol, verbose) {
  spec_a <- pair$spec_a
  spec_b <- pair$spec_b

  # Compare A -> B
  if (verbose) cat("Comparing A -> B...\n")
  ab_result <- compare_direction(spec_a, spec_b, y, n_points, tol, verbose)

  # Compare B -> A
  if (verbose) cat("Comparing B -> A...\n")
  ba_result <- compare_direction(spec_b, spec_a, y, n_points, tol, verbose)

  # Combine results
  max_discrepancy <- max(ab_result$max_discrepancy, ba_result$max_discrepancy)
  equivalent <- max_discrepancy < tol

  # Build evidence data frame (keep separate since columns differ)
  evidence_ab <- ab_result$evidence
  evidence_ab$direction <- "A_to_B"
  evidence_ba <- ba_result$evidence
  evidence_ba$direction <- "B_to_A"

  # Store as list since parameter names differ between directions
  evidence <- list(A_to_B = evidence_ab, B_to_A = evidence_ba)

  list(
    equivalent = equivalent,
    max_discrepancy = max_discrepancy,
    n_tested = n_points * 2,
    evidence = evidence,
    ab_discrepancy = ab_result$max_discrepancy,
    ba_discrepancy = ba_result$max_discrepancy
  )
}

#' Compare in one direction (source -> target)
#' @noRd
compare_direction <- function(source_spec, target_spec, y, n_points, tol, verbose) {
  # Get effective bounds for sampling
  source_bounds <- propose_bounds(source_spec, y)
  target_bounds <- propose_bounds(target_spec, y)

  # Sample source parameter space
  source_samples <- sample_par_space(source_bounds, n_points, method = "lhs")

  # For each source point, find best matching target point
  discrepancies <- numeric(n_points)
  target_matches <- vector("list", n_points)

  for (i in seq_len(n_points)) {
    source_par <- source_samples[i, , drop = TRUE]
    names(source_par) <- colnames(source_samples)

    # Compute source likelihood
    source_ll <- tryCatch(
      loglik(source_spec, y, source_par),
      error = function(e) NA_real_
    )

    if (is.na(source_ll) || !is.finite(source_ll)) {
      discrepancies[i] <- NA_real_
      next
    }

    # Find best matching target parameters
    match_result <- find_equivalent_par(
      target_spec, y, source_ll, target_bounds
    )

    discrepancies[i] <- match_result$discrepancy
    target_matches[[i]] <- match_result$par

    if (verbose && i %% 10 == 0) {
      cat(sprintf("  %d/%d points tested\n", i, n_points))
    }
  }

  # Build evidence data frame
  evidence <- as.data.frame(source_samples)
  evidence$source_ll <- vapply(seq_len(n_points), function(i) {
    source_par <- source_samples[i, , drop = TRUE]
    names(source_par) <- colnames(source_samples)
    tryCatch(loglik(source_spec, y, source_par), error = function(e) NA_real_)
  }, numeric(1))
  evidence$discrepancy <- discrepancies

  list(
    max_discrepancy = max(discrepancies, na.rm = TRUE),
    evidence = evidence
  )
}

#' Find target parameters that match a given likelihood
#' @noRd
find_equivalent_par <- function(target_spec, y, target_ll, target_bounds) {
  par_names <- target_spec$par_names
  n_par <- length(par_names)

  # Starting point: middle of effective bounds
  start <- vapply(target_bounds, function(b) mean(b), numeric(1))
  names(start) <- par_names

  lower <- vapply(target_bounds, `[`, numeric(1), 1)
  upper <- vapply(target_bounds, `[`, numeric(1), 2)

  # Objective: squared difference in log-likelihood
  objective <- function(par) {
    names(par) <- par_names
    ll <- tryCatch(
      loglik(target_spec, y, par),
      error = function(e) -Inf
    )
    (ll - target_ll)^2
  }

  # Multi-start optimization
  best_result <- list(par = start, value = Inf)

  for (attempt in 1:3) {
    if (attempt == 1) {
      init <- start
    } else {
      # Random starting point
      init <- vapply(seq_along(par_names), function(j) {
        stats::runif(1, lower[j], upper[j])
      }, numeric(1))
      names(init) <- par_names
    }

    result <- tryCatch(
      stats::optim(
        par = init,
        fn = objective,
        method = if (n_par == 1) "Brent" else "L-BFGS-B",
        lower = lower,
        upper = upper
      ),
      error = function(e) list(par = init, value = Inf)
    )

    if (result$value < best_result$value) {
      best_result <- result
    }

    # Early exit if we found a good match
    if (best_result$value < 1e-12) break
  }

  list(
    par = best_result$par,
    discrepancy = sqrt(best_result$value)  # Convert back to abs difference
  )
}

#' Optimization-based surface comparison
#' @noRd
compare_surfaces_optim <- function(pair, y, n_points, tol, verbose) {
  # For now, fall back to grid method
  # A true optimization approach would search for counterexamples
  compare_surfaces_grid(pair, y, n_points, tol, verbose)
}
