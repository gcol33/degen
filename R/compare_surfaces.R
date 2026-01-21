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
#' @param progress Logical; show progress bar with ETA (default TRUE in
#'   interactive sessions). Set to FALSE to suppress progress output.
#' @param cl Optional parallel cluster from `setup_cluster()`. If provided,
#'   grid points are evaluated in parallel.
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
                             verbose = FALSE,
                             progress = interactive(),
                             cl = NULL) {
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
    result <- compare_surfaces_grid(pair, y, n_points, tol, verbose, progress, cl)
  } else {
    result <- compare_surfaces_optim(pair, y, n_points, tol, verbose, progress, cl)
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
compare_surfaces_grid <- function(pair, y, n_points, tol, verbose, progress, cl = NULL) {
  spec_a <- pair$spec_a
  spec_b <- pair$spec_b

  # Compare A -> B
  if (verbose) cat("Comparing A -> B...\n")
  ab_result <- compare_direction(spec_a, spec_b, y, n_points, tol, verbose, progress, cl, "A->B")

  # Compare B -> A
  if (verbose) cat("Comparing B -> A...\n")
  ba_result <- compare_direction(spec_b, spec_a, y, n_points, tol, verbose, progress, cl, "B->A")

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
compare_direction <- function(source_spec, target_spec, y, n_points, tol, verbose, progress, cl = NULL, label = "") {
  # Get effective bounds for sampling
  source_bounds <- propose_bounds(source_spec, y)
  target_bounds <- propose_bounds(target_spec, y)

  # Sample source parameter space
  source_samples <- sample_par_space(source_bounds, n_points, method = "lhs")

  # Define worker function for a single point

  eval_single_point <- function(i) {
    source_par <- source_samples[i, , drop = TRUE]
    names(source_par) <- colnames(source_samples)

    # Compute source likelihood
    source_ll <- tryCatch(
      loglik(source_spec, y, source_par),
      error = function(e) NA_real_
    )

    if (is.na(source_ll) || !is.finite(source_ll)) {
      return(list(source_ll = source_ll, discrepancy = NA_real_, par = NULL))
    }

    # Find best matching target parameters
    match_result <- find_equivalent_par(
      target_spec, y, source_ll, target_bounds
    )

    list(
      source_ll = source_ll,
      discrepancy = match_result$discrepancy,
      par = match_result$par
    )
  }

  # Run in parallel or sequential
  if (!is.null(cl)) {
    # Export required objects to cluster
    parallel::clusterExport(cl, c("source_samples", "source_spec", "target_spec",
                                   "y", "target_bounds"),
                            envir = environment())
    results <- parallel::parLapply(cl, seq_len(n_points), eval_single_point)
  } else {
    results <- vector("list", n_points)
    prog <- create_timed_progress(n_points, show = progress, label = label)

    for (i in seq_len(n_points)) {
      results[[i]] <- eval_single_point(i)
      prog$update(i)

      if (verbose && i %% 10 == 0) {
        prog$message(sprintf("  %d/%d points tested", i, n_points))
      }
    }
    prog$close()
  }

  # Extract results
  source_lls <- vapply(results, function(r) r$source_ll, numeric(1))
  discrepancies <- vapply(results, function(r) r$discrepancy, numeric(1))

  # Build evidence data frame
  evidence <- as.data.frame(source_samples)
  evidence$source_ll <- source_lls
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
#'
#' Searches for counterexamples by maximizing the discrepancy between models.
#' This is more efficient than grid search when parameter spaces are large.
#' @noRd
compare_surfaces_optim <- function(pair, y, n_points, tol, verbose, progress, cl = NULL) {
  spec_a <- pair$spec_a
  spec_b <- pair$spec_b

  # Compare A -> B (find worst-case point in A's space)
  if (verbose) cat("Searching for counterexamples A -> B...\n")
  ab_result <- find_max_discrepancy(spec_a, spec_b, y, n_points, tol, verbose, progress, cl, "A->B")

  # Compare B -> A
  if (verbose) cat("Searching for counterexamples B -> A...\n")
  ba_result <- find_max_discrepancy(spec_b, spec_a, y, n_points, tol, verbose, progress, cl, "B->A")

  # Combine results
  max_discrepancy <- max(ab_result$max_discrepancy, ba_result$max_discrepancy)
  equivalent <- max_discrepancy < tol

  # Build evidence
  evidence_ab <- ab_result$evidence
  evidence_ab$direction <- "A_to_B"
  evidence_ba <- ba_result$evidence
  evidence_ba$direction <- "B_to_A"

  evidence <- list(A_to_B = evidence_ab, B_to_A = evidence_ba)

  list(
    equivalent = equivalent,
    max_discrepancy = max_discrepancy,
    n_tested = ab_result$n_tested + ba_result$n_tested,
    evidence = evidence,
    ab_discrepancy = ab_result$max_discrepancy,
    ba_discrepancy = ba_result$max_discrepancy
  )
}

#' Find maximum discrepancy between source and target models
#' @noRd
find_max_discrepancy <- function(source_spec, target_spec, y, n_starts, tol, verbose, progress, cl = NULL, label = "") {
  source_bounds <- propose_bounds(source_spec, y)
  target_bounds <- propose_bounds(target_spec, y)

  source_lower <- vapply(source_bounds, `[`, numeric(1), 1)
  source_upper <- vapply(source_bounds, `[`, numeric(1), 2)

  # Generate starting points using LHS
  start_samples <- sample_par_space(source_bounds, n_starts, method = "lhs")

  # Objective: for a given source_par, find minimum discrepancy to target
  # We want to MAXIMIZE this (find worst-case point)
  discrepancy_fn <- function(source_par) {
    names(source_par) <- source_spec$par_names

    source_ll <- tryCatch(
      loglik(source_spec, y, source_par),
      error = function(e) NA_real_
    )

    if (is.na(source_ll) || !is.finite(source_ll)) {
      return(0)  # Invalid point, not a counterexample
    }

    # Find best matching target
    match_result <- find_equivalent_par(target_spec, y, source_ll, target_bounds)
    match_result$discrepancy
  }

  # Worker function for a single starting point
  eval_single_start <- function(i) {
    start <- start_samples[i, , drop = TRUE]
    names(start) <- colnames(start_samples)

    # Try to maximize discrepancy from this starting point
    result <- tryCatch(
      stats::optim(
        par = start,
        fn = function(p) -discrepancy_fn(p),  # Negate for maximization
        method = if (length(start) == 1) "Brent" else "L-BFGS-B",
        lower = source_lower,
        upper = source_upper,
        control = list(maxit = 50)
      ),
      error = function(e) list(par = start, value = 0)
    )

    list(par = result$par, discrepancy = -result$value)
  }

  # Run in parallel or sequential
  if (!is.null(cl)) {
    # Export required objects to cluster
    parallel::clusterExport(cl, c("start_samples", "source_spec", "target_spec",
                                   "y", "source_lower", "source_upper",
                                   "target_bounds", "discrepancy_fn"),
                            envir = environment())
    tested_points <- parallel::parLapply(cl, seq_len(n_starts), eval_single_start)
  } else {
    tested_points <- vector("list", n_starts)
    best_so_far <- 0
    prog <- create_timed_progress(n_starts, show = progress, label = label)

    for (i in seq_len(n_starts)) {
      tested_points[[i]] <- eval_single_start(i)

      if (tested_points[[i]]$discrepancy > best_so_far) {
        best_so_far <- tested_points[[i]]$discrepancy
      }

      prog$update(i)

      # Early exit if we found a clear counterexample
      if (best_so_far > tol * 10) {
        if (verbose) prog$message(sprintf("  Found counterexample at iteration %d", i))
        tested_points <- tested_points[seq_len(i)]
        prog$close()
        break
      }

      if (verbose && i %% 10 == 0) {
        prog$message(sprintf("  %d/%d starts, max discrepancy: %.2e", i, n_starts, best_so_far))
      }
    }
    if (i == n_starts) prog$close()
  }

  # Find best result
  discrepancies <- vapply(tested_points, function(pt) pt$discrepancy, numeric(1))
  best_idx <- which.max(discrepancies)
  best_discrepancy <- discrepancies[best_idx]
  best_par <- tested_points[[best_idx]]$par

  # Build evidence data frame
  evidence <- do.call(rbind, lapply(tested_points, function(pt) {
    df <- as.data.frame(as.list(pt$par))
    df$discrepancy <- pt$discrepancy
    df
  }))

  list(
    max_discrepancy = best_discrepancy,
    worst_par = best_par,
    evidence = evidence,
    n_tested = length(tested_points)
  )
}
