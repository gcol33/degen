# Batch comparison utilities for degen
#
# Functions for comparing many models systematically.

#' Compare all pairs of models
#'
#' Perform pairwise comparisons between all models in a list.
#'
#' @param models Named list of `model_spec` objects
#' @param y Numeric vector of observed data
#' @param n_points Number of parameter points to sample per comparison
#' @param tol Tolerance for likelihood equality
#' @param verbose Logical; print progress information
#' @param cl Optional parallel cluster from `setup_cluster()`
#'
#' @return A list with:
#' \describe{
#'   \item{comparisons}{List of `surface_comparison` results}
#'   \item{matrix}{Logical matrix of pairwise equivalence}
#'   \item{n_equivalent}{Number of equivalent pairs}
#'   \item{n_total}{Total number of comparisons}
#' }
#'
#' @export
#'
#' @examples
#' exp_spec <- model_spec_exponential()
#' gamma1_spec <- model_spec_gamma(par = "rate", known_shape = 1)
#' gamma2_spec <- model_spec_gamma(par = "rate", known_shape = 2)
#' models <- list(exp = exp_spec, gamma1 = gamma1_spec, gamma2 = gamma2_spec)
#' y <- rexp(50, rate = 2)
#' result <- compare_all_pairs(models, y, n_points = 10)
#' result$matrix
compare_all_pairs <- function(models, y, n_points = 50, tol = 1e-6,
                              verbose = FALSE, cl = NULL) {
  if (!is.list(models) || length(models) < 2) {
    stop("`models` must be a list with at least 2 model_spec objects", call. = FALSE)
  }

  n_models <- length(models)
  model_names <- names(models)
  if (is.null(model_names)) {
    model_names <- paste0("model_", seq_len(n_models))
    names(models) <- model_names
  }

  # Generate all pairs
  pairs <- utils::combn(n_models, 2, simplify = FALSE)
  n_pairs <- length(pairs)

  if (verbose) {
    cat(sprintf("Comparing %d models (%d pairs)...\n", n_models, n_pairs))
  }

  # Initialize results
  comparisons <- vector("list", n_pairs)
  equiv_matrix <- matrix(FALSE, nrow = n_models, ncol = n_models,
                         dimnames = list(model_names, model_names))
  diag(equiv_matrix) <- TRUE  # Self-equivalence

  for (k in seq_len(n_pairs)) {
    i <- pairs[[k]][1]
    j <- pairs[[k]][2]

    if (verbose) {
      cat(sprintf("  [%d/%d] %s vs %s... ", k, n_pairs,
                  model_names[i], model_names[j]))
    }

    pair <- equivalence_pair(models[[i]], models[[j]])
    result <- compare_surfaces(pair, y, n_points = n_points, tol = tol, cl = cl)

    comparisons[[k]] <- result
    equiv_matrix[i, j] <- result$equivalent
    equiv_matrix[j, i] <- result$equivalent

    if (verbose) {
      cat(if (result$equivalent) "EQUIVALENT\n" else "NOT equivalent\n")
    }
  }

  # Name comparisons
  names(comparisons) <- vapply(pairs, function(p) {
    paste(model_names[p[1]], model_names[p[2]], sep = "_vs_")
  }, character(1))

  n_equivalent <- sum(vapply(comparisons, function(c) c$equivalent, logical(1)))

  list(
    comparisons = comparisons,
    matrix = equiv_matrix,
    n_equivalent = n_equivalent,
    n_total = n_pairs,
    model_names = model_names
  )
}


#' Create equivalence matrix visualization
#'
#' Generate a visual matrix showing pairwise equivalence relationships.
#'
#' @param x Result from `compare_all_pairs()` or `equivalence_classes()`
#' @param ... Additional arguments passed to `image()`
#'
#' @return Invisible equivalence matrix
#' @export
#'
#' @examples
#' exp_spec <- model_spec_exponential()
#' gamma1_spec <- model_spec_gamma(par = "rate", known_shape = 1)
#' gamma2_spec <- model_spec_gamma(par = "rate", known_shape = 2)
#' models <- list(exp = exp_spec, gamma1 = gamma1_spec, gamma2 = gamma2_spec)
#' y <- rexp(50, rate = 2)
#' result <- compare_all_pairs(models, y, n_points = 10)
#' equivalence_matrix(result)
equivalence_matrix <- function(x, ...) {
  # Extract matrix from different input types
  if (inherits(x, "equiv_classes")) {
    mat <- x$pairwise
    model_names <- x$model_names
  } else if (is.list(x) && "matrix" %in% names(x)) {
    mat <- x$matrix
    model_names <- x$model_names
  } else if (is.matrix(x)) {
    mat <- x
    model_names <- rownames(mat)
  } else {
    stop("Invalid input. Expected compare_all_pairs result or equivalence matrix",
         call. = FALSE)
  }

  n <- nrow(mat)

  # Convert logical to numeric for plotting
  num_mat <- matrix(as.numeric(mat), nrow = n, ncol = n)

  # Plot
  graphics::image(
    seq_len(n), seq_len(n), t(num_mat[n:1, ]),
    col = c("firebrick", "forestgreen"),
    axes = FALSE,
    xlab = "", ylab = "",
    main = "Equivalence Matrix",
    ...
  )

  # Add labels
  graphics::axis(1, at = seq_len(n), labels = model_names, las = 2, cex.axis = 0.8)
  graphics::axis(2, at = seq_len(n), labels = rev(model_names), las = 1, cex.axis = 0.8)
  graphics::box()

  # Add legend
  graphics::legend("topright",
                   legend = c("Not equivalent", "Equivalent"),
                   fill = c("firebrick", "forestgreen"),
                   bty = "n", cex = 0.8)

  invisible(mat)
}


#' Find all models equivalent to a reference
#'
#' Search a list of models to find those equivalent to a specified reference.
#'
#' @param reference A `model_spec` object (the reference model)
#' @param candidates Named list of `model_spec` objects to test
#' @param y Numeric vector of observed data
#' @param n_points Number of parameter points to sample per comparison
#' @param tol Tolerance for likelihood equality
#' @param verbose Logical; print progress information
#' @param cl Optional parallel cluster
#'
#' @return A list with:
#' \describe{
#'   \item{equivalent}{Names of equivalent models}
#'   \item{not_equivalent}{Names of non-equivalent models}
#'   \item{comparisons}{List of comparison results}
#' }
#'
#' @export
#'
#' @examples
#' ref <- model_spec_exponential()
#' candidates <- list(
#'   gamma1 = model_spec_gamma(par = "rate", known_shape = 1),
#'   gamma2 = model_spec_gamma(par = "rate", known_shape = 2),
#'   weibull = model_spec_weibull()
#' )
#' y <- rexp(50, rate = 2)
#' result <- find_equivalent_to(ref, candidates, y, n_points = 10)
#' result$equivalent
find_equivalent_to <- function(reference, candidates, y,
                               n_points = 50, tol = 1e-6,
                               verbose = FALSE, cl = NULL) {
  if (!is_model_spec(reference)) {
    stop("`reference` must be a model_spec object", call. = FALSE)
  }
  if (!is.list(candidates) || length(candidates) == 0) {
    stop("`candidates` must be a non-empty list of model_spec objects", call. = FALSE)
  }

  n_candidates <- length(candidates)
  candidate_names <- names(candidates)
  if (is.null(candidate_names)) {
    candidate_names <- paste0("candidate_", seq_len(n_candidates))
    names(candidates) <- candidate_names
  }

  if (verbose) {
    cat(sprintf("Testing %d candidates against reference...\n", n_candidates))
  }

  comparisons <- vector("list", n_candidates)
  names(comparisons) <- candidate_names
  equivalent <- character()
  not_equivalent <- character()

  for (i in seq_len(n_candidates)) {
    name <- candidate_names[i]

    if (verbose) {
      cat(sprintf("  [%d/%d] %s... ", i, n_candidates, name))
    }

    pair <- equivalence_pair(reference, candidates[[i]])
    result <- compare_surfaces(pair, y, n_points = n_points, tol = tol, cl = cl)
    comparisons[[name]] <- result

    if (result$equivalent) {
      equivalent <- c(equivalent, name)
      if (verbose) cat("EQUIVALENT\n")
    } else {
      not_equivalent <- c(not_equivalent, name)
      if (verbose) cat("NOT equivalent\n")
    }
  }

  list(
    equivalent = equivalent,
    not_equivalent = not_equivalent,
    comparisons = comparisons
  )
}
