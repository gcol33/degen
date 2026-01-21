#' Compute Fisher information matrix
#'
#' Compute the Fisher information matrix for a model specification at given
#' parameter values. Supports both observed information (negative Hessian) and
#' expected information (outer product of gradients estimator).
#'
#' @param spec A `model_spec` object
#' @param y Numeric vector of observed data
#' @param par Named numeric vector of parameter values at which to evaluate
#' @param type Type of information: "observed" (default) uses the negative
#'   Hessian at the data; "expected" uses the outer product of gradients (OPG)
#'   estimator computed from per-observation score contributions
#' @param method Computation method: "hessian" (default) uses numerical
#'   differentiation
#' @param cl Optional parallel cluster from `setup_cluster()`. Only used for
#'   `type = "expected"` to parallelize per-observation score computation.
#'
#' @return An S3 object of class `fisher_info` containing:
#' \describe{
#'   \item{matrix}{The Fisher information matrix}
#'   \item{eigenvalues}{Eigenvalues (sorted decreasing)}
#'   \item{eigenvectors}{Corresponding eigenvectors}
#'   \item{condition}{Condition number}
#'   \item{rank}{Numerical rank}
#'   \item{par}{Parameter values used}
#'   \item{par_names}{Parameter names}
#'   \item{type}{Type of information computed}
#' }
#'
#' @details
#' The Fisher information matrix characterizes the curvature of the
#' log-likelihood surface. Key properties:
#'
#' - Positive definite: all parameters locally identifiable
#' - Singular (rank deficient): some parameters not identifiable
#' - Near-singular (high condition number): some parameters weakly identified
#'
#' Two types of information are available:
#'
#' - **Observed information** (`type = "observed"`): The negative Hessian of
#'   the log-likelihood. This is the default and most common choice.
#'
#' - **Expected information** (`type = "expected"`): Estimated using the outer
#'   product of gradients (OPG), where scores are summed over observations.
#'   The score is the gradient of the log-likelihood for each observation.
#'   Under regularity conditions,
#'   this converges to the same limit as observed information but may differ
#'   in finite samples. Requires the log-likelihood to be additive over
#'   observations.
#'
#' @export
#'
#' @examples
#' spec <- model_spec(
#'   loglik_fn = function(y, mu, sigma) {
#'     sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
#'   },
#'   par_names = c("mu", "sigma"),
#'   par_bounds = list(sigma = c(1e-6, Inf))
#' )
#'
#' set.seed(123)
#' y <- rnorm(100, mean = 5, sd = 2)
#'
#' # Observed information (negative Hessian)
#' info_obs <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))
#' print(info_obs)
#'
#' # Expected information (OPG estimator)
#' info_exp <- fisher_information(spec, y, par = c(mu = 5, sigma = 2),
#'                                type = "expected")
#' print(info_exp)
fisher_information <- function(spec,
                               y,
                               par,
                               type = c("observed", "expected"),
                               method = c("hessian"),
                               cl = NULL) {
  # Validate inputs
  if (!is_model_spec(spec)) {
    stop("`spec` must be a model_spec object", call. = FALSE)
  }
  check_numeric(y, "y")
  if (length(y) == 0) {
    stop("`y` must not be empty", call. = FALSE)
  }

  type <- match.arg(type)
  method <- match.arg(method)

  # Validate and prepare parameters
  if (is.list(par)) par <- unlist(par)
  if (is.null(names(par))) {
    stop("`par` must be a named vector", call. = FALSE)
  }

  # Ensure par is in correct order
  par <- par[spec$par_names]
  if (any(is.na(par))) {
    missing <- spec$par_names[is.na(par)]
    stop(sprintf("Missing parameter(s): %s", paste(missing, collapse = ", ")),
         call. = FALSE)
  }

  if (type == "observed") {
    info_matrix <- compute_observed_info(spec, y, par)
  } else {
    info_matrix <- compute_expected_info(spec, y, par, cl)
  }

  rownames(info_matrix) <- spec$par_names
  colnames(info_matrix) <- spec$par_names

  # Eigendecomposition
  eig <- eigen(info_matrix, symmetric = TRUE)

  # Condition number
  eig_abs <- abs(eig$values)
  condition <- if (min(eig_abs) > .Machine$double.eps) {
    max(eig_abs) / min(eig_abs)
  } else {
    Inf
  }

  # Numerical rank (use more conservative tolerance)
  rank_tol <- max(dim(info_matrix)) * max(eig_abs) * sqrt(.Machine$double.eps)
  rank <- sum(eig_abs > rank_tol)

  structure(
    list(
      matrix = info_matrix,
      eigenvalues = eig$values,
      eigenvectors = eig$vectors,
      condition = condition,
      rank = rank,
      n_par = spec$n_par,
      par = par,
      par_names = spec$par_names,
      n_obs = length(y),
      type = type
    ),
    class = "fisher_info"
  )
}

#' Compute observed Fisher information (negative Hessian)
#' @noRd
compute_observed_info <- function(spec, y, par) {
  loglik_fn <- function(theta) {
    names(theta) <- spec$par_names
    loglik(spec, y, theta)
  }

  hess <- numDeriv::hessian(loglik_fn, par)
  -hess
}

#' Compute expected Fisher information (OPG estimator)
#' @noRd
compute_expected_info <- function(spec, y, par, cl = NULL) {
  n_obs <- length(y)
  n_par <- length(par)

  # Worker function for single observation score
  compute_score_i <- function(i) {
    loglik_i <- function(theta) {
      names(theta) <- spec$par_names
      loglik(spec, y[i], theta)
    }
    numDeriv::grad(loglik_i, par)
  }

  # Compute per-observation score contributions
  if (!is.null(cl)) {
    # Export required objects to cluster
    parallel::clusterExport(cl, c("spec", "y", "par"), envir = environment())
    score_list <- parallel::parLapply(cl, seq_len(n_obs), compute_score_i)
    scores <- do.call(rbind, score_list)
  } else {
    scores <- matrix(0, nrow = n_obs, ncol = n_par)
    for (i in seq_len(n_obs)) {
      scores[i, ] <- compute_score_i(i)
    }
  }

  # OPG estimator: sum of outer products of scores
  # I = sum_i(s_i * s_i')
  info_matrix <- crossprod(scores)

  info_matrix
}

#' @export
print.fisher_info <- function(x, ...) {
  type_str <- if (!is.null(x$type) && x$type == "expected") "expected" else "observed"
  cat(sprintf("<fisher_info> (%s) at par = (", type_str),
      paste(sprintf("%s=%.3g", x$par_names, x$par), collapse = ", "),
      ")\n", sep = "")

  cat(sprintf("Condition number: %.2g\n", x$condition))
  cat(sprintf("Rank: %d / %d", x$rank, x$n_par))
  if (x$rank == x$n_par) {
    cat(" (full)\n")
  } else {
    cat(" (RANK DEFICIENT)\n")
  }

  cat("Eigenvalues:", paste(sprintf("%.3g", x$eigenvalues), collapse = ", "), "\n")

  invisible(x)
}

#' @export
summary.fisher_info <- function(object, ...) {
  type_str <- if (!is.null(object$type) && object$type == "expected") {
    "Expected (OPG)"
  } else {
    "Observed (Hessian)"
  }
  cat("Fisher Information Analysis\n")
  cat(strrep("=", 50), "\n\n")

  cat("Type:", type_str, "\n")
  cat("Parameters:", paste(object$par_names, collapse = ", "), "\n")
  cat("Evaluated at:", paste(sprintf("%s=%.4g", object$par_names, object$par),
                             collapse = ", "), "\n")
  cat("Sample size:", object$n_obs, "\n\n")

  cat("Information Matrix:\n")
  print(round(object$matrix, 4))

  cat("\nEigenvalue Decomposition:\n")
  for (i in seq_along(object$eigenvalues)) {
    cat(sprintf("  eig_%d = %.4g", i, object$eigenvalues[i]))
    if (object$eigenvalues[i] < 1e-6) {
      cat(" (NEAR ZERO)")
    }
    cat("\n")
  }

  cat(sprintf("\nCondition number: %.2g\n", object$condition))
  cat(sprintf("Numerical rank: %d / %d\n", object$rank, object$n_par))

  # Interpretation
  cat("\nInterpretation:\n")
  if (object$rank < object$n_par) {
    cat("  WARNING: Model has NON-IDENTIFIABLE parameters\n")
    cat("  Some parameter combinations cannot be estimated from data\n")
  } else if (object$condition > 1000) {
    cat("  WARNING: Model is POORLY CONDITIONED\n")
    cat("  Some parameters may be weakly identified\n")
  } else if (object$condition > 100) {
    cat("  CAUTION: Moderate conditioning issues\n")
    cat("  Check eigenvalues for potential weak identifiability\n")
  } else {
    cat("  Model appears well-identified\n")
  }

  invisible(object)
}

#' @export
format.fisher_info <- function(x, ...) {
  sprintf("<fisher_info> rank %d/%d, condition %.2g",
          x$rank, x$n_par, x$condition)
}

#' Test if object is a fisher_info
#'
#' @param x Object to test
#'
#' @return Logical
#' @export
is_fisher_info <- function(x) {
  inherits(x, "fisher_info")
}

#' Extract eigenvalues from Fisher information
#'
#' @param x A `fisher_info` object
#'
#' @return Numeric vector of eigenvalues (sorted decreasing)
#' @export
info_eigenvalues <- function(x) {
  if (!is_fisher_info(x)) {
    stop("`x` must be a fisher_info object", call. = FALSE)
  }
  x$eigenvalues
}

#' Get condition number of Fisher information
#'
#' @param x A `fisher_info` object
#'
#' @return Numeric scalar
#' @export
info_condition <- function(x) {
  if (!is_fisher_info(x)) {
    stop("`x` must be a fisher_info object", call. = FALSE)
  }
  x$condition
}

#' Get numerical rank of Fisher information
#'
#' @param x A `fisher_info` object
#'
#' @return Integer
#' @export
info_rank <- function(x) {
  if (!is_fisher_info(x)) {
    stop("`x` must be a fisher_info object", call. = FALSE)
  }
  x$rank
}

#' Find directions of non-identifiability
#'
#' Returns eigenvectors corresponding to zero or near-zero eigenvalues.
#' These represent directions in parameter space along which the likelihood
#' is flat.
#'
#' @param x A `fisher_info` object
#' @param tol Tolerance for considering an eigenvalue as zero
#'
#' @return Matrix where each column is a null direction, or NULL if none
#' @export
#'
#' @examples
#' # Model where only a+b is identified, not a and b individually
#' spec <- model_spec(
#'   loglik_fn = function(y, a, b) {
#'     sum(dnorm(y, mean = a + b, sd = 1, log = TRUE))
#'   },
#'   par_names = c("a", "b")
#' )
#'
#' set.seed(123)
#' y <- rnorm(100, mean = 5, sd = 1)
#' info <- fisher_information(spec, y, par = c(a = 2, b = 3))
#' null_directions(info)
null_directions <- function(x, tol = 1e-6) {
  if (!is_fisher_info(x)) {
    stop("`x` must be a fisher_info object", call. = FALSE)
  }

  # Find near-zero eigenvalues
  null_idx <- which(abs(x$eigenvalues) < tol)

  if (length(null_idx) == 0) {
    return(NULL)
  }

  null_vecs <- x$eigenvectors[, null_idx, drop = FALSE]
  rownames(null_vecs) <- x$par_names
  colnames(null_vecs) <- paste0("null_", seq_along(null_idx))

  null_vecs
}
