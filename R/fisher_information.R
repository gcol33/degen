#' Compute Fisher information matrix
#'
#' Compute the observed Fisher information matrix (negative Hessian of
#' log-likelihood) for a model specification at given parameter values.
#'
#' @param spec A `model_spec` object
#' @param y Numeric vector of observed data
#' @param par Named numeric vector of parameter values at which to evaluate
#' @param type Type of information: "observed" (default) uses the negative
#'   Hessian at the data; "expected" would use theoretical expectation
#'   (not yet implemented)
#' @param method Computation method: "hessian" (default) uses numerical
#'   differentiation
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
#' info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))
#' print(info)
fisher_information <- function(spec,
                               y,
                               par,
                               type = c("observed", "expected"),
                               method = c("hessian")) {
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

  if (type == "expected") {
    stop("Expected information not yet implemented", call. = FALSE)
  }

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

  # Compute Hessian using numDeriv
  loglik_fn <- function(theta) {
    names(theta) <- spec$par_names
    loglik(spec, y, theta)
  }

  hess <- numDeriv::hessian(loglik_fn, par)

  # Fisher information is negative Hessian
  info_matrix <- -hess
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
      n_obs = length(y)
    ),
    class = "fisher_info"
  )
}

#' @export
print.fisher_info <- function(x, ...) {
  cat("<fisher_info> at par = (",
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
  cat("Fisher Information Analysis\n")
  cat(strrep("=", 50), "\n\n")

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
