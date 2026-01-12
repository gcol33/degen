#' Find equivalence classes among multiple models
#'
#' Given a list of model specifications, determine which models are
#' observationally equivalent and group them into equivalence classes.
#'
#' @param specs A named list of `model_spec` objects
#' @param y Numeric vector of observed data
#' @param n_points Number of parameter points to sample for each comparison
#' @param tol Tolerance for equivalence (default 1e-6)
#' @param verbose Logical; print progress
#'
#' @return An S3 object of class `equiv_classes` containing:
#' \describe{
#'   \item{classes}{List of equivalence classes (vectors of model names)}
#'   \item{n_classes}{Number of distinct classes}
#'   \item{membership}{Named vector mapping model names to class indices}
#'   \item{pairwise}{Matrix of pairwise equivalence results}
#'   \item{discrepancies}{Matrix of pairwise discrepancies}
#' }
#'
#' @details
#' The algorithm performs pairwise comparisons between all models and uses
#' union-find to group equivalent models. Two models are considered equivalent
#' if their maximum likelihood discrepancy is below the tolerance.
#'
#' @export
#'
#' @examples
#' # Define several models
#' exp_spec <- model_spec(
#'   loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
#'   par_names = "lambda",
#'   par_bounds = list(lambda = c(1e-6, 100)),
#'   name = "Exponential"
#' )
#'
#' gamma1_spec <- model_spec(
#'   loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
#'   par_names = "rate",
#'   par_bounds = list(rate = c(1e-6, 100)),
#'   name = "Gamma(1)"
#' )
#'
#' gamma2_spec <- model_spec(
#'   loglik_fn = function(y, rate) sum(dgamma(y, shape = 2, rate = rate, log = TRUE)),
#'   par_names = "rate",
#'   par_bounds = list(rate = c(1e-6, 100)),
#'   name = "Gamma(2)"
#' )
#'
#' models <- list(exp = exp_spec, gamma1 = gamma1_spec, gamma2 = gamma2_spec)
#'
#' set.seed(123)
#' y <- rexp(50, rate = 2)
#' classes <- equivalence_classes(models, y, n_points = 10)
#' print(classes)
equivalence_classes <- function(specs,
                                y,
                                n_points = 50,
                                tol = 1e-6,
                                verbose = FALSE) {
  # Validate inputs
  if (!is.list(specs)) {
    stop("`specs` must be a list of model_spec objects", call. = FALSE)
  }

  if (length(specs) < 2) {
    stop("`specs` must contain at least 2 models", call. = FALSE)
  }

  # Ensure all are model_spec
  for (i in seq_along(specs)) {
    if (!is_model_spec(specs[[i]])) {
      stop(sprintf("Element %d of `specs` is not a model_spec", i), call. = FALSE)
    }
  }

  # Ensure names
  if (is.null(names(specs))) {
    names(specs) <- paste0("model_", seq_along(specs))
  }

  model_names <- names(specs)
  n_models <- length(specs)

  check_numeric(y, "y")

  # Compute all pairwise comparisons
  equiv_matrix <- matrix(NA, n_models, n_models,
                         dimnames = list(model_names, model_names))
  discrep_matrix <- matrix(NA, n_models, n_models,
                           dimnames = list(model_names, model_names))

  # Diagonal is always equivalent (model with itself)
  diag(equiv_matrix) <- TRUE
  diag(discrep_matrix) <- 0

  n_pairs <- n_models * (n_models - 1) / 2
  pair_count <- 0

  for (i in seq_len(n_models - 1)) {
    for (j in (i + 1):n_models) {
      pair_count <- pair_count + 1

      if (verbose) {
        cat(sprintf("Comparing %s vs %s (%d/%d)...\n",
                    model_names[i], model_names[j], pair_count, n_pairs))
      }

      pair <- equivalence_pair(specs[[i]], specs[[j]])
      result <- compare_surfaces(pair, y, n_points = n_points, tol = tol)

      equiv_matrix[i, j] <- result$equivalent
      equiv_matrix[j, i] <- result$equivalent
      discrep_matrix[i, j] <- result$max_discrepancy
      discrep_matrix[j, i] <- result$max_discrepancy
    }
  }

  # Use union-find to build equivalence classes
  parent <- seq_len(n_models)
  names(parent) <- model_names

  find_root <- function(x) {
    while (parent[x] != which(model_names == x)) {
      x <- model_names[parent[x]]
    }
    x
  }

  union_sets <- function(x, y) {
    root_x <- find_root(x)
    root_y <- find_root(y)
    if (root_x != root_y) {
      parent[root_y] <<- which(model_names == root_x)
    }
  }

  # Union equivalent models
  for (i in seq_len(n_models - 1)) {
    for (j in (i + 1):n_models) {
      if (isTRUE(equiv_matrix[i, j])) {
        union_sets(model_names[i], model_names[j])
      }
    }
  }

  # Build classes from union-find
  roots <- vapply(model_names, find_root, character(1))
  unique_roots <- unique(roots)
  n_classes <- length(unique_roots)

  classes <- lapply(unique_roots, function(r) {
    model_names[roots == r]
  })
  names(classes) <- paste0("class_", seq_len(n_classes))

  # Membership vector
  membership <- integer(n_models)
  names(membership) <- model_names
  for (k in seq_len(n_classes)) {
    membership[classes[[k]]] <- k
  }

  structure(
    list(
      classes = classes,
      n_classes = n_classes,
      membership = membership,
      pairwise = equiv_matrix,
      discrepancies = discrep_matrix,
      model_names = model_names,
      n_models = n_models,
      tol = tol
    ),
    class = "equiv_classes"
  )
}

#' @export
print.equiv_classes <- function(x, ...) {
  cat("<equiv_classes>\n")
  cat(sprintf("%d models -> %d equivalence class%s\n\n",
              x$n_models, x$n_classes,
              if (x$n_classes == 1) "" else "es"))

  for (k in seq_len(x$n_classes)) {
    members <- x$classes[[k]]
    cat(sprintf("Class %d: %s", k, paste(members, collapse = ", ")))
    if (length(members) > 1) {
      cat(" (equivalent)")
    }
    cat("\n")
  }

  invisible(x)
}

#' @export
summary.equiv_classes <- function(object, ...) {
  cat("Equivalence Class Analysis\n")
  cat(strrep("=", 50), "\n\n")

  cat("Models:", paste(object$model_names, collapse = ", "), "\n")
  cat("Number of models:", object$n_models, "\n")
  cat("Number of classes:", object$n_classes, "\n")
  cat("Tolerance:", format(object$tol, scientific = TRUE), "\n\n")

  cat("Equivalence Classes:\n")
  for (k in seq_len(object$n_classes)) {
    members <- object$classes[[k]]
    cat(sprintf("\n  Class %d (%d member%s):\n", k, length(members),
                if (length(members) == 1) "" else "s"))
    for (m in members) {
      cat(sprintf("    - %s\n", m))
    }
  }

  cat("\nPairwise Discrepancies:\n")
  print(round(object$discrepancies, 6))

  invisible(object)
}

#' @export
format.equiv_classes <- function(x, ...) {
  sprintf("<equiv_classes> %d models -> %d classes",
          x$n_models, x$n_classes)
}

#' Test if object is an equiv_classes
#'
#' @param x Object to test
#'
#' @return Logical
#' @export
is_equiv_classes <- function(x) {
  inherits(x, "equiv_classes")
}

#' Get number of equivalence classes
#'
#' @param x An `equiv_classes` object
#'
#' @return Integer
#' @export
n_classes <- function(x) {
  if (!is_equiv_classes(x)) {
    stop("`x` must be an equiv_classes object", call. = FALSE)
  }
  x$n_classes
}

#' Get members of an equivalence class
#'
#' @param x An `equiv_classes` object
#' @param class_id Class index (integer)
#'
#' @return Character vector of model names
#' @export
class_members <- function(x, class_id) {
  if (!is_equiv_classes(x)) {
    stop("`x` must be an equiv_classes object", call. = FALSE)
  }
  if (class_id < 1 || class_id > x$n_classes) {
    stop(sprintf("class_id must be between 1 and %d", x$n_classes), call. = FALSE)
  }
  x$classes[[class_id]]
}

#' Check if two models are equivalent
#'
#' @param x An `equiv_classes` object
#' @param model1 Name of first model
#' @param model2 Name of second model
#'
#' @return Logical
#' @export
are_equivalent <- function(x, model1, model2) {
  if (!is_equiv_classes(x)) {
    stop("`x` must be an equiv_classes object", call. = FALSE)
  }
  if (!model1 %in% x$model_names) {
    stop(sprintf("Model '%s' not found", model1), call. = FALSE)
  }
  if (!model2 %in% x$model_names) {
    stop(sprintf("Model '%s' not found", model2), call. = FALSE)
  }

  x$membership[model1] == x$membership[model2]
}
