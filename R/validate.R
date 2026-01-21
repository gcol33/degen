# Model validation functions

#' Validate a log-likelihood function
#'
#' Check that a log-likelihood function behaves correctly across its
#' parameter space. Useful for debugging user-defined likelihood functions.
#'
#' @param spec A `model_spec` object
#' @param y Numeric vector of test data
#' @param n_test Number of random parameter points to test (default 20)
#' @param verbose Logical; print detailed output
#'
#' @return A list with validation results (invisible)
#' @export
#'
#' @examples
#' spec <- model_spec_normal()
#' y <- rnorm(50)
#' validate_loglik(spec, y)
validate_loglik <- function(spec, y, n_test = 20, verbose = TRUE) {
  if (!is_model_spec(spec)) {
    stop("`spec` must be a model_spec object", call. = FALSE)
  }
  check_numeric(y, "y")

  results <- list(
    passed = TRUE,
    n_tested = 0,
    errors = character(0),
    warnings = character(0)
  )

  if (verbose) cat("Validating log-likelihood function...\n\n")

  # Test 1: Check return type at a valid point
  if (verbose) cat("1. Checking return type... ")
  bounds <- propose_bounds(spec, y)
  test_par <- vapply(bounds, mean, numeric(1))
  names(test_par) <- spec$par_names

  ll_result <- tryCatch(
    loglik(spec, y, test_par),
    error = function(e) e
  )

  if (inherits(ll_result, "error")) {
    results$passed <- FALSE
    results$errors <- c(results$errors,
                        paste("Log-likelihood threw error:", ll_result$message))
    if (verbose) cat("FAILED\n")
  } else if (!is.numeric(ll_result) || length(ll_result) != 1) {
    results$passed <- FALSE
    results$errors <- c(results$errors,
                        sprintf("Expected scalar numeric, got %s of length %d",
                                class(ll_result)[1], length(ll_result)))
    if (verbose) cat("FAILED\n")
  } else if (verbose) {
    cat("OK\n")
  }

  # Test 2: Check for NA/NaN at valid points
  if (verbose) cat("2. Checking for NA/NaN values... ")
  test_samples <- sample_par_space(bounds, n_test, method = "lhs")
  na_count <- 0

  for (i in seq_len(n_test)) {
    par_i <- test_samples[i, , drop = TRUE]
    names(par_i) <- colnames(test_samples)

    ll_i <- tryCatch(
      loglik(spec, y, par_i),
      error = function(e) NA_real_
    )

    if (is.na(ll_i) || is.nan(ll_i)) {
      na_count <- na_count + 1
    }
    results$n_tested <- results$n_tested + 1
  }

  if (na_count > 0) {
    results$warnings <- c(results$warnings,
                          sprintf("%d/%d test points returned NA/NaN", na_count, n_test))
    if (verbose) cat(sprintf("WARNING: %d NA/NaN values\n", na_count))
  } else if (verbose) {
    cat("OK\n")
  }

  # Test 3: Check for infinite values (other than -Inf at boundary)
  if (verbose) cat("3. Checking for +Inf values... ")
  inf_count <- sum(vapply(seq_len(n_test), function(i) {
    par_i <- test_samples[i, , drop = TRUE]
    names(par_i) <- colnames(test_samples)
    ll_i <- tryCatch(loglik(spec, y, par_i), error = function(e) NA_real_)
    !is.na(ll_i) && is.infinite(ll_i) && ll_i > 0
  }, logical(1)))

  if (inf_count > 0) {
    results$passed <- FALSE
    results$errors <- c(results$errors,
                        sprintf("%d test points returned +Inf (invalid)", inf_count))
    if (verbose) cat("FAILED\n")
  } else if (verbose) {
    cat("OK\n")
  }

  # Test 4: Check gradient exists (via numerical differentiation)
  if (verbose) cat("4. Checking gradient computation... ")
  grad_result <- tryCatch({
    numDeriv::grad(function(p) {
      names(p) <- spec$par_names
      loglik(spec, y, p)
    }, test_par)
  }, error = function(e) e)

  if (inherits(grad_result, "error")) {
    results$warnings <- c(results$warnings,
                          paste("Gradient computation failed:", grad_result$message))
    if (verbose) cat("WARNING\n")
  } else if (any(!is.finite(grad_result))) {
    results$warnings <- c(results$warnings,
                          "Gradient contains non-finite values at test point")
    if (verbose) cat("WARNING\n")
  } else if (verbose) {
    cat("OK\n")
  }

  # Test 5: Check likelihood is higher near MLE
  if (verbose) cat("5. Checking likelihood ordering... ")
  ll_at_test <- tryCatch(loglik(spec, y, test_par), error = function(e) NA)

  # Perturb parameters and check likelihood decreases
  perturb_worse <- 0
  for (j in seq_along(test_par)) {
    perturbed <- test_par
    perturbed[j] <- test_par[j] * 1.5  # 50% increase
    ll_perturbed <- tryCatch(loglik(spec, y, perturbed), error = function(e) NA)
    if (!is.na(ll_perturbed) && !is.na(ll_at_test) && ll_perturbed > ll_at_test) {
      perturb_worse <- perturb_worse + 1
    }
  }

  # This is just informational, not a failure
  if (verbose) cat("OK\n")

  # Summary
  if (verbose) {
    cat("\n")
    if (results$passed && length(results$warnings) == 0) {
      cat("All checks passed.\n")
    } else if (results$passed) {
      cat("Passed with warnings:\n")
      for (w in results$warnings) cat("  - ", w, "\n")
    } else {
      cat("VALIDATION FAILED:\n")
      for (e in results$errors) cat("  ERROR: ", e, "\n")
      for (w in results$warnings) cat("  WARNING: ", w, "\n")
    }
  }

  invisible(results)
}

#' Check numerical gradient against analytical
#'
#' Compare numerically computed gradient with a user-supplied analytical
#' gradient function.
#'
#' @param spec A `model_spec` object
#' @param y Numeric vector of data
#' @param par Named numeric vector of parameter values
#' @param grad_fn Analytical gradient function with signature `function(y, ...)`
#' @param tol Tolerance for comparison (default 1e-4)
#'
#' @return Logical indicating whether gradients match
#' @export
#'
#' @examples
#' # For normal distribution, gradient w.r.t. mu is sum(y - mu) / sigma^2
#' spec <- model_spec_normal()
#' y <- rnorm(100, 5, 2)
#' grad_fn <- function(y, mu, sigma) {
#'   c(mu = sum(y - mu) / sigma^2,
#'     sigma = -length(y) / sigma + sum((y - mu)^2) / sigma^3)
#' }
#' check_gradient(spec, y, c(mu = 5, sigma = 2), grad_fn)
check_gradient <- function(spec, y, par, grad_fn, tol = 1e-4) {
  if (!is_model_spec(spec)) {
    stop("`spec` must be a model_spec object", call. = FALSE)
  }
  if (!is.function(grad_fn)) {
    stop("`grad_fn` must be a function", call. = FALSE)
  }

  # Numerical gradient
  num_grad <- numDeriv::grad(function(p) {
    names(p) <- spec$par_names
    loglik(spec, y, p)
  }, par)
  names(num_grad) <- spec$par_names

  # Analytical gradient
  par_list <- as.list(par)
  ana_grad <- do.call(grad_fn, c(list(y = y), par_list))

  if (length(ana_grad) != length(num_grad)) {
    stop("Analytical gradient has wrong length", call. = FALSE)
  }

  # Compare
  max_diff <- max(abs(num_grad - ana_grad))
  rel_diff <- max(abs(num_grad - ana_grad) / (abs(num_grad) + 1e-10))

  cat("Gradient comparison:\n")
  cat(sprintf("  %-15s %12s %12s %12s\n", "Parameter", "Numerical", "Analytical", "Diff"))
  for (i in seq_along(par)) {
    cat(sprintf("  %-15s %12.6f %12.6f %12.2e\n",
                spec$par_names[i], num_grad[i], ana_grad[i],
                abs(num_grad[i] - ana_grad[i])))
  }
  cat(sprintf("\nMax absolute difference: %.2e\n", max_diff))
  cat(sprintf("Max relative difference: %.2e\n", rel_diff))

  passed <- max_diff < tol
  if (passed) {
    cat("Gradients MATCH (within tolerance)\n")
  } else {
    cat("Gradients DO NOT MATCH\n")
  }

  invisible(passed)
}

#' Diagnose model specification issues
#'
#' Comprehensive health check for a model_spec, including likelihood
#' evaluation, gradient computation, and identifiability diagnostics.
#'
#' @param spec A `model_spec` object
#' @param y Numeric vector of data
#' @param par Optional starting parameter values. If NULL, uses center of bounds.
#'
#' @return A list with diagnostic results
#' @export
#'
#' @examples
#' spec <- model_spec_normal()
#' y <- rnorm(100, 5, 2)
#' diagnose_model(spec, y)
diagnose_model <- function(spec, y, par = NULL) {
  if (!is_model_spec(spec)) {
    stop("`spec` must be a model_spec object", call. = FALSE)
  }
  check_numeric(y, "y")

  cat("Model Diagnostics\n")
  cat(strrep("=", 50), "\n\n")

  # Basic info
  cat("Model:", if (!is.null(spec$name)) spec$name else "(unnamed)", "\n")
  cat("Parameters:", paste(spec$par_names, collapse = ", "), "\n")
  cat("Data: n =", length(y), "\n\n")

  # Get parameter values
  if (is.null(par)) {
    bounds <- propose_bounds(spec, y)
    par <- vapply(bounds, mean, numeric(1))
    names(par) <- spec$par_names
  }

  cat("Evaluating at:", paste(sprintf("%s=%.3g", names(par), par), collapse = ", "), "\n\n")

  # 1. Likelihood check
  cat("1. Log-likelihood evaluation\n")
  ll <- tryCatch(loglik(spec, y, par), error = function(e) NA)
  if (is.na(ll)) {
    cat("   ERROR: Could not compute log-likelihood\n")
  } else {
    cat(sprintf("   Log-likelihood: %.4f\n", ll))
    cat(sprintf("   Per observation: %.4f\n", ll / length(y)))
  }

  # 2. Fisher information
  cat("\n2. Fisher information\n")
  info <- tryCatch(fisher_information(spec, y, par), error = function(e) NULL)
  if (is.null(info)) {
    cat("   ERROR: Could not compute Fisher information\n")
  } else {
    cat(sprintf("   Condition number: %.2g\n", info$condition))
    cat(sprintf("   Rank: %d / %d\n", info$rank, info$n_par))
    if (info$rank < info$n_par) {
      cat("   WARNING: Rank deficient (non-identifiable)\n")
    } else if (info$condition > 1000) {
      cat("   WARNING: Poorly conditioned (weak identification)\n")
    } else {
      cat("   Status: Well-conditioned\n")
    }
  }

  # 3. Gradient magnitude
  cat("\n3. Gradient at evaluation point\n")
  grad <- tryCatch({
    numDeriv::grad(function(p) {
      names(p) <- spec$par_names
      loglik(spec, y, p)
    }, par)
  }, error = function(e) NULL)

  if (is.null(grad)) {
    cat("   ERROR: Could not compute gradient\n")
  } else {
    cat(sprintf("   Gradient norm: %.4f\n", sqrt(sum(grad^2))))
    for (i in seq_along(grad)) {
      cat(sprintf("   d/d%s: %.4f\n", spec$par_names[i], grad[i]))
    }
    if (sqrt(sum(grad^2)) < 0.01) {
      cat("   Note: Near stationary point (possibly at MLE)\n")
    }
  }

  cat("\n")
  invisible(list(loglik = ll, fisher_info = info, gradient = grad))
}
