#' @export
print.surface_comparison <- function(x, ...) {
  cat("<surface_comparison>\n")

  if (x$equivalent) {
    cat("Conclusion: Models appear EQUIVALENT\n")
  } else {
    cat("Conclusion: Models are NOT EQUIVALENT\n")
  }

  cat(sprintf("Points tested: %d\n", x$n_tested))
  cat(sprintf("Max discrepancy: %.2e\n", x$max_discrepancy))
  cat(sprintf("Tolerance: %.2e\n", x$tol))
  cat(sprintf("Method: %s\n", x$method))

  invisible(x)
}

#' @export
summary.surface_comparison <- function(object, ...) {
  cat("Surface Comparison Results\n")
  cat(strrep("=", 50), "\n\n")

  cat("Pair:", object$pair$name, "\n")
  cat("Data points:", object$y_length, "\n")
  cat("Method:", object$method, "\n")
  cat("Tolerance:", format(object$tol, scientific = TRUE), "\n\n")

  if (object$equivalent) {
    cat("CONCLUSION: Models appear observationally equivalent\n\n")
  } else {
    cat("CONCLUSION: Models are NOT observationally equivalent\n\n")
  }

  cat("Discrepancy summary:\n")
  cat(sprintf("  A -> B max: %.2e\n", object$ab_discrepancy))
  cat(sprintf("  B -> A max: %.2e\n", object$ba_discrepancy))
  cat(sprintf("  Overall max: %.2e\n", object$max_discrepancy))

  # Distribution of discrepancies
  discrep <- c(
    object$evidence$A_to_B$discrepancy,
    object$evidence$B_to_A$discrepancy
  )
  discrep <- discrep[is.finite(discrep)]

  if (length(discrep) > 0) {
    cat("\nDiscrepancy distribution:\n")
    cat(sprintf("  Min: %.2e\n", min(discrep)))
    cat(sprintf("  Median: %.2e\n", stats::median(discrep)))
    cat(sprintf("  Mean: %.2e\n", mean(discrep)))
    cat(sprintf("  Max: %.2e\n", max(discrep)))

    # Proportion below tolerance
    prop_equiv <- mean(discrep < object$tol)
    cat(sprintf("  %% < tolerance: %.1f%%\n", prop_equiv * 100))
  }

  invisible(object)
}

#' @export
format.surface_comparison <- function(x, ...) {
  status <- if (x$equivalent) "EQUIVALENT" else "NOT EQUIVALENT"
  sprintf("<surface_comparison> %s (max discrepancy: %.2e)", status, x$max_discrepancy)
}

#' Test if object is a surface_comparison
#'
#' @param x Object to test
#'
#' @return Logical
#' @export
is_surface_comparison <- function(x) {
  inherits(x, "surface_comparison")
}
