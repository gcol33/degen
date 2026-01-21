# Progress reporting utilities
#
# Optional progress bars and verbose output for long-running operations.

#' Create a progress bar
#'
#' Internal utility for consistent progress bar creation across the package.
#'
#' @param n Total number of iterations
#' @param show Logical; whether to show the progress bar
#'
#' @return A list with `bar` (the txtProgressBar object or NULL), `update`
#'   function, and `close` function.
#' @noRd
create_progress <- function(n, show = interactive()) {
  if (!show || n < 5) {
    # No progress bar for non-interactive or small n
    return(list(
      bar = NULL,
      update = function(i) invisible(NULL),
      close = function() invisible(NULL)
    ))
  }

  bar <- utils::txtProgressBar(min = 0, max = n, style = 3)

  list(
    bar = bar,
    update = function(i) utils::setTxtProgressBar(bar, i),
    close = function() close(bar)
  )
}

#' Create a timed progress bar with ETA
#'
#' Progress bar that shows estimated time remaining.
#'
#' @param n Total number of iterations
#' @param show Logical; whether to show the progress bar
#' @param label Optional label for the operation
#'
#' @return A list with `update`, `close`, and `message` functions.
#' @noRd
create_timed_progress <- function(n, show = interactive(), label = "") {
  if (!show || n < 5) {
    return(list(
      update = function(i) invisible(NULL),
      close = function() invisible(NULL),
      message = function(msg) invisible(NULL)
    ))
  }

  start_time <- Sys.time()
  last_print <- 0

  update_fn <- function(i) {
    # Only update every ~2% to avoid excessive output
    if (i < n && (i - last_print) < max(1, n %/% 50)) {
      return(invisible(NULL))
    }

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    pct <- i / n
    eta <- if (pct > 0.05 && i > 0) {
      remaining <- elapsed / pct * (1 - pct)
      format_time(remaining)
    } else {
      "calculating..."
    }

    bar_width <- 30
    filled <- round(pct * bar_width)
    bar <- paste0(
      "[", paste(rep("=", filled), collapse = ""),
      if (filled < bar_width) ">" else "",
      paste(rep(" ", max(0, bar_width - filled - 1)), collapse = ""),
      "]"
    )

    msg <- sprintf("\r%s %s %3.0f%% | ETA: %s    ",
                   if (nchar(label) > 0) label else "",
                   bar, pct * 100, eta)
    cat(msg)
    utils::flush.console()

    last_print <<- i
  }

  close_fn <- function() {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("\r%s Done in %s                              \n",
                if (nchar(label) > 0) label else "",
                format_time(elapsed)))
  }

  message_fn <- function(msg) {
    cat("\n", msg, "\n", sep = "")
  }

  list(
    update = update_fn,
    close = close_fn,
    message = message_fn
  )
}

#' Format seconds as human-readable time
#' @noRd
format_time <- function(seconds) {
  if (is.na(seconds) || seconds < 0) return("?")
  if (seconds < 60) {
    sprintf("%.0fs", seconds)
  } else if (seconds < 3600) {
    sprintf("%.1fmin", seconds / 60)
  } else {
    sprintf("%.1fhr", seconds / 3600)
  }
}

#' Debug comparison trace
#'
#' Perform a detailed trace of the comparison process for debugging.
#'
#' @param pair An `equivalence_pair` object
#' @param y Numeric vector of observed data
#' @param par Named numeric vector of source parameters to trace
#' @param direction Which direction to trace: "A_to_B" or "B_to_A"
#'
#' @return A list with detailed trace information including intermediate
#'   computations and diagnostics.
#'
#' @export
#'
#' @examples
#' exp_spec <- model_spec(
#'   loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
#'   par_names = "lambda",
#'   par_bounds = list(lambda = c(1e-6, Inf)),
#'   name = "Exponential"
#' )
#'
#' gamma_spec <- model_spec(
#'   loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
#'   par_names = "rate",
#'   par_bounds = list(rate = c(1e-6, Inf)),
#'   name = "Gamma(1)"
#' )
#'
#' pair <- equivalence_pair(exp_spec, gamma_spec)
#' set.seed(123)
#' y <- rexp(50, rate = 2)
#' trace <- debug_comparison(pair, y, par = c(lambda = 2), direction = "A_to_B")
#' print(trace)
debug_comparison <- function(pair, y, par, direction = c("A_to_B", "B_to_A")) {
  if (!is_equivalence_pair(pair)) {
    stop("`pair` must be an equivalence_pair object", call. = FALSE)
  }
  direction <- match.arg(direction)

  if (direction == "A_to_B") {
    source_spec <- pair$spec_a
    target_spec <- pair$spec_b
    source_name <- pair$spec_a$name
    target_name <- pair$spec_b$name
  } else {
    source_spec <- pair$spec_b
    target_spec <- pair$spec_a
    source_name <- pair$spec_b$name
    target_name <- pair$spec_a$name
  }

  cat("=== Debug Trace ===\n")
  cat(sprintf("Direction: %s -> %s\n", source_name, target_name))
  cat(sprintf("Source parameters: %s\n",
              paste(names(par), "=", format(par, digits = 4), collapse = ", ")))
  cat("\n")

  # Step 1: Compute source likelihood
  cat("Step 1: Computing source log-likelihood\n")
  source_ll <- tryCatch({
    ll <- loglik(source_spec, y, par)
    cat(sprintf("  Result: %.6f\n", ll))
    ll
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
    NA_real_
  })
  cat("\n")

  if (is.na(source_ll) || !is.finite(source_ll)) {
    return(list(
      direction = direction,
      source_par = par,
      source_ll = source_ll,
      success = FALSE,
      error = "Source log-likelihood is invalid"
    ))
  }

  # Step 2: Get target bounds
  cat("Step 2: Computing target parameter bounds\n")
  target_bounds <- propose_bounds(target_spec, y)
  for (p in names(target_bounds)) {
    cat(sprintf("  %s: [%.4g, %.4g]\n", p, target_bounds[[p]][1], target_bounds[[p]][2]))
  }
  cat("\n")

  # Step 3: Find matching target parameters
  cat("Step 3: Finding matching target parameters\n")
  cat(sprintf("  Target log-likelihood to match: %.6f\n", source_ll))

  match_result <- find_equivalent_par(target_spec, y, source_ll, target_bounds)

  cat(sprintf("  Found parameters: %s\n",
              paste(names(match_result$par), "=", format(match_result$par, digits = 4),
                    collapse = ", ")))
  cat("\n")

  # Step 4: Verify the match
  cat("Step 4: Verification\n")
  target_ll <- tryCatch(
    loglik(target_spec, y, match_result$par),
    error = function(e) NA_real_
  )
  cat(sprintf("  Target log-likelihood at matched params: %.6f\n", target_ll))
  cat(sprintf("  Discrepancy: %.2e\n", match_result$discrepancy))
  cat("\n")

  cat("=== Summary ===\n")
  if (match_result$discrepancy < 1e-6) {
    cat("  Status: Equivalent (discrepancy < 1e-6)\n")
  } else if (match_result$discrepancy < 1e-3) {
    cat("  Status: Approximately equivalent\n")
  } else {
    cat("  Status: NOT equivalent\n")
  }

  invisible(list(
    direction = direction,
    source_par = par,
    source_ll = source_ll,
    target_bounds = target_bounds,
    matched_par = match_result$par,
    target_ll = target_ll,
    discrepancy = match_result$discrepancy,
    success = TRUE
  ))
}
