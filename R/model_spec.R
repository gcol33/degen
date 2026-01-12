#' Create a model specification
#'
#' Define a parametric model through its log-likelihood function. This is the
#' foundational object for equivalence and identifiability analysis.
#'
#' @param loglik_fn A function with signature `function(y, ...)` where `y` is
#'   the data and `...` are named parameters. Must return a scalar log-likelihood.
#' @param par_names Character vector of parameter names. Must match the argument
#'   names in `loglik_fn` (excluding `y`).
#' @param par_bounds Optional named list of parameter bounds. Each element should
#'   be a length-2 numeric vector `c(lower, upper)`. Parameters not specified
#'   default to `c(-Inf, Inf)`.
#' @param name Optional character string naming the model. If `NULL`, a name is
#'   generated from the parameter names.
#' @param validate Logical; if `TRUE` (default), validate inputs on construction.
#'   Set to `FALSE` for performance in tight loops.
#'
#' @return An S3 object of class `model_spec` with components:
#' \describe{
#'   \item{loglik_fn}{The log-likelihood function}
#'   \item{par_names}{Character vector of parameter names}
#'   \item{par_bounds}{Named list of bounds for each parameter}
#'   \item{n_par}{Number of parameters}
#'   \item{name}{Model name}
#' }
#'
#' @export
#'
#' @examples
#' # Exponential model
#' exp_spec <- model_spec(
#'   loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
#'   par_names = "lambda",
#'   par_bounds = list(lambda = c(1e-6, Inf)),
#'   name = "Exponential"
#' )
#'
#' # Normal model with two parameters
#' norm_spec <- model_spec(
#'   loglik_fn = function(y, mu, sigma) {
#'     sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
#'   },
#'   par_names = c("mu", "sigma"),
#'   par_bounds = list(sigma = c(1e-6, Inf))
#' )
model_spec <- function(loglik_fn,
                       par_names,
                       par_bounds = NULL,
                       name = NULL,
                       validate = TRUE) {
  if (validate) {
    # Validate loglik_fn
    check_function(loglik_fn, "loglik_fn", required_args = "y")

    # Validate par_names
    if (!is.character(par_names) || length(par_names) == 0) {
      stop("`par_names` must be a non-empty character vector", call. = FALSE)
    }
    if (any(duplicated(par_names))) {
      stop("`par_names` must not contain duplicates", call. = FALSE)
    }

    # Check that par_names match loglik_fn formals (excluding y)
    fn_args <- names(formals(loglik_fn))
    fn_par_args <- setdiff(fn_args, "y")

    missing_in_fn <- setdiff(par_names, fn_par_args)
    if (length(missing_in_fn) > 0) {
      stop(
        sprintf(
          "Parameter(s) not found in `loglik_fn` arguments: %s",
          paste(missing_in_fn, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    extra_in_fn <- setdiff(fn_par_args, par_names)
    if (length(extra_in_fn) > 0) {
      stop(
        sprintf(
          "`loglik_fn` has extra arguments not in `par_names`: %s",
          paste(extra_in_fn, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    # Validate bounds
    check_bounds(par_bounds, par_names)

    # Validate name
    if (!is.null(name) && (!is.character(name) || length(name) != 1)) {
      stop("`name` must be a single character string or NULL", call. = FALSE)
    }
  }

  # Fill in default bounds
  all_bounds <- lapply(par_names, function(p) {
    if (!is.null(par_bounds) && p %in% names(par_bounds)) {
      par_bounds[[p]]
    } else {
      c(-Inf, Inf)
    }
  })
  names(all_bounds) <- par_names

  # Generate default name if not provided
  if (is.null(name)) {
    name <- paste0("model(", paste(par_names, collapse = ", "), ")")
  }


structure(
    list(
      loglik_fn = loglik_fn,
      par_names = par_names,
      par_bounds = all_bounds,
      n_par = length(par_names),
      name = name
    ),
    class = "model_spec"
  )
}

#' Validate a model specification
#'
#' Perform comprehensive validation of a model_spec object, including testing
#' that the log-likelihood function is callable.
#'
#' @param spec A `model_spec` object
#' @param test_y Optional test data to use for validation
#' @param test_par Optional test parameters to use for validation
#'
#' @return Invisibly returns `TRUE` if valid, otherwise throws an error
#' @noRd
validate_model_spec <- function(spec, test_y = NULL, test_par = NULL) {
  if (!is_model_spec(spec)) {
    stop("`spec` must be a model_spec object", call. = FALSE)
  }

  # Test that loglik_fn is callable if test values provided
if (!is.null(test_y) && !is.null(test_par)) {
    result <- tryCatch(
      loglik(spec, test_y, test_par),
      error = function(e) {
        stop(
          sprintf("Log-likelihood evaluation failed: %s", e$message),
          call. = FALSE
        )
      }
    )

    if (!is.numeric(result) || length(result) != 1) {
      stop("Log-likelihood function must return a scalar numeric value", call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Evaluate log-likelihood (internal)
#'
#' @param spec A model_spec object
#' @param y Observed data
#' @param par Named numeric vector of parameters
#' @param check_bounds Whether to check parameter bounds
#'
#' @return Scalar log-likelihood value
#' @noRd
evaluate_loglik <- function(spec, y, par, check_bounds = TRUE) {
  # Convert list to vector if needed
  if (is.list(par)) {
    par <- unlist(par)
  }

  # Validate parameter names
  if (is.null(names(par))) {
    stop("`par` must be a named vector", call. = FALSE)
  }

  missing_pars <- setdiff(spec$par_names, names(par))
  if (length(missing_pars) > 0) {
    stop(
      sprintf("Missing parameter(s): %s", paste(missing_pars, collapse = ", ")),
      call. = FALSE
    )
  }

  # Check bounds
  if (check_bounds) {
    check_par_in_bounds(par, spec$par_bounds)
  }

  # Build argument list
  args <- c(list(y = y), as.list(par[spec$par_names]))

  # Evaluate
  result <- do.call(spec$loglik_fn, args)

  if (!is.numeric(result) || length(result) != 1) {
    stop("Log-likelihood function must return a scalar numeric value", call. = FALSE)
  }

  result
}
