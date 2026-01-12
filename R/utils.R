#' Check that input is numeric
#'
#' @param x Object to check
#' @param name Name of the argument for error messages
#' @param allow_null Whether NULL is acceptable
#'
#' @return Invisibly returns TRUE if valid, otherwise throws an error
#' @noRd
check_numeric <- function(x, name = deparse(substitute(x)), allow_null = FALSE) {
  if (is.null(x)) {
    if (allow_null) return(invisible(TRUE))
    stop(sprintf("`%s` must not be NULL", name), call. = FALSE)
  }
  if (!is.numeric(x)) {
    stop(sprintf("`%s` must be numeric, not %s", name, class(x)[1]), call. = FALSE)
  }
  if (any(is.nan(x))) {
    stop(sprintf("`%s` contains NaN values", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Check that input is a function
#'
#' @param x Object to check
#' @param name Name of the argument for error messages
#' @param required_args Character vector of required argument names
#'
#' @return Invisibly returns TRUE if valid, otherwise throws an error
#' @noRd
check_function <- function(x, name = deparse(substitute(x)), required_args = NULL) {
  if (!is.function(x)) {
    stop(sprintf("`%s` must be a function, not %s", name, class(x)[1]), call. = FALSE)
  }
  if (!is.null(required_args)) {
    fn_args <- names(formals(x))
    missing_args <- setdiff(required_args, fn_args)
    if (length(missing_args) > 0) {
      stop(
        sprintf(
          "`%s` must have argument(s): %s",
          name,
          paste(missing_args, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

#' Check parameter bounds
#'
#' @param bounds Named list of bounds, each element a length-2 numeric vector
#' @param par_names Expected parameter names
#'
#' @return Invisibly returns TRUE if valid, otherwise throws an error
#' @noRd
check_bounds <- function(bounds, par_names) {
  if (is.null(bounds)) return(invisible(TRUE))

  if (!is.list(bounds)) {
    stop("`par_bounds` must be a named list", call. = FALSE)
  }

  if (is.null(names(bounds)) || any(names(bounds) == "")) {
    stop("`par_bounds` must be a named list", call. = FALSE)
  }

  unknown <- setdiff(names(bounds), par_names)
  if (length(unknown) > 0) {
    stop(
      sprintf(
        "Unknown parameter(s) in `par_bounds`: %s",
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  for (nm in names(bounds)) {
    b <- bounds[[nm]]
    if (!is.numeric(b) || length(b) != 2) {
      stop(
        sprintf("Bounds for `%s` must be a length-2 numeric vector", nm),
        call. = FALSE
      )
    }
    if (b[1] >= b[2]) {
      stop(
        sprintf("Lower bound must be less than upper bound for `%s`", nm),
        call. = FALSE
      )
    }
    if (any(is.nan(b))) {
      stop(sprintf("Bounds for `%s` contain NaN", nm), call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Check that parameter values are within bounds
#'
#' @param par Named numeric vector of parameter values
#' @param bounds Named list of bounds
#'
#' @return Invisibly returns TRUE if valid, otherwise throws an error
#' @noRd
check_par_in_bounds <- function(par, bounds) {
  if (is.null(bounds)) return(invisible(TRUE))

  for (nm in names(par)) {
    if (nm %in% names(bounds)) {
      b <- bounds[[nm]]
      if (par[nm] < b[1] || par[nm] > b[2]) {
        stop(
          sprintf(
            "Parameter `%s` = %g is outside bounds [%g, %g]",
            nm, par[nm], b[1], b[2]
          ),
          call. = FALSE
        )
      }
    }
  }

  invisible(TRUE)
}
