#' @export
print.model_spec <- function(x, ...) {
  cat("<model_spec>", x$name, "\n")
  cat("Parameters:", paste(x$par_names, collapse = ", "),
      sprintf("(%d)\n", x$n_par))

  # Show bounds for bounded parameters
  bounded <- vapply(x$par_bounds, function(b) {
    b[1] > -Inf || b[2] < Inf
  }, logical(1))

  if (any(bounded)) {
    cat("Bounds:\n")
    for (p in names(x$par_bounds)[bounded]) {
      b <- x$par_bounds[[p]]
      cat(sprintf("  %s in (%s, %s)\n",
                  p,
                  if (b[1] == -Inf) "-Inf" else format(b[1]),
                  if (b[2] == Inf) "Inf" else format(b[2])))
    }
  }

  invisible(x)
}

#' @export
summary.model_spec <- function(object, ...) {
  cat("Model Specification:", object$name, "\n")
  cat(strrep("-", 40), "\n")
  cat("Number of parameters:", object$n_par, "\n\n")

  cat("Parameters:\n")
  for (p in object$par_names) {
    b <- object$par_bounds[[p]]
    cat(sprintf("  %s: [%s, %s]\n",
                p,
                if (b[1] == -Inf) "-Inf" else format(b[1]),
                if (b[2] == Inf) "Inf" else format(b[2])))
  }

  cat("\nLog-likelihood function:\n")
  print(object$loglik_fn)

  invisible(object)
}

#' @export
format.model_spec <- function(x, ...) {
  sprintf("<model_spec> %s (%d parameters)", x$name, x$n_par)
}

#' Test if object is a model_spec
#'
#' @param x Object to test
#'
#' @return Logical indicating whether `x` is a `model_spec` object
#' @export
#'
#' @examples
#' spec <- model_spec(
#'   loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
#'   par_names = "lambda"
#' )
#' is_model_spec(spec)  # TRUE
#' is_model_spec(list())  # FALSE
is_model_spec <- function(x) {
  inherits(x, "model_spec")
}

#' @rdname par_names
#' @export
par_names.model_spec <- function(x, ...) {
  x$par_names
}

#' @rdname par_bounds
#' @export
par_bounds.model_spec <- function(x, ...) {
  x$par_bounds
}

#' @rdname n_par
#' @export
n_par.model_spec <- function(x, ...) {
  x$n_par
}

#' @rdname loglik
#' @export
#'
#' @examples
#' spec <- model_spec(
#'   loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
#'   par_names = "lambda",
#'   par_bounds = list(lambda = c(1e-6, Inf))
#' )
#' y <- rexp(100, rate = 2)
#' loglik(spec, y, par = c(lambda = 2))
loglik.model_spec <- function(object, y, par, ...) {
  evaluate_loglik(object, y, par)
}
