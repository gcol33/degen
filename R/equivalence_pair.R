#' Create an equivalence pair for model comparison
#'
#' Pairs two model specifications for equivalence comparison. The pair
#' represents a question: "Are these two models observationally equivalent?"
#'
#' @param spec_a A `model_spec` object (first model)
#' @param spec_b A `model_spec` object (second model)
#' @param name Optional character string naming this comparison. If `NULL`,
#'   a name is generated from the model names.
#'
#' @return An S3 object of class `equivalence_pair` with components:
#' \describe{
#'   \item{spec_a}{First model specification}
#'   \item{spec_b}{Second model specification}
#'   \item{name}{Comparison name}
#'   \item{n_par_a}{Number of parameters in model A}
#'   \item{n_par_b}{Number of parameters in model B}
#' }
#'
#' @export
#'
#' @examples
#' # Compare exponential and gamma(shape=1) models
#' exp_spec <- model_spec(
#'   loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
#'   par_names = "lambda",
#'   par_bounds = list(lambda = c(1e-6, Inf)),
#'   name = "Exponential"
#' )
#'
#' gamma_spec <- model_spec(
#'   loglik_fn = function(y, rate) {
#'     sum(dgamma(y, shape = 1, rate = rate, log = TRUE))
#'   },
#'   par_names = "rate",
#'   par_bounds = list(rate = c(1e-6, Inf)),
#'   name = "Gamma(1)"
#' )
#'
#' pair <- equivalence_pair(exp_spec, gamma_spec)
#' print(pair)
equivalence_pair <- function(spec_a, spec_b, name = NULL) {
  # Validate inputs
if (!is_model_spec(spec_a)) {
    stop("`spec_a` must be a model_spec object", call. = FALSE)
  }
  if (!is_model_spec(spec_b)) {
    stop("`spec_b` must be a model_spec object", call. = FALSE)
  }

  if (!is.null(name) && (!is.character(name) || length(name) != 1)) {
    stop("`name` must be a single character string or NULL", call. = FALSE)
  }

  # Generate default name
  if (is.null(name)) {
    name <- paste(spec_a$name, "vs", spec_b$name)
  }

  structure(
    list(
      spec_a = spec_a,
      spec_b = spec_b,
      name = name,
      n_par_a = spec_a$n_par,
      n_par_b = spec_b$n_par
    ),
    class = "equivalence_pair"
  )
}

#' @export
print.equivalence_pair <- function(x, ...) {
  cat("<equivalence_pair>", x$name, "\n")
  cat("Model A:", x$spec_a$name, sprintf("(%d parameter%s)\n",
      x$n_par_a, if (x$n_par_a == 1) "" else "s"))
  cat("Model B:", x$spec_b$name, sprintf("(%d parameter%s)\n",
      x$n_par_b, if (x$n_par_b == 1) "" else "s"))
  invisible(x)
}

#' @export
summary.equivalence_pair <- function(object, ...) {
  cat("Equivalence Pair:", object$name, "\n")
  cat(strrep("-", 50), "\n\n")

  cat("Model A:", object$spec_a$name, "\n")
  cat("  Parameters:", paste(par_names(object$spec_a), collapse = ", "), "\n")
  for (p in par_names(object$spec_a)) {
    b <- object$spec_a$par_bounds[[p]]
    cat(sprintf("    %s: [%s, %s]\n", p,
                if (b[1] == -Inf) "-Inf" else format(b[1]),
                if (b[2] == Inf) "Inf" else format(b[2])))
  }

  cat("\nModel B:", object$spec_b$name, "\n")
  cat("  Parameters:", paste(par_names(object$spec_b), collapse = ", "), "\n")
  for (p in par_names(object$spec_b)) {
    b <- object$spec_b$par_bounds[[p]]
    cat(sprintf("    %s: [%s, %s]\n", p,
                if (b[1] == -Inf) "-Inf" else format(b[1]),
                if (b[2] == Inf) "Inf" else format(b[2])))
  }

  cat("\nParameter counts:", object$n_par_a, "vs", object$n_par_b)
  if (object$n_par_a == object$n_par_b) {
    cat(" (equal)\n")
  } else {
    cat(" (different)\n")
  }

  invisible(object)
}

#' @export
format.equivalence_pair <- function(x, ...) {
  sprintf("<equivalence_pair> %s (%d vs %d parameters)",
          x$name, x$n_par_a, x$n_par_b)
}

#' Test if object is an equivalence_pair
#'
#' @param x Object to test
#'
#' @return Logical indicating whether `x` is an `equivalence_pair` object
#' @export
#'
#' @examples
#' exp_spec <- model_spec(
#'   loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
#'   par_names = "lambda"
#' )
#' pair <- equivalence_pair(exp_spec, exp_spec)
#' is_equivalence_pair(pair)  # TRUE
#' is_equivalence_pair(list())
#' is_equivalence_pair(list())  # FALSE
is_equivalence_pair <- function(x) {
  inherits(x, "equivalence_pair")
}

#' Extract model specification from pair
#'
#' @param pair An `equivalence_pair` object
#'
#' @return A `model_spec` object
#' @name pair_accessors
#' @export
spec_a <- function(pair) {
  if (!is_equivalence_pair(pair)) {
    stop("`pair` must be an equivalence_pair object", call. = FALSE)
  }
  pair$spec_a
}

#' @rdname pair_accessors
#' @export
spec_b <- function(pair) {
  if (!is_equivalence_pair(pair)) {
    stop("`pair` must be an equivalence_pair object", call. = FALSE)
  }
  pair$spec_b
}

#' Get parameter dimensions for both models
#'
#' @param pair An `equivalence_pair` object
#'
#' @return Named integer vector with elements `a` and `b`
#' @export
#'
#' @examples
#' exp_spec <- model_spec(
#'   loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
#'   par_names = "lambda"
#' )
#' norm_spec <- model_spec(
#'   loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
#'   par_names = c("mu", "sigma")
#' )
#' pair <- equivalence_pair(exp_spec, norm_spec)
#' par_dims(pair)  # c(a = 1, b = 2)
par_dims <- function(pair) {
  if (!is_equivalence_pair(pair)) {
    stop("`pair` must be an equivalence_pair object", call. = FALSE)
  }
  c(a = pair$n_par_a, b = pair$n_par_b)
}

#' Check if models have same parameter count
#'
#' @param pair An `equivalence_pair` object
#'
#' @return Logical
#' @export
same_par_count <- function(pair) {
  if (!is_equivalence_pair(pair)) {
    stop("`pair` must be an equivalence_pair object", call. = FALSE)
  }
  pair$n_par_a == pair$n_par_b
}
