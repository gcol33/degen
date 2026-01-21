# Export utilities for degen objects
#
# Provides as.data.frame methods and export functions for results.

#' Convert surface_comparison to data frame
#'
#' Extract the evidence from a surface comparison as a tidy data frame.
#'
#' @param x A `surface_comparison` object
#' @param ... Additional arguments (ignored)
#'
#' @return A data frame with columns for parameters, source likelihood,
#'   discrepancy, and direction
#' @export
#'
#' @examples
#' exp_spec <- model_spec_exponential()
#' gamma_spec <- model_spec_gamma(par = "rate", known_shape = 1)
#' pair <- equivalence_pair(exp_spec, gamma_spec)
#' y <- rexp(50, rate = 2)
#' result <- compare_surfaces(pair, y, n_points = 10)
#' df <- as.data.frame(result)
#' head(df)
as.data.frame.surface_comparison <- function(x, ...) {
  evidence <- x$evidence

  # Combine A->B and B->A evidence
  dfs <- list()

  if (!is.null(evidence$A_to_B) && nrow(evidence$A_to_B) > 0) {
    df_ab <- evidence$A_to_B
    df_ab$direction <- "A_to_B"
    dfs$ab <- df_ab
}

  if (!is.null(evidence$B_to_A) && nrow(evidence$B_to_A) > 0) {
    df_ba <- evidence$B_to_A
    df_ba$direction <- "B_to_A"
    dfs$ba <- df_ba
  }

  # Use rbind.fill-like behavior for different columns
  if (length(dfs) == 0) {
    return(data.frame())
  }

  # Get all column names
  all_cols <- unique(unlist(lapply(dfs, names)))

  # Add missing columns as NA
  for (i in seq_along(dfs)) {
    missing_cols <- setdiff(all_cols, names(dfs[[i]]))
    for (col in missing_cols) {
      dfs[[i]][[col]] <- NA
    }
    dfs[[i]] <- dfs[[i]][, all_cols, drop = FALSE]
  }

  result <- do.call(rbind, dfs)
  rownames(result) <- NULL
  result
}


#' Convert identifiability_result to data frame
#'
#' Extract parameter identifiability status as a data frame.
#'
#' @param x An `identifiability_result` object
#' @param ... Additional arguments (ignored)
#'
#' @return A data frame with columns: parameter, status, eigenvalue
#' @export
#'
#' @examples
#' spec <- model_spec_normal()
#' y <- rnorm(100, mean = 5, sd = 2)
#' result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))
#' as.data.frame(result)
as.data.frame.identifiability_result <- function(x, ...) {
  data.frame(
    parameter = x$par_names,
    value = x$par,
    status = x$status,
    stringsAsFactors = FALSE
  )
}


#' Convert equiv_classes to data frame
#'
#' Extract equivalence class membership as a data frame.
#'
#' @param x An `equiv_classes` object
#' @param ... Additional arguments (ignored)
#'
#' @return A data frame with columns: model, class
#' @export
#'
#' @examples
#' exp_spec <- model_spec_exponential()
#' gamma1_spec <- model_spec_gamma(par = "rate", known_shape = 1)
#' gamma2_spec <- model_spec_gamma(par = "rate", known_shape = 2)
#' models <- list(exp = exp_spec, gamma1 = gamma1_spec, gamma2 = gamma2_spec)
#' y <- rexp(50, rate = 2)
#' classes <- equivalence_classes(models, y, n_points = 10)
#' as.data.frame(classes)
as.data.frame.equiv_classes <- function(x, ...) {
  data.frame(
    model = x$model_names,
    class = x$membership,
    stringsAsFactors = FALSE
  )
}


#' Convert fisher_info to data frame
#'
#' Extract Fisher information matrix as a data frame.
#'
#' @param x A `fisher_info` object
#' @param row.names Ignored
#' @param optional Ignored
#' @param what What to extract: "matrix" (default), "eigenvalues", or "summary"
#' @param ... Additional arguments (ignored)
#'
#' @return A data frame
#' @export
#'
#' @examples
#' spec <- model_spec_normal()
#' y <- rnorm(100, mean = 5, sd = 2)
#' info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))
#' as.data.frame(info)
#' as.data.frame(info, what = "eigenvalues")
as.data.frame.fisher_info <- function(x, row.names = NULL, optional = FALSE,
                                      what = c("matrix", "eigenvalues", "summary"),
                                      ...) {
  what <- match.arg(what)

  if (what == "matrix") {
    as.data.frame(x$matrix)
  } else if (what == "eigenvalues") {
    data.frame(
      eigenvalue = x$eigenvalues,
      cumulative_variance = cumsum(x$eigenvalues) / sum(x$eigenvalues),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      parameter = x$par_names,
      value = x$par,
      condition = x$condition,
      rank = x$rank,
      n_par = x$n_par,
      type = x$type,
      stringsAsFactors = FALSE
    )
  }
}


#' Export results to file
#'
#' Write degen results to CSV or JSON format.
#'
#' @param x A degen result object (surface_comparison, identifiability_result,
#'   equiv_classes, or fisher_info)
#' @param file Output file path. Extension determines format (.csv or .json)
#' @param ... Additional arguments passed to write functions
#'
#' @return Invisible file path
#' @export
#'
#' @examples
#' spec <- model_spec_normal()
#' y <- rnorm(100, mean = 5, sd = 2)
#' result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))
#' tmp <- tempfile(fileext = ".csv")
#' export_results(result, tmp)
#' unlink(tmp)
export_results <- function(x, file, ...) {
  ext <- tolower(tools::file_ext(file))

  df <- as.data.frame(x)

  if (ext == "csv") {
    utils::write.csv(df, file, row.names = FALSE, ...)
  } else if (ext == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package 'jsonlite' required for JSON export", call. = FALSE)
    }
    jsonlite::write_json(df, file, ...)
  } else {
    stop("Unsupported file format. Use .csv or .json", call. = FALSE)
  }

  invisible(file)
}


#' Format results as LaTeX table
#'
#' Generate LaTeX table code for degen results.
#'
#' @param x A degen result object
#' @param caption Optional table caption
#' @param label Optional LaTeX label
#' @param ... Additional arguments passed to as.data.frame
#'
#' @return Character string containing LaTeX code
#' @export
#'
#' @examples
#' spec <- model_spec_normal()
#' y <- rnorm(100, mean = 5, sd = 2)
#' result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))
#' cat(to_latex(result))
to_latex <- function(x, caption = NULL, label = NULL, ...) {
  df <- as.data.frame(x, ...)
  n_col <- ncol(df)
  n_row <- nrow(df)

  # Build LaTeX table
  lines <- character()

  # Begin table
  lines <- c(lines, "\\begin{table}[htbp]")
  lines <- c(lines, "\\centering")

  if (!is.null(caption)) {
    lines <- c(lines, sprintf("\\caption{%s}", caption))
  }
  if (!is.null(label)) {
    lines <- c(lines, sprintf("\\label{%s}", label))
  }

  # Column alignment
  col_spec <- paste(rep("l", n_col), collapse = "")
  lines <- c(lines, sprintf("\\begin{tabular}{%s}", col_spec))
  lines <- c(lines, "\\hline")

  # Header
  header <- paste(names(df), collapse = " & ")
  lines <- c(lines, paste0(header, " \\\\"))
  lines <- c(lines, "\\hline")

  # Data rows
  for (i in seq_len(n_row)) {
    row_vals <- vapply(df[i, ], function(val) {
      if (is.numeric(val)) {
        format(val, digits = 4)
      } else {
        as.character(val)
      }
    }, character(1))
    row <- paste(row_vals, collapse = " & ")
    lines <- c(lines, paste0(row, " \\\\"))
  }

  lines <- c(lines, "\\hline")
  lines <- c(lines, "\\end{tabular}")
  lines <- c(lines, "\\end{table}")

  paste(lines, collapse = "\n")
}
