# Parallel processing utilities for degen
#
# Provides optional parallelization using the parallel package.
# All parallel functions check for parallel availability and fall back
# to sequential processing if not available.

#' Set up parallel cluster
#'
#' Create a parallel cluster for use with degen functions.
#'
#' @param n_cores Number of cores to use. Default is `parallel::detectCores() - 1`
#' @param type Cluster type: "PSOCK" (default, works on all platforms) or "FORK"
#'   (Unix only, more efficient)
#'
#' @return A cluster object, or NULL if parallel is not available
#' @export
#'
#' @examples
#' \donttest{
#' cl <- setup_cluster(2)  # CRAN policy: max 2 cores
#' # Use cl with parallel-enabled functions
#' stop_cluster(cl)
#' }
setup_cluster <- function(n_cores = NULL, type = c("PSOCK", "FORK")) {

  type <- match.arg(type)


  if (!requireNamespace("parallel", quietly = TRUE)) {
    message("Package 'parallel' not available. Using sequential processing.")
    return(NULL)
  }

  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }

  if (n_cores <= 1) {
    return(NULL)
  }

  # FORK only works on Unix

if (type == "FORK" && .Platform$OS.type == "windows") {
    type <- "PSOCK"
  }

  cl <- parallel::makeCluster(n_cores, type = type)

  # Export degen namespace to workers
  parallel::clusterCall(cl, function() {
    if (requireNamespace("degen", quietly = TRUE)) {
      library(degen)
    }
  })

  cl
}

#' Stop parallel cluster
#'
#' @param cl Cluster object from `setup_cluster()`
#'
#' @return NULL invisibly
#' @export
stop_cluster <- function(cl) {
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }
  invisible(NULL)
}

#' Apply function in parallel or sequentially
#'
#' Internal helper that uses parallel processing if a cluster is provided,
#' otherwise falls back to sequential lapply.
#'
#' @param X List or vector to iterate over
#' @param FUN Function to apply
#' @param cl Optional cluster from `setup_cluster()`
#' @param ... Additional arguments passed to FUN
#'
#' @return List of results
#' @noRd
par_lapply <- function(X, FUN, cl = NULL, ...) {
  if (is.null(cl)) {
    lapply(X, FUN, ...)
  } else {
    parallel::parLapply(cl, X, FUN, ...)
  }
}

#' Apply function in parallel with progress (internal)
#' @noRd
par_lapply_progress <- function(X, FUN, cl = NULL, verbose = FALSE, ...) {
  n <- length(X)

  if (is.null(cl)) {
    # Sequential with optional progress
    results <- vector("list", n)
    for (i in seq_len(n)) {
      results[[i]] <- FUN(X[[i]], ...)
      if (verbose && i %% 10 == 0) {
        cat(sprintf("  %d/%d complete\n", i, n))
      }
    }
    results
  } else {
    # Parallel (no progress available)
    parallel::parLapply(cl, X, FUN, ...)
  }
}
