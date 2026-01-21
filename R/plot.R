# Plot methods for degen objects
# All plots use base R graphics to minimize dependencies

#' Plot Fisher information diagnostics
#'
#' Visualize the eigenvalue spectrum and parameter loadings of a Fisher
#' information matrix.
#'
#' @param x A `fisher_info` object
#' @param type Type of plot: "eigenvalues" (default), "loadings", or "both"
#' @param ... Additional arguments passed to plotting functions
#'
#' @return Invisible `x`
#' @export
#'
#' @examples
#' spec <- model_spec(
#'   loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
#'   par_names = c("mu", "sigma"),
#'   par_bounds = list(sigma = c(1e-6, Inf))
#' )
#' y <- rnorm(100, mean = 5, sd = 2)
#' info <- fisher_information(spec, y, par = c(mu = 5, sigma = 2))
#' plot(info)
#' plot(info, type = "loadings")
plot.fisher_info <- function(x, type = c("eigenvalues", "loadings", "both"), ...) {
  type <- match.arg(type)

  if (type == "both") {
    old_par <- graphics::par(mfrow = c(1, 2))
    on.exit(graphics::par(old_par))
    plot_eigenvalues(x, ...)
    plot_loadings(x, ...)
  } else if (type == "eigenvalues") {
    plot_eigenvalues(x, ...)
  } else {
    plot_loadings(x, ...)
  }

  invisible(x)
}

#' Plot eigenvalue spectrum
#' @noRd
plot_eigenvalues <- function(x, main = "Eigenvalue Spectrum", ...) {
  eig <- x$eigenvalues
  n <- length(eig)

  # Color by magnitude (red for near-zero)
  cols <- ifelse(abs(eig) < 0.01 * max(abs(eig)), "firebrick", "steelblue")

  graphics::barplot(
    eig,
    names.arg = paste0("eig_", seq_len(n)),
    main = main,
    ylab = "Eigenvalue",
    col = cols,
    border = NA,
    ...
  )

  # Add reference line at zero
  graphics::abline(h = 0, lty = 2, col = "gray50")

  # Add condition number annotation
  graphics::mtext(
    sprintf("Condition: %.2g | Rank: %d/%d", x$condition, x$rank, x$n_par),
    side = 3, line = 0.3, cex = 0.8
  )
}

#' Plot parameter loadings on eigenvectors
#' @noRd
plot_loadings <- function(x, main = "Parameter Loadings", ...) {
  loadings <- x$eigenvectors
  n_par <- nrow(loadings)
  n_eig <- ncol(loadings)

  # Create heatmap-style plot
  image_matrix <- t(abs(loadings))[n_eig:1, , drop = FALSE]

  graphics::image(
    x = seq_len(n_par),
    y = seq_len(n_eig),
    z = t(image_matrix),
    col = grDevices::hcl.colors(20, "Blues", rev = TRUE),
    xlab = "Parameter",
    ylab = "Eigenvector",
    main = main,
    axes = FALSE,
    ...
  )

  graphics::axis(1, at = seq_len(n_par), labels = x$par_names, las = 2)
  graphics::axis(2, at = seq_len(n_eig), labels = rev(paste0("eig_", seq_len(n_eig))), las = 1)
  graphics::box()

  # Add values as text
  for (i in seq_len(n_par)) {
    for (j in seq_len(n_eig)) {
      val <- abs(loadings[i, n_eig - j + 1])
      if (val > 0.1) {
        graphics::text(i, j, sprintf("%.2f", val), cex = 0.7,
                       col = if (val > 0.5) "white" else "black")
      }
    }
  }
}


#' Plot surface comparison results
#'
#' Visualize the likelihood discrepancies from a surface comparison.
#'
#' @param x A `surface_comparison` object
#' @param type Type of plot: "discrepancy" (default) or "histogram"
#' @param ... Additional arguments passed to plotting functions
#'
#' @return Invisible `x`
#' @export
#'
#' @examples
#' exp_spec <- model_spec(
#'   loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
#'   par_names = "lambda",
#'   par_bounds = list(lambda = c(1e-6, 100))
#' )
#' gamma_spec <- model_spec(
#'   loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
#'   par_names = "rate",
#'   par_bounds = list(rate = c(1e-6, 100))
#' )
#' pair <- equivalence_pair(exp_spec, gamma_spec)
#' y <- rexp(50, rate = 2)
#' result <- compare_surfaces(pair, y, n_points = 20)
#' plot(result)
plot.surface_comparison <- function(x, type = c("discrepancy", "histogram"), ...) {
  type <- match.arg(type)

  if (type == "discrepancy") {
    plot_discrepancy_points(x, ...)
  } else {
    plot_discrepancy_histogram(x, ...)
  }

  invisible(x)
}

#' Plot discrepancy vs parameter values
#' @noRd
plot_discrepancy_points <- function(x, main = NULL, ...) {
  evidence <- x$evidence

  # Set up 2-panel plot for A->B and B->A
  old_par <- graphics::par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  on.exit(graphics::par(old_par))

  # Tolerance line
  tol <- x$tol

  # A -> B direction
  ev_ab <- evidence$A_to_B
  if (!is.null(ev_ab) && nrow(ev_ab) > 0) {
    disc_ab <- ev_ab$discrepancy
    idx <- seq_along(disc_ab)

    # Color by pass/fail
    cols <- ifelse(disc_ab < tol, "steelblue", "firebrick")

    graphics::plot(
      idx, disc_ab,
      pch = 19, col = cols,
      xlab = "Sample point",
      ylab = "Discrepancy",
      main = expression(A %->% B),
      log = "y",
      ...
    )
    graphics::abline(h = tol, lty = 2, col = "gray50")
    graphics::mtext(sprintf("Max: %.2e", max(disc_ab, na.rm = TRUE)),
                    side = 3, line = 0.2, cex = 0.7)
  }

  # B -> A direction
  ev_ba <- evidence$B_to_A
  if (!is.null(ev_ba) && nrow(ev_ba) > 0) {
    disc_ba <- ev_ba$discrepancy
    idx <- seq_along(disc_ba)

    cols <- ifelse(disc_ba < tol, "steelblue", "firebrick")

    graphics::plot(
      idx, disc_ba,
      pch = 19, col = cols,
      xlab = "Sample point",
      ylab = "Discrepancy",
      main = expression(B %->% A),
      log = "y",
      ...
    )
    graphics::abline(h = tol, lty = 2, col = "gray50")
    graphics::mtext(sprintf("Max: %.2e", max(disc_ba, na.rm = TRUE)),
                    side = 3, line = 0.2, cex = 0.7)
  }

  # Overall title
  equiv_str <- if (x$equivalent) "EQUIVALENT" else "NOT EQUIVALENT"
  graphics::mtext(equiv_str, outer = TRUE, line = -1.5, cex = 1.2,
                  col = if (x$equivalent) "darkgreen" else "firebrick")
}

#' Plot histogram of discrepancies
#' @noRd
plot_discrepancy_histogram <- function(x, main = "Discrepancy Distribution", ...) {
  # Combine all discrepancies
  all_disc <- c(
    x$evidence$A_to_B$discrepancy,
    x$evidence$B_to_A$discrepancy
  )
  all_disc <- all_disc[is.finite(all_disc)]

  graphics::hist(
    log10(all_disc + 1e-16),
    breaks = 20,
    main = main,
    xlab = expression(log[10](discrepancy)),
    col = "steelblue",
    border = "white",
    ...
  )

  # Add tolerance line
  graphics::abline(v = log10(x$tol), lty = 2, col = "firebrick", lwd = 2)
  graphics::legend("topright", legend = sprintf("tol = %.0e", x$tol),
                   lty = 2, col = "firebrick", bty = "n")
}


#' Plot identifiability diagnostics
#'
#' Visualize parameter identifiability status and Fisher information diagnostics.
#'
#' @param x An `identifiability_result` object
#' @param type Type of plot: "status" (default), "profile", or "both"
#' @param ... Additional arguments passed to plotting functions
#'
#' @return Invisible `x`
#' @export
#'
#' @examples
#' spec <- model_spec(
#'   loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
#'   par_names = c("mu", "sigma"),
#'   par_bounds = list(sigma = c(1e-6, Inf))
#' )
#' y <- rnorm(100, mean = 5, sd = 2)
#' result <- identifiability_check(spec, y, par = c(mu = 5, sigma = 2))
#' plot(result)
plot.identifiability_result <- function(x, type = c("status", "profile", "both"), ...) {
  type <- match.arg(type)

  if (type == "both") {
    if (is.null(x$profile_results)) {
      message("No profile likelihood results available. Showing status only.")
      plot_identifiability_status(x, ...)
    } else {
      old_par <- graphics::par(mfrow = c(1, 2))
      on.exit(graphics::par(old_par))
      plot_identifiability_status(x, ...)
      plot_profile_grid(x, ...)
    }
  } else if (type == "status") {
    plot_identifiability_status(x, ...)
  } else {
    if (is.null(x$profile_results)) {
      stop("No profile likelihood results. Run identifiability_check with level='profile'",
           call. = FALSE)
    }
    plot_profile_grid(x, ...)
  }

  invisible(x)
}

#' Plot parameter status
#' @noRd
plot_identifiability_status <- function(x, main = "Parameter Identifiability", ...) {
  status <- x$status
  n_par <- length(status)

  # Map status to colors and numeric values
  status_colors <- c(
    "identified" = "forestgreen",
    "weakly_identified" = "orange",
    "non_identifiable" = "firebrick"
  )
  status_values <- c(
    "identified" = 3,
    "weakly_identified" = 2,
    "non_identifiable" = 1
  )

  cols <- status_colors[status]
  vals <- status_values[status]

  graphics::barplot(
    vals,
    names.arg = x$par_names,
    col = cols,
    border = NA,
    main = main,
    ylab = "",
    ylim = c(0, 3.5),
    yaxt = "n",
    ...
  )

  # Add status labels
  graphics::axis(2, at = c(1, 2, 3),
                 labels = c("Non-ID", "Weak", "Identified"),
                 las = 1, tick = FALSE)

  # Add condition number
  graphics::mtext(
    sprintf("Condition: %.2g | Rank: %d/%d", x$condition, x$rank, x$n_par),
    side = 3, line = 0.3, cex = 0.8
  )
}

#' Plot profile likelihoods in a grid
#' @noRd
plot_profile_grid <- function(x, main = "Profile Likelihoods", ...) {
  profiles <- x$profile_results
  n_par <- length(profiles)

  if (n_par == 1) {
    plot_single_profile(profiles[[1]], x$par[1], x$status[1])
  } else {
    # Multi-panel layout
    n_col <- ceiling(sqrt(n_par))
    n_row <- ceiling(n_par / n_col)
    old_par <- graphics::par(mfrow = c(n_row, n_col), mar = c(3, 3, 2, 1))
    on.exit(graphics::par(old_par))

    for (i in seq_len(n_par)) {
      plot_single_profile(profiles[[i]], x$par[i], x$status[i])
    }
  }
}

#' Plot a single profile likelihood
#' @noRd
plot_single_profile <- function(profile_df, mle_value, status) {
  par_name <- profile_df$parameter[1]

  # Normalize to max = 0
  ll <- profile_df$loglik
  ll_norm <- ll - max(ll, na.rm = TRUE)

  status_col <- switch(status,
    "identified" = "forestgreen",
    "weakly_identified" = "orange",
    "non_identifiable" = "firebrick",
    "gray50"
  )

  graphics::plot(
    profile_df$value, ll_norm,
    type = "l",
    lwd = 2,
    col = status_col,
    xlab = par_name,
    ylab = expression(log(L) - log(L[max])),
    main = par_name
  )

  # Add MLE line
  graphics::abline(v = mle_value, lty = 2, col = "gray50")

  # Add 95% CI threshold
  graphics::abline(h = -1.92, lty = 3, col = "gray50")
}


#' Plot profile likelihood for a parameter
#'
#' Plot the profile likelihood curve for a single parameter with confidence
#' interval bounds.
#'
#' @param profile A data frame from `profile_likelihood()` or parameter name
#' @param ci_level Confidence level for interval (default 0.95)
#' @param mle Optional MLE value to mark on plot
#' @param ... Additional arguments passed to `plot()`
#'
#' @return Invisible profile data
#' @export
#'
#' @examples
#' spec <- model_spec(
#'   loglik_fn = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
#'   par_names = c("mu", "sigma"),
#'   par_bounds = list(sigma = c(1e-6, Inf))
#' )
#' y <- rnorm(100, mean = 5, sd = 2)
#' prof <- profile_likelihood(spec, y, par = c(mu = 5, sigma = 2), which_par = "mu")
#' plot_profile_likelihood(prof, mle = 5)
plot_profile_likelihood <- function(profile, ci_level = 0.95, mle = NULL, ...) {
  if (!is.data.frame(profile)) {
    stop("`profile` must be a data frame from profile_likelihood()", call. = FALSE)
  }

  par_name <- profile$parameter[1]
  ll <- profile$loglik
  vals <- profile$value

  # Normalize

  ll_norm <- ll - max(ll, na.rm = TRUE)

  # CI threshold
  threshold <- -stats::qchisq(ci_level, df = 1) / 2

  graphics::plot(
    vals, ll_norm,
    type = "l",
    lwd = 2,
    col = "steelblue",
    xlab = par_name,
    ylab = expression(log(L) - log(L[max])),
    main = sprintf("Profile Likelihood: %s", par_name),
    ...
  )

  # Add CI threshold
  graphics::abline(h = threshold, lty = 2, col = "firebrick")

  # Find CI bounds (approximate)
  above_threshold <- which(ll_norm >= threshold)
  if (length(above_threshold) > 0) {
    ci_lower <- vals[min(above_threshold)]
    ci_upper <- vals[max(above_threshold)]
    graphics::arrows(ci_lower, threshold, ci_upper, threshold,
                     code = 3, angle = 90, length = 0.05, col = "firebrick")
    graphics::mtext(sprintf("%.0f%% CI: [%.3g, %.3g]", ci_level * 100, ci_lower, ci_upper),
                    side = 3, line = 0.3, cex = 0.8)
  }

  # Add MLE if provided
  if (!is.null(mle)) {
    graphics::abline(v = mle, lty = 2, col = "gray50")
    graphics::points(mle, 0, pch = 19, col = "steelblue")
  }

  invisible(profile)
}


#' Plot equivalence classes
#'
#' Visualize equivalence relationships between models as a network diagram.
#'
#' @param x An `equiv_classes` object
#' @param layout Layout algorithm: "circle" (default), "grid", or "spring"
#' @param ... Additional arguments passed to plotting functions
#'
#' @return Invisible `x`
#' @export
#'
#' @examples
#' exp_spec <- model_spec(
#'   loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
#'   par_names = "lambda",
#'   par_bounds = list(lambda = c(1e-6, 100))
#' )
#' gamma1_spec <- model_spec(
#'   loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
#'   par_names = "rate",
#'   par_bounds = list(rate = c(1e-6, 100))
#' )
#' gamma2_spec <- model_spec(
#'   loglik_fn = function(y, rate) sum(dgamma(y, shape = 2, rate = rate, log = TRUE)),
#'   par_names = "rate",
#'   par_bounds = list(rate = c(1e-6, 100))
#' )
#' models <- list(exp = exp_spec, gamma1 = gamma1_spec, gamma2 = gamma2_spec)
#' y <- rexp(50, rate = 2)
#' classes <- equivalence_classes(models, y, n_points = 10)
#' plot(classes)
plot.equiv_classes <- function(x, layout = c("circle", "grid", "spring"), ...) {
  layout <- match.arg(layout)

  n_models <- x$n_models
  model_names <- x$model_names
  membership <- x$membership
  equiv_matrix <- x$pairwise

  # Generate node positions
  pos <- switch(layout,
    "circle" = layout_circle(n_models),
    "grid" = layout_grid(n_models),
    "spring" = layout_spring(n_models, equiv_matrix)
  )

  # Set up plot
  graphics::plot.new()
  graphics::plot.window(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), asp = 1)
  graphics::title(main = sprintf("Equivalence Classes (%d models -> %d classes)",
                                 n_models, x$n_classes))

  # Draw edges for equivalent pairs
  for (i in seq_len(n_models - 1)) {
    for (j in (i + 1):n_models) {
      if (isTRUE(equiv_matrix[i, j])) {
        graphics::segments(
          pos[i, 1], pos[i, 2],
          pos[j, 1], pos[j, 2],
          col = "forestgreen", lwd = 2
        )
      }
    }
  }

  # Draw nodes
  # Color by class membership
  class_colors <- grDevices::hcl.colors(x$n_classes, "Set2")
  node_colors <- class_colors[membership]

  graphics::points(pos[, 1], pos[, 2], pch = 19, cex = 3, col = node_colors)
  graphics::points(pos[, 1], pos[, 2], pch = 1, cex = 3, col = "black")

  # Add labels
  graphics::text(pos[, 1], pos[, 2] - 0.2, labels = model_names,
                 pos = 1, cex = 0.8, offset = 1)

  # Legend
  graphics::legend("bottomright",
                   legend = paste("Class", seq_len(x$n_classes)),
                   fill = class_colors,
                   bty = "n",
                   cex = 0.8)

  invisible(x)
}

#' Circle layout
#' @noRd
layout_circle <- function(n) {
  angles <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  cbind(cos(angles), sin(angles))
}

#' Grid layout
#' @noRd
layout_grid <- function(n) {
  n_col <- ceiling(sqrt(n))
  n_row <- ceiling(n / n_col)
  x <- rep(seq_len(n_col), n_row)[seq_len(n)]
  y <- rep(seq_len(n_row), each = n_col)[seq_len(n)]
  # Normalize to [-1, 1]
  x <- (x - mean(range(x))) / max(1, diff(range(x)) / 2)
  y <- (y - mean(range(y))) / max(1, diff(range(y)) / 2)
  cbind(x, y)
}

#' Simple spring layout (force-directed)
#' @noRd
layout_spring <- function(n, adj_matrix, iterations = 50) {
  # Start with circle

  pos <- layout_circle(n)

  # Simple force-directed adjustment

  for (iter in seq_len(iterations)) {
    forces <- matrix(0, n, 2)

    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (i != j) {
          diff <- pos[i, ] - pos[j, ]
          dist <- sqrt(sum(diff^2)) + 0.01

          # Repulsion (all pairs)
          forces[i, ] <- forces[i, ] + diff / dist^2 * 0.1

          # Attraction (equivalent pairs)
          if (isTRUE(adj_matrix[i, j])) {
            forces[i, ] <- forces[i, ] - diff * 0.05
          }
        }
      }
    }

    # Apply forces with damping
    pos <- pos + forces * 0.1

    # Keep in bounds
    pos <- pmax(pmin(pos, 1.2), -1.2)
  }

  pos
}


#' Plot likelihood surface
#'
#' Create a 2D contour plot of the log-likelihood surface for two-parameter
#' models. Useful for visualizing parameter identifiability and correlation.
#'
#' @param spec A `model_spec` object with exactly 2 parameters
#' @param y Numeric vector of observed data
#' @param par Named numeric vector of parameter values (used as center point)
#' @param which_par Optional character vector of length 2 specifying which
#'   parameters to plot (for models with >2 parameters)
#' @param n_grid Number of grid points per dimension (default 50)
#' @param range_mult Multiplier for parameter range around center (default 2)
#' @param type Plot type: "contour" (default), "filled", or "both"
#' @param show_mle Logical; show MLE point (default TRUE)
#' @param ... Additional arguments passed to plotting functions
#'
#' @return Invisible list with grid coordinates and likelihood values
#' @export
#'
#' @examples
#' spec <- model_spec_normal()
#' y <- rnorm(100, mean = 5, sd = 2)
#' plot_likelihood_surface(spec, y, par = c(mu = 5, sigma = 2))
#' plot_likelihood_surface(spec, y, par = c(mu = 5, sigma = 2), type = "filled")
plot_likelihood_surface <- function(spec, y, par,
                                    which_par = NULL,
                                    n_grid = 50,
                                    range_mult = 2,
                                    type = c("contour", "filled", "both"),
                                    show_mle = TRUE,
                                    ...) {
  if (!is_model_spec(spec)) {
    stop("`spec` must be a model_spec object", call. = FALSE)
  }

  type <- match.arg(type)


  # Determine which parameters to plot
  if (is.null(which_par)) {
    if (length(spec$par_names) != 2) {
      stop("Model has ", length(spec$par_names), " parameters. ",
           "Specify `which_par` to select 2 for plotting.", call. = FALSE)
    }
    which_par <- spec$par_names
  } else {
    if (length(which_par) != 2) {
      stop("`which_par` must specify exactly 2 parameters", call. = FALSE)
    }
    if (!all(which_par %in% spec$par_names)) {
      stop("Parameters not found: ",
           paste(setdiff(which_par, spec$par_names), collapse = ", "),
           call. = FALSE)
    }
  }

  par1_name <- which_par[1]
  par2_name <- which_par[2]
  par1_idx <- match(par1_name, spec$par_names)
  par2_idx <- match(par2_name, spec$par_names)

  # Get bounds
  bounds <- propose_bounds(spec, y)
  par1_center <- par[par1_name]
  par2_center <- par[par2_name]

  # Create grid ranges
  par1_range <- get_plot_range(par1_center, bounds[[par1_name]], range_mult)
  par2_range <- get_plot_range(par2_center, bounds[[par2_name]], range_mult)

  par1_seq <- seq(par1_range[1], par1_range[2], length.out = n_grid)
  par2_seq <- seq(par2_range[1], par2_range[2], length.out = n_grid)

  # Compute likelihood surface
  ll_matrix <- matrix(NA_real_, nrow = n_grid, ncol = n_grid)

  for (i in seq_len(n_grid)) {
    for (j in seq_len(n_grid)) {
      test_par <- par
      test_par[par1_name] <- par1_seq[i]
      test_par[par2_name] <- par2_seq[j]

      ll_matrix[i, j] <- tryCatch(
        loglik(spec, y, test_par),
        error = function(e) NA_real_
      )
    }
  }

  # Normalize to max = 0
  ll_max <- max(ll_matrix, na.rm = TRUE)
  ll_norm <- ll_matrix - ll_max

  # Create plot
  if (type == "both") {
    old_par <- graphics::par(mfrow = c(1, 2))
    on.exit(graphics::par(old_par))
    plot_filled_contour(par1_seq, par2_seq, ll_norm, par1_name, par2_name,
                        par1_center, par2_center, show_mle)
    plot_contour_lines(par1_seq, par2_seq, ll_norm, par1_name, par2_name,
                       par1_center, par2_center, show_mle, ...)
  } else if (type == "filled") {
    plot_filled_contour(par1_seq, par2_seq, ll_norm, par1_name, par2_name,
                        par1_center, par2_center, show_mle)
  } else {
    plot_contour_lines(par1_seq, par2_seq, ll_norm, par1_name, par2_name,
                       par1_center, par2_center, show_mle, ...)
  }

  invisible(list(
    par1 = par1_seq,
    par2 = par2_seq,
    loglik = ll_matrix,
    loglik_norm = ll_norm
  ))
}

#' Get plotting range for a parameter
#' @noRd
get_plot_range <- function(center, bounds, mult) {
  # Handle infinite bounds
  lower <- if (is.finite(bounds[1])) bounds[1] else center - abs(center) * mult

  upper <- if (is.finite(bounds[2])) bounds[2] else center + abs(center) * mult

  # Expand around center
  width <- max(upper - lower, abs(center) * 0.5)
  range_lower <- max(lower, center - width * mult / 2)
  range_upper <- min(upper, center + width * mult / 2)

  # Ensure positive for parameters that must be positive
  if (bounds[1] >= 0) {
    range_lower <- max(range_lower, bounds[1] + 1e-6)
  }

  c(range_lower, range_upper)
}

#' Plot filled contour
#' @noRd
plot_filled_contour <- function(x, y, z, xlab, ylab, x_mle, y_mle, show_mle) {
  # Custom color palette
  cols <- grDevices::hcl.colors(20, "YlOrRd", rev = TRUE)

  graphics::filled.contour(
    x, y, z,
    color.palette = function(n) grDevices::hcl.colors(n, "YlOrRd", rev = TRUE),
    xlab = xlab,
    ylab = ylab,
    main = "Log-Likelihood Surface",
    plot.axes = {
      graphics::axis(1)
      graphics::axis(2)
      if (show_mle) {
        graphics::points(x_mle, y_mle, pch = 4, cex = 2, lwd = 2)
      }
      # Add CI contours
      graphics::contour(x, y, z, levels = c(-1.92, -3.00, -4.61),
                        add = TRUE, col = "white", lwd = 1, lty = 2,
                        labels = c("95%", "90%", "80%"))
    }
  )
}

#' Plot contour lines
#' @noRd
plot_contour_lines <- function(x, y, z, xlab, ylab, x_mle, y_mle, show_mle, ...) {
  # CI levels (chi-squared / 2 for profile likelihood)
  ci_levels <- -c(1.92, 3.00, 4.61, 6.63)  # 95%, 90%, 80%, 70%

  graphics::contour(
    x, y, z,
    xlab = xlab,
    ylab = ylab,
    main = "Log-Likelihood Contours",
    levels = ci_levels,
    labels = c("95%", "90%", "80%", "70%"),
    col = "steelblue",
    lwd = 1.5,
    ...
  )

  # Add more contours at regular intervals
  graphics::contour(
    x, y, z,
    levels = seq(-20, -2, by = 2),
    add = TRUE,
    col = "gray70",
    lty = 3,
    labels = ""
  )

  # Mark MLE

  if (show_mle) {
    graphics::points(x_mle, y_mle, pch = 19, col = "firebrick", cex = 1.5)
    graphics::points(x_mle, y_mle, pch = 4, col = "black", cex = 1.5, lwd = 2)
  }
}
