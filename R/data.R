# Documentation for example datasets
#
# These datasets provide reproducible examples for the package documentation.

#' Equivalent model pairs
#'
#' A list of model specification pairs that are mathematically equivalent.
#' Useful for testing and demonstrating equivalence detection.
#'
#' @usage NULL
#' @format A list with 3 elements, each containing:
#' \describe{
#'   \item{model_a}{First model specification}
#'   \item{model_b}{Second model specification (equivalent to model_a)}
#'   \item{description}{Character string describing the equivalence}
#' }
#'
#' @details
#' The pairs include:
#' \enumerate{
#'   \item Exponential vs Gamma(shape=1): Rate parameterizations
#'   \item Normal(mu, sigma) vs Normal(mu, sigma^2): Variance parameterizations
#'   \item Weibull(shape=1) vs Exponential: Special case equivalence
#' }
#'
#' @examples
#' data(equivalent_models)
#' names(equivalent_models)
#'
#' # Test the first pair
#' pair1 <- equivalent_models[[1]]
#' y <- rexp(50, rate = 2)
#' ep <- equivalence_pair(pair1$model_a, pair1$model_b)
#' result <- compare_surfaces(ep, y, n_points = 20)
#' print(result)
"equivalent_models"


#' Non-identifiable model example
#'
#' A dataset demonstrating parameter non-identifiability, where the sum
#' of two parameters is identifiable but individual parameters are not.
#'
#' @usage NULL
#' @format A list with 3 elements:
#' \describe{
#'   \item{y}{Numeric vector of 100 simulated observations}
#'   \item{spec}{A model_spec object with non-identifiable parameters}
#'   \item{true_sum}{The true value of a + b used in simulation (5)}
#' }
#'
#' @details
#' The model is: y ~ Normal(a + b, 1)
#'
#' Only the sum (a + b) affects the likelihood, so infinitely many
#' combinations of (a, b) produce the same likelihood. The Fisher
#' information matrix is singular with rank 1.
#'
#' @examples
#' data(nonidentifiable_example)
#'
#' # Check identifiability
#' result <- identifiability_check(
#'   nonidentifiable_example$spec,
#'   nonidentifiable_example$y,
#'   par = c(a = 2, b = 3)
#' )
#' print(result)
#'
#' # Fisher information shows rank deficiency
#' info <- fisher_information(
#'   nonidentifiable_example$spec,
#'   nonidentifiable_example$y,
#'   par = c(a = 2, b = 3)
#' )
#' print(info)
"nonidentifiable_example"


#' Ecological population models
#'
#' Three competing models for population growth data, demonstrating
#' how to compare alternative ecological hypotheses.
#'
#' @usage NULL
#' @format A list with 4 elements:
#' \describe{
#'   \item{y}{Numeric vector of 50 population count observations}
#'   \item{models}{Named list of 3 model_spec objects}
#'   \item{true_model}{Name of the model used to generate data ("gompertz")}
#'   \item{true_params}{True parameter values used in simulation}
#' }
#'
#' @details
#' The three models are:
#' \enumerate{
#'   \item Exponential growth: y ~ Poisson(lambda * exp(r))
#'   \item Logistic growth: y ~ Poisson(K / (1 + exp(-r)))
#'   \item Gompertz growth: y ~ Poisson(K * exp(-exp(-r)))
#' }
#'
#' These models make different assumptions about density dependence
#' and are generally not equivalent except in limiting cases.
#'
#' @examples
#' data(ecological_models)
#'
#' # Compare all three models
#' classes <- equivalence_classes(
#'   ecological_models$models,
#'   ecological_models$y,
#'   n_points = 20
#' )
#' print(classes)
"ecological_models"


#' Mixture model with label switching
#'
#' A two-component Gaussian mixture model demonstrating the label
#' switching equivalence problem.
#'
#' @usage NULL
#' @format A list with 4 elements:
#' \describe{
#'   \item{y}{Numeric vector of 200 observations from a mixture}
#'   \item{spec}{A model_spec for the mixture model}
#'   \item{true_params}{True parameter values (mu1=0, mu2=3, sigma=1, pi=0.4)}
#'   \item{swapped_params}{Label-swapped equivalent parameters}
#' }
#'
#' @details
#' In mixture models, swapping component labels produces an equivalent
#' model. If (mu1, mu2, pi) are the parameters, then (mu2, mu1, 1-pi)
#' gives exactly the same likelihood for all data.
#'
#' This is a classic example of practical non-identifiability that
#' affects MCMC sampling and optimization.
#'
#' @examples
#' data(mixture_model)
#'
#' # Both parameter sets give same likelihood
#' ll1 <- loglik(mixture_model$spec, mixture_model$y, mixture_model$true_params)
#' ll2 <- loglik(mixture_model$spec, mixture_model$y, mixture_model$swapped_params)
#' abs(ll1 - ll2) < 1e-10  # TRUE
"mixture_model"
