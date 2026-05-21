# Package index

## Model Specification

Define models via their log-likelihood functions

- [`model_spec()`](https://gcol33.github.io/degen/reference/model_spec.md)
  : Create a model specification
- [`is_model_spec()`](https://gcol33.github.io/degen/reference/is_model_spec.md)
  : Test if object is a model_spec
- [`par_names()`](https://gcol33.github.io/degen/reference/par_names.md)
  : Get parameter names
- [`par_bounds()`](https://gcol33.github.io/degen/reference/par_bounds.md)
  : Get parameter bounds
- [`n_par()`](https://gcol33.github.io/degen/reference/n_par.md) : Get
  number of parameters
- [`loglik()`](https://gcol33.github.io/degen/reference/loglik.md) :
  Evaluate log-likelihood
- [`validate_loglik()`](https://gcol33.github.io/degen/reference/validate_loglik.md)
  : Validate a log-likelihood function
- [`diagnose_model()`](https://gcol33.github.io/degen/reference/diagnose_model.md)
  : Diagnose model specification issues
- [`check_gradient()`](https://gcol33.github.io/degen/reference/check_gradient.md)
  : Check numerical gradient against analytical

## Distribution Helpers

Pre-built model specifications for common distributions

- [`model_spec_normal()`](https://gcol33.github.io/degen/reference/model_spec_normal.md)
  : Create model_spec for Normal distribution
- [`model_spec_exponential()`](https://gcol33.github.io/degen/reference/model_spec_exponential.md)
  : Create model_spec for Exponential distribution
- [`model_spec_gamma()`](https://gcol33.github.io/degen/reference/model_spec_gamma.md)
  : Create model_spec for Gamma distribution
- [`model_spec_poisson()`](https://gcol33.github.io/degen/reference/model_spec_poisson.md)
  : Create model_spec for Poisson distribution
- [`model_spec_binomial()`](https://gcol33.github.io/degen/reference/model_spec_binomial.md)
  : Create model_spec for Binomial distribution
- [`model_spec_beta()`](https://gcol33.github.io/degen/reference/model_spec_beta.md)
  : Create model_spec for Beta distribution
- [`model_spec_lognormal()`](https://gcol33.github.io/degen/reference/model_spec_lognormal.md)
  : Create model_spec for Log-normal distribution
- [`model_spec_weibull()`](https://gcol33.github.io/degen/reference/model_spec_weibull.md)
  : Create model_spec for Weibull distribution
- [`model_spec_negbinom()`](https://gcol33.github.io/degen/reference/model_spec_negbinom.md)
  : Create model_spec for Negative Binomial distribution
- [`model_spec_uniform()`](https://gcol33.github.io/degen/reference/model_spec_uniform.md)
  : Create model_spec for Uniform distribution

## Model Comparison

Compare likelihood surfaces between models

- [`equivalence_pair()`](https://gcol33.github.io/degen/reference/equivalence_pair.md)
  : Create an equivalence pair for model comparison
- [`is_equivalence_pair()`](https://gcol33.github.io/degen/reference/is_equivalence_pair.md)
  : Test if object is an equivalence_pair
- [`spec_a()`](https://gcol33.github.io/degen/reference/pair_accessors.md)
  [`spec_b()`](https://gcol33.github.io/degen/reference/pair_accessors.md)
  : Extract model specification from pair
- [`par_dims()`](https://gcol33.github.io/degen/reference/par_dims.md) :
  Get parameter dimensions for both models
- [`same_par_count()`](https://gcol33.github.io/degen/reference/same_par_count.md)
  : Check if models have same parameter count
- [`compare_surfaces()`](https://gcol33.github.io/degen/reference/compare_surfaces.md)
  : Compare likelihood surfaces between two models
- [`is_surface_comparison()`](https://gcol33.github.io/degen/reference/is_surface_comparison.md)
  : Test if object is a surface_comparison
- [`debug_comparison()`](https://gcol33.github.io/degen/reference/debug_comparison.md)
  : Debug comparison trace

## Equivalence Classes

Group multiple models by equivalence

- [`equivalence_classes()`](https://gcol33.github.io/degen/reference/equivalence_classes.md)
  : Find equivalence classes among multiple models
- [`is_equiv_classes()`](https://gcol33.github.io/degen/reference/is_equiv_classes.md)
  : Test if object is an equiv_classes
- [`n_classes()`](https://gcol33.github.io/degen/reference/n_classes.md)
  : Get number of equivalence classes
- [`class_members()`](https://gcol33.github.io/degen/reference/class_members.md)
  : Get members of an equivalence class
- [`are_equivalent()`](https://gcol33.github.io/degen/reference/are_equivalent.md)
  : Check if two models are equivalent
- [`equivalence_matrix()`](https://gcol33.github.io/degen/reference/equivalence_matrix.md)
  : Create equivalence matrix visualization

## Batch Comparison

Compare multiple models systematically

- [`compare_all_pairs()`](https://gcol33.github.io/degen/reference/compare_all_pairs.md)
  : Compare all pairs of models
- [`find_equivalent_to()`](https://gcol33.github.io/degen/reference/find_equivalent_to.md)
  : Find all models equivalent to a reference

## Fisher Information

Compute and analyze Fisher information matrices

- [`fisher_information()`](https://gcol33.github.io/degen/reference/fisher_information.md)
  : Compute Fisher information matrix
- [`is_fisher_info()`](https://gcol33.github.io/degen/reference/is_fisher_info.md)
  : Test if object is a fisher_info
- [`info_eigenvalues()`](https://gcol33.github.io/degen/reference/info_eigenvalues.md)
  : Extract eigenvalues from Fisher information
- [`info_condition()`](https://gcol33.github.io/degen/reference/info_condition.md)
  : Get condition number of Fisher information
- [`info_rank()`](https://gcol33.github.io/degen/reference/info_rank.md)
  : Get numerical rank of Fisher information
- [`null_directions()`](https://gcol33.github.io/degen/reference/null_directions.md)
  : Find directions of non-identifiability
- [`fisher_analytical()`](https://gcol33.github.io/degen/reference/fisher_analytical.md)
  : Compute analytical Fisher information
- [`symbolic_fisher()`](https://gcol33.github.io/degen/reference/symbolic_fisher.md)
  : Symbolic Fisher information

## Identifiability

Diagnose identifiability issues within a model

- [`identifiability_check()`](https://gcol33.github.io/degen/reference/identifiability_check.md)
  : Check model identifiability
- [`is_identifiability_result()`](https://gcol33.github.io/degen/reference/is_identifiability_result.md)
  : Test if object is an identifiability_result
- [`profile_likelihood()`](https://gcol33.github.io/degen/reference/profile_likelihood.md)
  : Compute profile likelihood for one parameter
- [`profile_ci()`](https://gcol33.github.io/degen/reference/profile_ci.md)
  : Compute profile likelihood confidence intervals
- [`detect_linear_reparam()`](https://gcol33.github.io/degen/reference/detect_linear_reparam.md)
  : Detect linear reparameterization
- [`check_exact_equivalence()`](https://gcol33.github.io/degen/reference/check_exact_equivalence.md)
  : Check for exact parameter equivalence

## Model Fitting

Fit models and extract specifications from fitted objects

- [`fit_model()`](https://gcol33.github.io/degen/reference/fit_model.md)
  : Fit a model specification via MLE
- [`model_spec_from_fit()`](https://gcol33.github.io/degen/reference/model_spec_from_fit.md)
  : Extract model_spec from fitted R objects
- [`model_vcov()`](https://gcol33.github.io/degen/reference/model_vcov.md)
  : Compute variance-covariance from Fisher information
- [`coef(`*`<fitted_model_spec>`*`)`](https://gcol33.github.io/degen/reference/coef.fitted_model_spec.md)
  : Extract coefficients from fitted model_spec
- [`vcov(`*`<fitted_model_spec>`*`)`](https://gcol33.github.io/degen/reference/vcov.fitted_model_spec.md)
  : Extract variance-covariance matrix
- [`confint(`*`<model_spec>`*`)`](https://gcol33.github.io/degen/reference/confint.model_spec.md)
  : S3 method for confint

## Simulation

Simulate data from model specifications

- [`add_simulator()`](https://gcol33.github.io/degen/reference/add_simulator.md)
  : Add simulation function to model_spec
- [`can_simulate()`](https://gcol33.github.io/degen/reference/can_simulate.md)
  : Check if model_spec can simulate
- [`simulate(`*`<model_spec>`*`)`](https://gcol33.github.io/degen/reference/simulate.model_spec.md)
  : Simulate data from a model specification

## Sensitivity Analysis

Test robustness of equivalence conclusions

- [`sensitivity_analysis()`](https://gcol33.github.io/degen/reference/sensitivity_analysis.md)
  : Sensitivity analysis for equivalence testing
- [`bootstrap_equivalence()`](https://gcol33.github.io/degen/reference/bootstrap_equivalence.md)
  : Bootstrap test for equivalence
- [`cross_validate_equivalence()`](https://gcol33.github.io/degen/reference/cross_validate_equivalence.md)
  : Cross-validation test for equivalence

## Bayesian Methods

Posterior-based model comparison

- [`compare_posteriors()`](https://gcol33.github.io/degen/reference/compare_posteriors.md)
  : Compare posterior distributions
- [`prior_sensitivity()`](https://gcol33.github.io/degen/reference/prior_sensitivity.md)
  : Prior sensitivity analysis
- [`extract_samples()`](https://gcol33.github.io/degen/reference/extract_samples.md)
  : Extract posterior samples from fitted objects

## Visualization

Plot results and diagnostics

- [`plot_likelihood_surface()`](https://gcol33.github.io/degen/reference/plot_likelihood_surface.md)
  : Plot likelihood surface
- [`plot_profile_likelihood()`](https://gcol33.github.io/degen/reference/plot_profile_likelihood.md)
  : Plot profile likelihood for a parameter
- [`plot(`*`<equiv_classes>`*`)`](https://gcol33.github.io/degen/reference/plot.equiv_classes.md)
  : Plot equivalence classes
- [`plot(`*`<fisher_info>`*`)`](https://gcol33.github.io/degen/reference/plot.fisher_info.md)
  : Plot Fisher information diagnostics
- [`plot(`*`<identifiability_result>`*`)`](https://gcol33.github.io/degen/reference/plot.identifiability_result.md)
  : Plot identifiability diagnostics
- [`plot(`*`<surface_comparison>`*`)`](https://gcol33.github.io/degen/reference/plot.surface_comparison.md)
  : Plot surface comparison results

## Export and Conversion

Export results for reporting

- [`export_results()`](https://gcol33.github.io/degen/reference/export_results.md)
  : Export results to file
- [`to_latex()`](https://gcol33.github.io/degen/reference/to_latex.md) :
  Format results as LaTeX table
- [`as.data.frame(`*`<equiv_classes>`*`)`](https://gcol33.github.io/degen/reference/as.data.frame.equiv_classes.md)
  : Convert equiv_classes to data frame
- [`as.data.frame(`*`<fisher_info>`*`)`](https://gcol33.github.io/degen/reference/as.data.frame.fisher_info.md)
  : Convert fisher_info to data frame
- [`as.data.frame(`*`<identifiability_result>`*`)`](https://gcol33.github.io/degen/reference/as.data.frame.identifiability_result.md)
  : Convert identifiability_result to data frame
- [`as.data.frame(`*`<surface_comparison>`*`)`](https://gcol33.github.io/degen/reference/as.data.frame.surface_comparison.md)
  : Convert surface_comparison to data frame

## Parallel Processing

Enable parallel computation

- [`setup_cluster()`](https://gcol33.github.io/degen/reference/setup_cluster.md)
  : Set up parallel cluster
- [`stop_cluster()`](https://gcol33.github.io/degen/reference/stop_cluster.md)
  : Stop parallel cluster

## Example Datasets

Bundled datasets for examples and testing

- [`ecological_models`](https://gcol33.github.io/degen/reference/ecological_models.md)
  : Ecological population models
- [`equivalent_models`](https://gcol33.github.io/degen/reference/equivalent_models.md)
  : Equivalent model pairs
- [`mixture_model`](https://gcol33.github.io/degen/reference/mixture_model.md)
  : Mixture model with label switching
- [`nonidentifiable_example`](https://gcol33.github.io/degen/reference/nonidentifiable_example.md)
  : Non-identifiable model example
