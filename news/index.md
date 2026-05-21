# Changelog

## degen 0.15.0

- Code elegance improvements:
  - Extracted `format_eigenvector()` helper to eliminate duplicate code
    in identifiability analysis
  - Refactored `find_identified_functions()` and
    `describe_null_directions()` to use shared helper
- Fixed bug where `verbose = TRUE` output was silently swallowed in
  [`equivalence_classes()`](https://gcol33.github.io/degen/reference/equivalence_classes.md)
  when comparing fewer than 5 model pairs

## degen 0.14.0

- Added sensitivity analysis functions:
  - [`sensitivity_analysis()`](https://gcol33.github.io/degen/reference/sensitivity_analysis.md)
    for testing robustness to tolerance and grid size
  - [`bootstrap_equivalence()`](https://gcol33.github.io/degen/reference/bootstrap_equivalence.md)
    for bootstrap testing of equivalence claims
  - [`cross_validate_equivalence()`](https://gcol33.github.io/degen/reference/cross_validate_equivalence.md)
    for cross-validation of equivalence
  - Plot methods for all sensitivity results
- Added Bayesian model comparison:
  - [`compare_posteriors()`](https://gcol33.github.io/degen/reference/compare_posteriors.md)
    for comparing models using MCMC samples
  - [`prior_sensitivity()`](https://gcol33.github.io/degen/reference/prior_sensitivity.md)
    for testing how priors affect identifiability
  - [`extract_samples()`](https://gcol33.github.io/degen/reference/extract_samples.md)
    for extracting posterior samples from rstan, brms, rstanarm
- Added symbolic analysis:
  - [`symbolic_fisher()`](https://gcol33.github.io/degen/reference/symbolic_fisher.md)
    for analytical Fisher information (normal, exponential, gamma, etc.)
  - [`detect_linear_reparam()`](https://gcol33.github.io/degen/reference/detect_linear_reparam.md)
    for detecting linear reparameterizations between models
  - [`check_exact_equivalence()`](https://gcol33.github.io/degen/reference/check_exact_equivalence.md)
    for testing exact parameter equivalence

## degen 0.13.0

- Added
  [`plot_likelihood_surface()`](https://gcol33.github.io/degen/reference/plot_likelihood_surface.md)
  for 2D contour visualizations
- Extended parallel processing support:
  - [`compare_surfaces()`](https://gcol33.github.io/degen/reference/compare_surfaces.md)
    now accepts `cl` parameter for parallel grid evaluation
  - [`fisher_information()`](https://gcol33.github.io/degen/reference/fisher_information.md)
    accepts `cl` for parallel expected info computation
- Added export utilities:
  - [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
    methods for all result objects
  - [`export_results()`](https://gcol33.github.io/degen/reference/export_results.md)
    for CSV/JSON export
  - [`to_latex()`](https://gcol33.github.io/degen/reference/to_latex.md)
    for LaTeX table formatting
- Added batch comparison utilities:
  - [`compare_all_pairs()`](https://gcol33.github.io/degen/reference/compare_all_pairs.md)
    for systematic pairwise comparison
  - [`equivalence_matrix()`](https://gcol33.github.io/degen/reference/equivalence_matrix.md)
    for visual comparison matrix
  - [`find_equivalent_to()`](https://gcol33.github.io/degen/reference/find_equivalent_to.md)
    to find models equivalent to a reference
- Added model fitting integration:
  - [`fit_model()`](https://gcol33.github.io/degen/reference/fit_model.md)
    for MLE estimation
  - [`model_spec_from_fit()`](https://gcol33.github.io/degen/reference/model_spec_from_fit.md)
    to extract model_spec from lm/glm objects
  - [`coef()`](https://rdrr.io/r/stats/coef.html) and
    [`vcov()`](https://rdrr.io/r/stats/vcov.html) methods for fitted
    models
  - [`model_vcov()`](https://gcol33.github.io/degen/reference/model_vcov.md)
    for variance-covariance from Fisher information
- Added example datasets:
  - `equivalent_models`: Pairs of mathematically equivalent models
  - `nonidentifiable_example`: Non-identifiable sum model
  - `ecological_models`: Three competing population growth models
  - `mixture_model`: Gaussian mixture with label-switching
- Added progress reporting:
  - Progress bars with ETA for
    [`compare_surfaces()`](https://gcol33.github.io/degen/reference/compare_surfaces.md)
    and
    [`equivalence_classes()`](https://gcol33.github.io/degen/reference/equivalence_classes.md)
  - `progress` parameter to control progress bar display
  - [`debug_comparison()`](https://gcol33.github.io/degen/reference/debug_comparison.md)
    for detailed trace of equivalence testing
- Added new vignettes:
  - [`vignette("model-specification")`](https://gcol33.github.io/degen/articles/model-specification.md):
    Writing log-likelihood functions
  - [`vignette("comparing-models")`](https://gcol33.github.io/degen/articles/comparing-models.md):
    Model comparison workflows
  - [`vignette("identifiability-diagnostics")`](https://gcol33.github.io/degen/articles/identifiability-diagnostics.md):
    Identifiability analysis
  - [`vignette("case-studies")`](https://gcol33.github.io/degen/articles/case-studies.md):
    Real-world examples

## degen 0.12.0

- Added model specification helpers for 10 common distributions:
  - [`model_spec_normal()`](https://gcol33.github.io/degen/reference/model_spec_normal.md),
    [`model_spec_exponential()`](https://gcol33.github.io/degen/reference/model_spec_exponential.md),
    [`model_spec_gamma()`](https://gcol33.github.io/degen/reference/model_spec_gamma.md)
  - [`model_spec_poisson()`](https://gcol33.github.io/degen/reference/model_spec_poisson.md),
    [`model_spec_binomial()`](https://gcol33.github.io/degen/reference/model_spec_binomial.md),
    [`model_spec_beta()`](https://gcol33.github.io/degen/reference/model_spec_beta.md)
  - [`model_spec_lognormal()`](https://gcol33.github.io/degen/reference/model_spec_lognormal.md),
    [`model_spec_weibull()`](https://gcol33.github.io/degen/reference/model_spec_weibull.md),
    [`model_spec_negbinom()`](https://gcol33.github.io/degen/reference/model_spec_negbinom.md)
  - [`model_spec_uniform()`](https://gcol33.github.io/degen/reference/model_spec_uniform.md)
- Added data simulation support:
  - [`add_simulator()`](https://gcol33.github.io/degen/reference/add_simulator.md)
    to attach simulation function to model_spec
  - [`simulate.model_spec()`](https://gcol33.github.io/degen/reference/simulate.model_spec.md)
    S3 method for generating data
  - [`can_simulate()`](https://gcol33.github.io/degen/reference/can_simulate.md)
    predicate
- Added analytical Fisher information via
  [`fisher_analytical()`](https://gcol33.github.io/degen/reference/fisher_analytical.md):
  - Closed-form formulas for normal, exponential, poisson, gamma,
    binomial, beta, lognormal
  - Auto-detection of distribution from model name
  - Fallback to numerical computation
- Added model validation functions:
  - [`validate_loglik()`](https://gcol33.github.io/degen/reference/validate_loglik.md)
    for comprehensive likelihood validation
  - [`check_gradient()`](https://gcol33.github.io/degen/reference/check_gradient.md)
    to compare numerical vs analytical gradients
  - [`diagnose_model()`](https://gcol33.github.io/degen/reference/diagnose_model.md)
    for full model health check

## degen 0.11.0

- Added visualization functions using base R graphics:
  - [`plot.fisher_info()`](https://gcol33.github.io/degen/reference/plot.fisher_info.md)
    for eigenvalue spectrum and eigenvector heatmap
  - [`plot.surface_comparison()`](https://gcol33.github.io/degen/reference/plot.surface_comparison.md)
    for likelihood discrepancy scatter plots
  - [`plot.identifiability_result()`](https://gcol33.github.io/degen/reference/plot.identifiability_result.md)
    for parameter status visualization
  - [`plot_profile_likelihood()`](https://gcol33.github.io/degen/reference/plot_profile_likelihood.md)
    for profile likelihood curves with CI bands
  - [`plot.equiv_classes()`](https://gcol33.github.io/degen/reference/plot.equiv_classes.md)
    for network diagram of equivalence relationships
- Added parallel processing support:
  - [`setup_cluster()`](https://gcol33.github.io/degen/reference/setup_cluster.md)
    and
    [`stop_cluster()`](https://gcol33.github.io/degen/reference/stop_cluster.md)
    for cluster management
  - [`equivalence_classes()`](https://gcol33.github.io/degen/reference/equivalence_classes.md)
    and `profile_all()` accept `cl` argument
- Added profile likelihood confidence intervals:
  - [`profile_ci()`](https://gcol33.github.io/degen/reference/profile_ci.md)
    to extract CIs from profile likelihood
  - [`confint.model_spec()`](https://gcol33.github.io/degen/reference/confint.model_spec.md)
    S3 method for profile-based CIs
  - Support for multiple confidence levels (90%, 95%, 99%)

## degen 0.10.0

- Added Rcpp backend for sampling functions:
  - Latin hypercube sampling via `lhs_sample_cpp()`
  - Random sampling via `random_sample_cpp()`
  - `sample_par_space_cpp()` for bounded parameter space sampling
- Added expected Fisher information via outer product of gradients (OPG)
- Added optimization-based surface comparison method
- Performance optimizations:
  - Cached source log-likelihoods in
    [`compare_surfaces()`](https://gcol33.github.io/degen/reference/compare_surfaces.md)
  - Path compression in union-find algorithm
  - Hoisted loop invariants in identifiability classification

## degen 0.9.0

- Added comprehensive integration tests
- Added shared test fixtures in helper-fixtures.R
- Verified complete workflows end-to-end
- Tested edge cases and consistency properties

## degen 0.8.0

- Added introduction vignette with examples
- Organized pkgdown reference by topic
- Added vignette dependencies (knitr, rmarkdown)

## degen 0.7.0

- Added
  [`equivalence_classes()`](https://gcol33.github.io/degen/reference/equivalence_classes.md)
  for grouping multiple models by equivalence
- Added
  [`n_classes()`](https://gcol33.github.io/degen/reference/n_classes.md),
  [`class_members()`](https://gcol33.github.io/degen/reference/class_members.md),
  [`are_equivalent()`](https://gcol33.github.io/degen/reference/are_equivalent.md)
  utilities
- Union-find algorithm for efficient class detection
- Added
  [`is_equiv_classes()`](https://gcol33.github.io/degen/reference/is_equiv_classes.md)
  type predicate

## degen 0.6.0

- Added
  [`identifiability_check()`](https://gcol33.github.io/degen/reference/identifiability_check.md)
  for comprehensive identifiability diagnostics
- Added
  [`profile_likelihood()`](https://gcol33.github.io/degen/reference/profile_likelihood.md)
  for profile likelihood analysis
- Automatic parameter classification
  (identified/weakly/non-identifiable)
- Recommendations for addressing identifiability issues
- Added
  [`is_identifiability_result()`](https://gcol33.github.io/degen/reference/is_identifiability_result.md)
  type predicate

## degen 0.5.0

- Added
  [`fisher_information()`](https://gcol33.github.io/degen/reference/fisher_information.md)
  for computing observed Fisher information
- Added
  [`info_eigenvalues()`](https://gcol33.github.io/degen/reference/info_eigenvalues.md),
  [`info_condition()`](https://gcol33.github.io/degen/reference/info_condition.md),
  [`info_rank()`](https://gcol33.github.io/degen/reference/info_rank.md)
  accessors
- Added
  [`null_directions()`](https://gcol33.github.io/degen/reference/null_directions.md)
  to find non-identifiable parameter combinations
- Added
  [`is_fisher_info()`](https://gcol33.github.io/degen/reference/is_fisher_info.md)
  type predicate

## degen 0.4.0

- Added
  [`compare_surfaces()`](https://gcol33.github.io/degen/reference/compare_surfaces.md)
  for detecting observational equivalence
- Grid-based comparison with multi-start optimization
- Latin hypercube sampling for parameter space exploration
- Added
  [`is_surface_comparison()`](https://gcol33.github.io/degen/reference/is_surface_comparison.md)
  type predicate

## degen 0.3.0

- Added
  [`equivalence_pair()`](https://gcol33.github.io/degen/reference/equivalence_pair.md)
  for pairing models for comparison
- Added
  [`spec_a()`](https://gcol33.github.io/degen/reference/pair_accessors.md),
  [`spec_b()`](https://gcol33.github.io/degen/reference/pair_accessors.md)
  accessors
- Added
  [`par_dims()`](https://gcol33.github.io/degen/reference/par_dims.md),
  [`same_par_count()`](https://gcol33.github.io/degen/reference/same_par_count.md)
  utilities
- Added
  [`is_equivalence_pair()`](https://gcol33.github.io/degen/reference/is_equivalence_pair.md)
  type predicate

## degen 0.2.0

- Added
  [`model_spec()`](https://gcol33.github.io/degen/reference/model_spec.md)
  for defining models via log-likelihood functions
- Added generics:
  [`par_names()`](https://gcol33.github.io/degen/reference/par_names.md),
  [`par_bounds()`](https://gcol33.github.io/degen/reference/par_bounds.md),
  [`n_par()`](https://gcol33.github.io/degen/reference/n_par.md),
  [`loglik()`](https://gcol33.github.io/degen/reference/loglik.md)
- Added
  [`is_model_spec()`](https://gcol33.github.io/degen/reference/is_model_spec.md)
  type predicate
- S3 methods: `print`, `summary`, `format` for model_spec

## degen 0.1.0

- Initial package structure
- Added internal validation utilities (`check_numeric()`,
  `check_function()`, `check_bounds()`)
- Set up GitHub Actions for R CMD check, test coverage, and pkgdown
