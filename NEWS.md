# degen 0.15.0

* Code elegance improvements:
  - Extracted `format_eigenvector()` helper to eliminate duplicate code in identifiability analysis
  - Refactored `find_identified_functions()` and `describe_null_directions()` to use shared helper
* Fixed bug where `verbose = TRUE` output was silently swallowed in `equivalence_classes()` when comparing fewer than 5 model pairs

# degen 0.14.0

* Added sensitivity analysis functions:
  - `sensitivity_analysis()` for testing robustness to tolerance and grid size
  - `bootstrap_equivalence()` for bootstrap testing of equivalence claims
  - `cross_validate_equivalence()` for cross-validation of equivalence
  - Plot methods for all sensitivity results
* Added Bayesian model comparison:
  - `compare_posteriors()` for comparing models using MCMC samples
  - `prior_sensitivity()` for testing how priors affect identifiability
  - `extract_samples()` for extracting posterior samples from rstan, brms, rstanarm
* Added symbolic analysis:
  - `symbolic_fisher()` for analytical Fisher information (normal, exponential, gamma, etc.)
  - `detect_linear_reparam()` for detecting linear reparameterizations between models
  - `check_exact_equivalence()` for testing exact parameter equivalence

# degen 0.13.0

* Added `plot_likelihood_surface()` for 2D contour visualizations
* Extended parallel processing support:
  - `compare_surfaces()` now accepts `cl` parameter for parallel grid evaluation
  - `fisher_information()` accepts `cl` for parallel expected info computation
* Added export utilities:
  - `as.data.frame()` methods for all result objects
  - `export_results()` for CSV/JSON export
  - `to_latex()` for LaTeX table formatting
* Added batch comparison utilities:
  - `compare_all_pairs()` for systematic pairwise comparison
  - `equivalence_matrix()` for visual comparison matrix
  - `find_equivalent_to()` to find models equivalent to a reference
* Added model fitting integration:
  - `fit_model()` for MLE estimation
  - `model_spec_from_fit()` to extract model_spec from lm/glm objects
  - `coef()` and `vcov()` methods for fitted models
  - `model_vcov()` for variance-covariance from Fisher information
* Added example datasets:
  - `equivalent_models`: Pairs of mathematically equivalent models
  - `nonidentifiable_example`: Non-identifiable sum model
  - `ecological_models`: Three competing population growth models
  - `mixture_model`: Gaussian mixture with label-switching
* Added progress reporting:
  - Progress bars with ETA for `compare_surfaces()` and `equivalence_classes()`
  - `progress` parameter to control progress bar display
  - `debug_comparison()` for detailed trace of equivalence testing
* Added new vignettes:
  - `vignette("model-specification")`: Writing log-likelihood functions
  - `vignette("comparing-models")`: Model comparison workflows
  - `vignette("identifiability-diagnostics")`: Identifiability analysis
  - `vignette("case-studies")`: Real-world examples

# degen 0.12.0

* Added model specification helpers for 10 common distributions:
  - `model_spec_normal()`, `model_spec_exponential()`, `model_spec_gamma()`
  - `model_spec_poisson()`, `model_spec_binomial()`, `model_spec_beta()`
  - `model_spec_lognormal()`, `model_spec_weibull()`, `model_spec_negbinom()`
  - `model_spec_uniform()`
* Added data simulation support:
  - `add_simulator()` to attach simulation function to model_spec
  - `simulate.model_spec()` S3 method for generating data
  - `can_simulate()` predicate
* Added analytical Fisher information via `fisher_analytical()`:
  - Closed-form formulas for normal, exponential, poisson, gamma, binomial, beta, lognormal
  - Auto-detection of distribution from model name
  - Fallback to numerical computation
* Added model validation functions:
  - `validate_loglik()` for comprehensive likelihood validation
  - `check_gradient()` to compare numerical vs analytical gradients
  - `diagnose_model()` for full model health check

# degen 0.11.0

* Added visualization functions using base R graphics:
  - `plot.fisher_info()` for eigenvalue spectrum and eigenvector heatmap
  - `plot.surface_comparison()` for likelihood discrepancy scatter plots
  - `plot.identifiability_result()` for parameter status visualization
  - `plot_profile_likelihood()` for profile likelihood curves with CI bands
  - `plot.equiv_classes()` for network diagram of equivalence relationships
* Added parallel processing support:
  - `setup_cluster()` and `stop_cluster()` for cluster management
  - `equivalence_classes()` and `profile_all()` accept `cl` argument
* Added profile likelihood confidence intervals:
  - `profile_ci()` to extract CIs from profile likelihood
  - `confint.model_spec()` S3 method for profile-based CIs
  - Support for multiple confidence levels (90%, 95%, 99%)

# degen 0.10.0

* Added Rcpp backend for sampling functions:
  - Latin hypercube sampling via `lhs_sample_cpp()`
  - Random sampling via `random_sample_cpp()`
  - `sample_par_space_cpp()` for bounded parameter space sampling
* Added expected Fisher information via outer product of gradients (OPG)
* Added optimization-based surface comparison method
* Performance optimizations:
  - Cached source log-likelihoods in `compare_surfaces()`
  - Path compression in union-find algorithm
  - Hoisted loop invariants in identifiability classification

# degen 0.9.0

* Added comprehensive integration tests
* Added shared test fixtures in helper-fixtures.R
* Verified complete workflows end-to-end
* Tested edge cases and consistency properties

# degen 0.8.0

* Added introduction vignette with examples
* Organized pkgdown reference by topic
* Added vignette dependencies (knitr, rmarkdown)

# degen 0.7.0

* Added `equivalence_classes()` for grouping multiple models by equivalence
* Added `n_classes()`, `class_members()`, `are_equivalent()` utilities
* Union-find algorithm for efficient class detection
* Added `is_equiv_classes()` type predicate

# degen 0.6.0

* Added `identifiability_check()` for comprehensive identifiability diagnostics
* Added `profile_likelihood()` for profile likelihood analysis
* Automatic parameter classification (identified/weakly/non-identifiable)
* Recommendations for addressing identifiability issues
* Added `is_identifiability_result()` type predicate

# degen 0.5.0

* Added `fisher_information()` for computing observed Fisher information
* Added `info_eigenvalues()`, `info_condition()`, `info_rank()` accessors
* Added `null_directions()` to find non-identifiable parameter combinations
* Added `is_fisher_info()` type predicate

# degen 0.4.0

* Added `compare_surfaces()` for detecting observational equivalence
* Grid-based comparison with multi-start optimization
* Latin hypercube sampling for parameter space exploration
* Added `is_surface_comparison()` type predicate

# degen 0.3.0

* Added `equivalence_pair()` for pairing models for comparison
* Added `spec_a()`, `spec_b()` accessors
* Added `par_dims()`, `same_par_count()` utilities
* Added `is_equivalence_pair()` type predicate

# degen 0.2.0

* Added `model_spec()` for defining models via log-likelihood functions
* Added generics: `par_names()`, `par_bounds()`, `n_par()`, `loglik()`
* Added `is_model_spec()` type predicate
* S3 methods: `print`, `summary`, `format` for model_spec

# degen 0.1.0

* Initial package structure
* Added internal validation utilities (`check_numeric()`, `check_function()`, `check_bounds()`)
* Set up GitHub Actions for R CMD check, test coverage, and pkgdown
