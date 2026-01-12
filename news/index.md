# Changelog

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
