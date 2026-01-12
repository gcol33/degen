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
