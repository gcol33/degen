# degen 0.2.0

* Added `model_spec()` for defining models via log-likelihood functions
* Added generics: `par_names()`, `par_bounds()`, `n_par()`, `loglik()`
* Added `is_model_spec()` type predicate
* S3 methods: `print`, `summary`, `format` for model_spec

# degen 0.1.0

* Initial package structure
* Added internal validation utilities (`check_numeric()`, `check_function()`, `check_bounds()`)
* Set up GitHub Actions for R CMD check, test coverage, and pkgdown
