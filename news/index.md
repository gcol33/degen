# Changelog

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
