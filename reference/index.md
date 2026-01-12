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

## Identifiability

Diagnose identifiability issues within a model

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
- [`identifiability_check()`](https://gcol33.github.io/degen/reference/identifiability_check.md)
  : Check model identifiability
- [`is_identifiability_result()`](https://gcol33.github.io/degen/reference/is_identifiability_result.md)
  : Test if object is an identifiability_result
- [`profile_likelihood()`](https://gcol33.github.io/degen/reference/profile_likelihood.md)
  : Compute profile likelihood for one parameter
