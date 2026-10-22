# Story: degen

## Claim
Detects observational equivalence between model specifications and non-identifiability within a model, from their log-likelihood surfaces.

## Audience
Researchers comparing mechanistic or statistical model formulations (ecology, hierarchical and mixture models) who write or fit likelihood-based models in R.

## Relation
- to: none

## Arcs

### A1 Can the data tell two models apart?
- answer: compare_surfaces() on an equivalence_pair(); equivalence_classes() for sets (union-find)
- limits: numerical evidence over the points tested; depends on tol and n_points
- home: vignettes/comparing-models.Rmd
- also: README (opening, Usage), DESCRIPTION

### A2 Which parameters are identified?
- answer: Fisher information eigenvalues, identifiability_check(), null_directions(), profile likelihood
- home: vignettes/identifiability-diagnostics.Rmd
- also: README (Usage)

### A3 Any model
- answer: model_spec() from a log-likelihood, ten built-ins, model_spec_from_fit() for lm/glm
- home: vignettes/model-specification.Rmd

### A4 Stability and Bayesian models
- answer: sensitivity_analysis(), bootstrap_equivalence(), cross_validate_equivalence(), compare_posteriors(), prior_sensitivity()
- home: help pages

## Documents
- README / landing: claim, mechanism, Usage (A1, A2), routing
- Introduction to degen: tutorial
- Writing Model Specifications, Comparing Models, Identifiability Diagnostics, Case Studies

## Open
- The site URL in DESCRIPTION and _pkgdown.yml is gcol33.github.io/degen, which redirects to gillescolling.com/degen/, which returned 404 on 2026-10-23.
- check_exact_equivalence() takes data and candidate transformations; the former README described it as needing no data and returning a proof.
