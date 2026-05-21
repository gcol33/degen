# degen

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/gcol33/degen/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gcol33/degen/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/gcol33/degen/graph/badge.svg)](https://app.codecov.io/gh/gcol33/degen)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Detect Observational Equivalence and Non-Identifiability in Parametric Models**

The `degen` package tests whether competing model specifications produce identical likelihood surfaces, diagnoses identifiability problems within a single model, and groups models into equivalence classes. Define models as log-likelihood functions, and `degen` tells you which ones the data can actually distinguish.

## Quick Start

```r
library(degen)

# Define two models
spec_exp  <- model_spec_exponential()
spec_gam  <- model_spec_gamma(parameterization = "shape_rate")

# Test if they produce the same likelihood surface
pair <- equivalence_pair(spec_exp, spec_gam)
result <- compare_surfaces(pair, y = rexp(200, rate = 2))
print(result)

# Check identifiability of a single model
info <- fisher_information(spec_gam, y = rgamma(200, 2, 3),
                           par = c(shape = 2, rate = 3))
identifiability_check(info)
```

## Statement of Need

Different parameterizations of a statistical model can produce the same distribution over observable data. When this happens, no amount of data will distinguish between them. Similarly, parameters may be locally non-identifiable: the likelihood is flat in some direction, making those parameters unestimable.

These problems surface as convergence failures, ridge-like likelihood surfaces, or contradictory results across software packages. Diagnosing them usually requires manual comparison of likelihood functions.

`degen` automates this with systematic comparison of likelihood surfaces, Fisher information diagnostics, profile likelihood analysis, and equivalence class detection. It is relevant wherever model structure matters: mixture models with label switching, mechanistic ecological models with competing functional forms, or hierarchical models where variance components trade off.

## Features

### Model Specification

- **`model_spec()`**: Define a model from any log-likelihood function
- **Built-in distributions**: `model_spec_normal()`, `model_spec_exponential()`, `model_spec_gamma()`, `model_spec_poisson()`, `model_spec_binomial()`, `model_spec_beta()`, `model_spec_lognormal()`, `model_spec_weibull()`, `model_spec_negbinom()`, `model_spec_uniform()`
- **`model_spec_from_fit()`**: Extract specifications from fitted `lm` or `glm` objects
- **`simulate()`**: Generate data from any model specification

### Equivalence Testing

- **`compare_surfaces()`**: Compare likelihood surfaces between two models (grid or optimization)
- **`equivalence_pair()`**: Create comparison objects with automatic parameter mapping
- **`compare_all_pairs()`**: Pairwise comparison across a list of models
- **`equivalence_classes()`**: Group models into empirical equivalence classes
- **`detect_linear_reparam()`**: Detect linear reparameterizations between models
- **`check_exact_equivalence()`**: Symbolic equivalence checking

### Identifiability Diagnostics

- **`fisher_information()`**: Observed or expected Fisher information matrix
- **`identifiability_check()`**: Comprehensive analysis with eigenvalue decomposition
- **`null_directions()`**: Find directions of non-identifiability in parameter space
- **`profile_likelihood()`**: Compute and plot profile likelihoods
- **`profile_ci()`**: Confidence intervals from profile curves
- **`diagnose_model()`**: All-in-one diagnostic report

### Robustness & Sensitivity

- **`sensitivity_analysis()`**: Test robustness of equivalence to tolerance and grid settings
- **`bootstrap_equivalence()`**: Bootstrap resampling test
- **`cross_validate_equivalence()`**: Cross-validation test
- **`prior_sensitivity()`**: Bayesian prior sensitivity analysis

### Bayesian Integration

- **`compare_posteriors()`**: Compare posteriors from MCMC samples (brms, Stan, rstanarm)
- **`extract_samples()`**: Extract posterior samples from fitted Bayesian models

### Export

- **`export_results()`**: Export to JSON, CSV, or markdown
- **`to_latex()`**: LaTeX output for papers

## Installation

```r
install.packages("pak")
pak::pak("gcol33/degen")
```

## Usage Examples

### Comparing Two Model Specifications

```r
library(degen)

# Exponential vs Gamma(shape=1): are they distinguishable?
spec_a <- model_spec_exponential()
spec_b <- model_spec_gamma(parameterization = "shape_rate")

pair <- equivalence_pair(spec_a, spec_b)
result <- compare_surfaces(pair, y = rexp(200, rate = 2))
result
```

### Identifiability Diagnostics

```r
# Check a two-parameter model
spec <- model_spec_gamma(parameterization = "shape_rate")
y <- rgamma(500, shape = 2, rate = 3)

info <- fisher_information(spec, y = y, par = c(shape = 2, rate = 3))
id_check <- identifiability_check(info)
summary(id_check)

# Profile likelihood for individual parameters
prof <- profile_likelihood(spec, y = y, par = c(shape = 2, rate = 3),
                           which = "shape")
plot(prof)
```

### Equivalence Classes

```r
# Group multiple models by equivalence
models <- list(
  exp       = model_spec_exponential(),
  gamma_sr  = model_spec_gamma(parameterization = "shape_rate"),
  gamma_ss  = model_spec_gamma(parameterization = "shape_scale"),
  weibull   = model_spec_weibull()
)

classes <- equivalence_classes(models, y = rexp(200, rate = 2))
print(classes)
```

### Sensitivity Analysis

```r
sens <- sensitivity_analysis(pair, y = rexp(200, rate = 2),
                             tolerances = c(1e-4, 1e-6, 1e-8))
summary(sens)
```

## Documentation

- [Introduction](https://gcol33.github.io/degen/articles/introduction.html) - Package overview and core concepts
- [Model Specification](https://gcol33.github.io/degen/articles/model-specification.html) - Defining models
- [Comparing Models](https://gcol33.github.io/degen/articles/comparing-models.html) - Equivalence testing
- [Identifiability Diagnostics](https://gcol33.github.io/degen/articles/identifiability-diagnostics.html) - Fisher information and profile likelihoods
- [Case Studies](https://gcol33.github.io/degen/articles/case-studies.html) - Practical applications
- [Function Reference](https://gcol33.github.io/degen/reference/)

## Support

> "Software is like sex: it's better when it's free." — Linus Torvalds

I'm a PhD student who builds R packages in my free time because I believe good tools should be free and open. I started these projects for my own work and figured others might find them useful too.

If this package saved you some time, buying me a coffee is a nice way to say thanks. It helps with my coffee addiction.

[![Buy Me A Coffee](https://img.shields.io/badge/-Buy%20me%20a%20coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/gcol33)

## License

MIT (see the LICENSE.md file)

## Citation

```bibtex
@software{degen,
  author = {Colling, Gilles},
  title = {degen: Detect Observational Equivalence and Non-Identifiability in Parametric Models},
  year = {2025},
  url = {https://github.com/gcol33/degen}
}
```
