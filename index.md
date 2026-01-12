# degen

An R package for detecting observational equivalence, degeneracy, and
non-identifiability in parametric models.

## Purpose

Statistical models encode assumptions about data-generating processes.
Different model formulations (different parameterizations, latent
structures, or functional forms) can sometimes produce identical
distributions over observable data. When this occurs, no amount of data
can distinguish between them. The models are **observationally
equivalent**.

This package provides tools to:

- Detect whether two model specifications induce the same likelihood
  surface
- Diagnose identifiability problems within a single model
- Characterize equivalence classes among competing formulations

## Definitions

**Observational equivalence**: Two models are observationally equivalent
if they assign the same probability to every possible dataset. Formally,
for all data $y$ in the sample space, there exist parameter values
$\theta_{A}$ and $\theta_{B}$ such that
$\mathcal{L}_{A}\left( y;\theta_{A} \right) = \mathcal{L}_{B}\left( y;\theta_{B} \right)$.

**Degeneracy**: A model exhibits degeneracy when multiple parameter
configurations yield identical likelihoods. This is a failure of
injectivity in the mapping from parameters to distributions.

**Non-identifiability**: A parameter is non-identifiable if the
likelihood function is constant along some direction in parameter space.
The data provide no information for distinguishing values along that
direction.

These concepts are related but distinct. Degeneracy concerns the
structure of a single model’s parameter space. Equivalence concerns
relationships between different models. Both matter for inference.

## Installation

``` r
# From GitHub (development version)
# install.packages("pak")
pak::pak("gcol33/degen")
```

## Basic Usage

Define two model specifications by providing log-likelihood functions:

``` r
library(degen)

# Model A: simple exponential
spec_a <- model_spec(
  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(1e-6, Inf))
)

# Model B: Gamma with shape fixed at 1 (equivalent to exponential)
spec_b <- model_spec(
  loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
  par_names = "rate",
  par_bounds = list(rate = c(1e-6, Inf))
)

# Compare likelihood surfaces
pair <- equivalence_pair(spec_a, spec_b)
result <- compare_surfaces(pair, y = rexp(100, rate = 2))
```

For identifiability diagnostics on a single model:

``` r
# Check Fisher information for near-singularity
info <- fisher_information(spec_a, y = rexp(100), par = c(lambda = 2))
identifiability_check(info)
```

## What This Package Does Not Do

- **Model fitting**: `degen` does not estimate parameters. It examines
  properties of likelihood functions.
- **Model selection**: The package does not rank models by fit,
  predictive accuracy, or information criteria.
- **Causal inference**: Observational equivalence is a statement about
  likelihoods, not about causal structure. Two models may be
  observationally equivalent yet encode different causal claims.
- **Automated model extraction**: Users must supply likelihood functions
  explicitly. The package does not extract likelihoods from fitted model
  objects.

## Intended Audience

This package is designed for researchers who:

- Work with models where structural assumptions matter (e.g., population
  dynamics, species distribution models, mixture models)
- Need to assess whether competing mechanistic hypotheses are
  empirically distinguishable
- Want to diagnose why certain parameters are poorly estimated

Familiarity with likelihood-based inference is assumed. The package
provides diagnostics, not tutorials on identifiability theory.

## Related Work

The problem of observational equivalence has a long history in
econometrics, psychometrics, and structural equation modeling. This
package does not replicate or replace specialized tools in those
domains. It provides a general framework for likelihood-based
equivalence detection applicable across disciplines.

## License

MIT
