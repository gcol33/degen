# degen

> Small exact engines for scientific computing in R.

*two models the data can't tell apart*

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/gcol33/degen/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gcol33/degen/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/gcol33/degen/graph/badge.svg)](https://app.codecov.io/gh/gcol33/degen)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Detection of observational equivalence and non-identifiability in parametric models, from their log-likelihood surfaces.**

Hand it your model specifications. `degen` tells you which ones the data can actually
distinguish: it compares likelihood surfaces to find observationally equivalent models,
reads the Fisher information eigenspectrum to find parameters the likelihood is flat in,
and groups models into equivalence classes by union-find. Two parameterizations that
collapse to the same distribution will fit equally well forever, and the usual diagnostics
will not say so.

```r
library(degen)

# are these two models telling the data apart, or just telling stories?
pair   <- equivalence_pair(model_spec_exponential(),
                           model_spec_gamma(parameterization = "shape_rate"))
result <- compare_surfaces(pair, y = rexp(200, rate = 2))
result

# is a single model's likelihood flat in some direction?
info <- fisher_information(model_spec_gamma(parameterization = "shape_rate"),
                           y = rgamma(200, 2, 3), par = c(shape = 2, rate = 3))
identifiability_check(info)
```

## The question convergence checks skip

Standard diagnostics ask whether a fit converged, predicts, and leaves clean residuals.
None of them ask whether the model structure was distinguishable from its alternatives in
the first place. When two specifications encode the same distribution over observable data,
no amount of data separates them; when a parameter sits in a flat direction of the
likelihood, it is unestimable. Both show up downstream as convergence failures, ridge-like
surfaces, or answers that flip between software packages, and diagnosing them by hand means
comparing likelihood functions term by term.

`degen` does that comparison for you, and it can do it two ways: numerically, by searching
the likelihood surfaces for a separating point, and symbolically, by checking exact
parameter equivalence and detecting linear reparameterizations. The numerical verdict is
evidence (no counterexample found in the searched region); the symbolic verdict is a proof
of equivalence when it succeeds.

## What it works on

- **`model_spec()`**: define a model from any log-likelihood function, with named parameters
  and bounds. Ten built-ins are ready (`model_spec_normal()`, `model_spec_gamma()`,
  `model_spec_poisson()`, `model_spec_weibull()`, and the rest), and `model_spec_from_fit()`
  pulls a spec straight out of a fitted `lm` or `glm`.
- **`compare_surfaces()` / `equivalence_pair()`**: test two models for observational
  equivalence, by grid search or optimization, in both directions.
- **`check_exact_equivalence()` / `detect_linear_reparam()`**: the symbolic path, for when
  you want a proof rather than evidence.
- **`equivalence_classes()` / `compare_all_pairs()`**: group a list of models into empirical
  equivalence classes (union-find), or build the full pairwise comparison matrix.
- **`fisher_information()` / `identifiability_check()` / `null_directions()`**: observed or
  expected information, eigenvalue decomposition, and the parameter combinations the data
  cannot pin down.
- **`profile_likelihood()` / `profile_ci()`**: profile curves and the confidence intervals
  read off them.
- **`diagnose_model()`**: the single-call report covering all of the above.

## Numerical or symbolic?

|  | `compare_surfaces()` | `check_exact_equivalence()` |
|---|---|---|
| Needs data? | Yes | No |
| Returns | Evidence (no counterexample in searched region) | Proof of equivalence when it succeeds |
| Handles arbitrary log-likelihoods? | Yes | Within the supported algebra |
| Best for | Any pair of specifications | Suspected exact reparameterizations |

Run `compare_surfaces()` to see whether the data can tell two models apart; reach for the
symbolic path when you suspect they are exactly the same model wearing different parameters.

## Custom log-likelihoods

Any function that maps data and named parameters to a scalar log-likelihood is a model:

```r
# a model degen has never seen, defined in three lines
spec <- model_spec(
  loglik_fn  = function(y, mu, sigma) sum(dnorm(y, mu, sigma, log = TRUE)),
  par_names  = c("mu", "sigma"),
  par_bounds = list(sigma = c(1e-6, Inf))
)

info <- fisher_information(spec, y = rnorm(200, 1, 2), par = c(mu = 1, sigma = 2))
identifiability_check(info)
```

This is the case `degen` is built for: mixture models with label switching, mechanistic
models with competing functional forms, hierarchical models whose variance components trade
off. Wherever model structure carries meaning, it tells you how much of that structure the
data actually support.

## Robustness and Bayesian models

An equivalence verdict should not hinge on a tolerance setting. `sensitivity_analysis()`
sweeps tolerances and grid sizes, `bootstrap_equivalence()` and `cross_validate_equivalence()`
resample the test, and `prior_sensitivity()` checks how far a prior is doing the
identifying. For fitted Bayesian models, `compare_posteriors()` and `extract_samples()` work
on MCMC draws from `brms`, `rstan`, and `rstanarm`. Results export through `export_results()`
(JSON, CSV, markdown) and `to_latex()`.

## Installation

```r
install.packages("pak")                   # development version
pak::pak("gcol33/degen")
```

## Documentation

- [Introduction](https://gcol33.github.io/degen/articles/introduction.html)
- [Model Specification](https://gcol33.github.io/degen/articles/model-specification.html)
- [Comparing Models](https://gcol33.github.io/degen/articles/comparing-models.html)
- [Identifiability Diagnostics](https://gcol33.github.io/degen/articles/identifiability-diagnostics.html)
- [Case Studies](https://gcol33.github.io/degen/articles/case-studies.html)
- [Function Reference](https://gcol33.github.io/degen/reference/)

## Support

> "Software is like sex: it's better when it's free." — Linus Torvalds

I'm a PhD student who builds R packages in my free time because I believe good tools
should be free and open. I started these projects for my own work and figured others
might find them useful too.

If this package saved you some time, buying me a coffee is a nice way to say thanks.
It helps with my coffee addiction.

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
