# Step 8: Documentation

**Version target**: 0.8.0
**Scope**: pkgdown site, vignettes, and comprehensive examples

## Objective

Create documentation that serves both as reference material and as a guide to the conceptual framework. The documentation should enable users to apply the package correctly without deep background in identifiability theory.

## Documentation philosophy

1. **Concepts before code**: Explain what identifiability and equivalence mean before showing functions
2. **Worked examples**: Show complete analyses, not just function calls
3. **Honest limitations**: Be clear about what the package can and cannot determine
4. **Domain examples**: Include examples from ecology, econometrics, and other fields

## Tasks

### 8.1 Function documentation

All exported functions must have complete roxygen2 documentation:

- [ ] `model_spec()` - detailed parameter descriptions, examples
- [ ] `equivalence_pair()` - with example models
- [ ] `compare_surfaces()` - algorithm description, interpretation guide
- [ ] `fisher_information()` - mathematical background, interpretation
- [ ] `identifiability_check()` - complete workflow example
- [ ] `equivalence_classes()` - multi-model example
- [ ] All S3 methods and accessors

### 8.2 Vignettes

Create `vignettes/`:

#### `vignettes/introduction.Rmd`
- [ ] What is observational equivalence?
- [ ] What is identifiability?
- [ ] Why does this matter for inference?
- [ ] Package overview and quick start

#### `vignettes/model-specification.Rmd`
- [ ] How to write log-likelihood functions
- [ ] Parameter bounds and constraints
- [ ] Common pitfalls and how to avoid them
- [ ] Examples from different model families

#### `vignettes/comparing-models.Rmd`
- [ ] Setting up model pairs
- [ ] Interpreting comparison results
- [ ] What to do when models are equivalent
- [ ] What to do when models are not equivalent

#### `vignettes/identifiability-diagnostics.Rmd`
- [ ] Fisher information interpretation
- [ ] Profile likelihood analysis
- [ ] Reparameterization strategies
- [ ] Case study: A model with identifiability issues

#### `vignettes/ecological-examples.Rmd`
- [ ] Population dynamics models
- [ ] Species distribution models
- [ ] Mixture models for habitat use
- [ ] Competition and predation models

### 8.3 pkgdown site

Create `_pkgdown.yml`:

```yaml
url: https://gcol33.github.io/degen

template:
  bootstrap: 5
  bootswatch: flatly

navbar:
  structure:
    left: [intro, reference, articles]
    right: [search, github]
  components:
    articles:
      text: Articles
      menu:
        - text: Getting Started
          href: articles/introduction.html
        - text: "---"
        - text: Core Concepts
        - text: Model Specification
          href: articles/model-specification.html
        - text: Comparing Models
          href: articles/comparing-models.html
        - text: Identifiability
          href: articles/identifiability-diagnostics.html
        - text: "---"
        - text: Applications
        - text: Ecological Examples
          href: articles/ecological-examples.html

reference:
  - title: Model Specification
    desc: Define models via their log-likelihood functions
    contents:
      - model_spec
      - is_model_spec
      - par_names
      - par_bounds
      - loglik

  - title: Model Comparison
    desc: Compare likelihood surfaces between models
    contents:
      - equivalence_pair
      - compare_surfaces
      - equivalence_classes

  - title: Identifiability
    desc: Diagnose identifiability issues within a model
    contents:
      - fisher_information
      - identifiability_check
      - profile_likelihood
      - identified_functions
```

### 8.4 README enhancement

Update `README.md`:

- [ ] Add installation badges
- [ ] Expand quick start example
- [ ] Add links to vignettes
- [ ] Include a compelling use case summary

### 8.5 Example datasets

Consider adding example datasets in `data/`:

- [ ] `competing_models` - list of model specs that are partially equivalent
- [ ] `nonid_example` - data and model with known non-identifiability

### 8.6 Help page improvements

- [ ] Add `@seealso` links between related functions
- [ ] Include `@family` tags for function groups
- [ ] Ensure all examples are runnable
- [ ] Add `@references` for key papers

## Vignette structure template

```markdown
---
title: "Vignette Title"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

## Overview

Brief description of what this vignette covers.

## Conceptual Background

Explain the concepts (2-3 paragraphs).

## Setup

```r
library(degen)
# Any other setup
```

## Example 1: [Descriptive name]

Detailed worked example with:
- Problem statement
- Code
- Output interpretation

## Example 2: [Descriptive name]

Second example, possibly showing a different scenario.

## Common Issues

Troubleshooting and FAQs for this topic.

## Summary

Key takeaways.
```

## Files created

```
vignettes/
├── introduction.Rmd
├── model-specification.Rmd
├── comparing-models.Rmd
├── identifiability-diagnostics.Rmd
└── ecological-examples.Rmd
_pkgdown.yml
data/
├── competing_models.rda
└── nonid_example.rda
R/
└── data.R  # Documentation for datasets
```

## Quality checklist

- [ ] All functions have `@examples` that run without error
- [ ] Vignettes build without error: `devtools::build_vignettes()`
- [ ] pkgdown site builds: `pkgdown::build_site()`
- [ ] No broken internal links
- [ ] All mathematical notation renders correctly
- [ ] Code in vignettes is reproducible (set seeds where needed)

## Notes

- Use consistent terminology throughout (define in introduction vignette)
- Include citations to foundational papers on identifiability
- Consider adding a vignette comparing `degen` to related tools
- Screenshots/figures for pkgdown articles improve engagement
