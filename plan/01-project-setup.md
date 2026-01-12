# Step 1: Project Setup

**Version target**: 0.1.0
**Scope**: Establish R package infrastructure

## Objective

Create a functional R package skeleton with CI/CD, documentation scaffolding, and development tooling.

## Tasks

### 1.1 Package skeleton

- [ ] Run `usethis::create_package()` or manually create:
  - `DESCRIPTION` with initial metadata
  - `NAMESPACE` (empty, will be generated)
  - `R/` directory
  - `man/` directory
- [ ] Set package version to `0.0.0.9000`
- [ ] Define license: MIT (add `LICENSE` and `LICENSE.md`)
- [ ] Add `.Rbuildignore` with standard exclusions

### 1.2 DESCRIPTION file

```
Package: degen
Title: Detect Observational Equivalence and Non-Identifiability in Models
Version: 0.0.0.9000
Authors@R:
    person("Gilles", "Colling", email = "...", role = c("aut", "cre"))
Description: Tools for detecting whether statistical model specifications
    are observationally equivalent, diagnosing identifiability problems,
    and characterizing equivalence classes among competing formulations.
License: MIT + file LICENSE
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.2
Depends: R (>= 4.1.0)
Imports:
    stats,
    numDeriv
Suggests:
    testthat (>= 3.0.0),
    knitr,
    rmarkdown
Config/testthat/edition: 3
VignetteBuilder: knitr
URL: https://github.com/gcol33/degen
BugReports: https://github.com/gcol33/degen/issues
```

### 1.3 Development tooling

- [ ] `usethis::use_testthat(3)` for testing infrastructure
- [ ] `usethis::use_roxygen_md()` for markdown in documentation
- [ ] `usethis::use_pkgdown()` for documentation website
- [ ] `usethis::use_news_md()` for changelog

### 1.4 GitHub Actions CI

- [ ] `usethis::use_github_action("check-standard")` for R CMD check
- [ ] `usethis::use_github_action("test-coverage")` for codecov
- [ ] `usethis::use_github_action("pkgdown")` for documentation deployment

### 1.5 Git setup

- [ ] Create `.gitignore` with R-specific exclusions
- [ ] Add badges to README (lifecycle, R-CMD-check, codecov)
- [ ] Move existing `README.md` and `vision.md` to final locations

### 1.6 Internal package utilities

Create `R/utils.R` with:

- [ ] `check_numeric()` - validate numeric inputs
- [ ] `check_function()` - validate function inputs
- [ ] `check_bounds()` - validate parameter bounds

## Acceptance criteria

- [ ] `devtools::check()` passes with 0 errors, 0 warnings, 0 notes
- [ ] `devtools::document()` generates clean NAMESPACE
- [ ] `testthat::test_local()` runs (even if no tests yet)
- [ ] Package installs from source: `pak::local_install()`

## Dependencies to add

| Package | Purpose | Type |
|---------|---------|------|
| numDeriv | Numerical differentiation for gradients/Hessians | Imports |
| stats | Base statistical functions | Imports |
| testthat | Unit testing | Suggests |
| knitr | Vignette rendering | Suggests |
| rmarkdown | Vignette rendering | Suggests |

## Files created

```
degen/
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
├── LICENSE.md
├── NEWS.md
├── .Rbuildignore
├── .gitignore
├── R/
│   └── utils.R
├── man/
├── tests/
│   ├── testthat.R
│   └── testthat/
├── vignettes/
└── .github/
    └── workflows/
        ├── R-CMD-check.yaml
        ├── test-coverage.yaml
        └── pkgdown.yaml
```

## Notes

- Keep dependencies minimal; `numDeriv` is the only non-base import needed initially
- Use `@noRd` for internal utilities to avoid documenting them
- Consider whether to support older R versions (4.1.0 should cover 3 years back)
