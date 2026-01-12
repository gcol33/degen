# Step 10: Release

**Version target**: 1.0.0
**Scope**: Final polish, CRAN preparation, and release

## Objective

Prepare the package for public release. This includes final quality checks, CRAN compliance, and release logistics.

## Pre-release checklist

### 10.1 Code quality

- [ ] Run `lintr::lint_package()` and address issues
- [ ] Run `styler::style_pkg()` for consistent formatting
- [ ] Review all TODO comments in code
- [ ] Remove any debugging code or print statements
- [ ] Ensure no hardcoded paths or user-specific content

### 10.2 Documentation completeness

- [ ] All exported functions documented
- [ ] All `@param` tags present and accurate
- [ ] All `@return` tags describe return values
- [ ] All `@examples` run without error
- [ ] `?degen` package-level help exists
- [ ] NEWS.md documents all features
- [ ] README.md is current and accurate

### 10.3 R CMD check

Must pass with 0 errors, 0 warnings, 0 notes:

```r
devtools::check()
```

Address common issues:
- [ ] No undefined global variables
- [ ] No missing dependencies
- [ ] No overly large files
- [ ] Correct license specification
- [ ] Valid DESCRIPTION fields

### 10.4 CRAN-specific checks

```r
# Check for CRAN
devtools::check(remote = TRUE, manual = TRUE)

# Check on multiple platforms
devtools::check_win_devel()
devtools::check_mac_release()

# Check reverse dependencies (none initially, but good habit)
devtools::revdep_check()
```

### 10.5 Test coverage

```r
covr::package_coverage()
covr::report()
```

Target: 90% overall coverage

### 10.6 pkgdown site

- [ ] Site builds without warnings: `pkgdown::build_site()`
- [ ] All links work
- [ ] Examples render correctly
- [ ] Vignettes appear in articles
- [ ] Reference index is organized

## CRAN submission preparation

### 10.7 DESCRIPTION updates

```
Package: degen
Version: 1.0.0
Title: Detect Observational Equivalence and Non-Identifiability in Models
...
```

### 10.8 cran-comments.md

Create submission notes:

```markdown
## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Windows 11, R 4.5.2
* GitHub Actions: ubuntu-latest, windows-latest, macos-latest
* win-builder (devel and release)
* R-hub

## Downstream dependencies

This is a new package with no reverse dependencies.
```

### 10.9 NEWS.md

```markdown
# degen 1.0.0

Initial CRAN release.

## Features

* `model_spec()`: Define models via log-likelihood functions
* `equivalence_pair()`: Pair models for comparison
* `compare_surfaces()`: Detect observational equivalence between models
* `fisher_information()`: Compute Fisher information matrix
* `identifiability_check()`: Diagnose identifiability issues
* `equivalence_classes()`: Group multiple models by equivalence

## Documentation

* Introduction vignette: conceptual overview
* Model specification vignette: how to define models
* Comparison vignette: workflow for comparing models
* Identifiability vignette: diagnostic workflow
* Ecological examples vignette: domain-specific applications
```

## Release process

### 10.10 Version bump

```r
usethis::use_version("major")  # 0.9.0 -> 1.0.0
```

### 10.11 Final checks

```r
# Spell check
devtools::spell_check()

# URL check
urlchecker::url_check()

# Final R CMD check
devtools::check()
```

### 10.12 GitHub release

- [ ] Merge all feature branches to main
- [ ] Create release tag: `git tag -a v1.0.0 -m "Initial release"`
- [ ] Push tag: `git push origin v1.0.0`
- [ ] Create GitHub release with release notes

### 10.13 CRAN submission

```r
devtools::release()
```

This runs final checks and guides through submission.

### 10.14 Post-submission

- [ ] Monitor CRAN incoming queue
- [ ] Respond promptly to any CRAN feedback
- [ ] Update pkgdown site once accepted
- [ ] Announce release (Twitter, blog, relevant mailing lists)

## Common CRAN rejection reasons to avoid

| Issue | Prevention |
|-------|------------|
| Missing `\value` in Rd | Document all return values |
| Examples take >5s | Use `\donttest{}` for slow examples |
| Writing to user's home | Use `tempdir()` for any file operations |
| Non-standard files | Check `.Rbuildignore` |
| Large package size | Keep data minimal, compress |
| Missing LICENSE file | Use `usethis::use_mit_license()` |

## Post-release roadmap

### Version 1.1.0 (planned)

- Analytical Fisher information for common distributions
- Parallel computation for `compare_surfaces()`
- Additional visualization options

### Version 1.2.0 (planned)

- Support for Bayesian model comparison
- Profile likelihood confidence intervals
- Additional vignettes (econometric examples)

## Files to verify before release

```
degen/
├── DESCRIPTION        # Version 1.0.0, all fields complete
├── NAMESPACE          # Generated, no manual edits
├── LICENSE            # MIT license
├── LICENSE.md         # Full license text
├── NEWS.md            # Complete changelog
├── README.md          # Up to date
├── cran-comments.md   # Submission notes
├── .Rbuildignore      # Excludes dev files
├── R/                 # All code
├── man/               # Generated documentation
├── tests/             # Comprehensive tests
├── vignettes/         # All vignettes
├── data/              # Example datasets (if any)
└── _pkgdown.yml       # Site configuration
```

## Success criteria for 1.0.0

- [ ] R CMD check: 0 errors, 0 warnings, 0 notes
- [ ] Test coverage: ≥90%
- [ ] All vignettes build
- [ ] pkgdown site complete
- [ ] Accepted on CRAN
