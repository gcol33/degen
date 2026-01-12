# Step 6: Identifiability Diagnostics

**Version target**: 0.6.0
**Scope**: Comprehensive identifiability analysis for single models

## Objective

Build on Fisher information to provide user-friendly identifiability diagnostics. This step integrates numerical analysis with interpretable outputs that guide modeling decisions.

## Conceptual framework

Identifiability exists on a spectrum:

1. **Structurally non-identifiable**: No amount of data can distinguish parameter values
2. **Practically non-identifiable**: Parameters are theoretically identifiable but require unrealistic sample sizes
3. **Weakly identified**: Parameters are identifiable but estimates will be imprecise
4. **Well identified**: Parameters can be estimated reliably

## API

### Main diagnostic function

```r
identifiability_check(
  spec,
  y,
  par = NULL,
  level = c("local", "profile", "both"),
  threshold = 0.01,
  verbose = FALSE
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `spec` | model_spec | Model specification |
| `y` | numeric | Observed data |
| `par` | numeric | Parameter values (if NULL, find MLE first) |
| `level` | character | Type of analysis |
| `threshold` | numeric | Eigenvalue threshold for non-identifiability |
| `verbose` | logical | Print progress |

### Return value

An S3 object of class `identifiability_result` containing:

- `status`: Overall identifiability status per parameter
- `fisher_info`: The Fisher information object
- `profile_results`: Profile likelihood results (if computed)
- `identified_functions`: Linear combinations that are identified
- `non_identified`: Parameters/directions that are not identified
- `recommendations`: Suggested actions

## Tasks

### 6.1 Main diagnostic function

Create `R/identifiability.R`:

- [ ] `identifiability_check()` - main entry point
- [ ] Integrate Fisher information analysis
- [ ] Classify parameters into identifiability categories

### 6.2 Profile likelihood analysis

Profile likelihoods provide a more robust identifiability diagnostic than Fisher information alone:

- [ ] `profile_likelihood()` - compute profile for one parameter
- [ ] `profile_all()` - compute profiles for all parameters
- [ ] Detect flat profiles (non-identifiable)
- [ ] Detect asymmetric profiles (boundary issues)

### 6.3 Identified functions

When individual parameters are not identifiable, combinations may be:

- [ ] `identified_functions()` - find identifiable linear combinations
- [ ] Based on null space of Fisher information
- [ ] Report in human-readable form

### 6.4 Result class and methods

Create `R/identifiability_result.R`:

- [ ] `identifiability_result` S3 class
- [ ] `print.identifiability_result()` - summary
- [ ] `summary.identifiability_result()` - detailed report
- [ ] `plot.identifiability_result()` - profile likelihood plots

### 6.5 Diagnostic utilities

- [ ] `is_identified()` - check if a parameter/model is identified
- [ ] `identifiability_status()` - return status string
- [ ] `suggest_reparameterization()` - suggest fixes

## Example usage

```r
# A model with identifiability issues
spec <- model_spec(
  loglik_fn = function(y, a, b) {
    # Only a + b is identified, not a and b individually
    sum(dnorm(y, mean = a + b, sd = 1, log = TRUE))
  },
  par_names = c("a", "b"),
  par_bounds = list(a = c(-Inf, Inf), b = c(-Inf, Inf))
)

y <- rnorm(100, mean = 5, sd = 1)
result <- identifiability_check(spec, y, par = c(a = 2, b = 3))

print(result)
# <identifiability_result>
# Overall: Model has NON-IDENTIFIABLE parameters
#
# Parameter status:
#   a: NON-IDENTIFIABLE
#   b: NON-IDENTIFIABLE
#
# Identified functions:
#   a + b (identified)
#
# Recommendation: Reparameterize using sum = a + b

summary(result)
# Detailed analysis including:
# - Fisher information matrix
# - Eigenvalue decomposition
# - Profile likelihood results
# - Suggested reparameterizations
```

## Profile likelihood algorithm

```
For parameter θ_i:
1. Fix θ_i at a grid of values around MLE
2. For each fixed value, optimize over remaining parameters
3. Record maximized log-likelihood
4. Plot profile: θ_i vs profile log-likelihood
5. Flat profile → θ_i non-identifiable
```

## Tests to write

```r
# tests/testthat/test-identifiability.R

test_that("identifiable model passes check", {
  # Normal with mu, sigma both identifiable
})

test_that("non-identifiable model is detected", {
  # a + b model where only sum is identified
})

test_that("profile likelihood detects flat profiles", {...})

test_that("identified_functions finds correct combinations", {...})

test_that("recommendations are sensible", {...})
```

## Files created

```
R/
├── identifiability.R
├── identifiability_result.R
├── profile_likelihood.R
└── identified_functions.R
tests/testthat/
└── test-identifiability.R
```

## Edge cases

1. Models with exact (structural) non-identifiability
2. Models with near (practical) non-identifiability
3. Models where non-identifiability depends on data
4. High-dimensional models (profiles expensive)
5. Multimodal likelihood surfaces

## Output format for recommendations

```r
# Structure of recommendations
recommendations <- list(
  status = "non_identifiable",
  message = "Parameters a and b are not individually identifiable",
  identified = "a + b",
  suggested_action = "reparameterize",
  suggested_params = c("sum" = "a + b", "diff" = "a - b"),
  note = "After reparameterization, 'diff' will remain non-identifiable"
)
```

## Notes

- Profile likelihood is more robust than Fisher information for detecting identifiability
- Fisher information is local; profile likelihood explores globally
- For large models, computing all profiles may be slow
- Consider parallel computation for profiles
- May want to add simulation-based diagnostics (e.g., parameter recovery)
