# Step 2: Model Specification

**Version target**: 0.2.0
**Scope**: Core `model_spec()` constructor and S3 class

## Objective

Implement the foundational `model_spec` class that represents a parametric model through its log-likelihood function. This is the atomic unit of the package.

## Design principles

1. **Minimal interface**: Only require what is necessary (log-likelihood function)
2. **Explicit bounds**: Parameter constraints must be stated, not inferred
3. **No fitting**: The object describes a model, it does not estimate parameters
4. **Composable**: Should work naturally with other package functions

## API

```r
model_spec(
  loglik_fn,
  par_names,
  par_bounds = NULL,
  name = NULL,
  validate = TRUE
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `loglik_fn` | function | Log-likelihood function with signature `function(y, ...)` where `...` are named parameters |
| `par_names` | character | Names of parameters (must match arguments in `loglik_fn`) |
| `par_bounds` | named list | Lower/upper bounds as `list(par = c(lower, upper))`. Default: `c(-Inf, Inf)` |
| `name` | character | Optional human-readable name for the model |
| `validate` | logical | Whether to validate inputs on construction |

### Return value

An S3 object of class `model_spec` containing:

- `loglik_fn`: The log-likelihood function (possibly wrapped)
- `par_names`: Character vector of parameter names
- `par_bounds`: Named list of bounds
- `n_par`: Integer count of parameters
- `name`: Model name (or auto-generated)

## Tasks

### 2.1 Core constructor

Create `R/model_spec.R`:

- [ ] `model_spec()` - main constructor
- [ ] Input validation:
  - `loglik_fn` is a function
  - `loglik_fn` has `y` as first argument
  - `par_names` are valid R names
  - `par_names` match `loglik_fn` formals (excluding `y`)
  - Bounds are valid (lower < upper, no NaN)
- [ ] Create S3 class structure

### 2.2 S3 methods

Create `R/model_spec-methods.R`:

- [ ] `print.model_spec()` - concise summary
- [ ] `summary.model_spec()` - detailed information
- [ ] `is_model_spec()` - type predicate
- [ ] `format.model_spec()` - string representation

### 2.3 Accessor functions

- [ ] `par_names()` - generic + method for model_spec
- [ ] `par_bounds()` - generic + method for model_spec
- [ ] `n_par()` - generic + method for model_spec
- [ ] `loglik()` - generic for evaluating log-likelihood

### 2.4 Log-likelihood evaluation

Create internal function for safe evaluation:

```r
evaluate_loglik <- function(spec, y, par) {

  # par can be named vector or list

  # Validate par is within bounds

  # Call loglik_fn with appropriate arguments
  # Handle errors gracefully
}
```

- [ ] `evaluate_loglik()` - internal evaluation wrapper
- [ ] Parameter bounds checking
- [ ] Informative error messages for out-of-bounds

### 2.5 Validation utilities

- [ ] `validate_model_spec()` - comprehensive validation
- [ ] `check_par_in_bounds()` - bounds checking utility
- [ ] Test that loglik_fn is callable with given par_names

## Example usage

```r
# Exponential model
exp_spec <- model_spec(

  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(1e-6, Inf)),
  name = "Exponential"
)

print(exp_spec)
# <model_spec> Exponential
# Parameters: lambda (1)
# Bounds: lambda in (1e-06, Inf)

# Evaluate at a point
loglik(exp_spec, y = rexp(100), par = c(lambda = 2))
```

## Tests to write

```r
# tests/testthat/test-model_spec.R

test_that("model_spec creates valid object", {...})
test_that("model_spec validates loglik_fn signature", {...})
test_that("model_spec rejects invalid bounds", {...})
test_that("print method produces expected output", {...})
test_that("loglik evaluates correctly", {...})
test_that("out-of-bounds parameters error informatively", {...})
test_that("par_names match function arguments", {...})
```

## Files created

```
R/
├── model_spec.R
├── model_spec-methods.R
└── generics.R  # par_names, par_bounds, n_par, loglik generics
tests/testthat/
└── test-model_spec.R
```

## Edge cases to handle

1. Single-parameter models
2. High-dimensional parameter spaces (>10 parameters)
3. Parameters with one-sided bounds (e.g., `c(0, Inf)`)
4. Log-likelihood functions that return `-Inf` at boundaries
5. Non-vectorized log-likelihood functions

## Notes

- Do not normalize or transform the log-likelihood; store it as provided
- Parameter order should be deterministic (alphabetical or as provided)
- Consider whether to support gradient functions later (add `grad_fn` argument?)
- The `validate = FALSE` option exists for performance in tight loops
