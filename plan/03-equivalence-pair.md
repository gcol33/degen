# Step 3: Equivalence Pairs

**Version target**: 0.3.0
**Scope**: `equivalence_pair()` constructor for comparing two models

## Objective

Create a container class that pairs two `model_spec` objects for equivalence comparison. This enables the comparison workflow and stores metadata about the relationship between models.

## Design principles

1. **Symmetry**: The pair (A, B) and (B, A) are conceptually equivalent
2. **Lazy evaluation**: Don't compute anything on construction
3. **Clear semantics**: The pair represents a question, not an answer

## API

```r
equivalence_pair(

  spec_a,
  spec_b,
  name = NULL
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `spec_a` | model_spec | First model specification |
| `spec_b` | model_spec | Second model specification |
| `name` | character | Optional name for this comparison |

### Return value

An S3 object of class `equivalence_pair` containing:

- `spec_a`: First model_spec
- `spec_b`: Second model_spec
- `name`: Comparison name
- `par_a`: Parameter names for model A
- `par_b`: Parameter names for model B

## Tasks

### 3.1 Core constructor

Create `R/equivalence_pair.R`:

- [ ] `equivalence_pair()` - main constructor
- [ ] Input validation:
  - Both inputs are `model_spec` objects
  - Generate default name if not provided
- [ ] Create S3 class structure

### 3.2 S3 methods

- [ ] `print.equivalence_pair()` - concise display
- [ ] `summary.equivalence_pair()` - detailed comparison
- [ ] `is_equivalence_pair()` - type predicate
- [ ] `format.equivalence_pair()` - string representation

### 3.3 Accessor functions

- [ ] `spec_a()` / `spec_b()` - extract individual specs
- [ ] `par_dims()` - return parameter dimensions as named vector

### 3.4 Comparison utilities

- [ ] `same_par_count()` - check if models have equal parameter count
- [ ] `compatible_bounds()` - check if parameter spaces overlap

## Example usage

```r
exp_spec <- model_spec(

  loglik_fn = function(y, lambda) sum(dexp(y, rate = lambda, log = TRUE)),
  par_names = "lambda",
  par_bounds = list(lambda = c(1e-6, Inf))
)

gamma_spec <- model_spec(
  loglik_fn = function(y, rate) sum(dgamma(y, shape = 1, rate = rate, log = TRUE)),
  par_names = "rate",
  par_bounds = list(rate = c(1e-6, Inf))
)

pair <- equivalence_pair(exp_spec, gamma_spec, name = "Exp vs Gamma(1)")

print(pair)
# <equivalence_pair> Exp vs Gamma(1)
# Model A: Exponential (1 parameter)
# Model B: Gamma_fixed_shape (1 parameter)
```

## Tests to write

```r
# tests/testthat/test-equivalence_pair.R

test_that("equivalence_pair requires model_spec inputs", {...})
test_that("equivalence_pair generates default name", {...})
test_that("print method shows both models", {...})
test_that("accessors return correct specs", {...})
test_that("par_dims returns correct dimensions", {...})
```

## Files created

```
R/
└── equivalence_pair.R
tests/testthat/
└── test-equivalence_pair.R
```

## Design decisions

### Why a separate class?

1. **Semantics**: A pair represents a comparison context, not just two models
2. **Extensibility**: Can add metadata (e.g., known mapping between parameters)
3. **Method dispatch**: Enables `compare_surfaces(pair, ...)` syntax

### Why not allow >2 models?

- Pairwise comparison is cleaner conceptually
- Equivalence classes (Step 7) handle multi-model comparisons
- Simpler implementation and reasoning

## Notes

- Consider whether to store data with the pair or pass separately to analysis functions
- The pair should be lightweight; computation happens in `compare_surfaces()`
- May want to add `equivalence_set()` later for >2 models
