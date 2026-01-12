# Step 7: Equivalence Classes

**Version target**: 0.7.0
**Scope**: Multi-model equivalence class detection and characterization

## Objective

Extend pairwise comparison to handle multiple models, automatically grouping them into equivalence classes. This addresses the common case where a researcher has several candidate models and wants to know which are distinguishable.

## Conceptual background

Given models M1, M2, ..., Mn, equivalence classes partition them such that:
- Models within a class are observationally equivalent
- Models in different classes are distinguishable

This is more informative than n(n-1)/2 pairwise comparisons because it reveals the structure of the equivalence relation.

## API

### Main function

```r
equivalence_classes(
  specs,
  y,
  method = c("pairwise", "hierarchical"),
  tol = 1e-6,
  verbose = FALSE
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `specs` | list | List of model_spec objects |
| `y` | numeric | Observed data |
| `method` | character | Clustering method |
| `tol` | numeric | Tolerance for equivalence |
| `verbose` | logical | Print progress |

### Return value

An S3 object of class `equiv_classes` containing:

- `classes`: List of equivalence classes (indices or names)
- `n_classes`: Number of distinct classes
- `membership`: Vector mapping each model to its class
- `pairwise`: Matrix of pairwise equivalence results
- `representatives`: One model from each class
- `specs`: Original model specifications

## Tasks

### 7.1 Multi-model container

Create `R/model_set.R`:

- [ ] `model_set()` - constructor for collection of models
- [ ] Validation (all must be model_spec)
- [ ] S3 methods: print, summary, subset

### 7.2 Pairwise comparison matrix

- [ ] `compare_all_pairs()` - compute all pairwise comparisons
- [ ] Store as matrix or distance-like object
- [ ] Handle computational cost (parallel option)

### 7.3 Equivalence class detection

Create `R/equivalence_classes.R`:

- [ ] `equivalence_classes()` - main function
- [ ] Pairwise method: Union-find from pairwise results
- [ ] Hierarchical method: Cluster based on likelihood discrepancies

### 7.4 Result class and methods

Create `R/equiv_classes.R`:

- [ ] `equiv_classes` S3 class
- [ ] `print.equiv_classes()` - summary display
- [ ] `summary.equiv_classes()` - detailed output
- [ ] `plot.equiv_classes()` - dendrogram or network visualization

### 7.5 Utilities

- [ ] `n_classes()` - number of equivalence classes
- [ ] `class_members()` - models in a specific class
- [ ] `representative()` - get representative model from class
- [ ] `is_equivalent()` - check if two models are in same class

## Example usage

```r
# Define several candidate models
models <- list(
  exp = model_spec(...),      # Exponential
  gamma1 = model_spec(...),   # Gamma with shape=1
  gamma2 = model_spec(...),   # Gamma with shape=2
  weibull1 = model_spec(...), # Weibull with shape=1
  weibull2 = model_spec(...)  # Weibull with shape=2
)

y <- rexp(100, rate = 2)
classes <- equivalence_classes(models, y)

print(classes)
# <equiv_classes>
# 5 models -> 3 equivalence classes
#
# Class 1: exp, gamma1, weibull1 (equivalent)
# Class 2: gamma2
# Class 3: weibull2

# Visualize
plot(classes)  # Network or dendrogram
```

## Algorithm: Pairwise method

```
1. Compute all n(n-1)/2 pairwise comparisons
2. Build equivalence relation:
   - Initialize each model in its own class
   - For each equivalent pair (i, j), merge classes
3. Use union-find for efficient merging
4. Output: partition of models into classes
```

## Algorithm: Hierarchical method

```
1. Compute pairwise "distance" = max likelihood discrepancy
2. Apply hierarchical clustering
3. Cut tree at threshold tol
4. Each subtree is an equivalence class
```

## Tests to write

```r
# tests/testthat/test-equivalence_classes.R

test_that("known equivalent models are grouped together", {
  # Exp, Gamma(1), Weibull(1) should be in same class
})

test_that("non-equivalent models are in different classes", {...})

test_that("single model returns one class", {...})

test_that("transitivity is respected", {
  # If A~B and B~C, then A~C
})

test_that("hierarchical and pairwise methods agree", {...})
```

## Files created

```
R/
├── model_set.R
├── equivalence_classes.R
├── equiv_classes.R  # S3 class and methods
└── clustering.R     # Hierarchical clustering utilities
tests/testthat/
└── test-equivalence_classes.R
```

## Visualization options

### Network graph
- Nodes = models
- Edges = equivalent pairs
- Connected components = equivalence classes

### Dendrogram
- Hierarchical structure
- Height = discrepancy measure
- Cut at threshold shows classes

### Heatmap
- Pairwise discrepancy matrix
- Blocks on diagonal = equivalence classes

## Computational considerations

- n models → n(n-1)/2 comparisons
- Each comparison involves optimization
- For n > 10, computation becomes significant
- Options:
  - Parallel processing
  - Early stopping when class membership is determined
  - Approximate methods for large n

## Edge cases

1. All models equivalent → single class
2. No models equivalent → n classes
3. Models with different parameter counts
4. Near-equivalence (borderline cases)
5. Very large model sets (>20 models)

## Notes

- Consider adding `add_model()` to incrementally update classes
- May want to report "confidence" in class assignments
- For publication, report both within-class and between-class discrepancies
- Consider whether to support weighted comparisons
