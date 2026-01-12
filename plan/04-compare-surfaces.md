# Step 4: Likelihood Surface Comparison

**Version target**: 0.4.0
**Scope**: `compare_surfaces()` for detecting observational equivalence

## Objective

Implement the core algorithm for determining whether two model specifications produce equivalent likelihood surfaces. This is the main analytical engine of the package.

## Conceptual background

Two models are observationally equivalent if, for every parameter configuration of model A, there exists a parameter configuration of model B that produces the same likelihood for all possible data. This requires:

1. For any θ_A, find θ_B such that L_A(y; θ_A) = L_B(y; θ_B) for all y
2. This mapping must exist in both directions (A→B and B→A)

## API

```r
compare_surfaces(

  pair,
  y,
  n_points = 100,
  method = c("grid", "optimization", "both"),
  tol = 1e-6,
  verbose = FALSE
)
```

### Arguments

| Argument | Type | Description |
|----------|------|-------------|
| `pair` | equivalence_pair | The model pair to compare |
| `y` | numeric | Observed data vector |
| `n_points` | integer | Number of parameter points to sample |
| `method` | character | Comparison method |
| `tol` | numeric | Tolerance for likelihood equality |
| `verbose` | logical | Print progress information |

### Return value

An S3 object of class `surface_comparison` containing:

- `equivalent`: logical, overall equivalence conclusion
- `evidence`: data frame of sampled points and results
- `mapping`: estimated parameter mapping (if found)
- `discrepancies`: points where equivalence fails
- `method`: method used
- `pair`: the original pair
- `y`: the data used

## Tasks

### 4.1 Core comparison engine

Create `R/compare_surfaces.R`:

- [ ] `compare_surfaces()` - main function
- [ ] Grid-based method:
  - Sample parameter space of model A
  - For each θ_A, optimize to find best θ_B
  - Check if likelihoods match within tolerance
- [ ] Optimization-based method:
  - Use nonlinear optimization to find counterexamples
  - If none found after extensive search, conclude equivalence

### 4.2 Parameter space sampling

Create `R/sampling.R`:

- [ ] `sample_par_space()` - generate parameter grid/samples
  - Latin hypercube sampling for efficiency
  - Respect parameter bounds
  - Handle unbounded parameters (truncate to reasonable range)
- [ ] `propose_bounds()` - suggest reasonable bounds for unbounded parameters

### 4.3 Likelihood matching optimization

- [ ] `find_equivalent_par()` - given θ_A, find θ_B that matches
  - Use `optim()` with L-BFGS-B for bounded optimization
  - Objective: minimize |L_A(y; θ_A) - L_B(y; θ_B)|
- [ ] Handle multiple local optima (multi-start optimization)

### 4.4 Result class

Create `R/surface_comparison.R`:

- [ ] `surface_comparison` S3 class constructor
- [ ] `print.surface_comparison()` - summary output
- [ ] `summary.surface_comparison()` - detailed results
- [ ] `plot.surface_comparison()` - visualization (if feasible)

### 4.5 Evidence aggregation

- [ ] Collect results across all sampled points
- [ ] Compute summary statistics:
  - Proportion of points with successful matches
  - Maximum discrepancy found
  - Distribution of discrepancies
- [ ] Decision rule for equivalence conclusion

## Algorithm details

### Grid method

```
1. Sample n_points from parameter space of A
2. For each θ_A:
   a. Compute L_A(y; θ_A)
   b. Optimize over θ_B to minimize |L_A - L_B|
   c. Record minimum discrepancy
3. If all discrepancies < tol, conclude equivalent
4. Repeat in reverse direction (B → A)
```

### Optimization method

```
1. Define objective: max over θ_A of min over θ_B of |L_A - L_B|
2. If objective ≈ 0, models are equivalent
3. If objective > 0, return the counterexample θ_A
```

## Example usage

```r
pair <- equivalence_pair(exp_spec, gamma_spec)
y <- rexp(100, rate = 2)

result <- compare_surfaces(pair, y, n_points = 50)

print(result)
# <surface_comparison>
# Conclusion: Models appear EQUIVALENT
# Points tested: 50
# Max discrepancy: 2.3e-12
# Method: grid

# If not equivalent:
# <surface_comparison>
# Conclusion: Models are NOT EQUIVALENT
# Points tested: 50
# Counterexample found at θ_A = c(...)
# Discrepancy: 0.847
```

## Tests to write

```r
# tests/testthat/test-compare_surfaces.R

test_that("equivalent models are detected", {

  # Exp vs Gamma(shape=1) should be equivalent
})

test_that("non-equivalent models are detected", {
  # Exp vs Normal should not be equivalent
})

test_that("results are symmetric", {
  # compare(A, B) and compare(B, A) should agree
})

test_that("tolerance affects conclusion appropriately", {...})
test_that("insufficient points may miss non-equivalence", {...})
```

## Files created

```
R/
├── compare_surfaces.R
├── surface_comparison.R
└── sampling.R
tests/testthat/
└── test-compare_surfaces.R
```

## Edge cases

1. Flat likelihood surfaces (non-identifiable models)
2. Very peaked likelihoods (optimization may struggle)
3. Multimodal likelihood surfaces
4. Different parameter counts between models
5. Boundary parameters (at edge of allowed region)

## Performance considerations

- Grid method scales poorly with parameter dimension
- For >3 parameters, optimization method may be necessary
- Consider parallel evaluation of grid points
- Cache likelihood evaluations where possible

## Notes

- This is computationally intensive; provide progress indicators for `verbose = TRUE`
- Consider adding `max_time` argument to limit computation
- May want to return early if clear non-equivalence is found
- Document that this is numerical evidence, not proof
