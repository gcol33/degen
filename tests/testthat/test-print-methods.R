# Tests for print/summary/format methods using mock objects
# These tests don't require expensive surface comparisons

# Mock surface_comparison objects
make_mock_equivalent <- function() {
  structure(
    list(
      equivalent = TRUE,
      max_discrepancy = 1e-10,
      ab_discrepancy = 1e-10,
      ba_discrepancy = 1e-11,
      n_tested = 20,
      method = "grid",
      tol = 1e-6,
      pair = list(name = "ModelA vs ModelB"),
      y_length = 50,
      evidence = list(
        A_to_B = data.frame(discrepancy = c(1e-10, 1e-11, 1e-12)),
        B_to_A = data.frame(discrepancy = c(1e-11, 1e-12, 1e-13))
      )
    ),
    class = "surface_comparison"
  )
}

make_mock_not_equivalent <- function() {
  structure(
    list(
      equivalent = FALSE,
      max_discrepancy = 5.5,
      ab_discrepancy = 5.5,
      ba_discrepancy = 4.2,
      n_tested = 20,
      method = "grid",
      tol = 1e-6,
      pair = list(name = "ModelA vs ModelC"),
      y_length = 50,
      evidence = list(
        A_to_B = data.frame(discrepancy = c(5.5, 3.2, 2.1)),
        B_to_A = data.frame(discrepancy = c(4.2, 2.8, 1.9))
      )
    ),
    class = "surface_comparison"
  )
}

# surface_comparison print tests
test_that("print.surface_comparison shows EQUIVALENT for equivalent models", {
  mock <- make_mock_equivalent()
  expect_output(print(mock), "EQUIVALENT")
  expect_output(print(mock), "surface_comparison")
  expect_output(print(mock), "Points tested")
  expect_output(print(mock), "Max discrepancy")
})

test_that("print.surface_comparison shows NOT EQUIVALENT for non-equivalent models", {
  mock <- make_mock_not_equivalent()
  expect_output(print(mock), "NOT EQUIVALENT")
})

# surface_comparison summary tests
test_that("summary.surface_comparison shows equivalent conclusion", {
  mock <- make_mock_equivalent()
  expect_output(summary(mock), "Surface Comparison Results")
  expect_output(summary(mock), "observationally equivalent")
  expect_output(summary(mock), "Discrepancy summary")
  expect_output(summary(mock), "Discrepancy distribution")
  expect_output(summary(mock), "Min:")
  expect_output(summary(mock), "Median:")
  expect_output(summary(mock), "Mean:")
  expect_output(summary(mock), "Max:")
  expect_output(summary(mock), "% < tolerance")
})

test_that("summary.surface_comparison shows not equivalent conclusion", {
  mock <- make_mock_not_equivalent()
  expect_output(summary(mock), "NOT observationally equivalent")
})

# surface_comparison format tests
test_that("format.surface_comparison shows EQUIVALENT", {
  mock <- make_mock_equivalent()
  formatted <- format(mock)
  expect_match(formatted, "EQUIVALENT")
  expect_match(formatted, "surface_comparison")
})

test_that("format.surface_comparison shows NOT EQUIVALENT", {
  mock <- make_mock_not_equivalent()
  formatted <- format(mock)
  expect_match(formatted, "NOT EQUIVALENT")
})

# Mock equiv_classes objects
make_mock_single_class <- function() {
  structure(
    list(
      classes = list(class_1 = c("exp", "gamma1")),
      n_classes = 1,
      membership = c(exp = 1L, gamma1 = 1L),
      pairwise = matrix(c(TRUE, TRUE, TRUE, TRUE), 2, 2,
                        dimnames = list(c("exp", "gamma1"), c("exp", "gamma1"))),
      discrepancies = matrix(c(0, 1e-10, 1e-10, 0), 2, 2,
                             dimnames = list(c("exp", "gamma1"), c("exp", "gamma1"))),
      model_names = c("exp", "gamma1"),
      n_models = 2,
      tol = 1e-6
    ),
    class = "equiv_classes"
  )
}

make_mock_multiple_classes <- function() {
  structure(
    list(
      classes = list(class_1 = c("exp", "gamma1"), class_2 = "gamma2"),
      n_classes = 2,
      membership = c(exp = 1L, gamma1 = 1L, gamma2 = 2L),
      pairwise = matrix(c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE), 3, 3,
                        dimnames = list(c("exp", "gamma1", "gamma2"),
                                        c("exp", "gamma1", "gamma2"))),
      discrepancies = matrix(c(0, 1e-10, 5.5, 1e-10, 0, 4.2, 5.5, 4.2, 0), 3, 3,
                             dimnames = list(c("exp", "gamma1", "gamma2"),
                                             c("exp", "gamma1", "gamma2"))),
      model_names = c("exp", "gamma1", "gamma2"),
      n_models = 3,
      tol = 1e-6
    ),
    class = "equiv_classes"
  )
}

# equiv_classes print tests
test_that("print.equiv_classes shows single class correctly", {
  mock <- make_mock_single_class()
  expect_output(print(mock), "equiv_classes")
  expect_output(print(mock), "2 models")
  expect_output(print(mock), "1 equivalence class")
  expect_output(print(mock), "Class 1")
  expect_output(print(mock), "equivalent")
})

test_that("print.equiv_classes shows multiple classes correctly", {
  mock <- make_mock_multiple_classes()
  expect_output(print(mock), "3 models")
  expect_output(print(mock), "2 equivalence classes")
  expect_output(print(mock), "Class 1")
  expect_output(print(mock), "Class 2")
})

# equiv_classes summary tests
test_that("summary.equiv_classes produces detailed output", {
  mock <- make_mock_single_class()
  expect_output(summary(mock), "Equivalence Class Analysis")
  expect_output(summary(mock), "Models:")
  expect_output(summary(mock), "Number of models:")
  expect_output(summary(mock), "Number of classes:")
  expect_output(summary(mock), "Tolerance:")
  expect_output(summary(mock), "Equivalence Classes:")
  expect_output(summary(mock), "Pairwise Discrepancies")
})

test_that("summary.equiv_classes shows members for each class", {
  mock <- make_mock_multiple_classes()
  expect_output(summary(mock), "Class 1")
  expect_output(summary(mock), "Class 2")
  expect_output(summary(mock), "2 members")
  expect_output(summary(mock), "1 member")
})

# equiv_classes format tests
test_that("format.equiv_classes works", {
  mock <- make_mock_single_class()
  formatted <- format(mock)
  expect_match(formatted, "equiv_classes")
  expect_match(formatted, "2 models")
  expect_match(formatted, "1 classes")
})

# Accessor tests with mock objects
test_that("n_classes works with mock", {
  mock <- make_mock_single_class()
  expect_equal(n_classes(mock), 1)

  mock2 <- make_mock_multiple_classes()
  expect_equal(n_classes(mock2), 2)
})

test_that("class_members works with mock", {
  mock <- make_mock_multiple_classes()

  members1 <- class_members(mock, 1)
  expect_true("exp" %in% members1)
  expect_true("gamma1" %in% members1)

  members2 <- class_members(mock, 2)
  expect_equal(members2, "gamma2")
})

test_that("are_equivalent works with mock", {
  mock <- make_mock_multiple_classes()

  expect_true(are_equivalent(mock, "exp", "gamma1"))
  expect_false(are_equivalent(mock, "exp", "gamma2"))
})

# Mock fisher_info objects
make_mock_fisher_full_rank <- function() {
  structure(
    list(
      matrix = matrix(c(100, -5, -5, 50), 2, 2,
                      dimnames = list(c("mu", "sigma"), c("mu", "sigma"))),
      eigenvalues = c(102, 48),
      eigenvectors = matrix(c(0.98, -0.2, 0.2, 0.98), 2, 2),
      condition = 2.125,
      rank = 2,
      n_par = 2,
      par = c(mu = 5, sigma = 2),
      par_names = c("mu", "sigma"),
      n_obs = 100
    ),
    class = "fisher_info"
  )
}

make_mock_fisher_rank_deficient <- function() {
  structure(
    list(
      matrix = matrix(c(100, 100, 100, 100), 2, 2,
                      dimnames = list(c("a", "b"), c("a", "b"))),
      eigenvalues = c(200, 0),
      eigenvectors = matrix(c(0.707, 0.707, -0.707, 0.707), 2, 2),
      condition = Inf,
      rank = 1,
      n_par = 2,
      par = c(a = 2, b = 3),
      par_names = c("a", "b"),
      n_obs = 100
    ),
    class = "fisher_info"
  )
}

# fisher_info print tests
test_that("print.fisher_info shows full rank", {
  mock <- make_mock_fisher_full_rank()
  expect_output(print(mock), "fisher_info")
  expect_output(print(mock), "Condition number")
  expect_output(print(mock), "Rank: 2 / 2")
  expect_output(print(mock), "full")
  expect_output(print(mock), "Eigenvalues")
})

test_that("print.fisher_info shows rank deficient", {
  mock <- make_mock_fisher_rank_deficient()
  expect_output(print(mock), "Rank: 1 / 2")
  expect_output(print(mock), "RANK DEFICIENT")
})

# fisher_info summary tests
test_that("summary.fisher_info shows well-identified", {
  mock <- make_mock_fisher_full_rank()
  expect_output(summary(mock), "Fisher Information Analysis")
  expect_output(summary(mock), "Parameters:")
  expect_output(summary(mock), "Information Matrix")
  expect_output(summary(mock), "Eigenvalue Decomposition")
  expect_output(summary(mock), "Condition number")
  expect_output(summary(mock), "well-identified")
})

test_that("summary.fisher_info shows non-identifiable warning", {
  mock <- make_mock_fisher_rank_deficient()
  expect_output(summary(mock), "NON-IDENTIFIABLE")
})

test_that("summary.fisher_info shows poorly conditioned warning", {
  mock <- make_mock_fisher_full_rank()
  mock$condition <- 1500
  expect_output(summary(mock), "POORLY CONDITIONED")
})

test_that("summary.fisher_info shows moderate conditioning warning", {
  mock <- make_mock_fisher_full_rank()
  mock$condition <- 150
  expect_output(summary(mock), "Moderate conditioning")
})

test_that("summary.fisher_info flags near-zero eigenvalues", {
  mock <- make_mock_fisher_full_rank()
  mock$eigenvalues <- c(100, 1e-8)
  expect_output(summary(mock), "NEAR ZERO")
})

# fisher_info format tests
test_that("format.fisher_info works", {
  mock <- make_mock_fisher_full_rank()
  formatted <- format(mock)
  expect_match(formatted, "fisher_info")
  expect_match(formatted, "rank 2/2")
})

# fisher_info accessors
test_that("info_eigenvalues works", {
  mock <- make_mock_fisher_full_rank()
  eigs <- info_eigenvalues(mock)
  expect_equal(eigs, c(102, 48))
})

test_that("info_condition works", {
  mock <- make_mock_fisher_full_rank()
  expect_equal(info_condition(mock), 2.125)
})

test_that("info_rank works", {
  mock <- make_mock_fisher_full_rank()
  expect_equal(info_rank(mock), 2)
})

test_that("null_directions returns NULL for full rank", {
  mock <- make_mock_fisher_full_rank()
  expect_null(null_directions(mock))
})

test_that("null_directions returns vectors for rank deficient", {
  mock <- make_mock_fisher_rank_deficient()
  dirs <- null_directions(mock)
  expect_true(is.matrix(dirs))
  expect_equal(ncol(dirs), 1)
  expect_equal(rownames(dirs), c("a", "b"))
})
