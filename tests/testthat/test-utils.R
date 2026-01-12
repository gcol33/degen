test_that("check_numeric validates numeric input", {
  expect_invisible(check_numeric(1:10))
  expect_invisible(check_numeric(c(1.5, 2.5)))
  expect_error(check_numeric("a"), "must be numeric")
  expect_error(check_numeric(NULL), "must not be NULL")
  expect_invisible(check_numeric(NULL, allow_null = TRUE))
  expect_error(check_numeric(c(1, NaN)), "contains NaN")
})

test_that("check_function validates function input", {
  expect_invisible(check_function(mean))
  expect_error(check_function("not a function"), "must be a function")
  expect_invisible(check_function(function(x, y) x + y, required_args = c("x", "y")))
  expect_error(
    check_function(function(x) x, required_args = c("x", "y")),
    "must have argument"
  )
})

test_that("check_bounds validates bounds structure", {
  expect_invisible(check_bounds(NULL, "a"))
  expect_invisible(check_bounds(list(a = c(0, 1)), "a"))
  expect_error(check_bounds(c(0, 1), "a"), "must be a named list")
  expect_error(check_bounds(list(c(0, 1)), "a"), "must be a named list")
  expect_error(check_bounds(list(b = c(0, 1)), "a"), "Unknown parameter")
  expect_error(check_bounds(list(a = c(1, 0)), "a"), "Lower bound must be less")
  expect_error(check_bounds(list(a = c(0, 1, 2)), "a"), "length-2 numeric")
})

test_that("check_par_in_bounds validates parameter values",
{
  bounds <- list(a = c(0, 10), b = c(-1, 1))
  expect_invisible(check_par_in_bounds(c(a = 5, b = 0), bounds))
  expect_error(check_par_in_bounds(c(a = 15), bounds), "outside bounds")
  expect_error(check_par_in_bounds(c(b = -2), bounds), "outside bounds")
})
