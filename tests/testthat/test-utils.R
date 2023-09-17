library(dplyr)

test_that("Coercing to original classes works", {
  simple_factor <- factor(c("B", "A"), levels = c("A", "B"))

  # -> factor
  expect_equal(coerce_to_classes(c("B", "A"), simple_factor), simple_factor)
  # should throw an error when NAs are introduced for non-NAs
  expect_error(coerce_to_classes(c("B", "A", "C"), simple_factor), "reference object")
  # -> character
  expect_equal(coerce_to_classes(123, LETTERS), "123")
  expect_equal(coerce_to_classes(simple_factor, LETTERS), c("B", "A"))
  # -> numeric
  expect_equal(coerce_to_classes("123", 1111), 123)
  expect_equal(coerce_to_classes(simple_factor, 1111), c(2, 1))
  # -> integer
  expect_equal(coerce_to_classes("123", 1L), 123L)
  expect_equal(coerce_to_classes(simple_factor, 1L), c(2L, 1L))
  # -> logical
  expect_equal(coerce_to_classes(c(1, 0, 4), TRUE), c(TRUE, FALSE, TRUE))
  expect_equal(coerce_to_classes(c("TRUE", "FALSE"), TRUE), c(TRUE, FALSE))
  # should throw an error when NAs are introduced for non-NAs
  expect_error(coerce_to_classes(c("foo", "bar"), TRUE), "reference object")
  # -> glue
  expect_equal(coerce_to_classes("ABC", glue::glue("")), glue::glue("ABC"))
})

test_that("Partial string matching works", {
  # directly specified
  expect_equal(var_partial_match("mpg", mtcars), "mpg")
  # valid substring
  expect_equal(var_partial_match("mp", mtcars), "mpg")
  expect_equal(var_partial_match("pg", mtcars), "mpg")
  # non match
  expect_error(var_partial_match("highlander", mtcars), regex = "did not match")
  # too vague - 2+ matches
  expect_error(var_partial_match("m", mtcars), regex = "2\\+ variables")
})

