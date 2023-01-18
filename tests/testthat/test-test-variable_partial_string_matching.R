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
