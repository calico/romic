test_that("extract design as a table", {
  get_design_tbl(brauer_2008_tidy) %>%
    expect_snapshot()

  expect_invisible(check_design_in_tomic(brauer_2008_tidy))
})

test_that("get_design_tbl() works when directly passing a design instead of a tomic", {
  expect_equal(
    get_design_tbl(brauer_2008_tidy),
    get_design_tbl(brauer_2008_tidy$design)
  )
})

test_that("Catch malformed design objects", {

  malformed_design <- brauer_2008_tidy$design
  malformed_design$foo <- "bar"

  expect_snapshot(
    check_design(malformed_design),
    error = TRUE
  )


  malformed_design <- brauer_2008_tidy$design
  malformed_design$feature_pk <- NULL

  expect_snapshot(
    check_design(malformed_design),
    error = TRUE
  )

})

