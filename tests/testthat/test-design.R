test_that("extract design as a table", {
  get_design_tbl(brauer_2008_tidy) %>%
    expect_snapshot()
})
