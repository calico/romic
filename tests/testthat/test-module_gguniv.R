test_that("Univariate plotting works", {
  grob <- plot_univariate(
    get_tomic_table(simple_tidy, "measurements"),
    "features"
    )

  expect_s3_class(grob, "ggplot")
})
