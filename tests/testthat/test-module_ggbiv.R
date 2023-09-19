test_that("multiplication works", {

  brauer_augmented <- brauer_2008_tidy %>%
    add_pcs(npcs = 5, verbose = FALSE) %>%
    tomic_to("triple_omic")

  tomic_table <- brauer_augmented$samples
  grob <- plot_bivariate(tomic_table, "PC1", "PC2", "nutrient", "nutrient", 0.5, 10)
  expect_s3_class(grob, "ggplot")

  grob <- plot_bivariate(tomic_table, "nutrient", "PC2", "nutrient")
  expect_s3_class(grob, "ggplot")

  })

test_that("List viable variables for a plot", {

  expect_equal(
    get_design_vars(brauer_2008_tidy, plot_table = "measurements", "categorical"),
    c("name", "sample")
  )

  expect_equal(
    get_design_vars(brauer_2008_tidy, plot_table = "features", "all"),
    c("name", "systematic_name", "BP", "MF")
  )

  expect_equal(
    get_design_vars(brauer_2008_tidy, plot_table = "samples", "quantitative"),
    "DR"
  )

  expect_equal(
    get_design_vars(brauer_2008_tidy, plot_table = "samples", "all"),
    brauer_2008_tidy$design$samples$variable
  )

})
