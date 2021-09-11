library(dplyr)

test_that("PCs -> bivariate plots", {

  brauer_with_pcs <- brauer_2008_triple %>%
    add_pca_loadings(npcs = 5)

  pc_varex <- brauer_with_pcs$design$samples$variable %>%
    {.[stringr::str_detect(., "^PC")]} %>%
    purrr::map_dbl(function(x){
      as.numeric(stringr::str_extract(x, "[0-9]{1,2}.[0-9]"))
    })
  expect_equal(pc_varex, c(30.2, 23, 13.1, 7.3, 6.5))

  plot_bivariate(
    brauer_with_pcs$samples,
    x_var = "PC1 (30.2% variance explained)",
    y_var = "PC2 (23.0% variance explained)"
  )

})
