library(dplyr)

test_that("downsampling features (for creating a heatmap works)", {
  downsampled_df <- brauer_2008_tidy$data %>%
    dplyr::mutate(
      ordered_featureId = factor(name, levels = unique(name)),
      ordered_sampleId = factor(sample, levels = unique(sample))
    ) %>%
    downsample_heatmap(
      value_var = "expression",
      design = brauer_2008_tidy$design,
      max_display_features = 100
    )

  expect_equal(nrow(downsampled_df), 3600)
  expect_equal(length(unique(downsampled_df$name)), 100)
})
