library(dplyr)

test_that("hclust creates sane orders - synthetic example", {
  set.seed(1234)

  dat <- tibble::tibble(
    mu = c(10, 10, 10, 20, 20, 20, 30, 30, 30),
    letters = LETTERS[1:9]
  ) %>%
    tidyr::crossing(
      lc_letters = letters[1:10]
    ) %>%
    mutate(value = rnorm(dplyr::n(), mean = mu))

  reshape2::acast(dat, letters ~ lc_letters, value.var = "value")

  # test by clustering and verifying that the three groups are 1-2-3, 4-5-6,
  #  7-8-9
  groups <- tibble::tibble(letters = hclust_order(
    dat, "letters", "lc_letters", "value", "rows"
  )$rows) %>%
    left_join(
      dat %>%
        distinct(mu, letters),
      by = "letters"
    ) %>%
    dplyr::mutate(.entry = seq_len(n())) %>%
    dplyr::group_by(mu) %>%
    dplyr::summarize(val = sum(.entry))

  # sums of 1-2-3, 4-5-6, 7-8-9
  expect_equal(sort(groups$val), c(6, 15, 24))
})

test_that("hclust handled 1 row / column cases", {
  df <- tidyr::crossing(letters = LETTERS, numbers = 1) %>%
    mutate(noise = rnorm(n()))
  hclust_orders <- hclust_order(df, "letters", "numbers", "noise", "both")

  expect_length(hclust_orders$row, 26)
  expect_length(hclust_orders$columns, 1)
})

test_that("hclust creates sane orders - real example", {
  tomic <- romic::brauer_2008_triple %>%
    filter_tomic(
      filter_type = "category",
      filter_table = "features",
      filter_variable = "BP",
      filter_value = c(
        "protein biosynthesis",
        "rRNA processing",
        "response to stress"
      )
    ) %>%
    triple_to_tidy()

  expect_equal(
    hclust_order(
      tomic$data, "name", "sample", "expression", "rows", "corr"
    )$rows,
    c(
      "TIR1", "XBP1", "RPL1B", "MRPL35", "RPL8B", "RPL26A", "RPS10B", "RPL18B",
      "RPL20A", "CBS1", "MRPS16", "RPL13B", "ESF1", "RSM10"
    )
  )

  expect_equal(
    hclust_order(
      tomic$data, "name", "sample", "expression", "rows", "dist"
    )$rows,
    c(
      "TIR1", "XBP1", "MRPL35", "RPL18B", "RPL20A", "ESF1", "RSM10",
      "MRPS16", "CBS1", "RPL13B", "RPL26A", "RPS10B", "RPL1B", "RPL8B"
    )
  )
})

test_that("downsampling features (for creating a heatmap works)", {
  downsampled_df <- brauer_2008_tidy$data %>%
    dplyr::mutate(
      ordered_featureId = factor(name, levels = unique(name)),
      ordered_sampleId = factor(sample, levels = unique(sample))
    ) %>%
    downsample_heatmap(
      value_var = "expression",
      design = brauer_2008_tidy$design,
      max_display_features = 100,
      verbose = FALSE
    )

  expect_equal(nrow(downsampled_df), 3600)
  expect_equal(length(unique(downsampled_df$name)), 100)
})
