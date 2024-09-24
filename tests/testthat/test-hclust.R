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

test_that("hclust_tidy_omic() tests all logic branches", {

  simple_tidy_w_factors <- simple_tidy
  simple_tidy_w_factors$data <- simple_tidy_w_factors$data %>%
    dplyr::mutate(
      features = factor(features),
      samples = ordered(samples)
    ) %>%
    # shuffle so we can test ordering
    dplyr::sample_frac(1)

  hclust_w_fct_coercion <- hclust_tidy_omic(
    simple_tidy_w_factors,
    feature_var = simple_tidy_w_factors$design$feature_pk,
    sample_var = simple_tidy_w_factors$design$sample_pk,
    value_var = "measurement",
    cluster_dim = "both"
    )

  expect_s3_class(
    hclust_w_fct_coercion$data$features,
    "factor"
  )

  expect_s3_class(
    hclust_w_fct_coercion$data$samples,
    "ordered"
  )

  # catch corner cases

  expect_error(
    hclust_tidy_omic(
      simple_tidy_w_factors,
      feature_var = "features",
      sample_var = "samples",
      value_var = "features",
      cluster_dim = "both"
    ),
  "feature_pk, sample_pk, and value_var must all be different"
  )

  # preserve default feature ordering
  hclust_w_default_feature_orders <- hclust_tidy_omic(
    simple_tidy_w_factors,
    feature_var = simple_tidy_w_factors$design$feature_pk,
    sample_var = simple_tidy_w_factors$design$sample_pk,
    value_var = "measurement",
    cluster_dim = "columns"
  )

  expect_equal(
    class(hclust_w_default_feature_orders$data$features),
    class(hclust_w_default_feature_orders$data$ordered_featureId)
  )

  # factor ordering defined by original orders
  expect_equal(
    as.character(levels(hclust_w_default_feature_orders$data$features)),
    levels(hclust_w_default_feature_orders$data$ordered_featureId)
  )

  # preserve default sample orders
  hclust_w_default_sample_orders <- hclust_tidy_omic(
    simple_tidy_w_factors,
    feature_var = simple_tidy_w_factors$design$feature_pk,
    sample_var = simple_tidy_w_factors$design$sample_pk,
    value_var = "measurement",
    cluster_dim = "rows"
  )

  expect_equal(
    class(hclust_w_default_sample_orders$data$samples),
    class(hclust_w_default_sample_orders$data$ordered_sampleId)
  )

  # factor ordering defined by original orders
  expect_equal(
    as.character(levels(hclust_w_default_sample_orders$data$samples)),
    levels(hclust_w_default_sample_orders$data$ordered_sampleId)
  )

  # sort features by non-factor feature variable when clustering just columns

  simple_tidy_shuffle <- simple_tidy
  simple_tidy_shuffle$data <- dplyr::sample_frac(simple_tidy_shuffle$data)

  sorted_tidy_omic <- hclust_tidy_omic(
    simple_tidy_shuffle,
    feature_var = "features",
    sample_var = "samples",
    value_var = "measurement",
    cluster_dim = "columns",
  )

  expect_s3_class(sorted_tidy_omic$data$ordered_featureId, "factor")
  expect_equal(levels(sorted_tidy_omic$data$ordered_featureId), as.character(1:10))
})


test_that("hclust_tidy_omic() runs even if clustering initially fails due to missing values", {

  # create a dataset where distances between features and samples will create
  # some NAs
  disjoint_tomic <- simple_tidy
  disjoint_tomic$data <- dplyr::bind_rows(
    simple_tidy$data %>%
      dplyr::filter(
        features <= 5,
        samples <= 5
      ),
    simple_tidy$data %>%
      dplyr::filter(
        features > 5,
        samples > 5
      )
  )

  clustered_disjoint_tomic <- hclust_tidy_omic(
    disjoint_tomic,
    feature_var = "features",
    sample_var = "samples",
    value_var = "measurement",
    cluster_dim = "both"
  )

  expect_s3_class(clustered_disjoint_tomic, "tomic")
})

test_that("Catch apply_hclust() corner cases", {

  invalid_matrix <- matrix(1:4, nrow = 2)
  invalid_matrix <- invalid_matrix[-c(1:2),]

  expect_error(
    apply_hclust(invalid_matrix, "foo", "bar"),
    "contained zero rows"
  )

  # create data which will generate NAs in distance matrix
  disjoint_data_matrix <- dplyr::bind_rows(
    simple_tidy$data %>%
      dplyr::filter(
        features <= 5,
        samples <= 5
      ),
    simple_tidy$data %>%
      dplyr::filter(
        features > 5,
        samples > 5
      )
  ) %>%
    reshape2::acast(features ~ samples, value.var = "measurement")

  expect_error(
    apply_hclust(disjoint_data_matrix, "dist", "ward.D2"),
    "NA/NaN/Inf in foreign function call"
  )

  expect_error(
    apply_hclust(disjoint_data_matrix, "corr", "ward.D2"),
    "NA distances are not allowed with hierarchical clustering"
  )

  expect_error(
    apply_hclust(disjoint_data_matrix, "baz", "ward.D2"),
    "baz is not a defined distance_measure"
  )

})

test_that("collapse_feature_vars() is well behaved", {
  expect_equal(collapse_feature_vars("A"), "A")
  expect_equal(collapse_feature_vars(c("B", "A")), "B & A")
  expect_equal(collapse_feature_vars(1:5), 1)
})
