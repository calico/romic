library(dplyr)

test_that("Updating triple retains cohesiveness", {
  broken_brauer <- brauer_2008_triple
  broken_brauer$features <- broken_brauer$features %>% mutate(foo = "bar")
  expect_error(check_tomic(broken_brauer), "permutation")

  broken_brauer <- brauer_2008_triple
  broken_brauer$features$name <- factor(broken_brauer$features$name)
  expect_error(check_tomic(broken_brauer), "classes")
})


test_that("Test check_tidy_omic edge cases", {

  testthat::expect_s3_class(simple_tidy, "tidy_omic")

  double_data_tidy <- simple_tidy
  double_data_tidy$data <- rbind(double_data_tidy$data, double_data_tidy$data)

  # duplicated feature, sample, measurement tuples
  expect_snapshot(check_tidy_omic(double_data_tidy, fast_check = FALSE), error = TRUE)

  degenerate_attributes <- three_col_df %>%
    mutate(
      degen_sample_var = rep(1:10, each = 10),
      degen_feature_var = rep(1:10, times = 10)
      )

  # check verbose message
  expect_message(
    create_tidy_omic(
      three_col_df,
      feature_pk = "features",
      sample_pk = "samples"
      ),
    regexp = "1 measurement variables"
  )

  expect_snapshot(
    create_tidy_omic(
      degenerate_attributes %>% select(-degen_sample_var),
      feature_pk = "features",
      sample_pk = "samples",
      feature_var = "degen_feature_var",
      verbose = FALSE
    ),
    error = TRUE)

  expect_snapshot(
    create_tidy_omic(
      degenerate_attributes %>% select(-degen_feature_var),
      feature_pk = "features",
      sample_pk = "samples",
      sample_var = "degen_sample_var",
      verbose = FALSE
    ),
    error = TRUE)

})


test_that("Factor primary keys are preserved when converting from a tidy to a triple", {

  tidy <- create_tidy_omic(
    three_col_df_fct,
    feature_pk = "features",
    sample_pk = "samples",
    verbose = FALSE
    )

  # catch failure case
  expect_snapshot(
    create_tidy_omic(
      three_col_df_fct,
      feature_pk = "features",
      sample_pk = "samples",
      sample_vars = "measurement",
      feature_vars = "measurement",
      verbose = FALSE
      ),
    error = TRUE
    )

  triple_from_tidy <- tomic_to(tidy, "triple_omic")
  triple_from_tidy_check_status <- romic::check_tomic(triple_from_tidy, fast_check = FALSE)
  expect_equal(triple_from_tidy_check_status, 0)

  tidy_with_pcs <- add_pcs(tidy, verbose = FALSE)
  expect_true(sum(stringr::str_detect(colnames(tidy_with_pcs$data), "^PC")) > 1)
})

test_that("Numeric primary keys are preserved when converting from a tidy to a triple", {

  triple_from_tidy <- tomic_to(simple_tidy, "triple_omic")
  triple_from_tidy_check_status <- check_tomic(triple_from_tidy, fast_check = FALSE)
  expect_equal(triple_from_tidy_check_status, 0)

  tidy_with_pcs <- add_pcs(simple_tidy, verbose = FALSE)
  expect_true(sum(stringr::str_detect(colnames(tidy_with_pcs$data), "^PC")) > 1)
})

test_that("Create triple omic", {

  testthat::expect_s3_class(simple_triple, "triple_omic")

  # works without providing features or samples
  triple_omic <- create_triple_omic(
    triple_setup$measurement_df,
    feature_pk = "feature_id",
    sample_pk = "sample_id"
  )

})


test_that("Read wide data", {

  wide_measurements <- brauer_2008_triple[["measurements"]] %>%
    tidyr::spread(sample, expression)

  wide_df <- brauer_2008_triple[["features"]] %>%
    left_join(wide_measurements, by = "name")

  tidy_omic <- convert_wide_to_tidy_omic(
    wide_df,
    feature_pk = "name",
    feature_vars = c("BP", "MF", "systematic_name"),
    verbose = FALSE
  )

  testthat::expect_s3_class(tidy_omic, "tidy_omic")

})


test_that("Find primary or foreign keys in tomic table", {

  expect_equal(get_identifying_keys(brauer_2008_triple, "measurements"), c("name", "sample"))
  expect_equal(get_identifying_keys(brauer_2008_triple, "features"), "name")
  expect_equal(get_identifying_keys(brauer_2008_triple, "samples"), "sample")

})

test_that("Test that get_tomic_table() can retrieve various tables", {

  tidy_df <- get_tomic_table(brauer_2008_triple, "tidy")

  expect_equal(nrow(tidy_df), 18000)
  expect_equal(infer_tomic_table_type(brauer_2008_triple, tidy_df), "measurements")

  samples_df <- get_tomic_table(simple_tidy, "samples")

  expect_equal(dim(samples_df), c(10,1))
  expect_equal(infer_tomic_table_type(simple_tidy, samples_df), "samples")

  expect_snapshot(
    infer_tomic_table_type(simple_tidy, samples_df %>% rename(fake_samples = samples)),
    error = TRUE
  )
})
