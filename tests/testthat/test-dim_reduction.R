test_that("Removing missing values works", {
  # drop_features
  invisible(capture.output(n_features <- nrow(remove_missing_values(
    brauer_2008_triple,
    missing_val_method = "drop_features"
  )$features)))
  expect_equal(n_features, 460)

  # drop_samples with no missing samples should behave like drop_features
  invisible(capture.output(n_features <- nrow(remove_missing_values(
    brauer_2008_triple,
    missing_val_method = "drop_samples"
  )$features)))
  expect_equal(n_features, 460)

  brauer_missing_samples <- brauer_2008_triple
  brauer_missing_samples$measurements$expression[
    brauer_missing_samples$measurements$sample %in% c("G0.05", "G0.1")
  ] <- NA

  invisible(capture.output(filtered_brauer <- remove_missing_values(
    brauer_missing_samples,
    missing_val_method = "drop_samples"
  )))

  expect_equal(nrow(filtered_brauer$samples), 34)
  expect_equal(nrow(filtered_brauer$measurements), 15674)
})

test_that("Matrices keys are reconstructed with appropriate classes", {

  tomic_with_missing_values <- simple_triple
  tomic_with_missing_values$measurements <- tomic_with_missing_values$measurements %>%
    dplyr::slice(-c(1:5))

  if (!("impute" %in% rownames(utils::installed.packages()))) {
    cli::cli_alert("{.pkg impute} is not available for testing; imputation tests will be skipped")
  } else{
    imputed_tomic <- impute_missing_values(tomic_with_missing_values)
    expect_s3_class(imputed_tomic, "triple_omic")
  }

})

test_that("Sample mahalanobis distances are calculated", {

  pc_distances <- calculate_sample_mahalanobis_distances(brauer_2008_tidy)
  expect_snapshot(pc_distances)

})
