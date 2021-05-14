test_that("Removing missing values works", {

  # drop_features
  expect_equal(
    nrow(remove_missing_values(
      brauer_2008_triple,
      missing_val_method = "drop_features"
    )$features),
    460
  )

  # drop_samples with no missing samples should behave like drop_features
  expect_equal(
    nrow(remove_missing_values(
      brauer_2008_triple,
      missing_val_method = "drop_samples"
    )$features),
    460
  )

  brauer_missing_samples <- brauer_2008_triple
  brauer_missing_samples$measurements$expression[
    brauer_missing_samples$measurements$sample %in% c("G0.05", "G0.1")
  ] <- NA
  filtered_brauer <- remove_missing_values(
    brauer_missing_samples,
    missing_val_method = "drop_samples"
  )

  expect_equal(nrow(filtered_brauer$samples), 34)
  expect_equal(nrow(filtered_brauer$measurements), 15674)
})
