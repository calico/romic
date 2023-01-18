library(dplyr)

test_that("Factor primary keys are preserved when converting from a tidy to a triple", {
  tidy <- tidyr:::expand_grid(
    features = letters,
    samples = LETTERS
  ) %>%
    dplyr::mutate(
      features = factor(features, levels = letters),
      samples = factor(samples, levels = LETTERS),
      measurement = 1
    ) %>%
    create_tidy_omic(feature_pk = "features", sample_pk = "samples")

  triple_from_tidy <- tomic_to(tidy, "triple_omic")
  triple_from_tidy_check_status <- romic::check_tomic(triple_from_tidy, fast_check = FALSE)
  expect_equal(triple_from_tidy_check_status, 0)

  tidy_with_pcs <- add_pcs(tidy)
  expect_true(sum(stringr::str_detect(colnames(tidy_with_pcs$data), "^PC")) > 1)
})

test_that("Numeric primary keys are preserved when converting from a tidy to a triple", {
  tidy <- tidyr:::expand_grid(
    features = 1:10,
    samples = 1:10
  ) %>%
    dplyr::mutate(
      measurement = 1
    ) %>%
    create_tidy_omic(feature_pk = "features", sample_pk = "samples")

  triple_from_tidy <- tomic_to(tidy, "triple_omic")
  triple_from_tidy_check_status <- check_tomic(triple_from_tidy, fast_check = FALSE)
  expect_equal(triple_from_tidy_check_status, 0)

  tidy_with_pcs <- add_pcs(tidy)
  expect_true(sum(stringr::str_detect(colnames(tidy_with_pcs$data), "^PC")) > 1)
})
