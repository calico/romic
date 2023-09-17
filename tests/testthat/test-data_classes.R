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

  tidy_omic <- create_tidy_omic(
    three_col_df,
    feature_pk = "features",
    sample_pk = "samples",
    verbose = FALSE
  )

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

  tidy_with_pcs <- add_pcs(tidy)
  expect_true(sum(stringr::str_detect(colnames(tidy_with_pcs$data), "^PC")) > 1)
})

test_that("Numeric primary keys are preserved when converting from a tidy to a triple", {

  triple_from_tidy <- tomic_to(simple_tidy, "triple_omic")
  triple_from_tidy_check_status <- check_tomic(triple_from_tidy, fast_check = FALSE)
  expect_equal(triple_from_tidy_check_status, 0)

  tidy_with_pcs <- add_pcs(simple_tidy)
  expect_true(sum(stringr::str_detect(colnames(tidy_with_pcs$data), "^PC")) > 1)
})

