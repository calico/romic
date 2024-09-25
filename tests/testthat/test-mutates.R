test_that("Test that update_tomic can update a table", {
  expect_equal(
    update_tomic(
      simple_tidy,
      get_tomic_table(simple_tidy, "samples") %>%
        dplyr::mutate(new_entry = LETTERS[1:dplyr::n()])
    )$design$samples %>%
      dim(),
    c(2,2)
  )
})


test_that("center_tomic centers selected variables in a tomic", {

  centered_tomic <- center_tomic(brauer_2008_tidy)

  centered_tomic$data %>%
    dplyr::summarize(mean_val = mean(expression, na.rm = TRUE), .by = "name") %>%
    {abs(.$mean_val)} %>%
    {all(. < 1e-15)} %>%
    expect_true()

  centered_tomic <- center_tomic(brauer_2008_tidy, measurement_vars = "expression")

  centered_tomic$data %>%
    dplyr::summarize(mean_val = mean(expression, na.rm = TRUE), .by = "name") %>%
    {abs(.$mean_val)} %>%
    {all(. < 1e-15)} %>%
    expect_true()

  expect_snapshot(center_tomic(brauer_2008_tidy, measurement_vars = "foo"), error = TRUE)

})


test_that("Sort tables and update primary keys with new sort", {

  sort_triple_hclust(brauer_2008_triple, "samples", "expression") %>%
    expect_snapshot()

  sort_triple_hclust(brauer_2008_triple, "samples", "bar") %>%
    expect_snapshot(error = TRUE)
})

test_that("Sort tomic applies a sort to features and/or samples", {

  expect_equal(tomic_sort_status(brauer_2008_triple), "unsorted")

  sorted_samples <- sort_tomic(
    brauer_2008_triple,
    sort_type = "arrange",
    sort_table = "samples",
    sort_variables = c("nutrient", "DR")
  )

  expect_equal(tomic_sort_status(sorted_samples), "sorted_samples, unsorted features")

  sorted_features <- sort_tomic(
    brauer_2008_tidy,
    sort_type = "hclust",
    sort_table = "features",
    value_var = "expression"
    )

  expect_equal(tomic_sort_status(sorted_features), "sorted features, unsorted samples")

  sorted_features %>%
    sort_tomic(
      sort_type = "arrange",
      sort_table = "samples",
      sort_variables = c("nutrient", "DR")
    ) %>%
    tomic_sort_status() %>%
    expect_equal("fully sorted")
})

test_that("Factor levels can be updated using a list of factor orders", {

  NUTRIENT_ORDER <- c("G", "N", "P", "S", "L", "U")

  reordered_tidy <- update_sample_factors(
    brauer_2008_tidy,
    list(nutrient = NUTRIENT_ORDER)
    )

  expect_equal(levels(reordered_tidy$data$nutrient), NUTRIENT_ORDER)

  # test NA handling
  brauer_samples <- get_tomic_table(brauer_2008_tidy, "samples")
  brauer_samples$nutrient[1:2] <- NA

  brauer_2008_tidy_w_NAs <- update_tomic(
    brauer_2008_tidy,
    brauer_samples
  )

  expect_snapshot(
    brauer_w_mystery_nutrients <- update_sample_factors(
      brauer_2008_tidy_w_NAs,
      list(nutrient = NUTRIENT_ORDER)
    )
  )

  expect_equal(
    levels(brauer_w_mystery_nutrients$data$nutrient),
    c(NUTRIENT_ORDER, "unspecified")
  )

  # W/ unexpected levels

  CONFUSED_NUTRIENT_ORDER <- c("C", "N", "P", "S", "L", "U")

  expect_snapshot(
    reordered_tidy <- update_sample_factors(
      brauer_2008_tidy,
      list(nutrient = CONFUSED_NUTRIENT_ORDER)
    ))

  expect_equal(levels(reordered_tidy$data$nutrient), c(CONFUSED_NUTRIENT_ORDER, "G"))

  # invalid factor specifications

  expect_snapshot(
    update_sample_factors(brauer_2008_tidy, list(nutrient = 1:5)),
    error = TRUE
  )

  expect_snapshot(
    update_sample_factors(brauer_2008_tidy, list(nutrient = c("G", "G", "N", "L"))),
    error = TRUE
  )

  expect_snapshot(
    update_sample_factors(brauer_2008_tidy, list(DR = seq(0.05, 0.3, by = 0.05))),
    error = TRUE
  )

})

test_that("Update tidy omics with new added variables", {

  tidy_omic <- brauer_2008_tidy
  updated_tidy_data <- tidy_omic$data %>%
    dplyr::mutate(new_sample_var = "foo") %>%
    dplyr::select(-DR)

  new_variable_tables <- c("new_sample_var" = "samples")

  tidy_w_updated_samples <- update_tidy_omic(tidy_omic, updated_tidy_data, new_variable_tables)
  expect_equal(
    tidy_w_updated_samples$design$samples$variable,
    c('sample', "nutrient", "new_sample_var")
  )

  expect_snapshot(
    update_tidy_omic(tidy_omic, updated_tidy_data, c()),
    error = TRUE
    )

})
