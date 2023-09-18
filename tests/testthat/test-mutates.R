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

