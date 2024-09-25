cli::test_that_cli("Try all of the filters", {

  filter_tomic(
    brauer_2008_triple,
    filter_type = "category",
    filter_table = "features",
    filter_variable = "BP",
    filter_value = c("biological process unknown", "vacuolar acidification")
  )$features %>%
    distinct(BP) %>%
    expect_snapshot()

  filter_tomic(
    brauer_2008_triple,
    filter_type = "category",
    filter_table = "samples",
    filter_variable = "DR",
    filter_value = 0.05
  )$samples %>%
    nrow() %>%
    expect_equal(6)

  filter_tomic(
    brauer_2008_tidy,
    filter_type = "range",
    filter_table = "samples",
    filter_variable = "DR",
    filter_value = c(0, 0.1)
    ) %>%
    get_tomic_table("samples") %>%
    nrow() %>%
    expect_equal(12)

  filter_tomic(
    brauer_2008_triple,
    filter_type = "quo",
    filter_table = "features",
    filter_value = rlang::quo(BP == "biological process unknown")
    )$features %>%
    distinct(BP) %>%
    nrow() %>%
    expect_equal(1)

  # edge cases
  expect_snapshot(
    filter_tomic(
      brauer_2008_triple,
      filter_type = "category",
      filter_table = "features",
      filter_variable = "bar",
      filter_value = "biological process unknown"
    ),
    error = TRUE
    )

  expect_snapshot(
    filter_tomic(
      brauer_2008_triple,
      filter_type = "quo",
      filter_table = "features",
      filter_variable = "bar",
      filter_value = rlang::quo(BP == "biological process unknown")
    )
  )

})

test_that("Validate that filtering works with invert = TRUE", {

  retained_samples <- filter_tomic(
    brauer_2008_triple,
    filter_type = "category",
    filter_table = "samples",
    filter_variable = "nutrient",
    filter_value = c("U", "L"),
    invert = TRUE
  )$samples

  expect_equal(nrow(retained_samples), 24)
  expect_equal(unique(retained_samples$nutrient), c("G", "N", "P", "S"))
})
