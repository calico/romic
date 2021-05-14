library(dplyr)

test_that("triple <-> tidy", {
  tidy <- brauer_2008_triple %>%
    triple_to_tidy()

  expect_equal(
    tidy$data %>%
      dplyr::arrange(name, sample),
    brauer_2008_tidy$data %>%
      dplyr::arrange(name, sample)
  )

  expect_equal(
    tidy$design,
    brauer_2008_tidy$design
  )

  triple <- brauer_2008_tidy %>%
    tidy_to_triple()

  expect_equal(
    triple,
    brauer_2008_triple
  )
})
