library(dplyr)

test_that("Updating triple retains cohesiveness", {
  broken_brauer <- romic::brauer_2008_triple
  broken_brauer$features <- broken_brauer$features %>% mutate(foo = "bar")
  expect_error(check_tomic(broken_brauer), "permutation")

  broken_brauer <- romic::brauer_2008_triple
  broken_brauer$features$name <- factor(broken_brauer$features$name)
  expect_error(check_tomic(broken_brauer), "classes")
})
