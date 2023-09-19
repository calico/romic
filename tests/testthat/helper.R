# setup tidy omics

suppressPackageStartupMessages(library(dplyr))

three_col_df <- tidyr:::expand_grid(
  features = 1:10,
  samples = 1:10
) %>%
  dplyr::mutate(
    measurement = 1
  )

simple_tidy <- create_tidy_omic(
  three_col_df,
  feature_pk = "features",
  sample_pk = "samples",
  verbose = FALSE
  )

three_col_df_fct <- tidyr:::expand_grid(
  features = letters,
  samples = LETTERS
) %>%
  dplyr::mutate(
    features = factor(features, levels = letters),
    samples = factor(samples, levels = LETTERS),
    measurement = 1
  )

# setup triple omics

triple_setup <- list(
  measurement_df = tidyr::expand_grid(
    feature_id = 1:10,
    sample_id = LETTERS[1:5]
  ) %>%
    dplyr::mutate(value = rnorm(dplyr::n())),
  feature_df = tibble::tibble(
    feature_id = 1:10,
    feature_group = rep(c("a", "b"), each = 5)
  ),
  samples_df = tibble::tibble(
    sample_id = LETTERS[1:5],
    sample_group = c("a", "a", "b", "b", "b")
  )
)

simple_triple <- create_triple_omic(
  triple_setup$measurement_df,
  triple_setup$feature_df,
  triple_setup$samples_df,
  "feature_id",
  "sample_id"
)
