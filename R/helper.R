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
