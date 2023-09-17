# Factor primary keys are preserved when converting from a tidy to a triple

    Code
      create_tidy_omic(three_col_df_fct, feature_pk = "features", sample_pk = "samples",
        sample_vars = "measurement", feature_vars = "measurement", verbose = FALSE)
    Error <rlang_error>
      measurement were assigned to multiple classes of variables each variable should only belong to one class

