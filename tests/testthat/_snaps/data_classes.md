# Test check_tidy_omic edge cases

    Code
      check_tidy_omic(double_data_tidy, fast_check = FALSE)
    Error <simpleError>
      100 measurements were present multiple times with
           the same feature and sample primary keys
      
           For example:
      
           feature = 1 ; sample = 1
      feature = 1 ; sample = 2
      feature = 1 ; sample = 3
      feature = 1 ; sample = 4
      feature = 1 ; sample = 5
      feature = 1 ; sample = 6
      feature = 1 ; sample = 7
      feature = 1 ; sample = 8
      feature = 1 ; sample = 9
      feature = 1 ; sample = 10

---

    Code
      create_tidy_omic(degenerate_attributes %>% select(-degen_sample_var),
      feature_pk = "features", sample_pk = "samples", feature_var = "degen_feature_var",
      verbose = FALSE)
    Error <simpleError>
      "degen_feature_var" was duplicated for 10 features
      this variable should not be a feature attribute. 

---

    Code
      create_tidy_omic(degenerate_attributes %>% select(-degen_feature_var),
      feature_pk = "features", sample_pk = "samples", sample_var = "degen_sample_var",
      verbose = FALSE)
    Error <simpleError>
      "degen_sample_var" was duplicated for 10 features
      this variable should not be a feature attribute. 

# Factor primary keys are preserved when converting from a tidy to a triple

    Code
      create_tidy_omic(three_col_df_fct, feature_pk = "features", sample_pk = "samples",
        sample_vars = "measurement", feature_vars = "measurement", verbose = FALSE)
    Error <rlang_error>
      measurement were assigned to multiple classes of variables each variable should only belong to one class

# Test that get_tomic_table() can retrieve various tables

    Code
      infer_tomic_table_type(simple_tidy, samples_df %>% rename(fake_samples = samples))
    Error <simpleError>
      based on the "tomic" primary keys, tomic_table doesn't appear to
             be features, samples or measurements

