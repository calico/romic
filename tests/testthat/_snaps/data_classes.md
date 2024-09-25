# Test check_tidy_omic edge cases

    Code
      check_tidy_omic(double_data_tidy, fast_check = FALSE)
    Condition
      Error in `check_tidy_omic()`:
      ! 100 measurements were present multiple times with
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
    Condition
      Error in `check_tidy_omic()`:
      ! "degen_feature_var" was duplicated for 10 features
      this variable should not be a feature attribute. 

---

    Code
      create_tidy_omic(degenerate_attributes %>% select(-degen_feature_var),
      feature_pk = "features", sample_pk = "samples", sample_var = "degen_sample_var",
      verbose = FALSE)
    Condition
      Error in `check_tidy_omic()`:
      ! "degen_sample_var" was duplicated for 10 features
      this variable should not be a feature attribute. 

# Factor primary keys are preserved when converting from a tidy to a triple

    Code
      create_tidy_omic(three_col_df_fct, feature_pk = "features", sample_pk = "samples",
        sample_vars = "measurement", feature_vars = "measurement", verbose = FALSE)
    Condition
      Error in `create_tidy_omic()`:
      ! measurement were assigned to multiple classes of variables each variable should only belong to one class

# Catch corner cases when reading wide data

    Code
      convert_wide_to_tidy_omic(wide_df_nonunique_feature_id, feature_pk = "name")
    Condition
      Warning in `convert_wide_to_tidy_omic()`:
      4 rows did not contain a unique name; adding extra variables 'unique_name' & 'entry_number' to distinguish them
      Warning:
      `mutate_()` was deprecated in dplyr 0.7.0.
      i Please use `mutate()` instead.
      i See vignette('programming') for more help
    Message
      1 measurement variables were defined as the
      left overs from the specified feature and sample varaibles:
      abundance
    Output
      $data
      # A tibble: 19,500 x 5
         name    entry_number unique_name sample abundance                            
         <chr>          <int> <chr>       <chr>  <chr>                                
       1 YOL029C            1 YOL029C-1   BP     biological process unknown           
       2 YOL029C            2 YOL029C-2   BP     cytokinesis, completion of separation
       3 YOL029C            3 YOL029C-3   BP     biological process unknown           
       4 YOL029C            4 YOL029C-4   BP     cell wall organization and biogenesis
       5 YOL029C            5 YOL029C-5   BP     cell wall organization and biogenesi~
       6 FKH1               1 FKH1        BP     pseudohyphal growth*                 
       7 HOC1               1 HOC1        BP     cell wall mannoprotein biosynthesis* 
       8 CSN12              1 CSN12       BP     adaptation to pheromone during conju~
       9 YAL046C            1 YAL046C     BP     biological process unknown           
      10 SLG1               1 SLG1        BP     cell wall organization and biogenesi~
      # i 19,490 more rows
      
      $design
      $design$features
      # A tibble: 3 x 2
        variable     type               
        <chr>        <chr>              
      1 unique_name  feature_primary_key
      2 name         character          
      3 entry_number integer            
      
      $design$samples
      # A tibble: 1 x 2
        variable type              
        <chr>    <chr>             
      1 sample   sample_primary_key
      
      $design$measurements
      # A tibble: 3 x 2
        variable    type               
        <chr>       <chr>              
      1 unique_name feature_primary_key
      2 sample      sample_primary_key 
      3 abundance   character          
      
      $design$feature_pk
      [1] "unique_name"
      
      $design$sample_pk
      [1] "sample"
      
      
      attr(,"class")
      [1] "tidy_omic" "tomic"     "general"  

# Find primary or foreign keys in tomic table

    Code
      get_identifying_keys(brauer_2008_triple, "foo")
    Condition
      Error in `get_identifying_keys()`:
      ! Assertion on 'table' failed: Must be element of set {'features','samples','measurements'}, but is 'foo'.

# Test that get_tomic_table() can retrieve various tables

    Code
      infer_tomic_table_type(simple_tidy, samples_df %>% rename(fake_samples = samples))
    Condition
      Error in `infer_tomic_table_type()`:
      ! based on the "tomic" primary keys, tomic_table doesn't appear to
             be features, samples or measurements

# Catch corner cases when interconverting tomics

    Code
      check_tomic(mtcars)
    Condition
      Error in `check_tomic()`:
      ! Assertion on 'tomic' failed: Must inherit from class 'tomic', but has class 'data.frame'.

---

    Code
      tomic_to(romic::brauer_2008_tidy, "foo")
    Condition
      Error in `tomic_to()`:
      ! Assertion on 'to_class' failed: Must be element of set {'tidy_omic','triple_omic'}, but is 'foo'.

