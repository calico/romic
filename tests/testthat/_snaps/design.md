# extract design as a table

    Code
      .
    Output
      # A tibble: 10 x 3
         variable        type                table       
         <chr>           <chr>               <chr>       
       1 name            feature_primary_key features    
       2 systematic_name character           features    
       3 BP              character           features    
       4 MF              character           features    
       5 sample          sample_primary_key  samples     
       6 nutrient        character           samples     
       7 DR              numeric             samples     
       8 name            feature_primary_key measurements
       9 sample          sample_primary_key  measurements
      10 expression      numeric             measurements

# Catch malformed design objects

    Code
      check_design(malformed_design)
    Condition
      Error in `check_design()`:
      ! The following unexpected attributes were found in the design: foo

---

    Code
      check_design(malformed_design)
    Condition
      Error in `check_design()`:
      ! The following attributes were missing in the design: feature_pk

