# Try all of the filters

    Code
      .
    Output
      # A tibble: 1 x 1
        BP                        
        <chr>                     
      1 biological process unknown

---

    Code
      filter_tomic(brauer_2008_triple, filter_type = "category", filter_table = "features",
        filter_variable = "bar", filter_value = "biological process unknown")
    Condition
      Error in `filter_tomic()`:
      ! bar is not a valid value for "filter_type",
              valid values are all variables within the "features" table: name, BP, MF, systematic_name

