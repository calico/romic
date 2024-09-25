# center_tomic centers selected variables in a tomic

    Code
      center_tomic(brauer_2008_tidy, measurement_vars = "foo")
    Condition
      Error in `center_tomic()`:
      ! foo are not valid numeric or integer measurement variables.
              Valid measurements are: expression

# Sort tables and update primary keys with new sort

    Code
      .
    Output
      # A tibble: 36 x 4
         sample nutrient    DR order
         <chr>  <chr>    <dbl> <int>
       1 P0.05  P         0.05     1
       2 P0.1   P         0.1      2
       3 P0.15  P         0.15     3
       4 P0.2   P         0.2      4
       5 P0.25  P         0.25     5
       6 P0.3   P         0.3      6
       7 G0.25  G         0.25     7
       8 G0.3   G         0.3      8
       9 U0.25  U         0.25     9
      10 U0.3   U         0.3     10
      # i 26 more rows

---

    Code
      .
    Condition
      Error in `sort_triple_hclust()`:
      ! bar is not present in measurements, valid value_vars include:
      expression

# Factor levels can be updated using a list of factor orders

    Code
      brauer_w_mystery_nutrients <- update_sample_factors(brauer_2008_tidy_w_NAs,
        list(nutrient = NUTRIENT_ORDER))
    Message
      ! NA was present in the sample metadata's nutrient field but did not have a corresponding factor level in the `factor_levels` list. They will be added to the end of the specified factor levels
      ! The nutrient field in the sample metadata contains 2 NA values. These entries will be replaced with an "unspecified" level.

---

    Code
      reordered_tidy <- update_sample_factors(brauer_2008_tidy, list(nutrient = CONFUSED_NUTRIENT_ORDER))
    Message
      ! "G" was present in the sample metadata's nutrient field but did not have a corresponding factor level in the `factor_levels` list. They will be added to the end of the specified factor levels
      ! "C" was present in `factor_levels` for nutrient but did not have a corresponding entry in the sample metadata.

---

    Code
      update_sample_factors(brauer_2008_tidy, list(nutrient = 1:5))
    Condition
      Error in `set_factor_levels()`:
      ! The factor levels for nutrient were "integer". This should be a character vector.

---

    Code
      update_sample_factors(brauer_2008_tidy, list(nutrient = c("G", "G", "N", "L")))
    Condition
      Error in `set_factor_levels()`:
      ! 1 factor levels was duplicated in the `factor_levels` specification for "nutrient": G

---

    Code
      update_sample_factors(brauer_2008_tidy, list(DR = seq(0.05, 0.3, by = 0.05)))
    Condition
      Error in `set_factor_levels()`:
      ! The factor levels for DR were "numeric". This should be a character vector.

# Update tidy omics with new added variables

    Code
      update_tidy_omic(tidy_omic, updated_tidy_data, c())
    Condition
      Error in `update_tidy_omic()`:
      ! updated_tidy_data contains 1
      - new fields: new_sample_var.
      - Add these to "new_variable_tables" so that romic know how to
      - use them.

