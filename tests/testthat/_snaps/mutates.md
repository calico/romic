# center_tomic centers selected variables in a tomic

    Code
      center_tomic(brauer_2008_tidy, measurement_vars = "foo")
    Error <simpleError>
      foo are not valid numeric or integer measurement variables.
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
    Error <simpleError>
      bar is not present in measurements, valid value_vars include:
      expression

