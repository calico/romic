## romic 1.1.1

### New Features

- `plot_bivariate()` supports setting size, alpha, and shape. #48
- `add_pc_loadings()` changed to `add_pcs()` for accuracy. Added fraction of variability explained by PCs to `add_pcs()`. #32
- `plot_univariate()` and `plot_bivariate()` supporting providing a partial string match to a variable name. This helps to generate consisitent plots even if names might change (like PC1 (10%)). #44
- tidy_omics and triple_omics can now include a list with unstructured data. This puts the convention more in line with an H5AD file. #43. This is currently only used to return the fraction of variance explained by each PC as part of `add_pcs()`. #42
- Updated synteax for 

### Minor Improvements and Fixes

- `plot_heatmap()` rowlabels are suppressed when there are too many features rather than setting size to zero. #45
- `plot_heatmap()` supports ordered objects. #37
- ` coerce_to_classes()` added support for glue objects. #35
- Added additional checks to retain the class of primary keys and tests of coversions. #33
