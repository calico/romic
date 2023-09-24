## romic 1.1.3

### New Features

- `get_tomic_table()` makes it easy to pull individual tables out of tomic objects. #55

### Minor Improvements and Fixes

- Bug fix in `add_pcs()` #52
- added option to not display percent variance explained on PCs to `add_pcs()` #53
- Added passing of axis and colorbar labels in `plot_heatmap()` #60
- Updated romic-package alias so package documentation properly displays with `?romic` #63
- Renamed tests and greatly expanded test coverage. #64

## romic 1.1.1

### New Features

- `plot_bivariate()` supports setting size, alpha, and shape. #48
- `add_pc_loadings()` changed to `add_pcs()` for accuracy. Added fraction of variability explained by PCs to `add_pcs()`. #32
- `plot_univariate()` and `plot_bivariate()` supporting providing a partial string match to a variable name. This helps to generate consisitent plots even if names might change (like PC1 (10%)). #44
- tidy_omics and triple_omics can now include a list with unstructured data. This puts the convention more in line with an H5AD file. #43. This is currently only used to return the fraction of variance explained by each PC as part of `add_pcs()`. #42

### Minor Improvements and Fixes

- `plot_heatmap()` rowlabels are suppressed when there are too many features rather than setting size to zero. #45
- `plot_heatmap()` supports ordered objects. #37
- ` coerce_to_classes()` added support for glue objects. #35
- Added additional checks to retain the class of primary keys and tests of coversions. #33
