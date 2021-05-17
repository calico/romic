#' \code{romic} package
#'
#' @docType package
#' @name romic
#'
#' @description 'romic' represents high-dimensional data as tables of features,
#'   samples and measurements, and a design list for tracking the meaning of
#'   individual variables. Using this format, filtering, normalization, and
#'   other transformations of a dataset can be carried out in a flexible
#'   manner. 'romic' takes advantage of these transformations to create
#'   interactive shiny apps for exploratory data analysis such as an
#'   interactive heatmap.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @import ggplot2
#' @import shiny
utils::globalVariables(c(
  ".",
  "n",
  "variable",
  "attribute",
  "attribute_value",
  "n_entries",
  "entry_number",
  "x",
  "x_var",
  "y",
  "y_var",
  "type",
  "feature_pk",
  "feature_label",
  "ordered_featureId",
  "sample_pk",
  "sample_label",
  "ordered_sampleId",
  "orderedId",
  "valid_tables",
  "NAME",
  "name",
  "systematic_name",
  "number",
  "GID",
  "YORF",
  "GWEIGHT",
  "G0.05",
  "U0.3",
  "iris",
  "Sepal.Length",
  "Sepal.Width"
))

