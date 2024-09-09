#' Get Design Table
#'
#' Get a tabular summary of all variables.
#'
#' @param tomic_or_design Either a \code{tomic} object or its embedded design list
#'
#' @returns a tibble reflecting the \code{tomic} object's design.
#'
#' @examples
#' get_design_tbl(brauer_2008_triple)
#' get_design_tbl(brauer_2008_triple$design)
#'
#' @export
get_design_tbl <- function(tomic_or_design) {

  if (inherits(tomic_or_design, "tomic")) {
    design <- tomic_or_design$design
  } else {
    check_design(tomic_or_design)
    design <- tomic_or_design
  }

  design[c("features", "samples", "measurements")] %>%
    {
      purrr::map2(unname(.), names(.), function(x, y) {
        x %>%
          dplyr::mutate(table = y)
      })
    } %>%
    dplyr::bind_rows()
}

#' Check Design
#'
#' Check that the design list embedded in `tomic` objects is properly
#' formatted.
#'
#' @param tomic_design a list with named attributes describing feature,
#'   sample, and measurement variables.
#'
#' @return 0, invisibly
#'
#' @examples
#' check_design(brauer_2008_triple$design)
#'
#' @export
check_design <- function(tomic_design) {

  checkmate::assertList(tomic_design)

  EXPECTED_ATTRIBUTES <- c("feature_pk", "features", "measurements", "sample_pk", "samples")
  extra_elements <- setdiff(names(tomic_design), EXPECTED_ATTRIBUTES)
  if (length(extra_elements) > 0) {
    cli::cli_abort(
      "The following unexpected attributes were found in the design: {.field {extra_elements}}"
    )
  }

  missing_elements <- setdiff(EXPECTED_ATTRIBUTES, names(tomic_design))
  if (length(missing_elements) > 0) {
    cli::cli_abort(
      "The following attributes were missing in the design: {.field {missing_elements}}"
    )
  }

  checkmate::assertString(tomic_design$feature_pk)
  checkmate::assertDataFrame(tomic_design$features)
  checkmate::assertDataFrame(tomic_design$measurements)
  checkmate::assertString(tomic_design$sample_pk)
  checkmate::assertDataFrame(tomic_design$samples)
}

check_design_in_tomic <- function(tomic) {

  checkmate::assertClass(tomic, "tomic")
  stopifnot("design" %in% names(tomic))
  check_design(tomic$design)

}

