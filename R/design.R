#' Get Design Table
#'
#' Get a tabular summary of all variables.
#'
#' @inheritParams tomic_to
#'
#' @returns a tibble reflecting the \code{tomic} object's design.
#'
#' @examples
#' get_design_tbl(brauer_2008_triple)
#' @export
get_design_tbl <- function(tomic) {
  tomic$design[c("features", "samples", "measurements")] %>%
    {
      purrr::map2(unname(.), names(.), function(x, y) {
        x %>%
          dplyr::mutate(table = y)
      })
    } %>%
    dplyr::bind_rows()
}

check_design <- function(tomic) {
  checkmate::assertClass(tomic, "tomic")
  stopifnot("design" %in% names(tomic))
  stopifnot(all(
    sort(names(tomic$design)) ==
      c("feature_pk", "features", "measurements", "sample_pk", "samples")
  ))

  checkmate::assertString(tomic$design$feature_pk)
  checkmate::assertDataFrame(tomic$design$features)
  checkmate::assertDataFrame(tomic$design$measurements)
  checkmate::assertString(tomic$design$sample_pk)
  checkmate::assertDataFrame(tomic$design$samples)
}
