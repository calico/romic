#' Filter T* Omics
#'
#' Filter a tidy or triple omic to entries of interest.
#'
#' @inheritParams tomic_to
#' @param filter_type
#' \describe{
#'   \item{category}{filter filter_variable to categories specified in
#'     filter_value}
#'   \item{range}{filter filter_variable to using the range (i.e., lower and
#'     upper limit) provided in filter_value}
#'   \item{quo}{a quosure as a \code{filter_value} to a table of interest}
#' }
#' @param filter_table table where the filter should be applied
#' @param filter_variable variable to apply the filter to
#' @param filter_value values to filter based on
#' @param invert If FALSE (default) entities will be retained; if TRUE, they
#'  will be removed.
#'
#' @returns A \code{tomic} object where a subset of features, samples or
#'   measurmenets have been filtered.
#'
#' @examples
#'
#' filter_tomic(
#'   brauer_2008_triple,
#'   filter_type = "category",
#'   filter_table = "features",
#'   filter_variable = "BP",
#'   filter_value = c("biological process unknown", "vacuolar acidification")
#' )
#'
#' filter_tomic(
#'   brauer_2008_triple,
#'   filter_type = "category",
#'   filter_table = "samples",
#'   filter_variable = "DR",
#'   filter_value = 0.05
#' )
#'
#' filter_tomic(
#'   brauer_2008_tidy,
#'   filter_type = "range",
#'   filter_table = "samples",
#'   filter_variable = "DR",
#'   filter_value = c(0, 0.2)
#' )
#'
#' filter_tomic(
#'   brauer_2008_triple,
#'   filter_type = "quo",
#'   filter_table = "features",
#'   filter_value = rlang::quo(BP == "biological process unknown")
#' )
#' @export
filter_tomic <- function(
  tomic,
  filter_type,
  filter_table,
  filter_value,
  filter_variable = NULL,
  invert = FALSE
  ) {

  checkmate::assertClass(tomic, "tomic")
  checkmate::assertString(filter_type)
  checkmate::assertChoice(
    filter_table,
    c("features", "samples", "measurements")
  )
  checkmate::assertLogical(invert, len = 1)

  # convert to triple_omic
  triple_omic <- tomic_to(tomic, "triple_omic")

  VALID_FILTER_TYPES <-  c("category", "range", "quo")
  if (filter_type %in% c("category", "range")) {
    checkmate::assertString(filter_variable)

    valid_variables <- colnames(triple_omic[[filter_table]])
    if (!(filter_variable %in% valid_variables)) {
      cli::cli_abort(
      "{.field {filter_variable}} is is not a valid value for {.arg filter_type},
      valid values are all variables within the {filter_table} table:
      {.field {valid_variables}}"
      )
    }

    filter_var_type <- triple_omic$design[[filter_table]] %>%
      dplyr::filter(variable == filter_variable)

    filter_var_type <- filter_var_type$type[1]
  } else if (filter_type == "quo") {
    if (!("NULL" %in% class(filter_variable))) {
      cli::cli_alert_warning(
        "{.arg filter_variable} was provided when {.arg filter_type} is {.field quo}
        only a filter_value should be passed. filter_variable will be ignored"
      )
    }
  } else {
    cli::cli_abort("{filter_type} is not a valid {.arg filter_type}. Valid types are {.field {VALID_FILTER_TYPES}}")
  }

  if (filter_type == "category") {
    checkmate::assertVector(filter_value)

    updated_filtered_table <- triple_omic[[filter_table]] %>%
      dplyr::filter(
        !!rlang::sym(filter_variable) %in% !!rlang::quo(filter_value)
      )
  } else if (filter_type == "range") {
    stopifnot(
      any(c("integer", "numeric") %in% class(filter_value)),
      length(filter_value) == 2,
      filter_value[2] >= filter_value[1]
    )

    if (filter_var_type == "character") {
      stop(
        filter_variable, " is categorical but a numerical filter was provided"
      )
    }

    updated_filtered_table <- triple_omic[[filter_table]] %>%
      dplyr::filter(
        !!rlang::sym(filter_variable) >= !!rlang::quo(filter_value[1]),
        !!rlang::sym(filter_variable) <= !!rlang::quo(filter_value[2])
      )
  } else if (filter_type == "quo") {
    checkmate::assertClass(filter_value, "quosure")

    updated_filtered_table <- triple_omic[[filter_table]] %>%
      dplyr::filter(!!filter_value)
  } else {
    stop("Unexpected behavior")
  }

  # invert filter if invert is TRUE
  if (invert) {
    join_keys <- triple_omic$design[[filter_table]] %>%
      dplyr::filter(type %in% c("feature_primary_key", "sample_primary_key")) %>%
      dplyr::pull(variable)

    updated_filtered_table <- dplyr::anti_join(
      triple_omic[[filter_table]],
      updated_filtered_table,
      by = join_keys
    )
  }

  triple_omic[[filter_table]] <- updated_filtered_table

  # clear out data impacted by filters
  triple_omic <- reconcile_triple_omic(triple_omic)

  # convert back to initial class
  return(tomic_to(triple_omic, class(tomic)[1]))
}

#' Reconcile Triple Omic
#'
#' If some samples, feature or measurements have been dropped; update other
#'   tables.
#'
#' @inheritParams check_triple_omic
#'
#' @return a triple_omic object
reconcile_triple_omic <- function(triple_omic) {
  feature_pk <- triple_omic$design$feature_pk
  sample_pk <- triple_omic$design$sample_pk

  triple_omic$measurements <- triple_omic$measurements %>%
    dplyr::semi_join(triple_omic$samples, by = sample_pk) %>%
    dplyr::semi_join(triple_omic$features, by = feature_pk)

  triple_omic$features <- triple_omic$features %>%
    dplyr::semi_join(triple_omic$measurements, by = feature_pk)

  triple_omic$samples <- triple_omic$samples %>%
    dplyr::semi_join(triple_omic$measurements, by = sample_pk)

  return(triple_omic)
}
