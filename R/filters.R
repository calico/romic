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
#'   \item{apply}{a quosure as a \code{filter_value} to a table of interest}
#' }
#' @param filter_table table where the filter should be applied
#' @param filter_variable variable to apply the filter to
#' @param filter_value values to filter based on
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
filter_tomic <- function(tomic,
                         filter_type,
                         filter_table,
                         filter_value,
                         filter_variable = NULL) {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertChoice(filter_type, c("category", "range", "quo"))
  checkmate::assertChoice(
    filter_table,
    c("features", "samples", "measurements")
  )

  # convert to triple_omic
  triple_omic <- tomic_to(tomic, "triple_omic")

  if (filter_type %in% c("category", "range")) {
    checkmate::assertString(filter_variable)

    valid_variables <- colnames(triple_omic[[filter_table]])
    if (!(filter_variable %in% valid_variables)) {
      stop(
        filter_variable,
        " is not a valid value for \"filter_type\",
        valid values are all variables within the \"",
        filter_table,
        "\" table: ",
        paste(valid_variables, collapse = ", ")
      )
    }

    filter_var_type <- triple_omic$design[[filter_table]] %>%
      dplyr::filter(variable == filter_variable)

    filter_var_type <- filter_var_type$type[1]
  } else if (filter_type == "quo") {
    if (!("NULL" %in% class(filter_variable))) {
      warning(
        "filter_variable was provided when filter_type is quo
        only a filter_value should be passed. filter_variable will be ignored"
      )
    }
  } else {
    stop("invalid filter type")
  }

  if (filter_type == "category") {
    checkmate::assertVector(filter_value)

    triple_omic[[filter_table]] <- triple_omic[[filter_table]] %>%
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

    triple_omic[[filter_table]] <- triple_omic[[filter_table]] %>%
      dplyr::filter(
        !!rlang::sym(filter_variable) >= !!rlang::quo(filter_value[1]),
        !!rlang::sym(filter_variable) <= !!rlang::quo(filter_value[2])
      )
  } else if (filter_type == "quo") {
    checkmate::assertClass(filter_value, "quosure")

    triple_omic[[filter_table]] <- triple_omic[[filter_table]] %>%
      dplyr::filter(!!filter_value)
  } else {
    stop("invalid filter_type")
  }

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
