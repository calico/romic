#' Update T* Omic
#'
#' Provide an updated features, samples or measurements table to a
#'   \code{tomic}.
#'
#' @inheritParams infer_tomic_table_type
#'
#' @returns
#'
#' A \code{tomic} object with updated features, samples or measurements.
#'
#' @examples
#'
#' library(dplyr)
#' updated_features <- brauer_2008_triple$features %>%
#'   dplyr::filter(BP == "biological process unknown") %>%
#'   dplyr::mutate(chromosome = purrr::map_int(systematic_name, function(x) {
#'     which(LETTERS == stringr::str_match(x, "Y([A-Z])")[2])
#'   }))
#'
#' update_tomic(brauer_2008_triple, updated_features)
#'
#' @export
update_tomic <- function(tomic, tomic_table) {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertDataFrame(tomic_table)

  # convert to triple_omic
  triple_omic <- tomic_to(tomic, "triple_omic")

  tomic_table_type <- infer_tomic_table_type(tomic, tomic_table)

  # update design
  new_design <- tomic$design
  new_design[[tomic_table_type]] <- tibble::tibble(
    variable = colnames(tomic_table),
    type = tomic_table %>% purrr::map_chr(~ class(.)[1])
  ) %>%
    dplyr::mutate(
      type = ifelse(variable == new_design$feature_pk, "feature_primary_key", type),
      type = ifelse(variable == new_design$sample_pk, "sample_primary_key", type)
    )

  triple_omic$design <- new_design

  # update appropriate data table
  triple_omic[[tomic_table_type]] <- tomic_table

  # reconcile triple omic in case rows have been filtered
  triple_omic <- reconcile_triple_omic(triple_omic)
  check_triple_omic(triple_omic, fast_check = FALSE)

  # convert back to input class
  return(tomic_to(triple_omic, class(tomic)[1]))
}

#' Center T* Omic
#'
#' Center each measurement by subtracting the mean.
#'
#' @inheritParams tomic_to
#' @param measurement_vars measurement variables to center
#'
#' @return A \code{tomic} object where one or more measurements have
#'   been centered on a feature-by-feature basis.
#'
#' @examples
#' center_tomic(brauer_2008_tidy)
#'
#' @export
center_tomic <- function(tomic, measurement_vars = "all") {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertCharacter(measurement_vars)

  # convert to triple_omic
  triple_omic <- tomic_to(tomic, "triple_omic")

  possible_measurements <- triple_omic$design$measurements %>%
    dplyr::filter(type %in% c("integer", "numeric")) %>%
    {
      .$variable
    }
  stopifnot(class(measurement_vars) == "character")

  if (measurement_vars[1] != "all") {
    excess_measurements <- setdiff(measurement_vars, possible_measurements)
    if (length(excess_measurements) != 0) {
      stop(
        paste(excess_measurements, collapse = ", "),
        " are not valid numeric or integer measurement variables.
        Valid measurements are: ",
        paste(possible_measurements, collapse = ", ")
      )
    }

    valid_measurements <- intersect(measurement_vars, possible_measurements)

    if (length(valid_measurements) == 0) {
      stop(
        "No valid numeric or integer measurement variables provided.
        Valid measurements are: ",
        paste(possible_measurements, collapse = ", ")
      )
    }
  } else {
    valid_measurements <- possible_measurements
  }

  measurement_pk <- triple_omic$design$measurements$variable[
    triple_omic$design$measurements$type == "feature_primary_key"
  ]

  triple_omic$measurements <- triple_omic$measurements %>%
    dplyr::group_by(!!rlang::sym(measurement_pk)) %>%
    dplyr::mutate(dplyr::across(c(!!!syms(valid_measurements)), center)) %>%
    dplyr::ungroup()

  # convert back to initial class
  return(tomic_to(triple_omic, class(tomic)[1]))
}

center <- function(x) {
  x - mean(x)
}

#' Update Tidy Omic
#'
#' Update a Tidy 'Omics data and schema to reflect newly added fields.
#'
#' @inheritParams check_tidy_omic
#' @param updated_tidy_data a tibble of data to use to update \code{tidy_omic}.
#' @param new_variable_tables a named character vector of newly added variables
#'   in \code{updated_tidy_data} (names) and the table {features, samples,
#'   measurements} they apply to (values).
#'
#' @returns a \code{tidy_omic} object with an updated schema and/or data.
#'
#' @examples
#'
#' library(dplyr)
#'
#' tidy_omic <- brauer_2008_tidy
#' updated_tidy_data <- tidy_omic$data %>%
#'   mutate(new_sample_var = "foo") %>%
#'   select(-DR)
#' new_variable_tables <- c("new_sample_var" = "samples")
#'
#' @export
update_tidy_omic <- function(
  tidy_omic,
  updated_tidy_data,
  new_variable_tables = c()) {

  checkmate::assertClass(tidy_omic, "tomic")
  checkmate::assertClass(tidy_omic, "tidy_omic")
  checkmate::assertDataFrame(updated_tidy_data)
  checkmate::assertNamed(new_variable_tables, type = "unique")
  purrr::walk(
    names(new_variable_tables),
    checkmate::assertChoice,
    colnames(updated_tidy_data)
  )
  purrr::walk(
    unname(new_variable_tables),
    checkmate::assertChoice,
    c("features", "samples", "measurements")
  )

  # check whether all new variables are defined in new_variable_tables
  current_fields <- get_design_tbl(tidy_omic)

  new_variables <- setdiff(colnames(updated_tidy_data), current_fields$variable)
  unclassified_new_variables <- setdiff(
    new_variables,
    names(new_variable_tables)
  )

  if (length(unclassified_new_variables) > 0) {
    stop(glue::glue(
      "updated_tidy_data contains {length(unclassified_new_variables)}
       - new fields: {paste(unclassified_new_variables, collapse = ', ')}.
       - Add these to \"new_variable_tables\" so that romic know how to
       - use them."
    ))
  }

  current_fields <- get_design_tbl(tidy_omic)

  updated_fields <- current_fields %>%
    # remove unused variables
    dplyr::filter(
      variable %in% colnames(updated_tidy_data),
      !(type %in% c("feature_primary_key", "sample_primary_key"))
    ) %>%
    dplyr::select(-type) %>%
    # add new variables
    dplyr::bind_rows(
      tibble::tibble(
        variable = names(new_variable_tables),
        table = unname(new_variable_tables)
      )
    ) %>%
    # update classes of all variables
    dplyr::mutate(
      type = purrr::map_chr(
        variable,
        function(x) {
          class(updated_tidy_data[[x]])[1]
        }
      )
    ) %>%
    # remove absent fields
    dplyr::filter(variable %in% colnames(updated_tidy_data))

  # require the primary keys stay as-is
  updated_fields <- dplyr::bind_rows(
    current_fields %>%
      dplyr::filter(type %in% c("feature_primary_key", "sample_primary_key")),
    updated_fields
  )

  excess_vars <- union(
    updated_fields$variable,
    colnames(updated_tidy_data)
  ) %>%
    setdiff(
      intersect(
        updated_fields$variable,
        colnames(updated_tidy_data)
      )
    )

  if (length(excess_vars) > 0) {
    stop(glue::glue(
      "{length(excess_vars)} were not matched updated_tidy_data and its
       - to-be-created design list: {paste(excess_vars, collapse = ', ')}"
    ))
  }

  tidy_omic$data <- updated_tidy_data
  tidy_omic$design$features <- updated_fields %>%
    dplyr::filter(table == "features")
  tidy_omic$design$samples <- updated_fields %>%
    dplyr::filter(table == "samples")
  tidy_omic$design$measurements <- updated_fields %>%
    dplyr::filter(table == "measurements")

  check_tidy_omic(tidy_omic, fast_check = FALSE)

  return(tidy_omic)
}

#' Sort Triple Hclust
#'
#' Sort a \code{triple_omic} object using hierarchical clustering
#'
#' @inheritParams check_triple_omic
#' @inheritParams sort_tomic
#'
#' @returns A \code{triple_omic} with clustered features or samples.
sort_triple_hclust <- function(triple_omic, sort_table, value_var) {
  stopifnot(
    any(c("character", "factor", "ordered") %in% class(value_var)),
    length(value_var) == 1
  )
  available_value_vars <- triple_omic$design$measurements$variable[
    triple_omic$design$measurements$type == "numeric"
  ]
  if (length(available_value_vars) == 0) {
    stop(
      "No numeric variables present in measurement
        hierarchical clustering not possible"
    )
  }

  if (!(value_var %in% available_value_vars)) {
    stop(glue::glue(
      "{value_var} is not present in measurements, valid value_vars include:
        {paste(available_value_vars, collapse = ", ")}"
    ))
  }

  tidy_omic <- triple_to_tidy(triple_omic)
  pk <- ifelse(
    sort_table == "features",
    triple_omic$design$feature_pk,
    triple_omic$design$sample_pk
  )

  cluster_dim <- dplyr::case_when(
    sort_table == "features" ~ "rows",
    sort_table == "samples" ~ "columns"
  )

  cluster_orders <- hclust_order(
    tidy_omic$data,
    tidy_omic$design$feature_pk,
    tidy_omic$design$sample_pk,
    value_var,
    cluster_dim = cluster_dim
  ) %>% {
    .[[cluster_dim]]
  }

  if (class(tidy_omic$data[[pk]]) != "factor") {
    # match classes if needed to facilitate joins
    class(cluster_orders) <- class(tidy_omic$data[[pk]])
  }

  # use the ordered clusters to sort the appropriate sort_table

  sorted_table <- (triple_omic[[sort_table]] %>%
    dplyr::left_join(tibble::tibble(!!rlang::sym(pk) := cluster_orders) %>%
      dplyr::mutate(order = 1:dplyr::n()),
    by = pk
    ) %>%
    dplyr::arrange(order))

  return(sorted_table)
}

#' Sort Triple Arrange
#'
#' Sort a \code{triple_omic} object based on the values of one or more
#'   variables.
#'
#' @inheritParams check_triple_omic
#' @inheritParams sort_tomic
#'
#' @returns A \code{triple_omic} with sorted features or samples.
sort_triple_arrange <- function(triple_omic, sort_table, sort_variables) {
  stopifnot(
    any(
      c("character", "factor", "ordered", "numeric") %in% class(sort_variables)
    ),
    length(sort_variables) > 0
  )

  available_sort_vars <- triple_omic$design[[sort_table]]$variable
  invalid_sort_vars <- setdiff(sort_variables, available_sort_vars)
  if (length(invalid_sort_vars) != 0) {
    stop(glue::glue(
      "{length(invalid_sort_vars)} sort variables were not found in {sort_table},
      the variable present are: {paste(available_sort_vars, collapse = ', ')}"
    ))
  }

  pk <- ifelse(
    sort_table == "features",
    triple_omic$design$feature_pk,
    triple_omic$design$sample_pk
  )

  sorted_table <- triple_omic[[sort_table]] %>%
    dplyr::arrange(!!!rlang::syms(sort_variables))

  return(sorted_table)
}

#' Sort Triple Omic
#'
#' Sort a dataset's features or samples
#'
#' \code{sort_tomic} supports the reordering of features or samples using
#'   either hierarchical clustering or based on the levels of other variables.
#'   Sorting occurs by turning either the feature or sample primary key
#'   into a factor whose levels reflect the sort.
#'
#' @inheritParams tomic_to
#' @param sort_type
#' \describe{
#'   \item{hclust}{Arrange samples by hierarchical clustering of a provided
#'     \code{value_var}}
#'   \item{arrange}{Arrange samples by the factor or alphanumeric ordering of
#'     a set of \code{sort_variables}}
#' }
#' @param sort_table samples or features
#' @param sort_variables A set of attributes in sort_table to sort with in
#'   \code{arrange}.
#' @param value_var An abundance value to use with \code{hclust}
#'
#' @returns A \code{tomic} object where feature or sample primary keys have
#'   been turned into a factor reflecting how they are sorted.
#'
#' @examples
#'
#' library(dplyr)
#'
#' sort_tomic(brauer_2008_triple,
#'   sort_type = "arrange", sort_table = "samples",
#'   sort_variables = c("nutrient", "DR")
#' ) %>%
#'   sort_tomic(
#'     sort_type = "hclust",
#'     sort_table = "features",
#'     value_var = "expression"
#'   )
#'
#' @export
sort_tomic <- function(
  tomic,
  sort_type,
  sort_table,
  sort_variables = NULL,
  value_var = NULL
  ) {

  checkmate::assertClass(tomic, "tomic")
  checkmate::assertChoice(sort_type, c("hclust", "arrange"))
  checkmate::assertChoice(sort_table, c("features", "samples"))

  # convert to triple_omic
  triple_omic <- tomic_to(tomic, "triple_omic")

  # sorts return a tibble mapping

  if (sort_type == "hclust") {
    sorted_attributes <- sort_triple_hclust(triple_omic, sort_table, value_var)
  } else if (sort_type == "arrange") {
    sorted_attributes <- sort_triple_arrange(
      triple_omic,
      sort_table,
      sort_variables
    )
  } else {
    stop(sort_type, " has no defined sort method")
  }

  pk <- ifelse(
    sort_table == "features",
    triple_omic$design$feature_pk,
    triple_omic$design$sample_pk
  )

  sorted_attributes_fct <- sorted_attributes %>%
    dplyr::mutate(orderedId := factor(!!rlang::sym(pk),
      levels = !!rlang::sym(pk)
    ))

  # update features

  triple_omic[[sort_table]] <- sorted_attributes_fct %>%
    dplyr::select(-pk) %>%
    dplyr::rename(!!rlang::sym(pk) := orderedId) %>%
    dplyr::select(!!!rlang::syms(triple_omic$design[[sort_table]]$variable))

  # update measurements

  triple_omic$measurements <- triple_omic$measurements %>%
    dplyr::left_join(sorted_attributes_fct %>%
      dplyr::select(!!rlang::sym(pk), orderedId),
    by = pk
    ) %>%
    dplyr::select(-pk) %>%
    dplyr::rename(!!rlang::sym(pk) := orderedId) %>%
    dplyr::select(!!!rlang::syms(triple_omic$design$measurements$variable))

  # convert back to initial class
  return(tomic_to(triple_omic, class(tomic)[1]))
}

#' T* Omic Sort Status
#'
#' Determine whether features &/or samples have been sorted and stored as
#'   ordered_featureId and ordered_sampleId.
#'
#' @inheritParams tomic_to
#'
#' @returns length 1 character string indicating whether the \code{tomic}
#'   is sorted.
#'
#' @examples
#'
#' tomic_sort_status(brauer_2008_tidy)
#'
#' @export
tomic_sort_status <- function(tomic) {
  checkmate::assertClass(tomic, "tomic")

  status <- dplyr::case_when(
    !("ordered_featureId" %in% tomic$design$features$variable) ~ "unsorted",
    !("ordered_sampleId" %in% tomic$design$features$variable) ~ "unsorted",
    TRUE ~ "fully sorted"
  )

  return(status)
}
