#' Create Tidy Omic
#'
#' A tidy omics object contains a formatted dataset and a summary of the
#'   experimental design.
#'
#' @param df a data.frame (or tibble) containing some combination of feature,
#'   sample and observation-level variables
#' @inheritParams create_triple_omic
#' @param feature_vars a character vector of additional feature-level
#'   variables (or NULL if there are no additional variables)
#' @param sample_vars a character vector of additional sample-level variables
#'   (or NULL if there are no additional variables)
#' @param verbose extra reporting messages
#'
#' @returns An S3 \code{tidy_omic}/\code{tomic} object built on a \code{list}:
#'  \describe{
#'    \item{data}{A tibble with one row per measurement (i.e., features x
#'      samples)}
#'    \item{design}{A list which organized the dataset's meta-data:
#'      \describe{
#'        \item{feature_pk}{variable specifying a unique feature}
#'        \item{sample_pk}{variable specifying a unique sample}
#'        \item{features}{tibble of feature attributes}
#'        \item{samples}{tibble of sample attributes}
#'        \item{measurements}{tibble of measurement attributes}
#'      }
#'    }
#'  }
#'
#' @examples
#'
#' library(dplyr)
#'
#' measurement_df <- tidyr::expand_grid(
#'   feature_id = 1:10,
#'   sample_id = LETTERS[1:5]
#' ) %>%
#'   dplyr::mutate(value = rnorm(n()))
#'
#' feature_df <- tibble(
#'   feature_id = 1:10,
#'   feature_group = rep(c("a", "b"), each = 5)
#' )
#' sample_df <- tibble(
#'   sample_id = LETTERS[1:5],
#'   sample_group = c("a", "a", "b", "b", "b")
#' )
#'
#' triple_omic <- create_triple_omic(
#'   measurement_df, feature_df, sample_df,
#'   "feature_id", "sample_id"
#' )
#' raw_tidy_omic <- triple_to_tidy(triple_omic)$data
#'
#' create_tidy_omic(raw_tidy_omic,
#'   feature_pk = "feature_id",
#'   feature_vars = "feature_group", sample_pk = "sample_id",
#'   sample_vars = "sample_group"
#' )
#' @export
create_tidy_omic <- function(
    df,
    feature_pk,
    feature_vars = NULL,
    sample_pk,
    sample_vars = NULL,
    omic_type_tag = "general",
    verbose = TRUE
    ) {

  checkmate::assertDataFrame(df)
  checkmate::assertString(omic_type_tag)
  checkmate::assertChoice(feature_pk, colnames(df))
  stopifnot(class(feature_vars) %in% c("character", "NULL"))
  if (!inherits(feature_vars, "NULL")) {
    stopifnot(all(feature_vars %in% colnames(df)))
    stopifnot(!(feature_pk %in% feature_vars))
  }

  checkmate::assertChoice(sample_pk, colnames(df))
  stopifnot(class(sample_vars) %in% c("character", "NULL"))
  if (!inherits(sample_vars, "NULL")) {
    stopifnot(all(sample_vars %in% colnames(df)))
    stopifnot(!(sample_pk %in% sample_vars))
  }

  n_var_uses <- table(c(feature_pk, feature_vars, sample_pk, sample_vars))
  if (any(n_var_uses > 1)) {
    invalid_vars <- names(n_var_uses)[n_var_uses > 1]

    cli::cli_abort(
      "{paste(invalid_vars, collapse = ', ')} were assigned to multiple
      classes of variables each variable should only belong to one class"
    )
  }

  # determine the classes of all variables in df

  df_classes <- tibble::tibble(
    variable = colnames(df),
    type = df %>%
      purrr::map_chr(~ class(.)[1]) %>%
      unname()
  ) %>%
    dplyr::mutate(
      type = ifelse(variable == feature_pk, "feature_primary_key", type),
      type = ifelse(variable == sample_pk, "sample_primary_key", type)
    )

  # create the design list for the dataset

  output <- list(
    data = df %>%
      dplyr::ungroup(),
    design = list()
  )
  output$design$features <- tibble::tibble(
    variable = c(feature_pk, feature_vars)
  ) %>%
    dplyr::left_join(df_classes, by = "variable")
  output$design$samples <- tibble::tibble(
    variable = c(sample_pk, sample_vars)
  ) %>%
    dplyr::left_join(df_classes, by = "variable")
  output$design$measurements <- df_classes %>%
    dplyr::filter(
      !(variable %in% feature_vars),
      !(variable %in% sample_vars)
    )
  output$design$feature_pk <- feature_pk
  output$design$sample_pk <- sample_pk

  if (verbose) {
    measurement_vars <- setdiff(
      output$design$measurements$variable,
      c(feature_pk, sample_pk)
      )

    message(glue::glue(
      "{length(measurement_vars)} measurement variables were defined as the
      left overs from the specified feature and sample varaibles:
      {paste(measurement_vars, collapse = ', ')}"
    ))
  }

  class(output) <- c("tidy_omic", "tomic", omic_type_tag)
  check_tidy_omic(output, fast_check = FALSE)

  output
}

#' Check Tidy Omic
#'
#' Check a tidy omic dataset for consistency between the data and design and
#'   validate that the dataset follows the \code{tidy_omic}/\code{tomic}
#'   specification.
#'
#' @param tidy_omic an object of class tidy_omic produced by
#' \code{\link{create_tidy_omic}}
#' @param fast_check if TRUE then skip some checks which are slow and that are
#' generally only needed when a \code{tomic} object is first created.
#'
#' @return 0 invisibly
check_tidy_omic <- function(tidy_omic, fast_check = TRUE) {

  checkmate::assertClass(tidy_omic, "tidy_omic")
  checkmate::assertLogical(fast_check, len = 1)
  # check design
  check_design(tidy_omic)

  feature_pk <- tidy_omic$design$feature_pk
  sample_pk <- tidy_omic$design$sample_pk

  # check that design matches data

  join_variables <- tidy_omic$design[
    c("features", "samples", "measurements")
  ] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(variable) %>%
    unlist() %>%
    unname()

  if (length(setdiff(join_variables, colnames(tidy_omic$data))) != 0) {
    stop(paste0(
      paste(setdiff(join_variables, colnames(tidy_omic$data)), collapse = ", "),
      ": are present in the design but not data.frames"
    ))
  }

  if (length(setdiff(colnames(tidy_omic$data), join_variables)) != 0) {
    stop(paste0(
      paste(setdiff(colnames(tidy_omic$data), join_variables), collapse = ", "),
      ": are present in the data.frames but not in the design"
    ))
  }

  if (!fast_check) {
    # check that each measurement is uniquely defined by its feature
    # and sample keys

    unique_measurement_keys <- tidy_omic$data %>%
      dplyr::count(!!rlang::sym(feature_pk), !!rlang::sym(sample_pk))

    n_degenerate_keys <- nrow(tidy_omic$data) - nrow(unique_measurement_keys)

    if (n_degenerate_keys != 0) {

      degenerate_key_examples <- unique_measurement_keys %>%
        dplyr::filter(n > 1) %>%
        dplyr::slice(1:pmin(10, n_degenerate_keys)) %>%
        dplyr::arrange(!!rlang::sym(feature_pk), !!rlang::sym(sample_pk)) %>%
        dplyr::mutate(
          combined_label = paste(
            "feature =",
            {!!rlang::sym(feature_pk)},
            "; sample =",
            {!!rlang::sym(sample_pk)}
            ))

      stop(glue::glue(
      "{n_degenerate_keys} measurements were present multiple times with
      the same feature and sample primary keys

      For example:

      {paste(degenerate_key_examples$combined_label, collapse = '\n\t')}"
      ))
    }

    # check that multiple levels of a feature-variable are not associated
    # with the same feature. These probably aren't actually feature attributes.

    feature_df <- tidy_omic$data %>%
      dplyr::distinct(!!!rlang::syms(tidy_omic$design$features$variable))

    if (nrow(tidy_omic$design$features) > 1) {
      degenerate_feature_attributes <- feature_df %>%
        dplyr::mutate(dplyr::across(
          c(setdiff(colnames(feature_df), tidy_omic$design$feature_pk)),
          as.character
        )) %>%
        tidyr::gather(
          attribute,
          attribute_value,
          -!!rlang::sym(tidy_omic$design$feature_pk)
        ) %>%
        dplyr::distinct() %>%
        dplyr::group_by(
          !!rlang::sym(tidy_omic$design$feature_pk),
          attribute
        ) %>%
        dplyr::filter(dplyr::n() > 1) %>%
        dplyr::distinct(
          !!rlang::sym(tidy_omic$design$feature_pk),
          attribute
        ) %>%
        dplyr::ungroup() %>%
        dplyr::count(attribute)

      if (nrow(degenerate_feature_attributes) != 0) {
        stop(degenerate_feature_attributes %>%
          glue::glue_data(
            "\"{attribute}\" was duplicated for {n} features
            this variable should not be a feature attribute. "
          ))
      }
    }

    # check that multiple levels of a sample-variable are not associated
    # with the same sample. These probably aren't actually sample attributes.

    sample_df <- tidy_omic$data %>%
      dplyr::distinct(!!!rlang::syms(tidy_omic$design$samples$variable))

    if (nrow(tidy_omic$design$samples) > 1) {
      degenerate_sample_attributes <- sample_df %>%
        dplyr::mutate(dplyr::across(
          c(setdiff(colnames(sample_df), tidy_omic$design$sample_pk)),
          as.character
        )) %>%
        tidyr::gather(
          attribute,
          attribute_value,
          -!!rlang::sym(tidy_omic$design$sample_pk)
        ) %>%
        dplyr::distinct() %>%
        dplyr::group_by(
          !!rlang::sym(tidy_omic$design$sample_pk),
          attribute
        ) %>%
        dplyr::filter(dplyr::n() > 1) %>%
        dplyr::distinct(
          !!rlang::sym(tidy_omic$design$sample_pk),
          attribute
        ) %>%
        dplyr::ungroup() %>%
        dplyr::count(attribute)

      if (nrow(degenerate_sample_attributes) != 0) {
        stop(degenerate_sample_attributes %>%
          glue::glue_data("\"{attribute}\" was duplicated for {n} features
                          this variable should not be a feature attribute. "))
      }
    }
  }

  return(invisible(0))
}

#' Create Triple Omic
#'
#' A triple omics class contains three data.frames, one for features, one for
#'   samples, and one for abundances. This is a good format when there is a
#'   large amount of meta data associated with features or samples.
#'
#' @details for now primary keys are unique (rather than allowing for a
#'   multi-index)
#'
#' @param measurement_df A data.frame (or tibble) of measurements - one row
#'   for each combination of feature and sample
#' @param feature_df A data.frame (or tibble) of features - one row per feature
#' @param sample_df A data.frame (or tibble) of samples - one row per sample
#' @param feature_pk A unique identifier for features
#' @param sample_pk A unique identifier for samples
#' @param omic_type_tag an optional subtype of omic data: metabolomics,
#'   lipidomics, proteomics, genomics, general
#'
#' @returns An S3 \code{triple_omic}/\code{tomic} object built on a \code{list}:
#'  \describe{
#'    \item{features}{A tibble of feature meta-data (one row per feature)}
#'    \item{samples}{A tibble of sample meta-data (one row per sample)}
#'    \item{measurements}{A tibble with one row per measurement
#'      (i.e., features x samples)}
#'    \item{design}{A list which organized the dataset's meta-data:
#'      \describe{
#'        \item{feature_pk}{variable specifying a unique feature}
#'        \item{sample_pk}{variable specifying a unique sample}
#'        \item{features}{tibble of feature attributes}
#'        \item{samples}{tibble of sample attributes}
#'        \item{measurements}{tibble of measurement attributes}
#'      }
#'    }
#'  }
#'
#' @examples
#'
#' library(dplyr)
#'
#' measurement_df <- tidyr::expand_grid(
#'   feature_id = 1:10,
#'   sample_id = LETTERS[1:5]
#' ) %>%
#'   dplyr::mutate(value = rnorm(n()))
#'
#' feature_df <- tibble(
#'   feature_id = 1:10,
#'   feature_group = rep(c("a", "b"), each = 5)
#' )
#' sample_df <- tibble(
#'   sample_id = LETTERS[1:5],
#'   sample_group = c("a", "a", "b", "b", "b")
#' )
#'
#' triple_omic <- create_triple_omic(
#'   measurement_df, feature_df, sample_df,
#'   "feature_id", "sample_id"
#' )
#' @export
create_triple_omic <- function(measurement_df,
                               feature_df = NULL,
                               sample_df = NULL,
                               feature_pk,
                               sample_pk,
                               omic_type_tag = "general") {
  # testing

  checkmate::assertClass(measurement_df, "data.frame")
  checkmate::assertString(omic_type_tag)

  # features
  stopifnot(length(feature_pk) == 1, feature_pk %in% colnames(measurement_df))
  if (!is.null(feature_df)) {
    stopifnot("data.frame" %in% class(feature_df))
    stopifnot(feature_pk %in% colnames(feature_df))
  }

  # samples
  stopifnot(length(sample_pk) == 1, sample_pk %in% colnames(measurement_df))
  if (!is.null(sample_df)) {
    stopifnot("data.frame" %in% class(sample_df))
    stopifnot(sample_pk %in% colnames(sample_df))
  }

  # initialize default feature_df if one is not provided
  if (is.null(feature_df)) {
    feature_df <- measurement_df %>%
      dplyr::ungroup() %>%
      dplyr::distinct(!!rlang::sym(feature_pk))
  }

  # initialize default sample_df if one is not provided
  if (is.null(sample_df)) {
    sample_df <- measurement_df %>%
      dplyr::ungroup() %>%
      dplyr::distinct(!!rlang::sym(sample_pk))
  }

  # Format tables as tibbles
  measurement_df <- measurement_df %>%
    dplyr::ungroup() %>%
    dplyr::as_tibble()
  feature_df <- feature_df %>% dplyr::as_tibble()
  sample_df <- sample_df %>% dplyr::as_tibble()

  # setup output
  output <- list(
    features = feature_df,
    samples = sample_df,
    measurements = measurement_df
  )

  # define the experimental design
  output$design <- list()
  output$design$features <- tibble::tibble(
    variable = colnames(feature_df),
    type = feature_df %>% purrr::map_chr(~ class(.)[1])
  ) %>%
    dplyr::mutate(type = ifelse(
      variable == feature_pk,
      "feature_primary_key",
      type
    ))

  output$design$samples <- tibble::tibble(
    variable = colnames(sample_df),
    type = sample_df %>% purrr::map_chr(~ class(.)[1])
  ) %>%
    dplyr::mutate(type = ifelse(
      variable == sample_pk,
      "sample_primary_key",
      type
    ))

  output$design$measurements <- tibble::tibble(
    variable = colnames(measurement_df),
    type = measurement_df %>% purrr::map_chr(~ class(.)[1])
  ) %>%
    dplyr::mutate(
      type = ifelse(variable == feature_pk, "feature_primary_key", type),
      type = ifelse(variable == sample_pk, "sample_primary_key", type)
    )

  output$design$feature_pk <- feature_pk
  output$design$sample_pk <- sample_pk

  class(output) <- c("triple_omic", "tomic", omic_type_tag)
  check_triple_omic(output, fast_check = FALSE)

  output
}

#' Check Triple Omic
#'
#' Check a triple omic dataset for consistency between the data and design and
#'   validate that the dataset follows the \code{triple_omic}/\code{tomic}
#'   specification.
#'
#' @param triple_omic an object of class triple_omic produced by
#'   \code{\link{create_triple_omic}}
#' @inheritParams check_tidy_omic
#'
#' @return 0 invisibly
check_triple_omic <- function(triple_omic, fast_check = TRUE) {
  checkmate::assertClass(triple_omic, "triple_omic")
  checkmate::assertLogical(fast_check, len = 1)
  # check design
  check_design(triple_omic)

  # variables are same as design
  checkmate::assertNames(
    colnames(triple_omic$features),
    permutation.of = triple_omic$design$features$variable,
    type = "unique",
    what = "colnames"
  )
  checkmate::assertNames(
    colnames(triple_omic$samples),
    permutation.of = triple_omic$design$samples$variable,
    type = "unique",
    what = "colnames"
  )
  checkmate::assertNames(
    colnames(triple_omic$measurements),
    permutation.of = triple_omic$design$measurements$variable,
    type = "unique",
    what = "colnames"
  )

  # primary keys have matching classes and levels

  features_features <- triple_omic$features[[triple_omic$design$feature_pk]]
  measurements_features <- triple_omic$measurements[[
    triple_omic$design$feature_pk
  ]]
  if (!all(class(features_features) == class(measurements_features))) {
    stop(glue::glue(
      "{triple_omic$design$feature_pk} classes differ between the features
        and measurements table"
    ))
  }
  if (any(class(features_features) %in% c("factor", "ordered"))) {
    checkmate::checkFactor(features_features, levels(measurements_features))
  }

  samples_samples <- triple_omic$samples[[triple_omic$design$sample_pk]]
  measurements_samples <- triple_omic$measurements[[
    triple_omic$design$sample_pk
  ]]
  if (!all(class(samples_samples) == class(measurements_samples))) {
    stop(glue::glue(
      "{triple_omic$design$sample_pk} classes differ between the samples
        and measurements table"
    ))
  }
  if (any(class(samples_samples) %in% c("factor", "ordered"))) {
    checkmate::checkFactor(samples_samples, levels(measurements_samples))
  }

  # thorough checking

  if (!fast_check) {
    # classes match
    # one row per feature in features

    features_not_unique <- triple_omic$features %>%
      dplyr::count(!!rlang::sym(triple_omic$design$feature_pk)) %>%
      dplyr::filter(n > 1)

    if (nrow(features_not_unique) != 0) {
      stop(glue::glue(
        "{nrow(features_not_unique)} features were present multiple times with
        the same feature primary key"
      ))
    }

    # one row per sample in samples

    samples_not_unique <- triple_omic$samples %>%
      dplyr::count(!!rlang::sym(triple_omic$design$sample_pk)) %>%
      dplyr::filter(n > 1)

    if (nrow(samples_not_unique) != 0) {
      stop(glue::glue(
        "{nrow(samples_not_unique)} features were present multiple times with
        the same sample primary key"
      ))
    }

    # one row per measurement in measurements

    measurements_not_unique <- triple_omic$measurements %>%
      dplyr::count(
        !!rlang::sym(triple_omic$design$feature_pk),
        !!rlang::sym(triple_omic$design$sample_pk)
      ) %>%
      dplyr::filter(n > 1)

    if (nrow(measurements_not_unique) != 0) {
      stop(glue::glue(
        "{nrow(measurements_not_unique)} measurements were present multiple times with
        the same feature and sample primary keys"
      ))
    }
  }

  return(invisible(0))
}

#' Triple Omic to Tidy Omic
#'
#' Convert a \code{triple_omic} object into a \code{tidy_omic} oobject.
#'
#' Features, samples and measurements will be merged into a single \code{data}
#'   table, and the \code{design} will be preserved as-is.
#'
#' @inheritParams check_triple_omic
#'
#' @returns A \code{tidy_omic} object as created by
#'   \code{\link{create_tidy_omic}}.
#'
#' @examples
#'
#' library(dplyr)
#'
#' measurement_df <- tidyr::expand_grid(
#'   feature_id = 1:10,
#'   sample_id = LETTERS[1:5]
#' ) %>%
#'   dplyr::mutate(value = rnorm(n()))
#'
#' feature_df <- tibble(
#'   feature_id = 1:10,
#'   feature_group = rep(c("a", "b"), each = 5)
#' )
#' sample_df <- tibble(
#'   sample_id = LETTERS[1:5],
#'   sample_group = c("a", "a", "b", "b", "b")
#' )
#'
#' triple_omic <- create_triple_omic(
#'   measurement_df, feature_df, sample_df,
#'   "feature_id", "sample_id"
#' )
#' triple_to_tidy(triple_omic)
#' @export
triple_to_tidy <- function(triple_omic) {
  check_triple_omic(triple_omic)

  feature_pk <- triple_omic$design$feature_pk
  sample_pk <- triple_omic$design$sample_pk

  samples_measurements <- triple_omic$samples %>%
    dplyr::inner_join(
      triple_omic$measurements,
      by = sample_pk,
      multiple = "all"
    )

  tidy_output <- triple_omic$features %>%
    dplyr::inner_join(
      samples_measurements,
      by = feature_pk,
      multiple = "all"
    )

  output <- list()
  output$data <- tidy_output
  output$design <- triple_omic$design

  class(output) <- c("tidy_omic", "tomic", class(triple_omic)[3])

  output
}

#' Tidy omic to triple omic
#'
#' Convert a \code{tidy_omic} object into a \code{triple_omic} object.
#'
#' The \code{data} table will be converted into \code{features},
#'   \code{samples}, and \code{measurements} tables using the \code{design}
#'   to determine which variables belong in each table. The \code{design}
#'   will be preserved as-is.
#'
#' @inheritParams check_tidy_omic
#'
#' @returns A \code{triple_omic} object as created by
#'   \code{\link{create_triple_omic}}
#'
#' @examples
#' tidy_to_triple(brauer_2008_tidy)
#' @export
tidy_to_triple <- function(tidy_omic) {
  check_tidy_omic(tidy_omic)

  # `distinct()` used to return variables in the order existing in the
  # data. Since dplyr 1.1.0, itnow returns variables in the order they
  # were supplied. To prevent a behaviour change, we now supply the
  # variables in data order by subsetting the original variables first.
  vars <- names(tidy_omic$data)
  features_vars <- intersect(vars, tidy_omic$design$features$variable)
  samples_vars <- intersect(vars, tidy_omic$design$samples$variable)

  # This has always returned the variables in supplied order because
  # that's how `select()` orders the output
  measurements_vars <- tidy_omic$design$measurements$variable

  feature_df <- tidy_omic$data %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(features_vars)))

  sample_df <- tidy_omic$data %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(samples_vars)))

  measurement_df <- tidy_omic$data %>%
    dplyr::select(dplyr::all_of(measurements_vars))

  output <- list(
    features = feature_df,
    samples = sample_df,
    measurements = measurement_df,
    design = tidy_omic$design
  )

  class(output) <- c("triple_omic", "tomic", class(tidy_omic)[3])

  output
}

#' Convert Wide to Tidy Omic
#'
#' Convert a wide dataset of species' abundances (gene product, metabolites,
#'   lipids, ...) into a triple_omic dataset (one observation per row)
#'
#' @param wide_df a data.frame (or tibble) containing 1+ columns of feature
#'   attributes and many columns of samples
#' @inheritParams create_tidy_omic
#' @param sample_var variable name to use for samples
#' @param measurement_var variable name to use for measurements
#' @inheritParams create_tidy_omic
#'
#' @returns A \code{tidy_omic} object as produced by \code{create_tidy_omic}.
#'
#' @examples
#'
#' library(dplyr)
#'
#' wide_measurements <- brauer_2008_triple[["measurements"]] %>%
#'   tidyr::spread(sample, expression)
#'
#' wide_df <- brauer_2008_triple[["features"]] %>%
#'   left_join(wide_measurements, by = "name")
#'
#' convert_wide_to_tidy_omic(wide_df,
#'   feature_pk = "name",
#'   feature_vars = c("BP", "MF", "systematic_name")
#' )
#' @export
convert_wide_to_tidy_omic <- function(
  wide_df,
  feature_pk,
  feature_vars = NULL,
  sample_var = "sample",
  measurement_var = "abundance",
  omic_type_tag = "general",
  verbose = TRUE
  ) {

  checkmate::assertDataFrame(wide_df)
  checkmate::assertChoice(feature_pk, colnames(wide_df))
  stopifnot(class(feature_vars) %in% c("character", "NULL"))
  if (!inherits(feature_vars, "NULL")) {
    stopifnot(all(feature_vars %in% colnames(wide_df)))
    stopifnot(!(feature_pk %in% feature_vars))
  }
  checkmate::assertString(sample_var)
  checkmate::assertString(measurement_var)
  checkmate::assertString(omic_type_tag)
  checkmate::assertLogical(verbose, len = 1)

  # test other provided variables
  reserved_variable_names <- c(
    sample_var,
    measurement_var,
    "entry_number",
    paste0("unique_", feature_pk)
  )
  reserved_variable_use <- intersect(
    reserved_variable_names,
    c(feature_pk, feature_vars)
  )
  if (length(reserved_variable_use) != 0) {
    stop(
      paste(reserved_variable_use, collapse = ", "),
      " are reserved variable names"
    )
  }

  # test whether unique_feature_variable is really unique
  grouped_by_unique_var <- wide_df %>%
    dplyr::group_by(!!rlang::sym(feature_pk)) %>%
    dplyr::mutate(entry_number = seq_len(dplyr::n()))

  if (sum(grouped_by_unique_var$entry_number != 1) == 0) {
    grouped_by_unique_var <- grouped_by_unique_var %>%
      dplyr::select(-entry_number)
  } else {
    warning(
      sum(grouped_by_unique_var$entry_number != 1),
      " rows did not contain a unique ",
      feature_pk,
      "; adding extra variables 'unique_",
      feature_pk,
      "' & 'entry_number' to distinguish them"
    )

    mutate_call <- function(unique_var, n_entries, entry_number) {
      stats::setNames(
        list(lazyeval::interp(
          ~ ifelse(
            n_entries == 1,
            unique_var,
            paste0(unique_var, "-", entry_number)
          ),
          n_entries = as.name(n_entries),
          unique_var = as.name(unique_var),
          entry_number = as.name(entry_number)
        )),
        paste0("unique_", feature_pk)
      )
    }

    # force each feature to be a unique variable (if multiple peaks are called
    # for the same compound)
    unique_feature_names <- grouped_by_unique_var %>%
      dplyr::select(!!!rlang::syms(c(feature_pk, "entry_number"))) %>%
      dplyr::group_by(!!rlang::sym(feature_pk)) %>%
      dplyr::mutate(n_entries = dplyr::n()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate_(.dots = mutate_call(
        feature_pk,
        "n_entries",
        "entry_number"
      )) %>%
      dplyr::select(-n_entries)

    grouped_by_unique_var <- grouped_by_unique_var %>%
      dplyr::left_join(
        unique_feature_names,
        by = c(feature_pk, "entry_number")
      )

    feature_vars <- c(feature_pk, "entry_number", feature_vars)
    feature_pk <- paste0("unique_", feature_pk)
  }

  sample_names <- setdiff(
    colnames(grouped_by_unique_var),
    c(feature_pk, feature_vars)
  )

  tall_dataset <- grouped_by_unique_var %>%
    dplyr::ungroup() %>%
    tidyr::gather(
      !!rlang::sym(sample_var),
      !!rlang::sym(measurement_var),
      !!!rlang::syms(sample_names)
    )

  tidy_omic <- create_tidy_omic(
    df = tall_dataset,
    feature_pk = feature_pk,
    feature_vars = feature_vars,
    sample_pk = sample_var,
    omic_type_tag = omic_type_tag,
    verbose = verbose
  )

  return(tidy_omic)
}

#' T* Omic To
#'
#' Takes in any \code{romic} reprsentation of a dataset and returns a specific
#'   representation.
#'
#' @param tomic Either a \code{tidy_omic} or \code{triple_omic} object
#' @param to_class The class to return, either \code{tidy_omic} or
#'   \code{triple_omic}
#'
#' @returns tomic transformed to \code{to_class} class (or un-transformed if
#'   it started that way).
#'
#' @examples
#'
#' tomic_to(brauer_2008_tidy, "triple_omic")
#' @export
tomic_to <- function(tomic, to_class) {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertChoice(to_class, c("tidy_omic", "triple_omic"))

  current_primary_class <- class(tomic)[1]

  if (current_primary_class == to_class) {
    output <- tomic
  } else {
    if (current_primary_class == "tidy_omic" && to_class == "triple_omic") {
      output <- tidy_to_triple(tomic)
    } else if (
      current_primary_class == "triple_omic" && to_class == "tidy_omic"
    ) {
      output <- triple_to_tidy(tomic)
    } else {
      stop(glue::glue(
        "No converter exist for {current_primary_class} to {to_class}"
      ))
    }
  }

  return(output)
}

#' Check T*Omic
#'
#' Check a tidy or triple 'omic object for common pathologies, such as a
#'   mismatch between data and schema and non-uniqueness of primary keys.
#'
#' @inheritParams tomic_to
#' @inheritParams check_tidy_omic
#'
#' @returns 0 invisibly
#'
#' @examples
#' check_tomic(brauer_2008_triple)
#' @export
check_tomic <- function(tomic, fast_check = TRUE) {
  checkmate::assertClass(tomic, "tomic")

  if ("triple_omic" %in% class(tomic)) {
    check_triple_omic(tomic, fast_check)
  } else if ("tidy_omic" %in% class(tomic)) {
    check_tidy_omic(tomic, fast_check)
  } else {
    stop("tomic is not a tidy_omic or triple_omic. This is unexpected since
           the object has the \"tomic\" class.")
  }

  return(invisible(0))
}

#' Get Tomic Table
#'
#' Extract one of the specific tables from a tomic object
#'
#' @inheritParams tomic_to
#' @param table_type The type of table to extract from the \code{tomic} object.
#' \describe{
#'   \item{tidy}{one row per measurements with feature and sample attributes added. Equivalent to the $data field of a tidy omic object}
#'   \item{measurements}{one row per measurements defined a feature and sample foreign key. Equivalent to the $measurements field of a triple omic object}
#'   \item{features}{one row per feature defined by a feature primary key. Equivalent to the $features field of a triple omic object}
#'   \item{samples}{one row per sample defined by a sample primary key. Equivalent to the $samples field of a triple omic object}
#' }
#'
#' @returns a tibble matching the \code{table_type} of the \code{tomic} object
#'
#' @export
#'
#' @examples
#' get_tomic_table(brauer_2008_triple, "samples")
#' get_tomic_table(brauer_2008_tidy, "features")
get_tomic_table <- function(tomic, table_type) {

  checkmate::assertClass(tomic, "tomic")
  valid_table_types <- c("tidy", "measurements", "features", "samples")
  checkmate::assertChoice(table_type, valid_table_types)

  if (table_type == "tidy") {
    # convert to tidy-omic if needed
    tidy_omic <- tomic_to(tomic, "tidy_omic")
    return(tidy_omic$data)

  } else if (table_type %in% c("measurements", "features", "samples")) {

    triple_omic <- tomic_to(tomic, "triple_omic")
    return(triple_omic[[table_type]])

  } else {
    stop ("This case should not be reached - please contact a dev")
  }
}


get_identifying_keys <- function(tomic, table) {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertChoice(table, c("features", "samples", "measurements"))

  if (table == "features") {
    ids <- tomic$design$feature_pk
  } else if (table == "samples") {
    ids <- tomic$design$sample_pk
  } else if (table == "measurements") {
    ids <- c(tomic$design$feature_pk, tomic$design$sample_pk)
  } else {
    stop(glue::glue("{table} is not a valid choice"))
  }

  return(ids)
}

#' Infer Tomic Table Type
#'
#' From a tomic_table, choose whether it reflects features, samples or
#'   measurements
#'
#' @inheritParams tomic_to
#' @inheritParams plot_bivariate
#'
#' @returns features, samples or measurements
infer_tomic_table_type <- function(tomic, tomic_table) {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertClass(tomic_table, "data.frame")

  feature_pk <- tomic$design$feature_pk
  sample_pk <- tomic$design$sample_pk
  tomic_table_vars <- colnames(tomic_table)

  table_type <- dplyr::case_when(
    feature_pk %in% tomic_table_vars && sample_pk %in% tomic_table_vars ~ "measurements",
    feature_pk %in% tomic_table_vars ~ "features",
    sample_pk %in% tomic_table_vars ~ "samples"
  )

  if (is.na(table_type)) {
    stop(
      "based on the \"tomic\" primary keys, tomic_table doesn't appear to
       be features, samples or measurements"
    )
  }

  return(table_type)
}

