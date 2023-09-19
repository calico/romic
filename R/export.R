#' Export T*Omic as Triple
#'
#' Export features, samples and measurements tables
#'
#' @inheritParams tomic_to
#' @param dir_path path to save outputs
#' @param name_preamble start of output file name
#' @inheritParams create_tidy_omic
#'
#' @returns Export three tables:
#' \itemize{
#'     \item{features: one row per features measured (i.e., a metabolite)}
#'     \item{sample: one row per sample}
#'     \item{measurements: one row per measurement (i.e., one metabolite in
#'       one sample)}
#' }
#'
#' @examples
#' if (interactive()) {
#'   export_tomic_as_triple(brauer_2008_triple, "/tmp", "brauer")
#' }
#' @export
export_tomic_as_triple <- function(
    tomic,
    dir_path,
    name_preamble,
    verbose = TRUE
    ) {
  checkmate::assertDirectory(dir_path)
  checkmate::assertString(name_preamble)
  checkmate::assertLogical(verbose, len = 1)

  triple_omic <- tomic_to(tomic, "triple_omic")

  if (verbose) {
    message(glue::glue(
      "Saving {name_preamble}_features.tsv, {name_preamble}_samples.tsv, and
     {name_preamble}_measurements.tsv to {dir_path}"
    ))
  }

  for (k in c("features", "samples", "measurements")) {
    readr::write_tsv(
      triple_omic[[k]],
      file = file.path(dir_path, paste0(name_preamble, "_", k, ".tsv"))
    )
  }

  invisible(0)
}

#' Export T*Omic in Tidy Format
#'
#' Export a data table including all fields from features, samples and
#'   measurements.
#'
#' @inheritParams export_tomic_as_triple
#' @inheritParams create_tidy_omic
#'
#' @returns Export one table which is one row per peak, which includes
#'   all feature and sample attributes.
#'
#' @examples
#'
#' if (interactive()) {
#'   export_tomic_as_tidy(brauer_2008_triple, "/tmp", "brauer")
#' }
#' @export
export_tomic_as_tidy <- function(tomic, dir_path, name_preamble, verbose = TRUE) {
  checkmate::assertDirectory(dir_path)
  checkmate::assertString(name_preamble)
  checkmate::assertLogical(verbose, len = 1)

  tidy_omic <- tomic_to(tomic, "tidy_omic")

  filename <- paste0(name_preamble, "_tidy.tsv")
  if (verbose) {
    message(glue::glue("Saving {filename} to {dir_path}"))
  }

  readr::write_tsv(
    tidy_omic$data,
    file = file.path(dir_path, filename)
  )

  invisible(0)
}

#' Export T*Omic as Wide Data
#'
#' abundances form a matrix with metabolites as rows and samples as columns.
#'   Use transpose to treat samples as rows
#' filename
#' @inheritParams export_tomic_as_triple
#' @param value_var measurement variable to use for the matrix
#' @param transpose if TRUE then samples will be stored as rows
#' @inheritParams create_tidy_omic
#'
#' @returns Export one table which contains metabolites as rows and samples
#'   as columns.
#'
#' @examples
#'
#' if (interactive()) {
#'   export_tomic_as_wide(brauer_2008_triple, "/tmp", "brauer")
#' }
#' @export
export_tomic_as_wide <- function(
    tomic,
    dir_path,
    name_preamble,
    value_var = NULL,
    transpose = FALSE,
    verbose = TRUE
    ) {
  checkmate::assertDirectory(dir_path)
  checkmate::assertString(name_preamble)
  checkmate::assertLogical(transpose, len = 1)
  checkmate::assertLogical(verbose, len = 1)

  triple_omic <- tomic_to(tomic, "triple_omic")
  design <- triple_omic$design

  valid_value_vars <- design$measurements %>%
    dplyr::filter(
      !(type %in% c("feature_primary_key", "sample_primary_key"))
    ) %>%
    {
      .$variable
    }

  if (is.null(value_var)) {
    if (length(valid_value_vars) == 1) {
      value_var <- valid_value_vars
    } else {
      stop(glue::glue(
        "\"value_var\" was not provided and an appropriate value could not
        - be automatically chosen since there are {length(valid_value_vars)}
        - valid value variables: {paste(valid_value_vars, collapse = ', ')}"
      ))
    }
  } else {
    checkmate::assertChoice(value_var, valid_value_vars)
  }

  # structure measurements
  if (transpose) {
    cast_formula <- stats::as.formula(glue::glue(
      "{design$sample_pk} ~ {design$feature_pk}"
    ))
  } else {
    cast_formula <- stats::as.formula(glue::glue(
      "{design$feature_pk} ~ {design$sample_pk}"
    ))
  }

  measurements_matrix <- triple_omic$measurements %>%
    reshape2::acast(formula = cast_formula, value.var = value_var)

  if (transpose) {
    feature_labels <- colnames(measurements_matrix)
    sample_labels <- rownames(measurements_matrix)
  } else {
    feature_labels <- rownames(measurements_matrix)
    sample_labels <- colnames(measurements_matrix)
  }

  # create a top-left-null section
  n_feature_attr <- nrow(design$features)
  n_sample_attr <- nrow(design$samples)

  top_left_void <- matrix(nrow = n_sample_attr, ncol = n_feature_attr)
  if (transpose) {
    top_left_void <- t(top_left_void)
  }
  # remove one row to leave space for row attribute labels
  top_left_void <- top_left_void[-1, , drop = FALSE]

  # setup sample and peakgroup attributes
  # match feature and sample primary keys to class of acast to make
  # sure sample and features will be aligned with their measurements

  ordered_samples <- triple_omic$samples %>%
    dplyr::mutate(!!rlang::sym(design$sample_pk) := factor(
      !!rlang::sym(design$sample_pk),
      levels = sample_labels
    )) %>%
    dplyr::arrange(!!rlang::sym(design$sample_pk)) %>%
    dplyr::mutate_all(as.character)

  ordered_features <- triple_omic$features %>%
    dplyr::mutate(!!rlang::sym(design$feature_pk) := factor(
      !!rlang::sym(design$feature_pk),
      levels = feature_labels
    )) %>%
    dplyr::arrange(!!rlang::sym(design$feature_pk)) %>%
    dplyr::mutate_if(is.numeric, round, 3) %>%
    dplyr::mutate_all(as.character)

  if (transpose) {
    stopifnot(
      rownames(measurements_matrix) == ordered_samples[[design$sample_pk]]
    )
    stopifnot(
      colnames(measurements_matrix) == ordered_features[[design$feature_pk]]
    )

    left_matrix <- rbind(
      top_left_void,
      matrix(colnames(ordered_samples), nrow = 1),
      unname(as.matrix(ordered_samples))
    )

    top_right_matrix <- rbind(
      matrix(colnames(ordered_features), nrow = 1),
      unname(as.matrix(ordered_features))
    ) %>%
      t()

    bottom_right_matrix <- cbind(
      matrix(rownames(measurements_matrix), ncol = 1),
      measurements_matrix
    )

    right_matrix <- rbind(
      top_right_matrix,
      bottom_right_matrix
    )

    output <- cbind(left_matrix, right_matrix)
  } else {
    stopifnot(
      rownames(measurements_matrix) == ordered_features[[design$feature_pk]]
    )
    stopifnot(
      colnames(measurements_matrix) == ordered_samples[[design$sample_pk]]
    )

    left_matrix <- rbind(
      top_left_void,
      matrix(colnames(ordered_features), nrow = 1),
      unname(as.matrix(ordered_features))
    )

    top_right_matrix <- rbind(
      matrix(colnames(ordered_samples), nrow = 1),
      unname(as.matrix(ordered_samples))
    ) %>%
      t()

    bottom_right_matrix <- cbind(
      matrix(rownames(measurements_matrix), ncol = 1),
      measurements_matrix
    )

    right_matrix <- rbind(
      top_right_matrix,
      bottom_right_matrix
    )

    output <- cbind(left_matrix, right_matrix)
  }

  filename <- paste0(name_preamble, "_", "wide.tsv")
  if (verbose) {
    message(glue::glue("Saving {filename} to {dir_path}"))
  }

  output %>%
    as.data.frame() %>%
    readr::write_tsv(
      file = file.path(dir_path, filename),
      col_names = FALSE
    )

  invisible(0)
}
