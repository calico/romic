#' Add PCA Loadings
#'
#' Add Principal Components Analysis Loadings to a tidy or triple omics
#'   dataset.
#'
#' @inheritParams tomic_to
#' @inheritParams sort_tomic
#' @param center_rows center rows before performing PCA
#' @param npcs number of principal component loadings to add to samples
#'   (default is number of samples)
#' @inheritParams remove_missing_values
#' @param label_percent_varex If true then PCs will be labelled by the percent
#'   of variability they explain.
#' @inheritParams create_tidy_omic
#'
#' @returns A \code{tomic} object with principal components added to samples.
#'
#' @examples
#' add_pcs(brauer_2008_triple, npcs = 5)
#'
#' @export
add_pcs <- function(
    tomic,
    value_var = NULL,
    center_rows = TRUE,
    npcs = NULL,
    missing_val_method = "drop_samples",
    label_percent_varex = TRUE,
    verbose = TRUE
    ) {

  checkmate::assertClass(tomic, "tomic")
  checkmate::assertLogical(center_rows, len = 1)
  stopifnot(length(npcs) <= 1, class(npcs) %in% c("NULL", "numeric", "integer"))
  checkmate::assertLogical(label_percent_varex, len = 1)
  checkmate::assertLogical(verbose, len = 1)

  design <- tomic$design
  feature_pk <- design$feature_pk
  sample_pk <- design$sample_pk

  value_var <- value_var_handler(value_var = value_var, design)

  triple_omic <- tomic_to(tomic, "triple_omic") %>%
    remove_missing_values(
      value_var = value_var,
      missing_val_method = missing_val_method,
      verbose = verbose
    )

  cast_formula <- stats::as.formula(paste0(feature_pk, " ~ ", sample_pk))

  omic_matrix <- triple_omic$measurements %>%
    reshape2::acast(formula = cast_formula, value.var = value_var)

  if (is.null(npcs)) {
    npcs <- min(dim(omic_matrix))
  }
  stopifnot(npcs <= ncol(omic_matrix), npcs <= nrow(omic_matrix))
  npcs <- round(npcs)

  # center

  if (center_rows) {
    omic_matrix <- omic_matrix - rowMeans(omic_matrix)
  }

  mat_svd <- svd(omic_matrix)

  # add eigenvalues and fraction of variance explained as unstructured data
  varex_df <- tibble::tibble(
    pc_number = seq_len(length(mat_svd$d)),
    eigenvalue = mat_svd$d
  ) %>%
    dplyr::mutate(
      fraction_varex = eigenvalue^2 / (sum(eigenvalue^2))
    )

  if (label_percent_varex) {
    varex_df <- varex_df %>%
      dplyr::mutate(pc_label = glue::glue("PC{pc_number} ({scales::percent_format(2)(fraction_varex)})"))
  } else {
    varex_df <- varex_df %>%
      dplyr::mutate(pc_label = glue::glue("PC{pc_number}"))
  }

  # find the npcs leading principal components
  pcs <- mat_svd$v[, 1:npcs, drop = FALSE]
  # calculate percent variance explained by PC
  colnames(pcs) <- varex_df$pc_label[1:npcs]

  pcs <- pcs %>%
    as.data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      !!rlang::sym(sample_pk) := colnames(omic_matrix),
      # convert the PK to the same class as the original primary key
      !!rlang::sym(sample_pk) := coerce_to_classes(
        !!rlang::sym(sample_pk),
        triple_omic$samples[[sample_pk]]
      )
    )

  triple_omic$samples <- triple_omic$samples %>%
    # drop existing PCs
    dplyr::select_at(vars(!dplyr::starts_with("PC"))) %>%
    # create a copy of the primary key to join on
    dplyr::left_join(pcs, by = sample_pk)

  triple_omic$design$samples <- triple_omic$design$samples %>%
    dplyr::filter(!stringr::str_detect(variable, "^PC")) %>%
    dplyr::bind_rows(tibble::tibble(
      variable = colnames(pcs),
      type = "numeric"
    ))

  triple_omic$unstructured$scree_df <- varex_df

  return(tomic_to(triple_omic, class(tomic)[1]))
}

#' Remove Missing Values
#'
#' Account for missing values by dropping features, samples or using
#'   imputation.
#'
#' @inheritParams tomic_to
#' @inheritParams sort_tomic
#' @param missing_val_method Approach to remove missing values:
#' \describe{
#'   \item{drop_features}{Drop features with missing values}
#'   \item{drop_samples}{Drop samples which are missing all features,
#'     then drop features}
#'   \item{impute}{Impute missing values}
#' }
#' @param missing_value_types Types of values to treat as missing. One or more
#'   of "NA", "NaN" and "Inf"
#' @inheritParams create_tidy_omic
#'
#' @returns A \code{tomic} object where missing values have been accounted
#'   for.
#'
#' @examples
#' remove_missing_values(brauer_2008_triple)
#'
#' @export
remove_missing_values <- function(
  tomic,
  value_var = NULL,
  missing_val_method = "drop_samples",
  missing_value_types = c("NA", "NaN", "Inf"),
  verbose = FALSE
  ) {

  checkmate::assertClass(tomic, "tomic")
  checkmate::assertChoice(
    missing_val_method,
    c("drop_features", "drop_samples")
  )
  checkmate::assertCharacter(missing_value_types, min.len = 1)
  checkmate::assertLogical(verbose, len = 1)

  VALID_MISSING_VALUE_TYPES <- c("NA", "NaN", "Inf")
  purrr::walk(missing_value_types, checkmate::assertChoice, VALID_MISSING_VALUE_TYPES)

  triple_omic <- tomic_to(tomic, "triple_omic")

  design <- tomic$design
  feature_pk <- design$feature_pk
  sample_pk <- design$sample_pk
  n_initial_samples <- nrow(triple_omic$samples)

  value_var <- value_var_handler(value_var = value_var, design)

  # find missing values of value_var
  found_missing_values <- find_triple_omic_missing_values(
    triple_omic,
    value_var,
    missing_value_types
  )
  observed_measurements <- found_missing_values$observed_measurements
  missing_values <- found_missing_values$missing_values

  if (nrow(missing_values) > 0) {
    if (missing_val_method == "drop_features") {
      triple_omic$measurements <- observed_measurements %>%
        dplyr::anti_join(missing_values, by = feature_pk)

      triple_omic <- reconcile_triple_omic(triple_omic)
    } else if (missing_val_method == "drop_samples") {
      missing_values <- missing_values %>%
        # only consider missing values where a sample has 1+ measurements
        dplyr::semi_join(observed_measurements, sample_pk)

      triple_omic$measurements <- observed_measurements %>%
        dplyr::anti_join(missing_values, by = feature_pk)

      triple_omic <- reconcile_triple_omic(triple_omic)
    } else {
      stop(missing_val_method, " is not an implemented missing value method")
    }
  } else {
    if (verbose) {
      message("No missing values found; returning input tomic")
    }

    return(tomic)
  }

  if (nrow(triple_omic$measurement) == 0) {
    plot_missing_values(tomic %>% tomic_to("triple_omic"), value_var)
    stop(
      "All measurements were filtered using missing_val_method = ",
      missing_val_method, "\na missing value plot was printed"
    )
  }

  n_dropped_samples <- n_initial_samples - nrow(triple_omic$samples)

  if (n_dropped_samples != 0) {
    if (verbose) {
      print(
        glue::glue("{n_dropped_samples} samples dropped due to missing values")
      )
    }
  }

  n_dropped_features <- observed_measurements %>%
    dplyr::semi_join(missing_values, by = feature_pk) %>%
    dplyr::distinct(!!rlang::sym(feature_pk)) %>%
    nrow()

  if (n_dropped_features != 0) {
    if (verbose) {
      print(
        glue::glue("{n_dropped_features} features dropped due to missing values")
      )
    }
  }

  return(tomic_to(triple_omic, class(tomic)[1]))
}

#' Impute Missing Values
#'
#' Impute missing values using K-nearest neighbors imputation
#'
#' @inheritParams tomic_to
#' @inheritParams sort_tomic
#' @param impute_var_name variable to create for imputed measurements
#' @param ... additional arguments to pass to \link[impute]{impute.knn}
#'
#' @returns A \code{tomic} object with imputed measurements.
#'
#' @examples
#' impute_missing_values(brauer_2008_triple)
#'
#' @export
impute_missing_values <- function(
  tomic,
  impute_var_name = "imputed",
  value_var = NULL,
  ...
  ){

  if (!("impute" %in% rownames(utils::installed.packages()))) {
    stop("Install \"impute\" using remotes::install_bioc(\"impute\") to use this function")
  }

  checkmate::assertClass(tomic, "tomic")
  triple_omic <- tomic_to(tomic, "triple_omic")
  design <- tomic$design
  feature_pk <- design$feature_pk
  sample_pk <- design$sample_pk

  value_var <- value_var_handler(value_var = value_var, design)

  checkmate::assertString(impute_var_name)
  existing_measurements <- design$measurements %>%
    {
      .$variable[!(.$type %in% c("feature_primary_key", "sample_primary_key"))]
    }
  if (impute_var_name %in% existing_measurements) {
    warning(glue::glue(
      "impute_var_name of \"{impute_var_name}\" already exists in measurements;
      -  the existing variable will be overwritten"
    ))
  }

  # logging
  found_missing_values <- find_triple_omic_missing_values(
    triple_omic,
    value_var
  )
  missing_values <- found_missing_values$missing_values
  if (nrow(missing_values) == 0) {
    message("No missing values found; returning input tomic")
    return(tomic)
  }

  # impute data

  # format as a matrix
  cast_formula <- stats::as.formula(paste0(feature_pk, " ~ ", sample_pk))
  omic_matrix <- triple_omic$measurements %>%
    reshape2::acast(formula = cast_formula, value.var = value_var)

  # impute
  imputed_measurements <- impute::impute.knn(
    omic_matrix,
    ...
  )$data %>%
    # convert back into a tall dataset
    as.data.frame() %>%
    dplyr::mutate(!!rlang::sym(feature_pk) := rownames(.)) %>%
    tidyr::gather(
      !!rlang::sym(sample_pk),
      !!rlang::sym(impute_var_name),
      -rlang::sym(feature_pk)
    ) %>%
    dplyr::as_tibble()

  # coerce feature and/or sample variables to their original classes
  # (they will be converted to characters by impute.knn)

  imputed_measurements[[feature_pk]] <- coerce_to_classes(
    imputed_measurements[[feature_pk]],
    triple_omic$features[[feature_pk]]
  )

  imputed_measurements[[sample_pk]] <- coerce_to_classes(
    imputed_measurements[[sample_pk]],
    triple_omic$samples[[sample_pk]]
  )

  # if in-place imputation is desired then remove the old variable
  updated_measurements <- triple_omic$measurements
  if (value_var == impute_var_name) {
    updated_measurements <- updated_measurements %>%
      dplyr::select(-!!rlang::sym(value_var))
  }

  updated_measurements <- updated_measurements %>%
    dplyr::full_join(imputed_measurements, by = c(feature_pk, sample_pk))

  updated_triple <- update_tomic(
    triple_omic,
    updated_measurements
  )

  return(tomic_to(updated_triple, class(tomic)[1]))
}

#' Plot Missing Values
#'
#' Create a simple plot of missing values.
#'
#' @inheritParams tomic_to
#' @param value_var the measurement variable to check for missingness (NA or no entry)
#'
#' @returns a ggplot2 grob
#'
#' @export
#'
#' @examples
#' plot_missing_values(brauer_2008_triple)
plot_missing_values <- function(tomic, value_var = NULL) {

  checkmate::assertClass(tomic, "tomic")
  design <- tomic$design
  value_var = value_var_handler(value_var, design)

  cast_formula <- stats::as.formula(paste0(design$feature_pk, " ~ ", design$sample_pk))

  omic_matrix <- get_tomic_table(tomic, "measurements") %>%
    reshape2::acast(formula = cast_formula, value.var = value_var)

  graphics::image(t(is.na(omic_matrix)))
}

value_var_handler <- function(value_var = NULL, design) {
  possible_value_vars <- design$measurements$variable[
    design$measurements$type %in% c("numeric", "integer")
  ]
  if (length(possible_value_vars) == 0) {
    stop(
      "no quantitative (numeric or integer) variables were found in the
      triple_omic measurements table."
    )
  }

  if (length(possible_value_vars) > 1 && is.null(value_var)) {
    cli::cli_abort(
      "{.var \"value_var\"} was not provided and an appropriate value could not
        be automatically chosen since there are {length(possible_value_vars)}
        valid value variables: {.field {possible_value_vars}}"
    )
  }

  if (is.null(value_var)) {
    value_var <- possible_value_vars
  } else {
    checkmate::assertChoice(value_var, possible_value_vars)
  }

  return(value_var)
}

find_triple_omic_missing_values <- function(
  triple_omic,
  value_var,
  missing_values_types = c("NA", "NaN", "Inf")
  ) {

  all_expected_obs <- tidyr::expand_grid(
    triple_omic$features[triple_omic$design$feature_pk],
    triple_omic$samples[triple_omic$design$sample_pk]
  )

  observed_measurements <- triple_omic$measurements

  if ("NA" %in% missing_values_types) {
    observed_measurements <- observed_measurements %>%
      dplyr::filter(
        !is.na(!!rlang::sym(value_var))
      )
  }

  if ("NaN" %in% missing_values_types) {
    observed_measurements <- observed_measurements %>%
      dplyr::filter(
        !is.nan(!!rlang::sym(value_var))
      )
  }

  if ("Inf" %in% missing_values_types) {
    observed_measurements <- observed_measurements %>%
      dplyr::filter(
        is.finite(!!rlang::sym(value_var))
      )
  }

  missing_values <- all_expected_obs %>%
    dplyr::anti_join(
      observed_measurements,
      by = c(
        triple_omic$design$feature_pk,
        triple_omic$design$sample_pk
      )
    )

  output <- list(
    observed_measurements = observed_measurements,
    missing_values = missing_values
  )

  return(output)
}

#' Calculate Sample Mahalanobis Distances
#'
#' Determine each samples distance from the center of the data using Mahalanobis distance.
#'
#' @inheritParams tomic_to
#' @param value_var the measurement variable to use for calculating distances
#' @param max_pcs the maximum number of principal components to used for
#'   representing the covariance matrix.
#' @param scale if TRUE then the data will be scaled before calculating distances
#'
#' @returns The samples tibble with a new column `pc_distance` which contains the
#'   Mahalanobis distances of individual samples from the PC elipsoid
#'
#' @details
#' Since `romic` is built around using tall data where there are more features than
#' samples calculating Mahalanobis distance off of the covariance matrix is not
#' possible. Instead, we use SVD to create a low-dimensional representation of the
#' covariance matrix and calculate distances from the center of the data in this
#' space. This essentially involves weighting the principal components by their
#' loadings.
#'
#' @examples
#' calculate_sample_mahalanobis_distances(brauer_2008_tidy)
#' @export
calculate_sample_mahalanobis_distances <- function (
  tomic,
  value_var = NULL,
  max_pcs = 10,
  scale = FALSE
  ) {

  checkmate::assertClass(tomic, "tomic")
  value_var <- value_var_handler(value_var, tomic$design)
  checkmate::assertIntegerish(max_pcs, len = 1)

  n_features <- nrow(romic::get_tomic_table(tomic, "features"))
  n_samples <- nrow(romic::get_tomic_table(tomic, "samples"))
  npcs <- pmin(max_pcs, n_features, n_samples)

  X_std <- tomic %>%
    # remove features with missing values
    romic::remove_missing_values(value_var, "drop_features", verbose = FALSE) %>%
    # convert to a matrix
    romic::tomic_to_matrix(value_var) %>%
    {scale(t(.), scale = scale) %>% t()}

  # if X is constant for a feature (such all reads are 0) then
  # scaling will put it to NaN. We need to remove these features
  valid_X_std <- X_std[rowSums(is.nan(X_std)) == 0,]

  # perform a singular value decomposition
  svd_results <- svd(valid_X_std, nv = npcs)

  # re-weight principal components by eigenvalues
  X_std_v <- svd_results$v * matrix(svd_results$d[1:npcs], ncol = npcs, nrow = ncol(valid_X_std), byrow = TRUE)
  rownames(X_std_v) <- colnames(valid_X_std)

  pc_distances_by_sample <- tibble::tibble(
    !!rlang::sym(tomic$design$sample_pk) := rownames(X_std_v),
    pc_distance = rowSums(X_std_v^2)
  ) %>%
    dplyr::arrange(dplyr::desc(pc_distance))

  # validate calculation
  varex_leading_components <- sum(svd_results$d^2) * sum((svd_results$d^2 / sum(svd_results$d^2))[1:npcs])

  stopifnot((sum(pc_distances_by_sample$pc_distance) - varex_leading_components)/varex_leading_components < 0.01)

  samples_w_pc_distances <- romic::get_tomic_table(tomic, "samples") %>%
    dplyr::left_join(pc_distances_by_sample, by = tomic$design$sample_pk)

  return(samples_w_pc_distances)
}
