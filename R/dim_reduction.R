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
    missing_val_method = "drop_samples") {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertLogical(center_rows, len = 1)
  stopifnot(length(npcs) <= 1, class(npcs) %in% c("NULL", "numeric", "integer"))

  design <- tomic$design
  feature_pk <- design$feature_pk
  sample_pk <- design$sample_pk

  value_var <- value_var_handler(value_var = value_var, design)

  triple_omic <- tomic_to(tomic, "triple_omic") %>%
    remove_missing_values(
      value_var = value_var,
      missing_val_method = missing_val_method
    )

  cast_formula <- stats::as.formula(paste0(feature_pk, " ~ ", sample_pk))

  omic_matrix <- triple_omic$measurements %>%
    reshape2::acast(formula = cast_formula, value.var = value_var)

  if (is.null(npcs)) {
    npcs <- ncol(omic_matrix)
  }
  stopifnot(npcs <= ncol(omic_matrix))
  npcs <- round(npcs)

  # center

  if (center_rows) {
    omic_matrix <- omic_matrix - rowMeans(omic_matrix)
  }

  mat_svd <- svd(omic_matrix)

  # add eigenvalues and fraction of variance explained as unstructured data
  varex_df <- tibble::tibble(
    pc_number = seq_len(ncol(omic_matrix)),
    eigenvalue = mat_svd$d
  ) %>%
    dplyr::mutate(
      fraction_varex = eigenvalue^2 / (sum(eigenvalue^2)),
      pc_label = glue::glue("PC{pc_number} ({scales::percent_format(2)(fraction_varex)})")
    )

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
    missing_val_method = "drop_samples") {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertChoice(
    missing_val_method,
    c("drop_features", "drop_samples")
  )

  triple_omic <- tomic_to(tomic, "triple_omic")

  design <- tomic$design
  feature_pk <- design$feature_pk
  sample_pk <- design$sample_pk
  n_initial_samples <- nrow(triple_omic$samples)

  value_var <- value_var_handler(value_var = value_var, design)

  # find missing values of value_var
  found_missing_values <- find_triple_omic_missing_values(
    triple_omic,
    value_var
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
    message("No missing values found; returning input tomic")
    return(tomic)
  }

  if (nrow(triple_omic$measurement) == 0) {
    plot_missing_values(triple_omic, value_var)
    stop(
      "All measurements were filtered using missing_val_method = ",
      missing_val_method, "\na missing value plot was printed"
    )
  }

  n_dropped_samples <- n_initial_samples - nrow(triple_omic$samples)

  if (n_dropped_samples != 0) {
    print(
      glue::glue("{n_dropped_samples} samples dropped due to missing values")
    )
  }

  n_dropped_features <- observed_measurements %>%
    dplyr::semi_join(missing_values, by = feature_pk) %>%
    dplyr::distinct(!!rlang::sym(feature_pk)) %>%
    nrow()

  if (n_dropped_features != 0) {
    print(
      glue::glue("{n_dropped_features} features dropped due to missing values")
    )
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
    ...) {
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

  # imput data
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

plot_missing_values <- function(triple_omic, value_var) {
  cast_formula <- stats::as.formula(paste0(feature_pk, " ~ ", sample_pk))

  omic_matrix <- triple_omic$measurements %>%
    reshape2::acast(formula = cast_formula, value.var = value_var)

  graphics::image(t(omic_matrix))
}

value_var_handler <- function(value_var = NULL, design) {
  possible_value_vars <- design$measurements$variable[
    design$measurements$type %in% c("numeric", "integer")
  ]
  if (length(possible_value_vars) == 0) {
    stop(
      "no quantitative (numeric or integer) variables were found in the
      triple_omic measurements table pca can only be applied to quantitative
      variables"
    )
  }

  if (length(possible_value_vars) > 1 && is.null(value_var)) {
    stop(
      "value_var must be specified since multiple quantitative measurement
      variables exist"
    )
  }

  if (is.null(value_var)) {
    value_var <- possible_value_vars
  } else {
    checkmate::assertChoice(value_var, possible_value_vars)
  }

  return(value_var)
}

find_triple_omic_missing_values <- function(triple_omic, value_var) {
  all_expected_obs <- tidyr::expand_grid(
    triple_omic$features[triple_omic$design$feature_pk],
    triple_omic$samples[triple_omic$design$sample_pk]
  )

  observed_measurements <- triple_omic$measurements %>%
    # drop missing values
    dplyr::filter_at(value_var, function(x) !is.na(x))

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
