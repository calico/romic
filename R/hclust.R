hclust_tidy_omic <- function(
    tidy_omic,
    feature_var,
    sample_var,
    value_var,
    cluster_dim,
    distance_measure = "dist",
    hclust_method = "ward.D2") {
  check_tidy_omic(tidy_omic)

  checkmate::assertChoice(feature_var, tidy_omic$design$features$variable)
  checkmate::assertChoice(sample_var, tidy_omic$design$samples$variable)
  checkmate::assertChoice(value_var, tidy_omic$design$measurements$variable)
  checkmate::assertChoice(cluster_dim, c("rows", "columns", "both"))
  checkmate::assertChoice(distance_measure, c("corr", "dist"))
  checkmate::assertString(hclust_method)

  # order rows and/or columns

  cluster_orders <- hclust_order(
    df = tidy_omic$data,
    feature_pk = tidy_omic$design$feature_pk,
    sample_pk = tidy_omic$design$sample_pk,
    value_var = value_var,
    cluster_dim = cluster_dim,
    distance_measure = distance_measure,
    hclust_method = hclust_method
  )

  # save classes of sampleId and groupId so appropriate class coercion occurs
  #  when adding cluster orders by joining on primary keys
  cluster_orders$rows <- coerce_to_classes(
    cluster_orders$rows,
    tidy_omic$data[[tidy_omic$design$feature_pk]]
  )

  cluster_orders$columns <-
    coerce_to_classes(
      cluster_orders$columns,
      tidy_omic$data[[tidy_omic$design$feature_pk]]
    )

  # order rows and columns

  distinct_features <- tidy_omic$data %>%
    dplyr::distinct(
      !!rlang::sym(tidy_omic$design$feature_pk),
      !!rlang::sym(feature_var)
    )

  if (cluster_dim == "columns") {
    # order by factor or alpha-numerically

    if (
      any(class(distinct_features[[feature_var]]) %in% c("factor", "ordered"))
    ) {
      # retain previous ordering

      ordered_distinct_features <- distinct_features %>%
        dplyr::arrange(!!rlang::sym(feature_var))
    } else {
      ordered_distinct_features <- distinct_features %>%
        dplyr::arrange(!!rlang::sym(feature_var))
    }

    ordered_distinct_features <- ordered_distinct_features %>%
      dplyr::mutate(ordered_featureId = factor(
        !!rlang::sym(tidy_omic$design$feature_pk),
        levels = !!rlang::sym(tidy_omic$design$feature_pk)
      ))
  } else {
    # order with hclust

    ordered_distinct_features <- distinct_features %>%
      dplyr::left_join(
        tibble::tibble(
          !!rlang::sym(tidy_omic$design$feature_pk) := cluster_orders$rows
        ) %>%
          dplyr::mutate(order = seq_len(dplyr::n())),
        by = tidy_omic$design$feature_pk
      ) %>%
      dplyr::arrange(order) %>%
      dplyr::mutate(ordered_featureId = factor(
        !!rlang::sym(tidy_omic$design$feature_pk),
        levels = !!rlang::sym(tidy_omic$design$feature_pk)
      ))
  }

  distinct_samples <- tidy_omic$data %>%
    dplyr::distinct(
      !!rlang::sym(tidy_omic$design$sample_pk),
      !!rlang::sym(sample_var)
    )

  if (cluster_dim == "rows") {
    # order by factor or alpha-numerically

    if (any(class(distinct_samples[[sample_var]]) %in% c("factor", "ordered"))) {
      # retain previous ordering

      ordered_distinct_samples <- distinct_samples %>%
        dplyr::arrange(!!rlang::sym(sample_var))
    } else {
      ordered_distinct_samples <- distinct_samples %>%
        dplyr::arrange(!!rlang::sym(sample_var))
    }

    ordered_distinct_samples <- ordered_distinct_samples %>%
      dplyr::mutate(ordered_sampleId = factor(
        !!rlang::sym(tidy_omic$design$sample_pk),
        levels = !!rlang::sym(tidy_omic$design$sample_pk)
      ))
  } else {
    # order with hclust

    ordered_distinct_samples <- distinct_samples %>%
      dplyr::left_join(
        tibble::tibble(
          !!rlang::sym(tidy_omic$design$sample_pk) := cluster_orders$columns
        ) %>%
          dplyr::mutate(order = seq_len(dplyr::n())),
        by = tidy_omic$design$sample_pk
      ) %>%
      dplyr::arrange(order) %>%
      dplyr::mutate(ordered_sampleId = factor(
        !!rlang::sym(tidy_omic$design$sample_pk),
        levels = !!rlang::sym(tidy_omic$design$sample_pk)
      ))
  }

  # update labels
  ordered_distinct_features <- ordered_distinct_features %>%
    dplyr::mutate(feature_label = format_names_for_plotting(
      !!rlang::sym(feature_var)
    ))

  ordered_distinct_samples <- ordered_distinct_samples %>%
    dplyr::mutate(sample_label = format_names_for_plotting(
      !!rlang::sym(sample_var)
    ))

  # setup abundance values

  updated_tidy_data <- tidy_omic$data %>%
    # order all rows and columns
    dplyr::left_join(
      ordered_distinct_features %>%
        dplyr::select(
          !!rlang::sym(tidy_omic$design$feature_pk),
          ordered_featureId,
          feature_label
        ),
      by = tidy_omic$design$feature_pk
    ) %>%
    dplyr::left_join(
      ordered_distinct_samples %>%
        dplyr::select(
          !!rlang::sym(tidy_omic$design$sample_pk),
          ordered_sampleId,
          sample_label
        ),
      by = tidy_omic$design$sample_pk
    )

  # update tidy omic data table and schema
  tidy_omic <- update_tidy_omic(
    tidy_omic,
    updated_tidy_data,
    new_variable_tables = c(
      "ordered_featureId" = "features",
      "feature_label" = "features",
      "ordered_sampleId" = "samples",
      "sample_label" = "samples"
    )
  )

  return(tidy_omic)
}


#' Hierarchical clustering order
#'
#' Format and hierarchically cluster a data.frame. If hclust could not normally
#'   be produced (usually because no samples are in common for a feature) pad
#'   the matrix with zeros and still calculate the distance
#'
#' @param df data.frame to cluster
#' @param feature_pk variable uniquely defining a row
#' @param sample_pk variable uniquely defining a sample
#' @inheritParams sort_tomic
#' @param cluster_dim rows, columns, or both
#' @param distance_measure variable to use for computing dis-similarity
#' \describe{
#'   \item{corr}{pearson correlation}
#'   \item{dist}{euclidean distance}
#' }
#' @param hclust_method method from stats::hclust to use for clustering
#'
#' @returns a list containing a hierarchically clustered set of rows and/or
#'   columns
#'
#' @examples
#'
#' library(dplyr)
#'
#' df <- tidyr::crossing(letters = LETTERS, numbers = 1:10) %>%
#'   mutate(noise = rnorm(n()))
#' hclust_order(df, "letters", "numbers", "noise", "rows")
#' @export
hclust_order <- function(
    df,
    feature_pk,
    sample_pk,
    value_var,
    cluster_dim,
    distance_measure = "dist",
    hclust_method = "ward.D2") {
  checkmate::assertDataFrame(df)
  checkmate::assertChoice(feature_pk, colnames(df))
  checkmate::assertChoice(sample_pk, colnames(df))
  checkmate::assertChoice(value_var, colnames(df))
  if (length(unique(c(feature_pk, sample_pk, value_var))) != 3) {
    stop("feature_pk, sample_pk, and value_var must all be different")
  }
  checkmate::assertChoice(cluster_dim, c("rows", "columns", "both"))
  checkmate::assertChoice(distance_measure, c("corr", "dist"))
  checkmate::assertString(hclust_method)

  quant_matrix <- df %>%
    reshape2::acast(
      stats::as.formula(glue::glue("{feature_pk} ~ {sample_pk}")),
      value.var = value_var
    )

  output <- list()

  if (cluster_dim %in% c("rows", "both")) {
    cluster_rows <- try(
      apply_hclust(quant_matrix, distance_measure, hclust_method),
      silent = TRUE
    )

    # if distance cannot be computed (because of missing values) pad with
    # zeros and recalculate
    if (inherits(cluster_rows, "try-error")) {
      pad_matrix <- matrix(0, ncol = 2, nrow = nrow(quant_matrix))
      colnames(pad_matrix) <- c("pad1", "pad2")
      quant_matrix_pad <- cbind(quant_matrix, pad_matrix)

      cluster_rows <- apply_hclust(
        quant_matrix_pad,
        distance_measure,
        hclust_method
      )
    }

    output$rows <- rownames(quant_matrix)[cluster_rows$order]
  }

  if (cluster_dim %in% c("columns", "both")) {
    cluster_cols <- try(
      apply_hclust(t(quant_matrix), distance_measure, hclust_method),
      silent = TRUE
    )

    # if distance cannot be computed (because of missing values) pad with zeros
    # and recalculate
    if (inherits(cluster_cols, "try-error")) {
      pad_matrix <- matrix(0, ncol = 2, nrow = ncol(quant_matrix))
      colnames(pad_matrix) <- c("pad1", "pad2")
      quant_matrix_pad <- cbind(t(quant_matrix), pad_matrix)

      cluster_cols <- apply_hclust(
        quant_matrix_pad,
        distance_measure,
        hclust_method
      )
    }

    output$columns <- colnames(quant_matrix)[cluster_cols$order]
  }

  output
}

apply_hclust <- function(quant_matrix, distance_measure, hclust_method) {
  checkmate::assertMatrix(quant_matrix)
  if (nrow(quant_matrix) == 0) {
    stop(quant_matrix, "contained zero rows")
  } else if (nrow(quant_matrix) == 1) {
    # if there is only one entry then we don't need to cluster it
    return(list(order = 1))
  }

  if (distance_measure == "dist") {
    distance_matrix <- stats::dist(quant_matrix)
  } else if (distance_measure == "corr") {
    distance_matrix <- 1 - stats::cor(
      t(quant_matrix),
      use = "pairwise.complete.obs"
    ) %>%
      stats::as.dist()

    if (any(is.na(distance_matrix))) {
      stop("NA distances are not allowed with hierarchical clustering")
    }
  } else {
    stop(glue::glue("{distance_measure} is not a defined distance_measure"))
  }

  stats::hclust(distance_matrix, method = hclust_method)
}

#' Downsample Heatmap
#'
#' Combine rows to speed up rendering of large heatmaps
#'
#' @param tidy_data The data frame from a \code{tidy_omic} object containing
#'   ordered feature and sample primary keys defined by ordered_featureId
#'   and ordered_sampleId.
#' @inheritParams plot_heatmap
#' @param design a list summarizing the design of the tidy dataset
#' @param max_display_features aggregate and downsample distinct feature to
#'   this number to speed to up heatmap rendering.
#' @inheritParams create_tidy_omic
#'
#' @returns tidy_data with rows collapsed if the number of distinct features is
#'   greater than \code{max_display_features}
#'
downsample_heatmap <- function(
    tidy_data,
    value_var,
    design,
    max_display_features = 1000,
    verbose = TRUE
    ) {
  checkmate::assertDataFrame(tidy_data)
  checkmate::assertChoice(value_var, colnames(tidy_data))
  checkmate::assertNumber(max_display_features)
  checkmate::assertLogical(verbose, len = 1)

  if (!("ordered_featureId" %in% colnames(tidy_data))) {
    stop("ordered_featureId is a requred variable in tidy_data")
  }
  if (!("ordered_sampleId" %in% colnames(tidy_data))) {
    stop("ordered_sampleId is a requred variable in tidy_data")
  }

  checkmate::assertFactor(tidy_data$ordered_featureId)
  checkmate::assertFactor(tidy_data$ordered_sampleId)

  n_features <- tidy_data %>%
    dplyr::distinct(ordered_featureId) %>%
    nrow()

  if (n_features <= max_display_features) {
    return(tidy_data)
  }

  # update the target number of n_features so (almost) all final features will
  # combine the same number of orignal features
  realized_max_display_features <- ceiling(
    n_features / ceiling(n_features / max_display_features)
  )
  if (verbose) {
    message(glue::glue(
      "Downsampling {n_features} features to {realized_max_display_features}, targeting {max_display_features}"
    ))
  }

  collapsed_rows_merges <- tibble::tibble(ordered_featureId_int = 1:n_features) %>%
    dplyr::mutate(collapsed_row_number = rep(
      1:max_display_features,
      each = ceiling(n_features / max_display_features)
    )[ordered_featureId_int])

  downsampled_df <- tidy_data %>%
    dplyr::mutate(ordered_featureId_int = as.integer(ordered_featureId)) %>%
    dplyr::left_join(collapsed_rows_merges, by = "ordered_featureId_int")

  # average value_var and take the first entry for other variables

  downsampled_matrix_values <- downsampled_df %>%
    dplyr::group_by(collapsed_row_number, ordered_sampleId) %>%
    dplyr::summarize(
      !!rlang::sym(value_var) := mean(!!rlang::sym(value_var)),
      .groups = "drop"
    )

  # if there are missing values then different features will be
  # selected for different samples

  reduced_feature_attrs <- downsampled_df %>%
    dplyr::distinct(!!!rlang::syms(
      c("collapsed_row_number", "ordered_featureId", design$features$variable)
    )) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::group_by(collapsed_row_number) %>%
    dplyr::summarize_all(collapse_feature_vars) %>%
    dplyr::arrange(collapsed_row_number) %>%
    # order featureId by collapsed_row_number
    dplyr::mutate(ordered_featureId = factor(ordered_featureId, levels = ordered_featureId))

  other_attrs <- setdiff(
    colnames(downsampled_df),
    c(value_var, design$features$variable, "ordered_featureId", "ordered_featureId_int")
  )

  downsampled_attributes <- downsampled_df %>%
    dplyr::select(!!!rlang::syms(other_attrs)) %>%
    dplyr::group_by(collapsed_row_number, ordered_sampleId) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    # add back collapsed feature attrs
    dplyr::left_join(reduced_feature_attrs, by = "collapsed_row_number")

  # check that the ordering of ordered_featureId's value are the same
  # as collapsed_row_number
  failed_collapses <- downsampled_attributes %>%
    dplyr::mutate(ordered_featureId_int = as.integer(ordered_featureId)) %>%
    dplyr::filter(ordered_featureId_int != collapsed_row_number)
  if (nrow(failed_collapses != 0)) {
    stop(glue::glue(
      "{nrow(failed_collapses)} downsampled rows were misordered
      this is unexpected behavior"
    ))
  }

  downsampled_tidy_data <- downsampled_attributes %>%
    # combine aggregated and downsampled entries
    dplyr::left_join(
      downsampled_matrix_values,
      by = c("collapsed_row_number", "ordered_sampleId")
    ) %>%
    # discard collapsed_row_number since this
    dplyr::select(!!!rlang::syms(colnames(tidy_data)))

  return(downsampled_tidy_data)
}

collapse_feature_vars <- function(x) {
  if (class(x) %in% c("character")) {
    paste(unique(x), collapse = " & ")
  } else {
    x[1]
  }
}
