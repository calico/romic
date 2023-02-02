#' Interactive Heatmap
#'
#' Generate a \code{shiny} interactive heatmap that allows for on demand
#'   filtering, ordering and faceting by variables of interest.
#'
#' @inheritParams tomic_to
#'
#' @returns A \code{shiny} app
#'
#' @examples
#'
#' if (interactive()) {
#'   app_heatmap(brauer_2008_tidy)
#' }
#' @export
app_heatmap <- function(tomic) {
  checkmate::assertClass(tomic, "tomic")

  shinyApp(
    ui = fluidPage(
      tags$head(tags$style(
        type = "text/css",
        "h1, h2, h3, h4, h5, h6 { color: #5BB867;}",
        "label { font-size: 20px;}",
        "div { font-size: 15px;}",
        "body {width: 100% !important; max-width: 100% !important;}"
      )),

      # Application title
      headerPanel("Interactive Heatmap"),

      # Sidebar with a slider input for the number of bins
      sidebarLayout(
        sidebarPanel(
          tags$div(
            HTML("<h4>Filter</h4>")
          ),
          filterInput("filter_features", "features"),
          filterInput("filter_samples", "samples"),
          tags$div(
            HTML("<h4>Organize</h4>")
          ),
          wellPanel(
            selectizeInput("feature_facets", "Separate features by:",
              choices = NULL, multiple = TRUE
            ),
            selectizeInput("sample_facets", "Separate samples by:",
              choices = NULL, multiple = TRUE
            )
          ),
          organizeInput("organize"),
          tags$div(
            HTML("<h4>Visualize</h4>")
          ),
          wellPanel(
            selectInput("measurement_var", "Heatmap measurement variable:",
              choices = NULL
            ),
            checkboxInput("do_floor_values", "Floor values?", value = FALSE),
            conditionalPanel(
              condition = "input.do_floor_values == true",
              sliderInput("floor_value", "Floor magnitude:",
                min = 0, max = 5, value = 3, step = 0.2
              )
            )
          ),
          tags$div(
            HTML("<h4>Save</h4>")
          ),
          plotsaverInput("ggsave")
        ),
        mainPanel(plotOutput("heatmap", height = "1000px"))
      )
    ),
    server = function(input, output, session) {
      # defining options available to user for sorting and filtering
      design <- tomic$design

      # create tomic from tidy_omic or triple_omic

      tidy_omic <- reactive({
        tomic_to(tomic, "tidy_omic")
      })

      # call filtering module

      tidy_filtered_features <- reactive({
        req(tidy_omic())
        tidy_filtered_features <- filterServer(
          "filter_features",
          tidy_omic(),
          "features"
        )
      })

      tidy_filtered_samples <- reactive({
        req(tidy_filtered_features())
        tidy_filtered_samples <- filterServer(
          "filter_samples",
          tidy_filtered_features()(),
          "samples"
        )
      })

      observe({
        # need double parenthesis since its a reactive of a reactive
        print(glue::glue(
          "Filtering results: tidy_filtered_samples is {nrow(tidy_filtered_samples()()$data)} rows"
        ))
      })

      # setup feature and sample facets
      feature_facet_options <- design$features$variable[
        design$features$type %in% c("character", "factor", "ordered")
      ]
      updateSelectizeInput(session, "feature_facets",
        choices = feature_facet_options, selected = NULL
      )

      sample_facet_options <- design$samples$variable[
        design$samples$type %in% c("character", "factor", "ordered")
      ]
      updateSelectizeInput(session, "sample_facets",
        choices = sample_facet_options, selected = NULL
      )

      # define facet formula

      facet_expression <- shiny::reactive({
        paste0(
          ifelse(
            class(input$feature_facets) == "NULL",
            "",
            paste(input$feature_facets, collapse = " + ")
          ),
          " ~ ",
          ifelse(
            class(input$sample_facets) == "NULL",
            ".",
            paste(input$sample_facets, collapse = " + ")
          )
        )
      })

      shiny::observe({
        shiny::req(facet_expression())
        print(glue::glue("Faceting with formula: {facet_expression()}"))
      })

      # define measurement variables

      # specify measurement variable
      measurement_vars <- design$measurements$variable[
        design$measurements$type == "numeric"
      ]
      shiny::updateSelectInput(
        session,
        "measurement_var",
        choices = measurement_vars
      )

      # sort samples

      tidy_organized <- reactive({
        req(tidy_filtered_samples()())

        organizeServer(
          "organize",
          tidy_filtered_samples()(),
          feature_vars = setdiff(
            design$features$variable,
            input$feature_facets
          ),
          sample_vars = setdiff(
            design$samples$variable,
            input$sample_facets
          ),
          input$measurement_var
        )
      })

      shiny::observe({
        shiny::req(tidy_organized()())
        print(glue::glue(
          "Organization sort status: {tomic_sort_status(tidy_organized()())}"
        ))
      })

      thresholded_val <- shiny::reactive({
        if (input$do_floor_values) {
          input$floor_value
        } else {
          Inf
        }
      })

      # create heatmap

      heatmap_plot <- shiny::reactive({
        # create a heatmap and if not possibly return try-error

        shiny::req(tidy_organized()())

        try(
          plot_heatmap(
            tidy_organized()(),
            feature_var = NULL,
            sample_var = NULL,
            value_var = input$measurement_var,
            cluster_dim = "both",
            change_threshold = thresholded_val(),
            plot_type = "grob",
            # suppress feature aggregatin when feature facets are present
            max_display_features = ifelse(
              is.null(input$feature_facets),
              800,
              Inf
            )
          ),
          silent = TRUE
        )
      })

      heatmap_plot_sanitize <- shiny::reactive({
        if ("try-error" %in% class(heatmap_plot())) {
          # return a blank plot
          ggplot(data.frame(x = 0, y = 0), aes(x = x, y = y)) +
            geom_text(label = "No data available", size = 15) +
            theme(text = element_blank(), line = element_blank())
        } else {
          # return either a faceted or unfaced plot
          if (!(is.null(input$feature_facets) & is.null(input$sample_facets))) {
            heatmap_plot() +
              facet_grid(
                stats::as.formula(facet_expression()),
                space = "free",
                scales = "free"
              )
          } else {
            heatmap_plot()
          }
        }
      })

      output$heatmap <- shiny::renderPlot({
        heatmap_plot_sanitize()
      })

      # save heatmap
      shiny::observe({
        shiny::req(heatmap_plot_sanitize())
        plotsaverServer("ggsave", heatmap_plot_sanitize())
      })
    },
    options = list(height = 1000)
  )
}

#' Plot Heatmap
#'
#' Generate a heatmap visualization of a features x samples matrix of
#'   measurements.
#'
#' @inheritParams tomic_to
#' @param feature_var variable from "features" to use as a unique feature
#'   label.
#' @param sample_var variable from "samples" to use as a unique sample label.
#' @param value_var which variable in "measurements" to use for quantification.
#' @inheritParams hclust_order
#' @param change_threshold values with a more extreme absolute change will be
#'   thresholded to this value.
#' @param plot_type plotly (for interactivity) or grob (for a static ggplot)
#' @inheritParams downsample_heatmap
#'
#' @returns a ggplot2 grob
#'
#' @examples
#'
#' library(dplyr)
#'
#' tomic <- brauer_2008_triple %>%
#'   filter_tomic(
#'     filter_type = "category",
#'     filter_table = "features",
#'     filter_variable = "BP",
#'     filter_value = c(
#'       "protein biosynthesis",
#'       "rRNA processing", "response to stress"
#'     )
#'   )
#'
#' plot_heatmap(
#'   tomic = tomic,
#'   value_var = "expression",
#'   change_threshold = 5,
#'   cluster_dim = "rows",
#'   plot_type = "grob",
#'   distance_measure = "corr"
#' )
#' @export
plot_heatmap <- function(
    tomic,
    feature_var = NULL,
    sample_var = NULL,
    value_var = NULL,
    cluster_dim = "both",
    distance_measure = "dist",
    hclust_method = "ward.D2",
    change_threshold = Inf,
    plot_type = "grob",
    max_display_features = 800) {
  checkmate::assertClass(tomic, "tomic")

  if ("NULL" %in% class(feature_var)) {
    feature_var <- tomic$design$feature_pk
  }
  checkmate::assertChoice(feature_var, tomic$design$features$variable)

  if ("NULL" %in% class(sample_var)) {
    sample_var <- tomic$design$sample_pk
  }
  checkmate::assertChoice(sample_var, tomic$design$samples$variable)

  value_var <- value_var_handler(value_var, tomic$design)

  checkmate::assertChoice(cluster_dim, c("columns", "rows", "both"))
  checkmate::assertChoice(distance_measure, c("corr", "dist"))
  checkmate::assertString(hclust_method)
  checkmate::assertNumber(change_threshold, lower = 0)
  checkmate::assertChoice(plot_type, c("plotly", "grob"))
  checkmate::assertNumber(max_display_features)

  tidy_omic <- tomic_to(tomic, "tidy_omic")

  # convert groupId and sampleId to factors so they are ordered appropriately

  if (tomic_sort_status(tidy_omic)[1] == "fully sorted") {
    # pre-sorted data
    clustered_tidy_omic <- tidy_omic
    # add fields that would be expected had organization occurred
    # using hclust_tidy_omic()
    clustered_tidy_omic$data <- clustered_tidy_omic$data %>%
      dplyr::mutate(
        ordered_featureId = !!rlang::sym(clustered_tidy_omic$design$feature_pk),
        feature_label = format_names_for_plotting(ordered_featureId),
        ordered_sampleId = !!rlang::sym(clustered_tidy_omic$design$sample_pk),
        sample_label = format_names_for_plotting(ordered_sampleId)
      )
  } else {
    clustered_tidy_omic <- hclust_tidy_omic(
      tidy_omic = tidy_omic,
      feature_var = feature_var,
      sample_var = sample_var,
      value_var = value_var,
      cluster_dim = cluster_dim,
      distance_measure = distance_measure,
      hclust_method = hclust_method
    )
  }

  augmented_tidy_omic_data <- clustered_tidy_omic$data %>%
    # threshold max
    dplyr::mutate(!!rlang::sym(value_var) := pmax(
      pmin(!!rlang::sym(value_var), change_threshold),
      -1 * change_threshold
    ))

  # downsample to speed to up heatmap rendering
  augmented_tidy_omic_data <- downsample_heatmap(
    tidy_data = augmented_tidy_omic_data,
    value_var = value_var,
    design = tomic$design,
    max_display_features = max_display_features
  )

  # figure out font sizes for row labels
  feature_pk <- tomic$design$feature_pk
  sample_pk <- tomic$design$sample_pk

  n_features <- augmented_tidy_omic_data %>%
    dplyr::distinct(!!rlang::sym(feature_pk)) %>%
    nrow()

  n_samples <- augmented_tidy_omic_data %>%
    dplyr::distinct(!!rlang::sym(sample_pk)) %>%
    nrow()

  heatmap_theme <- theme_minimal() +
     theme(
       text = element_text(size = 16, color = "black"),
       title = element_text(size = 20, color = "black"),
       axis.title.y = element_blank(),
       strip.text = element_text(size = 18),
       legend.position = "top",
       strip.background = element_rect(fill = "gray80")
    )

  if (n_features > 200) {
    heatmap_theme <- heatmap_theme + theme(axis.text.y = element_blank())
  } else {
    heatmap_theme <- heatmap_theme + theme(axis.text.y = element_text(size = pmin(20, 60 * sqrt(1 / n_features))))
  }

  if (n_samples > 200) {
    heatmap_theme <- heatmap_theme + theme(axis.text.x = element_blank())
  } else {
    heatmap_theme <- heatmap_theme + theme(axis.text.x = element_text(
      size = pmin(20, 60 * sqrt(1 / n_samples)),
      angle = 90,
      hjust = 1
    ))
  }

  heatmap_plot <- ggplot(
    augmented_tidy_omic_data,
    aes_string(
      x = "ordered_sampleId",
      y = "ordered_featureId",
      fill = value_var
    )
  ) +
    geom_raster() +
    scale_fill_gradient2(
      expression(log[2] ~ abundance),
      low = "steelblue1",
      mid = "black",
      high = "yellow",
      midpoint = 0
    ) +
    scale_x_discrete(sample_var,
      breaks = augmented_tidy_omic_data$ordered_sampleId,
      labels = augmented_tidy_omic_data$sample_label
    ) +
    scale_y_discrete(feature_var,
      breaks = augmented_tidy_omic_data$ordered_featureId,
      labels = augmented_tidy_omic_data$feature_label,
      position = "right"
    ) +
    expand_limits(fill = c(-1 * change_threshold, change_threshold)) +
    heatmap_theme

  if (plot_type == "grob") {
    return(heatmap_plot)
  } else if (plot_type == "plotly") {
    suppressWarnings(
      plotly::ggplotly(heatmap_plot) %>%
        plotly::layout(margin = 0)
    )
  } else {
    stop("undefined plotting type logic")
  }
}


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
#'
#' @returns tidy_data with rows collapsed if the number of distinct features is
#'   greater than \code{max_display_features}
#'
downsample_heatmap <- function(
    tidy_data,
    value_var,
    design,
    max_display_features = 1000) {
  checkmate::assertDataFrame(tidy_data)
  checkmate::assertChoice(value_var, colnames(tidy_data))
  checkmate::assertNumber(max_display_features)

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
  message(glue::glue(
    "Downsampling {n_features} features to {realized_max_display_features}, targeting {max_display_features}"
  ))

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
