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
#' @param x_label label for x-axis (if NULL then use \code{feature_var})
#' @param y_label label for y-axis (if NULL then use \code{sample_var})
#' @param colorbar_label label for color-bar; default is log2 abundance
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
    max_display_features = 800,
    x_label = NULL,
    y_label = NULL,
    colorbar_label = NULL
    ) {
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

  if ("NULL" %in% class(x_label)) {
    x_label <- feature_var
  }
  checkmate::assertMultiClass(x_label, c("character", "expression"))

  if ("NULL" %in% class(y_label)) {
    y_label <- sample_var
  }
  checkmate::assertMultiClass(y_label, c("character", "expression"))

  if ("NULL" %in% class(colorbar_label)) {
    colorbar_label <- expression(log[2] ~ abundance)
  }
  checkmate::assertMultiClass(colorbar_label, c("character", "expression"))

  # format convert tomic to tidy format if needed

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
       strip.text = element_text(size = 18),
       legend.position = "top",
       strip.background = element_rect(fill = "gray80")
    )

  if (n_features > 200) {
    heatmap_theme <- heatmap_theme +
      theme(axis.text.y = element_blank())
  } else {
    heatmap_theme <- heatmap_theme +
      theme(axis.text.y = element_text(size = pmin(20, 60 * sqrt(1 / n_features))))
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
    aes(
      x = !!rlang::sym("ordered_sampleId"),
      y = !!rlang::sym("ordered_featureId"),
      fill = !!rlang::sym(value_var)
    )
  ) +
    geom_raster() +
    scale_fill_gradient2(
      colorbar_label,
      low = "steelblue1",
      mid = "black",
      high = "yellow",
      midpoint = 0
    ) +
    scale_x_discrete(
      x_label,
      breaks = augmented_tidy_omic_data$ordered_sampleId,
      labels = augmented_tidy_omic_data$sample_label
    ) +
    scale_y_discrete(
      y_label,
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
