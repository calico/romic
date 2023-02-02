#' Shiny ggBivariate Test
#'
#' Test the shiny ggBivariate module as a stand-alone application.
#'
#' @inheritParams tomic_to
#' @inheritParams ggBivServer
#'
#' @returns a \code{shiny} app
#'
#' @examples
#'
#' if (interactive()) {
#'   shiny_ggbiv_test(
#'     add_pcs(brauer_2008_triple, npcs = 5),
#'     plot_table = "samples"
#'   )
#'   shiny_ggbiv_test(
#'     brauer_2008_triple,
#'     plot_table = "measurements"
#'   )
#' }
#' @export
shiny_ggbiv_test <- function(tomic, plot_table = "samples") {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertChoice(plot_table, c("features", "samples", "measurements"))

  shiny::shinyApp(
    ui = shiny::fluidPage(

      # Sidebar with a slider input for the number of bins
      shiny::verticalLayout(
        ggBivOutput("ggplot", return_brushed_points = TRUE),
        shiny::dataTableOutput("selected_df")
      )
    ),
    server = function(input, output, session) {
      selected_data <- ggBivServer(
        "ggplot",
        tomic,
        plot_table,
        return_brushed_points = TRUE
      )
      output$selected_df <- shiny::renderDataTable(selected_data())
    }
  )
}

#' ggBivariate Output
#'
#' UI components for the ggBivariate module.
#'
#' @inheritParams shiny::moduleServer
#' @inheritParams ggBivServer
#'
#' @returns A \code{shiny} UI
#'
#' @export
ggBivOutput <- function(id, return_brushed_points = FALSE) {
  checkmate::assertLogical(return_brushed_points, len = 1)

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(ns("x_var"), "X-axis", choices = NULL),
        shiny::selectInput(ns("y_var"), "Y-axis", choices = NULL),
        shiny::checkboxInput(
          ns("use_color"),
          label = "Color values?",
          value = FALSE
        ),
        shiny::uiOutput(ns("color_ui"))
      )
    ),
    shiny::plotOutput(
      ns("ggplot"),
      brush = brush_config(return_brushed_points, "xy", ns)
    ),
    plotsaverInput(ns("ggsave"), ui_format = "wide")
  )
}

#' ggBivariate Server
#'
#' Server components for the ggBivariate module.
#'
#' @inheritParams shiny::moduleServer
#' @inheritParams tomic_to
#' @param plot_table table containing the data to be plotted
#' @param return_brushed_points Return values selected on the plot
#'
#' @returns a tomic_table if return_brushed_points is TRUE, and 0 otherwise
#'
#' @export
ggBivServer <- function(id, tomic, plot_table, return_brushed_points = FALSE) {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertChoice(plot_table, c("features", "samples", "measurements"))
  checkmate::assertLogical(return_brushed_points, len = 1)

  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # populate attributes for

      all_vars <- get_design_vars(tomic, plot_table, "all")
      quantitative_vars <- get_design_vars(tomic, plot_table, "quantitative")
      categorical_vars <- setdiff(all_vars, quantitative_vars)

      if (plot_table == "measurements") {
        # measurements have access to all attributes
        all_vars <- sort(unique(c(
          get_design_vars(tomic, "features", "all"),
          get_design_vars(tomic, "samples", "all"),
          get_design_vars(tomic, "measurements", "all")
        )))
        quantitative_vars <- sort(unique(c(
          get_design_vars(tomic, "features", "quantitative"),
          get_design_vars(tomic, "samples", "quantitative"),
          get_design_vars(tomic, "measurements", "quantitative")
        )))
        categorical_vars <- setdiff(all_vars, quantitative_vars)
      }

      shiny::updateSelectInput(session, "x_var", choices = all_vars)

      available_yvars <- shiny::reactive({
        shiny::req(input$x_var)

        setdiff(quantitative_vars, input$x_var)
      })

      shiny::observe({
        shiny::req(available_yvars())
        if (length(available_yvars()) == 0) {
          warning("No valid y-variables, a bivariate plot cannot be created")
        }

        shiny::updateSelectInput(session, "y_var", choices = available_yvars())
      })

      available_colors <- shiny::reactive({
        req(input$x_var, input$y_var)

        # if x is numeric then color can be numeric also
        if (input$x_var %in% quantitative_vars) {
          available_colors <- setdiff(all_vars, c(input$x_var, input$y_var))
        } else {
          available_colors <- setdiff(
            categorical_vars,
            c(input$x_var, input$y_var)
          )
        }
        available_colors
      })

      shiny::observe({
        # disable color if there aren't any available colors
        shiny::req(available_colors())

        if (length(available_colors) == 0) {
          shiny::updateCheckboxInput(session, "use_color", value = FALSE)
        }
      })

      output$color_ui <- shiny::renderUI({
        shiny::req(input$use_color, available_colors())
        ns <- session$ns

        # generate color UI if colors are available and desired (checkbox)
        shiny::conditionalPanel(
          condition = input$use_color,
          selectInput(ns("color_var"), "Color By", choices = available_colors())
        )
      })

      # pull out the relevant data table

      if (plot_table == "measurements") {
        tomic_table <- tomic_to(tomic, "tidy_omic")$data
      } else {
        tomic_table <- tomic_to(tomic, "triple_omic")[[plot_table]]
      }

      # plot

      shiny::observe({
        shiny::req(tomic_table, input$x_var, input$y_var)

        if (input$use_color) {
          shiny::req(input$color_var)
          color_var <- input$color_var
        } else {
          color_var <- NULL
        }

        if (length(available_yvars()) == 0) {
          grob <- invalid_grob(
            "No valid y-variables, a bivariate plot cannot be created"
          )
        } else {
          grob <- try(
            plot_bivariate(tomic_table, input$x_var, input$y_var, color_var),
            silent = TRUE
          )
          if ("try-error" %in% class(grob)) {
            grob <- invalid_grob("Plot not available yet")
          }
        }

        output$ggplot <- renderPlot(grob)
        plotsaverServer("ggsave", grob)
      })

      if (return_brushed_points) {
        brushed_entries <- shiny::reactive({
          shiny::req(tomic_table, input$plot_brush)
          try_brushedPoints(df = tomic_table, input$plot_brush)
        })

        return(brushed_entries)
      } else {
        return(invisible(0))
      }
    }
  )
}

#' Bivariate Plot
#'
#' Create a scatter or boxplot from a tomic dataset.
#'
#' @param tomic_table A table taken from a tidy (i.e., augmented measurements)
#'   or triple omic dataset
#' @param x_var x-axis variable
#' @param y_var y-axis variable
#' @param color_var coloring variable (NULL to suppress coloring)
#' @param shape_var shape variable (NULL to suppress shape)
#' @param alpha_var alpha variable or numeric for constant alpha (NULL to suppress alpha)
#' @param size_var size variable or integer/numeric for constant size (NULL to suppress size)
#'
#' @return a ggplot2 grob
#'
#' @examples
#' library(dplyr)
#'
#' brauer_augmented <- brauer_2008_tidy %>%
#'   add_pcs(npcs = 5) %>%
#'   tomic_to("triple_omic")
#'
#' tomic_table <- brauer_augmented$samples
#' plot_bivariate(tomic_table, "PC1", "PC2", "nutrient", "nutrient", 0.5, 10)
#' plot_bivariate(tomic_table, "PC1", "PC2", NULL)
#' plot_bivariate(tomic_table, "nutrient", "PC2", "nutrient")
#' @export
plot_bivariate <- function(tomic_table, x_var, y_var, color_var = NULL, shape_var = NULL, alpha_var = NULL, size_var = NULL) {
  checkmate::assertClass(tomic_table, "data.frame")
  # allow for partial string matching
  x_var <- var_partial_match(x_var, tomic_table)
  y_var <- var_partial_match(y_var, tomic_table)

  aes_args <- list(
    x = rlang::sym(x_var),
    y = rlang::sym(y_var)
  )

  if (class(tomic_table[[x_var]]) %in% c("numeric", "integer")) {
    plot_type <- "scatter"
  } else {
    plot_type <- "boxplot"
  }

  # setup list for fixed aesthetics, e.g., size or alpha
  geom_dots <- list()

  # color
  if (!inherits(color_var, "NULL")) {
    color_var <- var_partial_match(color_var, tomic_table)

    if (!(class(tomic_table[[color_var]]) %in% c("numeric", "integer"))) {
      distinct_color_levels <- unique(tomic_table[[color_var]])

      if (length(distinct_color_levels) > 30) {
        overflow_plot <- invalid_grob(glue::glue(
          "Too many categorical colors to display ({length(distinct_color_levels)})
          color by a different variable or suppress coloring with NULL"
        ))
        return(overflow_plot)
      }
    }

    if (plot_type == "scatter") {
      aes_args$color = rlang::sym(color_var)
    } else {
      aes_args$fill = rlang::sym(color_var)
    }
  }

  # shape
  if (!inherits(shape_var, "NULL")) {
    shape_var <- var_partial_match(shape_var, tomic_table)
    aes_args$shape <- rlang::sym(shape_var)
  }

  # alpha
  if (!inherits(alpha_var, "NULL")) {
    if (inherits(alpha_var, "numeric")) {
      checkmate::assertNumber(alpha_var, lower = 0, upper = 1)
      geom_dots$alpha <- alpha_var
    } else {
      alpha_var <- var_partial_match(alpha_var, tomic_table)
      aes_args$alpha <- rlang::sym(alpha_var)
    }
  }

  # size
  # by default ignore size as a number for specifying constant size
  if (!inherits(size_var, "NULL")) {
    if (class(size_var) %in% c("numeric", "integer")) {
      geom_dots$size <- size_var
    } else {
      # see if size matches one of the df's vars
      size_var <- var_partial_match(size_var, tomic_table)
      aes_args$size <- rlang::sym(size_var)
    }
  }

  # map requiredd and optional inputs to aesthetics
  running_aes <- do.call(ggplot2::aes, aes_args)

  # determine plot type from variable classes

  if (plot_type == "scatter") {

    # setup plot call with fixed aesthetics
    plot_call <- do.call(ggplot2::geom_point, geom_dots)
    grob <- ggplot(tomic_table, running_aes) +
      plot_call +
      theme_bw()

  } else if (plot_type == "boxplot") {

    plot_call <- do.call(ggplot2::geom_boxplot, geom_dots)
    grob <- ggplot(tomic_table, running_aes) +
      plot_call +
      theme_bw() +
      theme(axis.text = element_text(angle = 90, hjust = 1))
  } else {
    stop ("undefined plot_type")
  }

  return(grob)
}




invalid_grob <- function(message) {
  ggplot(data.frame(x = 0, y = 0), aes(x = x, y = y)) +
    geom_text(label = message, size = 10) +
    theme(text = element_blank(), line = element_blank())
}

brush_config <- function(return_brushed_points, dir = "xy", ns) {
  checkmate::assertLogical(return_brushed_points, len = 1)
  checkmate::assertChoice(dir, c("xy", "x", "y"))

  if (return_brushed_points) {
    brush <- shiny::brushOpts(
      ns("plot_brush"),
      resetOnNew = TRUE,
      direction = dir
    )
  } else {
    brush <- NULL
  }

  return(brush)
}

get_design_vars <- function(tomic, plot_table, filter_type) {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertChoice(plot_table, c("features", "samples", "measurements"))
  checkmate::assertChoice(filter_type, c("categorical", "quantitative", "all"))

  variable_types <- tomic$design[[plot_table]] %>%
    dplyr::mutate(category = dplyr::case_when(
      type %in% c("numeric", "integer") ~ "quantitative",
      TRUE ~ "categorical"
    ))

  if (filter_type == "all") {
    return(variable_types$variable)
  } else if (filter_type == "categorical") {
    variable_types$variable[variable_types$category == "categorical"]
  } else if (filter_type == "quantitative") {
    variable_types$variable[variable_types$category == "quantitative"]
  } else {
    stop("invalid filter_type")
  }
}

#' Try brushedPoints
#'
#' This function wraps brushedPoints in a try statement to catch cases where
#'   the brushing is out-of-sync with the df that is selected.
#'
#' @param ... args to pass to \link[shiny]{brushedPoints}
#'
#' @returns a df of brushed points
try_brushedPoints <- function(...) {
  obs <- try(brushedPoints(...), silent = TRUE)

  if ("try-error" %in% class(obs)) {
    return(tibble::tibble())
  } else {
    return(obs)
  }
}
