#' Shiny ggUnivariate Test
#'
#' Test the shiny ggUnivariate module as a stand-alone application.
#'
#' @inheritParams tomic_to
#' @inheritParams ggBivServer
#'
#' @returns A \code{shiny} app
#'
#' @examples
#'
#' if (interactive()) {
#'   shiny_gguniv_test(
#'     add_pcs(brauer_2008_triple, npcs = 5),
#'     plot_table = "samples"
#'   )
#'   shiny_gguniv_test(brauer_2008_triple, plot_table = "measurements")
#'   shiny_gguniv_test(brauer_2008_triple, plot_table = "features")
#' }
#' @export
shiny_gguniv_test <- function(tomic, plot_table = "samples") {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertChoice(plot_table, c("features", "samples", "measurements"))

  shiny::shinyApp(
    ui = shiny::fluidPage(

      # Sidebar with a slider input for the number of bins
      shiny::verticalLayout(
        ggUnivOutput("ggplot", return_brushed_points = TRUE),
        shiny::dataTableOutput("selected_df")
      )
    ),
    server = function(input, output, session) {
      selected_data <- ggUnivServer(
        "ggplot",
        tomic,
        plot_table,
        return_brushed_points = TRUE
      )
      output$selected_df <- renderDataTable(selected_data())
    }
  )
}

#' ggUnivariate Output
#'
#' UI components for the ggUnivariate module.
#'
#' @inheritParams shiny::moduleServer
#' @inheritParams ggplotServer
#'
#' @returns A \code{shiny} UI
#'
#' @export
ggUnivOutput <- function(id, return_brushed_points = FALSE) {
  checkmate::assertLogical(return_brushed_points, len = 1)

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::selectInput(ns("x_var"), "X-axis", choices = NULL),
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
      brush = brush_config(return_brushed_points, "x", ns)
    ),
    plotsaverInput(ns("ggsave"), ui_format = "wide")
  )
}

#' ggUnivariate Server
#'
#' Server components for the ggUnivariate module
#'
#' @inheritParams ggBivServer
#'
#' @returns a tomic_table if return_brushed_points is TRUE, and 0 otherwise.
#'
#' @export
ggUnivServer <- function(id, tomic, plot_table, return_brushed_points = FALSE) {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertChoice(plot_table, c("features", "samples", "measurements"))
  checkmate::assertLogical(return_brushed_points, len = 1)

  shiny::moduleServer(
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

      if (length(quantitative_vars) == 0) {
        warning("No numeric variables; a univariate plot is not possible")
      }
      shiny::updateSelectInput(session, "x_var", choices = quantitative_vars)

      available_colors <- shiny::reactive({
        shiny::req(input$x_var)
        setdiff(categorical_vars, input$x_var)
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
          shiny::selectInput(
            ns("color_var"),
            "Color By",
            choices = available_colors()
          )
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
        shiny::req(tomic_table, input$x_var)

        if (input$use_color) {
          shiny::req(input$color_var)
          color_var <- input$color_var
        } else {
          color_var <- NULL
        }

        if (length(quantitative_vars) == 0) {
          grob <- invalid_grob(
            "No numeric variables; a univariate plot is not possible"
          )
        } else {
          grob <- try(
            plot_univariate(tomic_table, input$x_var, color_var),
            silent = TRUE
          )
          if ("try-error" %in% class(grob)) {
            grob <- invalid_grob("Plot not available yet")
          }
        }

        output$ggplot <- shiny::renderPlot(grob)
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

#' Univariate Plot
#'
#' Create a histogram from a tomic dataset.
#'
#' @inheritParams plot_bivariate
#'
#' @returns A ggplot2 grob
#'
#' @examples
#' library(dplyr)
#'
#' brauer_augmented <- brauer_2008_tidy %>%
#'   add_pcs(npcs = 5) %>%
#'   tomic_to("triple_omic")
#'
#' plot_univariate(brauer_augmented$samples, "PC1", "nutrient")
#' plot_univariate(brauer_augmented$measurements, "expression", NULL)
#' @export
plot_univariate <- function(tomic_table, x_var, color_var = NULL) {
  checkmate::assertClass(tomic_table, "data.frame")

  x_var <- var_partial_match(x_var, tomic_table)
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
  }

  # determine plot type from variable classes

  if (!class(tomic_table[[x_var]]) %in% c("numeric", "integer")) {
    stop("Univariate plot only accepts a numeric/integer x-axis variable")
  }

  grob <- ggplot(tomic_table, aes(x = !!rlang::sym(x_var))) +
    theme_bw()

  if (is.null(color_var)) {
    grob <- grob +
      geom_histogram(bins = 50)
  } else {
    grob <- grob +
      geom_histogram(aes(fill = !!rlang::sym(color_var)), bins = 50)
  }

  return(grob)
}
