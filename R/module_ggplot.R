#' Shiny ggplot Test
#'
#' Test the shiny ggplot module as a stand-alone application.
#'
#' @inheritParams tomic_to
#'
#' @returns A \code{shiny} app
#'
#' @examples
#'
#' if (interactive()) {
#'   shiny_ggplot_test(add_pca_loadings(brauer_2008_triple, npcs = 5))
#'   shiny_ggplot_test(brauer_2008_triple)
#' }
#'
#' @export
shiny_ggplot_test <- function(tomic) {
  checkmate::assertClass(tomic, "tomic")

  shiny::shinyApp(
    ui = shiny::fluidPage(

      # Sidebar with a slider input for the number of bins
      shiny::verticalLayout(
        ggplotOutput("ggplot"),
        shiny::dataTableOutput("selected_df")
      )
    ),
    server = function(input, output, session) {
      selected_data <- shiny::reactive({
        ggplotServer("ggplot", tomic, return_brushed_points = TRUE)
      })

      shiny::observe({
        shiny::req(selected_data()())
        output$selected_df <- shiny::renderDataTable(selected_data()())
      })
    }
  )
}

#' ggplot Output
#'
#' UI components for the ggplot module.
#'
#' @inheritParams shiny::moduleServer
#'
#' @returns A \code{shiny} UI
#'
#' @export
ggplotOutput <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        5,
        shiny::radioButtons(
          ns("data_type"),
          "Data Type",
          selected = "samples",
          choices = c("samples", "features", "measurements"),
          inline = TRUE
        )
      ),
      shiny::column(
        4,
        shiny::radioButtons(
          ns("plot_type"),
          "Plot Type",
          selected = "univariate",
          choices = c("univariate", "bivariate"),
          inline = TRUE
        )
      )
    ),
    shiny::uiOutput(ns("ggplot_ui"))
  )
}

#' ggplot Server
#'
#' Server components for the ggplot module.
#'
#' @inheritParams shiny::moduleServer
#' @inheritParams tomic_to
#' @param return_brushed_points Return values selected on the plot
#'
#' @returns a \code{tibble} of selected observations if
#'   \code{return_brushed_points} is TRUE. Otherwise, returns NULL.
#'
#' @export
ggplotServer <- function(id, tomic, return_brushed_points = FALSE) {
  shiny::moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      checkmate::assertClass(tomic, "tomic")
      checkmate::assertLogical(return_brushed_points, len = 1)

      shiny::observe({
        shiny::req(input$plot_type)
        ns <- session$ns

        if (input$plot_type == "univariate") {
          output$ggplot_ui <- shiny::renderUI({
            ggUnivOutput(ns("univ_grob"), return_brushed_points)
          })
        } else if (input$plot_type == "bivariate") {
          output$ggplot_ui <- shiny::renderUI({
            ggBivOutput(ns("biv_grob"), return_brushed_points)
          })
        } else {
          stop(
            "invalid plot_type, valid types are \"univariate\" and \"bivariate\""
          )
        }
      })

      if (input$plot_type == "univariate") {
        selected_data <- ggUnivServer(
          "univ_grob",
          tomic,
          input$data_type,
          return_brushed_points
        )
      } else if (input$plot_type == "bivariate") {
        selected_data <- ggBivServer(
          "biv_grob",
          tomic,
          input$data_type,
          return_brushed_points
        )
      } else {
        stop(
          "invalid plot_type, valid types are \"univariate\" and \"bivariate\""
        )
      }

      return(selected_data)
    }
  )
}
