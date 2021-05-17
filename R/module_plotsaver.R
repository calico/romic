#' Shiny Plot Saver Test
#'
#' Test the shiny plotsaver module as a stand-alone application.
#'
#' @returns a \code{shiny} app
#'
#' @examples
#'
#' if (interactive()) {
#'   shiny_plotsaver_test()
#' }
#'
#' @export
shiny_plotsaver_test <- function() {
  shinyApp(
    ui = fluidPage(
      verticalLayout(
        plotsaverInput("ggsave"),
        plotOutput("ggplot")
      )
    ),
    server = function(input, output, session) {
      grob <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
        geom_point(shape = 1)

      output$ggplot <- renderPlot({
        grob
      })

      observe({
        plotsaverServer("ggsave", grob)
      })
    }
  )
}

#' Plot Saver Input
#'
#' UI components for the plot saver module.
#'
#' @inheritParams shiny::moduleServer
#'
#' @returns a \code{shiny} UI
#'
#' @export
plotsaverInput <- function(id) {
  ns <- NS(id)

  tagList(
    textInput(
      ns("save_width"),
      "width (inches)",
      value = 8,
      width = "100px"
    ),
    textInput(
      ns("save_height"),
      "height (inches)",
      value = 8,
      width = "100px"
    ),
    downloadButton(ns("downloadPlot"), "Save Plot")
  )
}

#' Plot Saver Server
#'
#' Server components for the plot saver module.
#'
#' @inheritParams shiny::moduleServer
#' @param grob a ggplot2 plot
#'
#' @returns None
#'
#' @export
plotsaverServer <- function(id, grob) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      save_width <- reactive({
        as.numeric(input$save_width)
      })
      checkmate::assertNumber(save_width(), lower = 0.1)

      save_height <- reactive({
        as.numeric(input$save_height)
      })
      checkmate::assertNumber(save_height(), lower = 0.1)

      output$downloadPlot <- downloadHandler(
        filename = "grob.png",
        content = function(file) {
          ggsave(
            file,
            plot = grob,
            device = "png",
            width = save_width(),
            height = save_height()
          )
        }
      )
    }
  )
}
