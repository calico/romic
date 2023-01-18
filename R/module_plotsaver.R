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
#' @export
shiny_plotsaver_test <- function() {
  shinyApp(
    ui = fluidPage(
      verticalLayout(
        plotsaverInput("ggsave", ui_format = "wide"),
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
        plotsaverServer("ggsave", grob, "foo.png")
      })
    }
  )
}

#' Plot Saver Input
#'
#' UI components for the plot saver module.
#'
#' @inheritParams shiny::moduleServer
#' @param ui_format Set UI appearance
#' \describe{
#'   \item{tall}{stack all UI elements}
#'   \item{wide}{UI elements are side-by-side}
#' }
#'
#' @returns a \code{shiny} UI
#'
#' @export
plotsaverInput <- function(id, ui_format = "tall") {
  ns <- shiny::NS(id)
  checkmate::assertChoice(ui_format, choices = c("tall", "wide"))

  if (ui_format == "tall") {
    shiny::tagList(
      shiny::textInput(
        ns("save_width"),
        "width (inches)",
        value = 8,
        width = "100px"
      ),
      shiny::textInput(
        ns("save_height"),
        "height (inches)",
        value = 8,
        width = "100px"
      ),
      shiny::downloadButton(ns("downloadPlot"), "Save Plot")
    )
  } else if (ui_format == "wide") {
    shiny::fluidRow(
      shiny::column(
        2,
        shiny::textInput(
          ns("save_width"),
          "width (inches)",
          value = 8,
          width = "100px"
        )
      ),
      shiny::column(
        2,
        shiny::textInput(
          ns("save_height"),
          "height (inches)",
          value = 8,
          width = "100px"
        )
      ),
      shiny::column(
        2,
        shiny::downloadButton(ns("downloadPlot"), "Save Plot")
      )
    )
  } else {
    stop("undefined format")
  }
}

#' Plot Saver Server
#'
#' Server components for the plot saver module.
#'
#' @inheritParams shiny::moduleServer
#' @param grob a ggplot2 plot
#' @param filename filename for saving plot. The extension will be respected
#'  by \link[ggplot2]{ggsave}.
#'
#' @returns None
#'
#' @export
plotsaverServer <- function(id, grob, filename = "grob.png") {
  checkmate::assertString(filename)
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
        filename = filename,
        content = function(file) {
          ggsave(
            file,
            plot = grob,
            width = save_width(),
            height = save_height()
          )
        }
      )
    }
  )
}
