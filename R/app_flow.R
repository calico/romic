#' Flow
#'
#' Using \code{shiny} comb through datasets by iterating between
#'   plotting steps, and lassoing steps to select points of interest.
#'
#' @inheritParams tomic_to
#'
#' @returns A \code{shiny} app
#'
#' @examples
#'
#' if (interactive()) {
#'   # library(reactlog)
#'   # reactlog_enable()
#'   app_flow(brauer_2008_triple)
#'   # shiny::reactlogShow()
#' }
#'
#' @export
app_flow <- function(tomic) {
  checkmate::assertClass(tomic, "tomic")

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::headerPanel("Flow"),

      # Sidebar with a slider input for the number of bins
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::actionButton("reset_tomic", "Reset tomic?"),
          shiny::uiOutput("lasso_ui"),
          shiny::textOutput("tomic_log_out")
        ),
        shiny::mainPanel(
          ggplotOutput("ggplot"),
          shiny::dataTableOutput("selected_df")
        )
      )
    ),
    server = function(input, output, session) {
      working_tomic <- shiny::reactiveVal(tomic)
      tomic_log <- shiny::reactiveVal(c())

      shiny::observeEvent(input$reset_tomic, {
        # reset reactive values
        working_tomic(tomic)
        tomic_log(c())
        output$tomic_log_out <- renderText("tomic reset")
      })

      # update selected data
      selected_data <- shiny::reactive({
        shiny::req(working_tomic())
        ggplotServer("ggplot", working_tomic(), return_brushed_points = TRUE)
      })

      # render selected data
      shiny::observe({
        shiny::req(selected_data()())
        output$selected_df <- shiny::renderDataTable(selected_data()())
      })

      # add the lasso UI if observations are selected

      output$lasso_ui <- shiny::renderUI({
        ns <- session$ns
        shiny::conditionalPanel(
          condition = nrow(selected_data()()) > 0,
          lassoInput(ns("lasso"))
        )
      })

      # update a running dataset requires reactive values
      # and updating them appropriately is best done with observeEvent.
      # the rub is that the event trigger is buried in the lasso module
      observeEvent(input[["lasso-do_lasso_method"]],
        {
          # apply lasso options to generate an updated tomic and a
          # logging message
          tomic_mod <- lassoServer(
            "lasso",
            working_tomic(),
            selected_data()()
          )
          # update working tomic data
          working_tomic(tomic_mod()$tomic)
          # update logging of flow operations
          new_message <- tomic_mod()$lasso_message
          tomic_log(c(tomic_log(), new_message))

          log_out <- paste(
            paste(seq_along(tomic_log()), tomic_log(), sep = ": "),
            collapse = " >--> "
          )
          output$tomic_log_out <- shiny::renderText(log_out)
        },
        label = "update reactive tomic"
      )
    },
    options = list(height = 1000)
  )
}
