#' Shiny Sort Test
#'
#' Test the shiny sorting module as a stand-alone app.
#'
#' @inheritParams check_triple_omic
#' @inheritParams sortServer
#'
#' @returns a \code{shiny} app
#'
#' @examples
#'
#' if (interactive()) {
#'   shiny_sort_test(brauer_2008_triple,
#'     valid_sort_vars = c("sample", "nutrient", "DR"),
#'     value_var = "expression"
#'   )
#' }
#' @export
shiny_sort_test <- function(triple_omic, valid_sort_vars, value_var) {
  checkmate::assertClass(triple_omic, "triple_omic")

  shinyApp(
    ui = fluidPage(

      # Sidebar with a slider input for the number of bins
      sidebarLayout(
        sidebarPanel(
          sortInput("sort_samples", "samples")
        ),
        mainPanel()
      )
    ),
    server = function(input, output, session) {
      sorted_samples <- sortServer(
        "sort_samples",
        triple_omic,
        "samples",
        valid_sort_vars,
        value_var
      )

      observe({
        req(sorted_samples())
        sample_classes <- paste(
          levels(sorted_samples()$samples$sample),
          collapse = ", "
        )
        print(glue::glue(
          "Sorting results: sorted_samples's samples have the class {sample_classes}"
        ))
      })
    }
  )
}

#' Sort Input
#'
#' UI components for the sort module.
#'
#' @inheritParams shiny::moduleServer
#' @param sort_table table to sort
#'
#' @returns A \code{shiny} UI
#'
#' @export
sortInput <- function(id, sort_table) {
  ns <- NS(id)

  tagList(
    radioButtons(
      ns("sort_mode"),
      glue::glue("Sort {sort_table} by:"),
      choices = c("hclust", "category")
    ),
    uiOutput(ns("sort_ui"))
  )
}

#' Sort Server
#'
#' Server components for the sort module.
#'
#' @inheritParams shiny::moduleServer
#' @inheritParams tomic_to
#' @param valid_sort_vars variables available for categorical arranging
#' @inheritParams sort_tomic
#'
#' @returns A sorted \code{tomic} object.
#'
#' @export
sortServer <- function(id,
                       tomic,
                       sort_table,
                       valid_sort_vars = NULL,
                       value_var = NULL) {
  checkmate::assertClass(tomic, "tomic")

  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # create sorting ui based on selected sort mode

      observe({
        output$sort_ui <- renderUI({
          req(input$sort_mode)

          ns <- session$ns

          if (input$sort_mode == "hclust") {
            return()
          } else if (input$sort_mode == "category") {
            selectizeInput(
              ns("sample_sorts"),
              NULL,
              choices = valid_sort_vars,
              multiple = TRUE
            )
          } else {
            stop("Invalid sort mode")
          }
        })
      })

      # sort tidy_omic according to selected sort_mode and sort vars

      sorted_tomic <- reactive({
        req(input$sort_mode)

        if (input$sort_mode == "hclust") {
          sort_tomic(tomic,
            "hclust",
            sort_table,
            value_var = value_var
          )
        } else if (input$sort_mode == "category") {
          req(input$sample_sorts)

          sort_tomic(tomic,
            "arrange",
            sort_table,
            sort_variables = input$sample_sorts
          )
        } else {
          stop("Invalid sort mode")
        }
      })

      return(sorted_tomic)
    }
  )
}
