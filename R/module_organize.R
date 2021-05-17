#' Shiny Organize Test
#'
#' Tests the shiny organization module as stand-alone application.
#'
#' @inheritParams organizeServer
#'
#' @returns a \code{shiny} app
#'
#' @examples
#'
#' if (interactive()) {
#'   shiny_organize_test(
#'     brauer_2008_tidy,
#'     feature_vars = c("BP", "MF"),
#'     sample_vars = c("sample", "nutrient", "DR"),
#'     value_var = "expression"
#'   )
#' }
#'
#' @export
shiny_organize_test <- function(
  tidy_omic,
  feature_vars,
  sample_vars,
  value_var
  ) {
  checkmate::assertClass(tidy_omic, "tidy_omic")

  shinyApp(
    ui = fluidPage(

      # Sidebar with a slider input for the number of bins
      sidebarLayout(
        sidebarPanel(
          organizeInput("organize")
        ),
        mainPanel()
      )
    ),
    server = function(input, output, session) {
      tidy_organized <- reactive({
        organizeServer(
          "organize",
          tidy_omic,
          feature_vars,
          sample_vars,
          value_var
        )
      })

      observe({
        req(tidy_organized()())
        print(glue::glue(
          "Organization sort status: {tomic_sort_status(tidy_organized()())}"
        ))
      })
    }
  )
}

#' Organize Input
#'
#' UI components for the organize input module.
#'
#' @inheritParams shiny::moduleServer
#'
#' @returns A \code{shiny} UI
#'
#' @export
organizeInput <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(2, actionButton(ns("update_row_sorts"),
        label = NULL, icon = icon("arrow-alt-circle-right"),
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; height:80px"
      )),
      column(10, sortInput(ns("features"), "features"))
    ),
    fluidRow(
      column(2, actionButton(ns("update_col_sorts"),
        label = NULL, icon = icon("arrow-alt-circle-right"),
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; height:80px"
      )),
      column(10, sortInput(ns("samples"), "samples"))
    )
  )
}

#' Organize Servers
#'
#' Server components for the organize input module.
#'
#' @inheritParams shiny::moduleServer
#' @inheritParams check_tidy_omic
#' @param feature_vars variables available for arranging features
#' @param sample_vars variables available for arrange samples
#' @inheritParams sort_tomic
#'
#' @returns A \code{tomic} with sorted features and/or samples.
#'
#' @export
organizeServer <- function(
  id,
  tidy_omic,
  feature_vars,
  sample_vars,
  value_var
  ) {
  checkmate::assertClass(tidy_omic, "tomic")

  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      running_tomic <- reactiveVal(tidy_omic)

      observeEvent(input$update_row_sorts, {
        running_tomic(sortServer(
          "features",
          running_tomic(),
          "features",
          valid_sort_vars = feature_vars,
          value_var = value_var
        )())
        updateActionButton(
          session,
          "update_row_sorts",
          label = NULL,
          icon = icon("refresh")
        )
        print("Sorted features")
      })

      observeEvent(input$update_col_sorts, {
        running_tomic(sortServer(
          "samples",
          running_tomic(),
          "samples",
          valid_sort_vars = sample_vars,
          value_var = value_var
        )())
        updateActionButton(
          session,
          "update_col_sorts",
          label = NULL,
          icon = icon("refresh")
        )
        print("Sorted samples")
      })

      return(running_tomic)
    }
  )
}
