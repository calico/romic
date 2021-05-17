#' Shiny Filter Test
#'
#' Tests the shiny filter module as a stand-alone application.
#'
#' @inheritParams check_tidy_omic
#' @inheritParams filterInput
#'
#' @returns A \code{shiny} app
#'
#' @examples
#' if (interactive()) {
#'   shiny_filter_test(brauer_2008_tidy)
#' }
#'
#' @export
shiny_filter_test <- function(tidy_omic, filter_table = "features") {
  stopifnot("tidy_omic" %in% class(tidy_omic))

  shinyApp(
    ui = fluidPage(

      # Sidebar with a slider input for the number of bins
      sidebarLayout(
        sidebarPanel(
          filterInput("filter_features", "features"),
          filterInput("filter_samples", "samples")
        ),
        mainPanel()
      )
    ),
    server = function(input, output, session) {
      tidy_filtered_features <- filterServer(
        "filter_features",
        tidy_omic,
        "features"
      )

      tidy_filtered_samples <- reactive({
        req(tidy_filtered_features())
        tidy_filtered_samples <- filterServer(
          "filter_samples",
          tidy_filtered_features(),
          "samples"
        )
      })

      observe({
        req(tidy_filtered_samples()())
        print(glue::glue(
          "Filtering results: filtered_tidy_data is {nrow(tidy_filtered_samples()()$data)} rows"
        ))
      })
    }
  )
}

#' Filter Input
#'
#' UI components for the filter module.
#'
#' @inheritParams shiny::moduleServer
#' @param filter_table table to filter
#'
#' @returns A \code{shiny} UI
#'
#' @export
filterInput <- function(id, filter_table) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectInput(
      ns("filter_var"),
      glue::glue("Filter {filter_table} by:"),
      choices = NULL
    ),
    shiny::uiOutput(ns("filter_ui"))
  )
}

#' Filter Server
#'
#' Server components for the filter module.
#'
#' @inheritParams shiny::moduleServer
#' @inheritParams check_tidy_omic
#' @inheritParams filterInput
#'
#' @returns A \code{tidy_omic} with some features and/or samples filtered.
#'
#' @export
filterServer <- function(id, tidy_omic, filter_table) {
  shiny::moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      checkmate::assertChoice(filter_table, c("features", "samples"))
      design <- tidy_omic$design
      pk <- dplyr::case_when(
        filter_table == "features" ~ design$features$variable[design$features$type == "feature_primary_key"],
        filter_table == "samples" ~ design$samples$variable[design$samples$type == "sample_primary_key"]
      )

      shiny::updateSelectInput(
        session,
        "filter_var",
        choices = c("None", design[[filter_table]]$variable),
        selected = "None"
      )

      filter_category <- shiny::reactive({
        shiny::req(input$filter_var)
        if (input$filter_var == "None") {
          "none"
        } else {
          categorical_vars <- design[[filter_table]]$variable[
            design[[filter_table]]$type %in% c("character", "factor", "ordered")
          ]

          ifelse(
            input$filter_var %in% c(pk, categorical_vars),
            "category",
            "numeric"
          )
        }
      })

      output$filter_ui <- shiny::renderUI({
        shiny::req(filter_category())

        ns <- session$ns

        if (filter_category() == "none") {
          # not filtering
          return()
        } else {
          if (filter_category() == "category") {
            valid_levels <- tidy_omic$data %>%
              dplyr::distinct_(.dots = input$filter_var) %>%
              unlist() %>%
              unname()
            # filtering to a set of choices
            shiny::selectizeInput(
              ns("filter_categories"),
              "Include:",
              choices = valid_levels,
              multiple = TRUE
            )
          } else if (filter_category() == "numeric") {
            filter_range <- range(
              tidy_omic$data %>%
                dplyr::distinct_(.dots = input$filter_var) %>%
                unlist() %>%
                unname()
            )

            # filter by a range
            shiny::sliderInput(
              ns("filter_range"),
              "Include",
              min = filter_range[1],
              max = filter_range[2],
              value = c(filter_range[1], filter_range[2])
            )
          } else {
            stop("invalid filter_category")
          }
        }
      })

      shiny::observe({
        shiny::req(filter_category())
        print(glue::glue(
          "Filtering {filter_table} by {filter_category()}, {input$filter_var}"
        ))
      })

      # filter tidy_data based on filter settings

      filtered_tidy_data <- shiny::reactive({
        shiny::req(filter_category())

        working_data <- tidy_omic

        # filter features
        if (filter_category() == "category") {
          shiny::req(input$filter_var, input$filter_categories)

          working_data <- filter_tomic(tidy_omic,
            filter_type = "category",
            filter_table = filter_table,
            filter_variable = input$filter_var,
            filter_value = input$filter_categories
          )
        } else if (filter_category() == "numeric") {
          shiny::req(input$filter_var, input$filter_range)

          working_data <- filter_tomic(tidy_omic,
            filter_type = "range",
            filter_table = filter_table,
            filter_variable = input$filter_var,
            filter_value = input$filter_range
          )
        } else if (filter_category() == "none") {
          # leave as is
        } else {
          stop("Undefined filter type")
        }

        working_data
      })

      shiny::observe({
        print(glue::glue(
          "Filtering results: filtered_tidy_data is {nrow(filtered_tidy_data()$data)} rows"
        ))
      })

      # return filtered results
      return(filtered_tidy_data)
    }
  )
}
