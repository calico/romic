#' PC Plot
#'
#' Generate a Shiny interactive scatter plot which allows visualization of
#' features, measurements, and samples (with principal components added).
#'
#' @inheritParams tomic_to
#'
#' @returns A \code{shiny} app
#'
#' @examples
#' if (interactive()) {
#'   app_pcs(brauer_2008_tidy)
#' }
#' @export
app_pcs <- function(tomic) {
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
      headerPanel("Scatter Plot"),

      # Sidebar with a slider input for the number of bins
      sidebarLayout(
        sidebarPanel(
          tags$div(
            HTML("<h4>Filter</h4>")
          ),
          filterInput("filter_features", "features"),
          filterInput("filter_samples", "samples"),
          selectInput("measurement_var",
            "Measurement variable:",
            choices = NULL
          ),
        ),
        mainPanel(
          ggplotOutput("ggplot", default_plot_type = "bivariate"),
          dataTableOutput("selected_df")
        )
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

      # specify measurement variable
      measurement_vars <- design$measurements$variable[
        design$measurements$type == "numeric"
      ]
      updateSelectInput(session, "measurement_var", choices = measurement_vars)

      # add PCs
      featurized_tidy_omic <- reactive({
        req(tidy_filtered_samples()(), input$measurement_var)
        add_pcs(
          tidy_filtered_samples()(),
          value_var = input$measurement_var,
          npcs = 5
        )
      })

      # reorder table design so that PCs show up at top
      reorganized_tidy_omic <- reactive({
        req(featurized_tidy_omic())
        dat <- featurized_tidy_omic()

        updated_sample_design <- dplyr::bind_rows(
          dat$design$samples %>%
            dplyr::filter(stringr::str_detect(variable, "^PC")),
          dat$design$samples %>%
            dplyr::filter(!stringr::str_detect(variable, "^PC"))
        )

        dat$design$samples <- updated_sample_design

        return(dat)
      })

      # create a plot and return brushed points
      selected_data <- reactive({
        req(reorganized_tidy_omic())
        ggplotServer(
          "ggplot",
          reorganized_tidy_omic(),
          return_brushed_points = TRUE
        )
      })

      observe({
        req(selected_data()())
        output$selected_df <- renderDataTable(selected_data()())
      })
    },
    options = list(height = 1000)
  )
}
