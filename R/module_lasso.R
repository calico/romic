#' Shiny Lasso Test
#'
#' Tests the shiny lasso module as a stand-alone application.
#'
#' @inheritParams tomic_to
#' @inheritParams plot_bivariate
#'
#' @returns A \code{shiny} app
#'
#' @examples
#' if (interactive()) {
#'   tomic <- brauer_2008_triple
#'   tomic_table <- tomic[["samples"]] %>% dplyr::filter(nutrient == "G")
#'   shiny_lasso_test(tomic, tomic_table)
#' }
#'
#' @export
shiny_lasso_test <- function(tomic, tomic_table) {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertClass(tomic_table, "data.frame")

  shiny::shinyApp(
    ui = shiny::fluidPage(

      # Sidebar with a slider input for the number of bins
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          lassoInput("lasso")
        ),
        shiny::mainPanel(shiny::dataTableOutput("mod_df"))
      )
    ),
    server = function(input, output, session) {
      plot_table <- infer_tomic_table_type(tomic, tomic_table)

      tomic_mod <- shiny::reactive({
        lassoServer("lasso", tomic, tomic_table)
      })

      shiny::observe({
        shiny::req(tomic_mod()())

        print(tomic_mod()()$lasso_message)
        output$mod_df <- shiny::renderDataTable(
          tomic_to(tomic_mod()()$tomic, "triple_omic")[[plot_table]]
        )
      })
    }
  )
}

#' Shiny Lasso Test w/ Reactive Values
#'
#' Tests the shiny lasso module as a stand-alone application when the
#'   \code{tomic} is a \code{reativeVal}.
#'
#' @inheritParams tomic_to
#' @inheritParams plot_bivariate
#'
#' @returns A \code{shiny} app
#'
#' @examples
#' if (interactive()) {
#'   tomic <- brauer_2008_triple
#'   tomic_table <- tomic[["samples"]] %>% dplyr::filter(nutrient == "G")
#'   shiny_lasso_test_reactval(tomic, tomic_table)
#'
#'   tomic_table <- tomic[["measurements"]] %>% dplyr::filter(expression < -3)
#'   shiny_lasso_test_reactval(tomic, tomic_table)
#' }
#'
#' @export
shiny_lasso_test_reactval <- function(tomic, tomic_table) {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertClass(tomic_table, "data.frame")

  shiny::shinyApp(
    ui = shiny::fluidPage(

      # Sidebar with a slider input for the number of bins
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          lassoInput("lasso")
        ),
        shiny::mainPanel(shiny::dataTableOutput("mod_df"))
      )
    ),
    server = function(input, output, session) {
      r_tomic <- shiny::reactiveVal(tomic)
      plot_table <- infer_tomic_table_type(tomic, tomic_table)

      # update a running dataset requires reactive values
      # and updating them appropriately is best done with observeEvent.
      # the rub is that the event trigger is buried in the lasso module
      shiny::observeEvent(input[["lasso-do_lasso_method"]],
        {
          # apply lasso options to generate an updated tomic and a logging
          # message
          tomic_mod <- lassoServer("lasso", r_tomic(), tomic_table)
          # update working tomic data
          r_tomic(tomic_mod()$tomic)
          # update logging of flow operations
          print(tomic_mod()$lasso_message)

          output$mod_df <- shiny::renderDataTable(
            tomic_to(tomic_mod()$tomic, "triple_omic")[[plot_table]]
          )
        },
        label = "update reactive tomic"
      )
    }
  )
}

#' Lasso Input
#'
#' UI components for the lasso module.
#'
#' @inheritParams shiny::moduleServer
#'
#' @returns A \code{shiny} UI
#'
#' @export
lassoInput <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::radioButtons(
      ns("lasso_method"),
      label = "what would you like to do with the selected points?",
      choices = c("filter", "tag"),
      selected = "filter"
    ),
    shiny::conditionalPanel(
      condition = "input.lasso_method == 'tag'",
      ns = ns,
      shiny::textInput(
        ns("tag_name"),
        label = "Tag Variable",
        value = "my_tag"
      )
    ),
    shiny::actionButton(
      ns("do_lasso_method"),
      label = "~ FLOW ~",
      class = "btn-primary btn-lg"
    )
  )
}

#' Lasso Server
#'
#' Take a subset of entries from a tomic table (generally selected using the
#'   lasso function) and then either filter a tomic object to these entries
#'   or tag the entries of interest with a user-specified variable.
#'
#' @inheritParams shiny::moduleServer
#' @inheritParams tomic_to
#' @inheritParams plot_bivariate
#'
#' @returns A \code{tomic} object ammended based on the lasso selection.
#'
#' @export
lassoServer <- function(id, tomic, tomic_table) {
  shiny::moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      checkmate::assertClass(tomic, "tomic")
      checkmate::assertClass(tomic_table, "data.frame")

      plot_table <- infer_tomic_table_type(tomic, tomic_table)
      identifying_keys <- get_identifying_keys(tomic, plot_table)
      triple_omic <- tomic_to(tomic, "triple_omic")

      tomic_out <- shiny::eventReactive(input$do_lasso_method,
        {
          if (input$lasso_method == "filter") {
            lasso_message <- glue::glue(
              "filter: retained {nrow(tomic_table)} of {nrow(triple_omic[[plot_table]])} {plot_table}"
            )

            triple_omic[[plot_table]] <- tomic_table %>%
              # only elements in the design are retained. Feature and sample
              # attributes will have been added to measurements and must be
              # discarded
              dplyr::select(
                !!!rlang::syms(colnames(triple_omic[[plot_table]]))
              )

            triple_omic <- reconcile_triple_omic(triple_omic)
          } else if (input$lasso_method == "tag") {
            tag_name <- validate_tagname(
              input$tag_name,
              colnames(triple_omic[[plot_table]])
            )
            lasso_message <- glue::glue(
              "tag: tagged {nrow(tomic_table)} of {nrow(triple_omic[[plot_table]])} {plot_table} with {tag_name}"
            )

            tagged_table <- triple_omic[[plot_table]] %>%
              dplyr::left_join(
                # tag selected samples, features or measurements
                tomic_table %>%
                  dplyr::select(!!!rlang::syms(identifying_keys)) %>%
                  dplyr::mutate(!!rlang::sym(tag_name) := TRUE),
                by = identifying_keys
              ) %>%
              dplyr::mutate(
                !!rlang::sym(tag_name) := ifelse(
                  is.na(!!rlang::sym(tag_name)),
                  FALSE,
                  TRUE
                )
              )

            updated_design <- dplyr::bind_rows(
              triple_omic$design[[plot_table]],
              tibble::tribble(
                ~variable, ~type,
                tag_name, "logical"
              )
            )

            triple_omic[[plot_table]] <- tagged_table
            triple_omic$design[[plot_table]] <- updated_design
          } else {
            stop(glue::glue(
              "{input$lasso_method} is not a valid lasso method"
            ))
          }

          tomic_out <- list(
            tomic = triple_omic %>%
              tomic_to(class(tomic)[1]),
            lasso_message = lasso_message
          )
        },
        label = "execute lasso method"
      )

      return(tomic_out)
    }
  )
}

get_identifying_keys <- function(tomic, table) {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertChoice(table, c("features", "samples", "measurements"))

  if (table == "features") {
    ids <- tomic$design$feature_pk
  } else if (table == "samples") {
    ids <- tomic$design$sample_pk
  } else if (table == "measurements") {
    ids <- c(tomic$design$feature_pk, tomic$design$sample_pk)
  } else {
    stop(glue::glue("{table} is not a valid choice"))
  }

  return(ids)
}

#' Infer Tomic Table Type
#'
#' From a tomic_table, choose whether it reflects features, samples or
#'   measurements
#'
#' @inheritParams tomic_to
#' @inheritParams plot_bivariate
#'
#' @returns features, samples or measurements
infer_tomic_table_type <- function(tomic, tomic_table) {
  checkmate::assertClass(tomic, "tomic")
  checkmate::assertClass(tomic_table, "data.frame")

  feature_pk <- tomic$design$feature_pk
  sample_pk <- tomic$design$sample_pk
  tomic_table_vars <- colnames(tomic_table)

  table_type <- dplyr::case_when(
    feature_pk %in% tomic_table_vars && sample_pk %in% tomic_table_vars ~ "measurements",
    feature_pk %in% tomic_table_vars ~ "features",
    sample_pk %in% tomic_table_vars ~ "samples"
  )

  if (is.na(table_type)) {
    stop(
      "based on the \"tomic\" primary keys, tomic_table doesn't appear to be features, samples or measurements"
    )
  }

  return(table_type)
}

validate_tagname <- function(tag_name, existing_names) {
  if (tag_name %in% existing_names) {
    n_versions <- sum(stringr::str_detect(existing_names, tag_name))
    new_tag <- stringr::str_c(tag_name, "_", n_versions + 1)
    warning(glue::glue(
      "{tag_name} has already been used; {new_tag} will be used instead"
    ))
    return(new_tag)
  } else {
    return(tag_name)
  }
}
