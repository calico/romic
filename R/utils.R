#' Format Names for Plotting
#'
#' Wrap long names over multiple lines so that they will look better on plots.
#'
#' @param chars a character vector (or a variable that can be converted to one)
#' @inheritParams stringr::str_wrap
#' @param truncate_at max character length
#'
#' @return a reformatted character vector of the same length as the input.
#'
#' @examples
#' chars <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer
#'   ac arcu semper erat porttitor egestas. Etiam sagittis, sapien at mattis."
#'
#' format_names_for_plotting(chars)
#' @export
format_names_for_plotting <- function(chars, width = 40, truncate_at = 80) {
  as.character(chars) %>%
    stringr::str_trunc(width = truncate_at, side = "right") %>%
    stringr::str_wrap(
      width = width,
      exdent = 2
    )
}

coerce_to_classes <- function(obj, reference_obj) {
  reference_obj_class <- class(reference_obj)

  if (any(reference_obj_class %in% "glue")) {
    out <- glue::as_glue(obj)
  } else if (any(reference_obj_class %in% c("factor", "ordered"))) {
    out <-
      do.call(
        reference_obj_class,
        list(
          x = obj,
          levels = levels(reference_obj)
        )
      )
  } else if (reference_obj_class == "character") {
    out <- as.character(obj)
  } else if (reference_obj_class == "numeric") {
    out <- as.numeric(obj)
  } else if (reference_obj_class == "integer") {
    out <- as.integer(obj)
  } else if (reference_obj_class == "logical") {
    out <- as.logical(obj)
  } else {
    stop(glue::glue("converting to {reference_obj_class} not implemented"))
  }

  if (all(!is.na(reference_obj_class)) && any(is.na(out))) {
    stop(glue::glue(
      "{sum(is.na(out))} values were converted to NAs
    when zero NAs are expected based on the reference object"
    ))
  }

  return(out)
}

#' Var Partial Match
#'
#' Partial string matching of a provided variable to the variables available
#' in a table
#'
#' @param x a variable name or regex match to a variable name
#' @param df a data.frame or tibble
#'
#' @return a single variable from df
var_partial_match <- function(x, df) {
  checkmate::assertString(x)
  checkmate::assertDataFrame(df)

  valid_vars <- colnames(df)
  # character match
  if (x %in% valid_vars) {
    return(x)
  }

  # treat x as a regular expression
  var_match <- valid_vars[stringr::str_detect(valid_vars, x)]
  if (length(var_match) == 1) {
    return(var_match)
  } else if (length(var_match) == 0) {
    stop(glue::glue("{x} did not match any variables. Valid variables are {paste(valid_vars, collapse = ', ')}. You can also specify a variable with a unique substring"))
  } else {
    stop(glue::glue("{x} matched 2+ variables: {paste(var_match, collapse = ', ')}. This function treats the provided variable as a regular expression so please pass either a more completely defined variable name or a regular expression which will match a single variable"))
  }
}
