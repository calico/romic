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
#'
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

  if (reference_obj_class %in% c("factor", "ordered")) {
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

  return(out)
}
