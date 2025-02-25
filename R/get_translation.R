#' Get translation for a specific field
#'
#' @param field_name The field to translate
#' @return The translated text string
get_translation <- function(field_name) {
  translations |>
    dplyr::filter(var == field_name) |>
    dplyr::pull(language)
}