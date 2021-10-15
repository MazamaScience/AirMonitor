
#' @export
#' @title Add an AQI legend to a map
#' @param x x Coordinate passed on to the \code{legend()} command.
#' @param y y Coordinate passed on to the \code{legend()} command.
#' @param pollutant EPA AQS criteria pollutant.
#' @param palette Named color palette to use for AQI categories.
#' @param languageCode ISO 639-2 alpha-3 language code.
#' @param ... Additional arguments to be passed to \code{legend()}.
#' @description This function is a convenience wrapper around
#' \code{graphics::legend()}. It will show the AQI colors and
#' names by default if \code{col} and \code{legend} are not specified.

addAQILegend <- function(
  x = "topright",
  y = NULL,
  pollutant = c("PM2.5", "CO"),
  palette = c("EPA", "subdued", "deuteranopia"),
  languageCode = c("eng", "spa"),
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  pollutant <- match.arg(pollutant)
  palette <- match.arg(palette)
  languageCode <- match.arg(languageCode)

  colors <- US_AQI[[paste0("colors_", palette)]]
  names <- US_AQI[[paste0("names_", languageCode)]]

  # ----- Create argsList ------------------------------------------------------

  argsList <- list(...)

  argsList$x = x
  argsList$y = y

  if ( ("col" %in% names(argsList)) ) {
    argsList$col <- col
  } else {
    argsList$col <- colors
  }

  if ( ("legend" %in% names(argsList)) ) {
    argsList$legend <- legend
  } else {
    argsList$legend <- names
  }

  if ( !("pch" %in% names(argsList)) )
    argsList$pch <- 16

  if ( !("title" %in% names(argsList)) )
    argsList$title <- paste0(pollutant, " Air Quality Index")


  do.call(legend, argsList)

}
