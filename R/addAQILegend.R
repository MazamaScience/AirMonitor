
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
#'
#' AQI categories are arranged with lower levels at the bottom of the legend
#' to match the arrangement in the plot. This is different from the default
#' "reading order" so you may wish to reverse the order of user supplied
#' arguments with \code{rev()} .
#' @return A list with components \code{rect} and \code{text} is returned
#' invisbly. (See \link{legend}.)

addAQILegend <- function(
  x = "topright",
  y = NULL,
  pollutant = c("PM2.5", "CO", "OZONE", "PM10", "AQI"),
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
    argsList$col <- rev(colors)      # Lower levels on the bottom
  }

  if ( ("legend" %in% names(argsList)) ) {
    argsList$legend <- legend
  } else {
    argsList$legend <- rev(names)    # Lower levels on the bottom
  }

  if ( !("pch" %in% names(argsList)) ) {
    argsList$pch <- 16
  }

  if ( !("title" %in% names(argsList)) )
    argsList$title <- paste0(pollutant, " Air Quality Index")


  do.call(legend, argsList)

}
