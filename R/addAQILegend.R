
#' @export
#' @title Add an AQI legend to a map
#' @param x x Coordinate passed on to the \code{legend()} command.
#' @param y y Coordinate passed on to the \code{legend()} command.
#' @param pollutant EPA AQS criteria pollutant.
#' @param palette Named color palette to use for AQI categories.
#' @param languageCode ISO 639-2 alpha-3 language code.
#' @param ... Additional arguments to be passed to \code{legend()}.
#' @param NAAQS Version of NAAQS levels to use. See Note.
#'
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
#'
#' @note
#' On February 7, 2024, EPA strengthened the National Ambient Air Quality
#' Standards for Particulate Matter (PM NAAQS) to protect millions of Americans
#' from harmful and costly health impacts, such as heart attacks and premature
#' death. Particle or soot pollution is one of the most dangerous forms of air
#' pollution, and an extensive body of science links it to a range of serious
#' and sometimes deadly illnesses. EPA is setting the level of the primary
#' (health-based) annual PM2.5 standard at 9.0 micrograms per cubic meter to
#' provide increased public health protection, consistent with the available
#' health science.
#' See \href{https://www.epa.gov/pm-pollution/final-reconsideration-national-ambient-air-quality-standards-particulate-matter-pm}{PM NAAQS update}.
#'


addAQILegend <- function(
  x = "topright",
  y = NULL,
  pollutant = c("PM2.5", "CO", "OZONE", "PM10", "AQI"),
  palette = c("EPA", "subdued", "deuteranopia"),
  languageCode = c("eng", "spa"),
  NAAQS = c("PM2.5", "PM2.5_2024"),
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  pollutant <- match.arg(pollutant)
  palette <- match.arg(palette)
  languageCode <- match.arg(languageCode)
  NAAQS = match.arg(NAAQS)

  colors <- US_AQI[[paste0("colors_", palette)]]
  names <- US_AQI[[paste0("names_", languageCode)]]

  # Handle the added NAAQS argument
  if ( pollutant == "PM2.5" && NAAQS == "PM2.5_2024" ) {
    breaks <- US_AQI$breaks_PM2.5_2024
  }

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
