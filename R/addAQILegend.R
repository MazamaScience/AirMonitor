
#' @export
#' @title Add an AQI legend to a map
#' @param x x Coordinate passed on to the \code{legend()} command.
#' @param y y Coordinate passed on to the \code{legend()} command.
#' @param pollutant EPA AQS criteria pollutant.
#' @param palette Named color palette to use for AQI categories.
#' @param languageCode ISO 639-2 alpha-3 language code.
#' @param ... Additional arguments to be passed to \code{legend()}.
#' @param NAAQS User provided NAAQS levels to use.
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
#' By default, an appropriate set of NAAQS levels will be chosen for each
#' \code{pollutant}. Users can override these values by providing an alternate
#' set of breaks, \emph{e.g.}, \code{NAAQS = US_AQI$breaks_PM2.5_24hr_pre_2024}.
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
  palette = c("EPA", "EPA_colorVisionAssist"),
  languageCode = c("eng", "spa"),
  NAAQS = NULL,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  pollutant <- match.arg(pollutant)
  palette <- match.arg(palette)
  languageCode <- match.arg(languageCode)

  colors <- US_AQI[[paste0("colors_", palette)]]
  names <- US_AQI[[paste0("names_", languageCode)]]
  breaks <- US_AQI[[paste0("breaks_", pollutant)]]

  # Use NAAQS if provided and valid
  if ( !is.null(NAAQS) ) {
    if ( !is.numeric(NAAQS) || length(NAAQS) != 7 ) {
      warning("User provided 'NAAQS' must have 7 numeric levels")
    } else {
      breaks <- NAAQS
    }
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
