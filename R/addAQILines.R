#' @export
#' @title Add AQI lines to a plot
#' @param pollutant EPA AQS criteria pollutant.
#' @param palette Named color palette to use for AQI categories.
#' @param ... additional arguments to be passed to \code{abline()}
#' @param NAAQS User provided NAAQS levels to use.
#'
#' @description Draws AQI lines across a plot at the levels appropriate for
#' The \link{monitor_timeseriesPlot} function uses this function internally when
#' specifying \code{addAQI = TRUE}.
#' \code{pollutant}.
#' @return No return value, called to add lines to a time series plot.
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


addAQILines <- function(
  pollutant = c("PM2.5", "CO", "OZONE", "PM10", "AQI"),
  palette = c("EPA", "EPA_colorVisionAssist"),
  NAAQS = NULL,
  ...
) {

  pollutant <- match.arg(pollutant)
  palette <- match.arg(palette)

  colors <- US_AQI[[paste0("colors_", palette)]]
  breaks <- US_AQI[[paste0("breaks_", pollutant)]]

  # Use NAAQS if provided and valid
  if ( !is.null(NAAQS) ) {
    if ( !is.numeric(NAAQS) || length(NAAQS) != 7 ) {
      warning("User provided 'NAAQS' must have 7 numeric levels")
    } else {
      breaks <- NAAQS
    }
  }

  graphics::abline(
    h = breaks,
    col = colors,
    ...
  )

  # NOTE:  Most breaks begin with -Inf so we add a zero line here
  abline(h = 0, col = colors[1])

}
