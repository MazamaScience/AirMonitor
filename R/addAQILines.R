#' @export
#' @title Add AQI lines to a plot
#' @param pollutant EPA AQS criteria pollutant.
#' @param palette Named color palette to use for AQI categories.
#' @param ... additional arguments to be passed to \code{abline()}
#' @param NAAQS Version of NAAQS levels to use. See Note.
#'
#' @description Draws AQI lines across a plot at the levels appropriate for
#' The \link{monitor_timeseriesPlot} function uses this function internally when
#' specifying \code{addAQI = TRUE}.
#' \code{pollutant}.
#' @return No return value, called to add lines to a time series plot.
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
  palette = c("EPA", "subdued", "deuteranopia"),
  NAAQS = c("PM2.5", "PM2.5_2024"),
  ...
) {

  pollutant <- match.arg(pollutant)
  palette <- match.arg(palette)
  NAAQS = match.arg(NAAQS)

  breaks <- US_AQI[[paste0("breaks_", pollutant)]]
  colors <- US_AQI[[paste0("colors_", palette)]]

  # Handle the added NAAQS argument
  if ( pollutant == "PM2.5" && NAAQS == "PM2.5_2024" ) {
    breaks <- US_AQI$breaks_PM2.5_2024
  }

  graphics::abline(
    h = breaks,
    col = colors,
    ...
  )

  # NOTE:  Most breaks begin with -Inf so we add a zero line here
  abline(h = 0, col = colors[1])

}
