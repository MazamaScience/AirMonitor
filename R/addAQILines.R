#' @export
#' @title Add AQI lines to a plot
#' @param pollutant EPA AQS criteria pollutant.
#' @param palette Named color palette to use for AQI categories.
#' @param ... additional arguments to be passed to \code{abline()}
#' @description Draws AQI lines across a plot at the levels appropriate for
#' The \link{monitor_timeseriesPlot} function uses this function internally when
#' specifying \code{addAQI = TRUE}.
#' \code{pollutant}.
#' @return No return value, called to add lines to a time series plot.

addAQILines <- function(
  pollutant = c("PM2.5", "CO", "OZONE", "PM10", "AQI"),
  palette = c("EPA", "subdued", "deuteranopia"),
  ...
) {

  pollutant <- match.arg(pollutant)
  palette <- match.arg(palette)

  breaks <- US_AQI[[paste0("breaks_", pollutant)]]
  colors <- US_AQI[[paste0("colors_", palette)]]

  graphics::abline(
    h = breaks,
    col = colors,
    ...
  )

  # NOTE:  Most breaks begin with -Inf so we add a zero line here
  abline(h = 0, col = colors[1])

}
