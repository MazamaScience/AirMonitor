#' @export
#' @title Create stacked AQI bar
#' @param pollutant EPA AQS criteria pollutant.
#' @param width Width of the bar as a fraction of the width of the plot area.
#' @param height Height of the bar as a fraction of the height of the plot area.
#' @param pos Position of the stacked bar relative to the plot.
#' @param palette Named color palette to use for AQI categories.
#' @description Draws a stacked bar indicating AQI levels on one side of a plot
#' The \link{monitor_timeseriesPlot} function uses this function internally when
#' specifying \code{addAQI = TRUE}.
#' @return No return value, called to add color bars to a time series plot.

addAQIStackedBar <- function(
  pollutant = c("PM2.5", "CO", "OZONE", "PM10", "AQI"),
  palette = c("EPA", "subdued", "deuteranopia"),
  width = .01,
  height = 1,
  pos = c("left", "right")
) {

  pollutant <- match.arg(pollutant)
  pos <- match.arg(pos)
  palette <- match.arg(palette)

  usr <- par("usr")

  if (pos == "right") {
    l <- usr[2] - width*(usr[2] - usr[1])
    r <- usr[2]
  } else if (pos == "left") {
    l <- usr[1]
    r <- usr[1] + width*(usr[2] - usr[1])
  }

  breaks <- US_AQI[[paste0("breaks_", pollutant)]]
  colors <- US_AQI[[paste0("colors_", palette)]]

  for (i in 1:6) {
    rect(
      xleft = l,
      ybottom = min(max(0, breaks[i]), height*usr[4]),
      xright = r,
      ytop = min(breaks[i + 1], height*usr[4]),
      col = colors[i],
      xpd = NA,
      border = NA
    )
  }

}
