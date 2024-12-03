#' @export
#' @title Create stacked AQI bar
#' @param pollutant EPA AQS criteria pollutant.
#' @param width Width of the bar as a fraction of the width of the plot area.
#' @param height Height of the bar as a fraction of the height of the plot area.
#' @param pos Position of the stacked bar relative to the plot.
#' @param palette Named color palette to use for AQI categories.
#' @param NAAQS User provided NAAQS levels to use.
#'
#' @description Draws a stacked bar indicating AQI levels on one side of a plot
#' The \link{monitor_timeseriesPlot} function uses this function internally when
#' specifying \code{addAQI = TRUE}.
#' @return No return value, called to add color bars to a time series plot.
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


addAQIStackedBar <- function(
  pollutant = c("PM2.5", "CO", "OZONE", "PM10", "AQI"),
  palette = c("EPA", "EPA_colorVisionAssist"),
  width = .01,
  height = 1,
  pos = c("left", "right"),
  NAAQS = NULL
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
