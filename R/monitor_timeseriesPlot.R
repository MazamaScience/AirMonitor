#' @title Create Timeseries Plot
#'
#' @description
#' Creates a time series plot of data from a \emph{mts_monitor} object.
#' By default, points are plotted as semi-transparent squares. All data values
#' are plotted from all monitors found in the \emph{mts_monitor} object.
#'
#' Reasonable defaults are chosen for annotations and plot characteristics.
#' Users can override any defaults by passing in parameters accepted by
#' \code{graphics::plot.default}.
#'
#' @param monitor \emph{mts_monitor} object.
# #' @param localTime Logical specifying whether \code{tlim} is in local time or
# #'   UTC.
#' @param shadedNight Logical specifying whether to add nighttime shading.
#' @param add Logical specifying whether to add to the current plot.
#' @param addAQI Logical specifying whether to add AQI levels and legend.
#' @param opacity Opacity to use for points. By default, an opacity is chosen based
#' on the number of points so that trends are highlighted while outliers diminish
#' in visual importance as the number of points increases.
#' @param ... Additional arguments to be passed to \code{graphics::plot.default()}.
#'
#' @import graphics
#' @importFrom grDevices adjustcolor
#' @export
#'
monitor_timeseriesPlot <- function(
  monitor,
  shadedNight = FALSE,
  add = FALSE,
  addAQI = TRUE,
  opacity = NULL,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  monitor <- monitor_dropEmpty(monitor)

  if ( ncol(monitor$data) < 2 )
    stop("no valid data in 'monitor'")

  meta <- monitor$meta
  data <- monitor$data

  # ----- Time axis ------------------------------------------------------------

  # Identify timezone(s)
  timezone <- monitor_bestTimezone(monitor)

  # Pull out time data
  datetime <- lubridate::with_tz(data$datetime, tzone = timezone)

  # ----- argsList -------------------------------------------------------------

  argsList <- list(...)

  argsList$x <- datetime
  argsList$y <- data[, 2]

  # * Plot limits -----

  if ( !("ylim" %in% names(argsList)) ) {
    ymin <- min(data[, -1], na.rm = TRUE)
    ymin <- min(0, ymin)
    ymax <- max(data[, -1], na.rm = TRUE)
    buffer <- 0.04 * (ymax - ymin) # Standard R buffer around min/max
    argsList$ylim <- c(ymin - buffer, ymax + buffer)
  }

  # * Annotations -----

  middleDatetime <- datetime[round(length(datetime)/2)]
  year <- MazamaCoreUtils::timeStamp(middleDatetime, timezone, unit = "year")

  if ( !("xlab" %in% names(argsList)) ) {
    if ( timezone == "UTC" ) {
      argsList$xlab <- "UTC"
    } else {
      argsList$xlab <- paste0(year, "  (local time)")
    }
  }

  if ( !("ylab" %in% names(argsList)) )
    argsList$ylab <- sprintf("%s (%s)", meta$pollutant[1], meta$units[1])

  if ( !("main" %in% names(argsList)) ) {
    if ( nrow(meta) == 1 )
      argsList$main <- sprintf("Hourly %s at %s", meta$pollutant[1], meta$locationName)
    else
      argsList$main <- paste0("Hourly ", meta$pollutant[1])
  }

  # * Plot style -----

  if ( !("pch" %in% names(argsList)) )
    argsList$pch <- 15

  if ( !"col" %in% names(argsList) )
    argsList$col <- "black"

  # * argsListBlank -----

  argsListBlank <- argsList

  argsListBlank$col <- "transparent"
  argsListBlank$axes <- FALSE

  # ----- Base plot ------------------------------------------------------------

  # Base plot for background
  if ( !add ) {

    # Create blank plot
    do.call(plot, argsListBlank)

    # Shaded Night
    if ( shadedNight ) {
      lat <- mean(meta$latitude)
      lon <- mean(meta$longitude)
      timeInfo <- MazamaTimeSeries::timeInfo(datetime, lon, lat, timezone)
      addShadedNight(timeInfo)
    }

    # Put a box around the plot area
    box()

    # Add axes
    axis(2, las = 1)

    # TODO: better x axis smarts, e.g. keep from saying "Monday, Tuesday" etc...
    axis.POSIXct(1, datetime)

  }

  # ----- Overlay data ---------------------------------------------------------

  if ( is.null(opacity) ) {
    # Set opacity based on total number of valid measurements
    dims <- dim(as.matrix(data[, -1]))
    naCount <- length(which(is.na(data[, -1])))
    validCount <- dims[1] * dims[2] - naCount
    if ( validCount < 200 ) opacity <- 0.9
    else if ( validCount < 500 ) opacity <- 0.7
    else if ( validCount < 1000 ) opacity <- 0.5
    else if ( validCount < 2000 ) opacity <- 0.3
    else if ( validCount < 5000 ) opacity <- 0.2
    else opacity <- 0.15
  }

  for ( id in meta$deviceDeploymentID ) {
    argsList$y <- data[[id]] # same as data[, id]
    argsList$col <- adjustcolor(argsList$col, alpha.f = opacity)
    # Add the points
    do.call(points, argsList)
  }

  # ----- AQI ------------------------------------------------------------------

  if ( addAQI ) {
    addAQILines(meta$pollutant[1])
    addAQIStackedBar(meta$pollutant[1])
    addAQILegend("topright", pollutant = meta$pollutant[1])
  }


}

