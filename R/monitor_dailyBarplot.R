#' @title Create daily barplot
#'
#' @description
#' Creates a daily barplot of data from a \emph{mts_monitor} object.
#'
#' Reasonable defaults are chosen for annotations and plot characteristics.
#' Users can override any defaults by passing in parameters accepted by
#' \code{graphics::barplot}.
#'
#' @param monitor \emph{mts_monitor} object.
#' @param id \code{deviceDeploymentID} for a single time series found in \code{monitor}.
#' (Optional if \code{monitor} contains only a single time series.)
#' @param add Logical specifying whether to add to the current plot.
#' @param addAQI Logical specifying whether to add AQI levels and legend.
#' @param palette Named color palette to use when adding AQI decorations.
#' @param opacity Opacity to use for bars.
#' @param ... Additional arguments to be passed to \code{graphics::barplot()}.
#' @param minHours Minimum number of valid hourly records per day required to
#' calculate statistics. Days with fewer valid records will be assigned \code{NA}.
#' @param dayBoundary Treatment of daylight savings time:  "clock" uses daylight
#' savings time as defined in the local timezone, "LST" uses "local standard time"
#' all year round.
#'
#' @import graphics
#' @importFrom grDevices adjustcolor
#' @export
#'
monitor_dailyBarplot <- function(
  monitor = NULL,
  id = NULL,
  add = FALSE,
  addAQI = FALSE,
  palette = c("EPA", "subdued", "deuteranopia"),
  opacity = NULL,
  ...,
  minHours = 18,
  dayBoundary = c("clock", "LST")
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  palette <- match.arg(palette)
  MazamaCoreUtils::stopIfNull(minHours)
  dayBoundary <- match.arg(dayBoundary)


  # Subset 'monitor' to a single time series
  if ( nrow(monitor$meta) > 1 ) {

    MazamaCoreUtils::stopIfNull(id)
    if ( !id %in% monitor$meta$deviceDeploymentID )
      stop("id = \"%s\" is not found in 'monitor'")

    monitor <-
      monitor %>%
      monitor_filter(.data$deviceDeploymentID == !!id)

  }

  monitor <-
    monitor %>%
    monitor_dropEmpty()

  if ( ncol(monitor$data) < 2 )
    stop("no valid data in 'monitor'")

  if ( nrow(monitor$meta) > 1 )
    stop("multiple records found in 'monitor$meta'")

  # ----- Calculate daily average ----------------------------------------------

  daily <-
    monitor_dailyStatistic(
      monitor = monitor,
      FUN = mean,
      na.rm = TRUE,
      ...,
      minHours = minHours,
      dayBoundary = dayBoundary
    )

  meta <- daily$meta
  data <- daily$data

  pollutant <- meta$pollutant
  units <- meta$units
  locationName <- meta$locationName
  timezone <- meta$timezone

  localTime <- data$datetime
  dailyAverage <- data %>% dplyr::pull(2)

  # ----- argsList -------------------------------------------------------------

  argsList <- list(...)

  # Height and color
  argsList$height <- dailyAverage
  argsList$col <- aqiColors(dailyAverage)

  # X axis labeling is handled after the plot

  # Y axis labeling
  argsList$ylab <- ifelse(
    "ylab" %in% names(argsList),
    argsList$ylab,
    sprintf("%s (%s)", pollutant, units)
  )

  # Additional small tweaks
  argsList$las <- ifelse("las" %in% names(argsList), argsList$las, 1)

  # Title
  argsList$main <- ifelse(
    "main" %in% names(argsList),
    argsList$main,
    sprintf("%s -- Daily Average %s", locationName, pollutant)
  )

  # Explicitly declare defaults for use in creating the x axis
  argsList$axes <- ifelse("axes" %in% names(argsList), argsList$axes, TRUE)
  argsList$space <- ifelse("space" %in% names(argsList), argsList$space, 0.2)
  argsList$cex.names <-
    ifelse("cex.names" %in% names(argsList), argsList$cex.names, par("cex.axis"))

  # ----- Plotting -------------------------------------------------------------

  if ( addAQI ) {
    do.call(barplot, argsList)
    addAQIStackedBar(pollutant = pollutant, palette = palette)
    addAQILines(pollutant = pollutant, palette = palette)
    argsList$add <- TRUE
  }

  do.call(barplot, argsList)

  # Add default X axis
  if ( argsList$axes && !("names.arg" %in% names(argsList)) ) {

    barCount <- length(argsList$height)
    allIndices <- 1:barCount
    allLabels <- strftime(localTime, "%b %d", tz = timezone)
    maxLabelCount <- 16
    stride <- round(barCount / maxLabelCount)
    if ( stride == 0 ) {
      indices <- allIndices
      labels <- allLabels
    } else {
      indices <- allIndices[seq(1, barCount, by = stride)]
      labels <- allLabels[seq(1, barCount, by = stride)]
    }
    labels_x <- (indices - 0.5) + (indices * argsList$space)
    labels_y <- -0.06 * (par("usr")[4] - par("usr")[3])
    text(labels_x, labels_y, labels, cex = argsList$cex.names, xpd = NA)
    # Now add tick marks
    axis(1, at = labels_x, labels = FALSE, lwd = 0, lwd.ticks = 1)

  }

  if ( addAQI ) {
    addAQILegend("topright", pollutant = pollutant, palette = palette)
  }

}

# ===== DEBUG ==================================================================

if ( FALSE ) {


  monitor = Carmel_Valley
  id = NULL
  add = FALSE
  addAQI = FALSE
  palette = "EPA"
  opacity = NULL
  minHours = 18
  dayBoundary = c("clock", "LST")




}
