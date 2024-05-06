#' @title Create daily barplot
#'
#' @description
#' Creates a daily barplot of data from a \emph{mts_monitor} object.
#'
#' Reasonable defaults are chosen for annotations and plot characteristics.
#' Users can override any defaults by passing in parameters accepted by
#' \code{graphics::barplot}.
#'
#' @note
#' The underlying axis for this plot is not a time axis so you cannot use this
#' function to "add" bars on top of a \code{monitor_timeseriesPlot()}. See
#' the \pkg{AirMonitorPlots} package for more flexibility in plotting.
#'
#' @param monitor \emph{mts_monitor} object.
#' @param id \code{deviceDeploymentID} for a single time series found in \code{monitor}.
#' (Optional if \code{monitor} contains only a single time series.)
#' @param add Logical specifying whether to add to the current plot.
#' @param addAQI Logical specifying whether to add visual AQI decorations.
#' @param palette Named color palette to use when adding AQI decorations.
#' @param opacity Opacity to use for bars.
#' @param minHours Minimum number of valid hourly records per day required to
#' calculate statistics. Days with fewer valid records will be assigned \code{NA}.
#' @param dayBoundary Treatment of daylight savings time:  "clock" uses daylight
#' savings time as defined in the local timezone, "LST" uses "local standard time"
#' all year round.
#' @param NAAQS Version of NAAQS levels to use. See Note.
#' @param ... Additional arguments to be passed to \code{graphics::barplot()}.
#'
#' @return No return value. This function is called to draw an air quality
#' daily average plot on the active graphics device.
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
#' @import graphics
#' @importFrom grDevices adjustcolor
#' @export
#'
#' @examples
#' library(AirMonitor)
#'
#' layout(matrix(seq(2)))
#'
#' Carmel_Valley %>% monitor_dailyBarplot()
#' title("(pre-2024 PM NAAQS)", line = 0)
#'
#' Carmel_Valley %>% monitor_dailyBarplot(NAAQS = "PM2.5_2024")
#' title("(updated PM NAAQS)", line = 0)
#'
#' layout(1)
#'
monitor_dailyBarplot <- function(
    monitor = NULL,
    id = NULL,
    add = FALSE,
    addAQI = FALSE,
    palette = c("EPA", "subdued", "deuteranopia"),
    opacity = NULL,
    minHours = 18,
    dayBoundary = c("clock", "LST"),
    NAAQS = c("PM2.5_2024", "PM2.5"),
    ...
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  palette <- match.arg(palette)
  MazamaCoreUtils::stopIfNull(minHours)
  dayBoundary <- match.arg(dayBoundary)
  NAAQS = match.arg(NAAQS)


  # Subset 'monitor' to a single time series
  if ( nrow(monitor$meta) > 1 ) {

    MazamaCoreUtils::stopIfNull(id)
    if ( !id %in% monitor$meta$deviceDeploymentID )
      stop("id = \"%s\" is not found in 'monitor'")

    monitor <-
      monitor %>%
      monitor_filter(.data$deviceDeploymentID == !!id)

  }

  monitor <- monitor_dropEmpty(monitor)

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

  dailyAverage <- data %>% dplyr::pull(2) %>% round(digits = 1)

  if ( all(is.na(dailyAverage)) )
    stop("not enough data to calculate daily averages")

  # ----- argsList -------------------------------------------------------------

  argsList <- list(...)

  # Height and color
  argsList$height <- dailyAverage
  argsList$col <-
    aqiColors(
      dailyAverage,
      pollutant = pollutant,
      palette = palette,
      na.color = NA,
      NAAQS = NAAQS
    )

  # X axis labeling is handled after the plot

  # NOTE:  For mathematical notation in R see:
  # NOTE:    https://magnusmetz.github.io/2013/04/mathematical-annotation-in-r/

  # Y axis labeling
  if ( !("ylab" %in% names(argsList)) ) {
    if ( meta$units == "UG/M3") {
      # Most common case
      argsList$ylab <- expression(paste(PM[2.5] * " (", mu, "g/m"^3, ")"))
    } else {
      argsList$ylab <- sprintf("%s (%s)", meta$pollutant[1], meta$units[1])
    }
  }


  # Additional small tweaks
  argsList$las <- ifelse("las" %in% names(argsList), argsList$las, 1)

  # Title
  argsList$main <- ifelse(
    "main" %in% names(argsList),
    argsList$main,
    sprintf("%s -- Daily Average %s", locationName, pollutant)
  )

  # Subitle
  argsList$sub <- ifelse(
    "sub" %in% names(argsList),
    argsList$sub,
    strftime(localTime[1], format = "%Y", tz = timezone)
  )

  # Explicitly declare defaults for use in creating the x axis
  argsList$axes <- ifelse("axes" %in% names(argsList), argsList$axes, TRUE)
  argsList$space <- ifelse("space" %in% names(argsList), argsList$space, 0.2)
  argsList$cex.names <-
    ifelse("cex.names" %in% names(argsList), argsList$cex.names, par("cex.axis") * 0.8)

  # ----- Plotting -------------------------------------------------------------

  if ( addAQI ) {
    do.call(barplot, argsList)
    addAQIStackedBar(pollutant = pollutant, palette = palette, NAAQS = NAAQS)
    addAQILines(pollutant = pollutant, palette = palette, NAAQS = NAAQS)
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
  dayBoundary = "clock"




}
