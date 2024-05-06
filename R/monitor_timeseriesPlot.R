#' @export
#' @import graphics
#' @importFrom grDevices adjustcolor
#'
#' @title Create timeseries plot
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
#' @param id \code{deviceDeploymentID} used to limit plotting to a single time
#' series found in \code{monitor}.
#' @param shadedNight Logical specifying whether to add nighttime shading.
#' @param add Logical specifying whether to add to the current plot.
#' @param addAQI Logical specifying whether to add visual AQI decorations.
#' @param palette Named color palette to use when adding AQI decorations.
#' @param opacity Opacity to use for points. By default, an opacity is chosen based
#' on the number of points so that trends are highlighted while outliers diminish
#' in visual importance as the number of points increases.
#' @param NAAQS Version of NAAQS levels to use. See Note.
#' @param ... Additional arguments to be passed to \code{graphics::plot.default()}.
#'
#' @return No return value. This function is called to draw an air quality
#' time series plot on the active graphics device.
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
#' @examples
#' library(AirMonitor)
#'
#' # Single monitor
#' Carmel_Valley %>%
#'   monitor_timeseriesPlot()
#'
#' # Multiple monitors
#' Camp_Fire %>%
#'   monitor_filter(countyName == "Alameda") %>%
#'   monitor_timeseriesPlot(main = "All Alameda County Monitors")
#'
#' # Standard extras
#' Carmel_Valley %>%
#'   monitor_timeseriesPlot(
#'     shadedNight = TRUE,
#'     addAQI = TRUE
#'   )
#' addAQILegend()
#'
#' # Standard extras using the updated PM NAAQS
#' Carmel_Valley %>%
#'   monitor_timeseriesPlot(
#'     shadedNight = TRUE,
#'     addAQI = TRUE,
#'     NAAQS = "PM2.5_2024"
#'   )
#' addAQILegend(NAAQS = "PM2.5_2024")
#'
#' # Fancy plot based on pm2.5 values
#' pm2.5 <- Carmel_Valley$data[,2]
#' Carmel_Valley %>%
#'   monitor_timeseriesPlot(
#'     shadedNight = TRUE,
#'     pch = 16,
#'     cex = pmax(pm2.5 / 100, 0.5),
#'     col = aqiColors(pm2.5),
#'     opacity = 0.8
#'   )
#' addAQILegend(pch = 16, cex = 0.6, bg = "white")

monitor_timeseriesPlot <- function(
  monitor = NULL,
  id = NULL,
  shadedNight = FALSE,
  add = FALSE,
  addAQI = FALSE,
  palette = c("EPA", "subdued", "deuteranopia"),
  opacity = NULL,
  NAAQS = c("PM2.5_2024", "PM2.5"),
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  shadedNight <- MazamaCoreUtils::setIfNull(shadedNight, FALSE)
  add <- MazamaCoreUtils::setIfNull(add, FALSE)
  addAQI <- MazamaCoreUtils::setIfNull(addAQI, FALSE)
  palette <- match.arg(palette)
  NAAQS = match.arg(NAAQS)

  # Subset 'monitor' to a single time series
  if ( !is.null(id) ) {

    if ( !id %in% monitor$meta$deviceDeploymentID )
      stop("id = \"%s\" is not found in 'monitor'")

    monitor <-
      monitor %>%
      monitor_filter(.data$deviceDeploymentID == !!id)

  }

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

  argsList$x <- data$datetime
  argsList$y <- data %>% dplyr::pull(2) %>% round(digits = 1)

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
      argsList$xlab <- paste0(year[1], "  (UTC)")
    } else {
      argsList$xlab <- paste0(year[1], "  (local time)")
    }
  }

  # NOTE:  For mathematical notation in R see:
  # NOTE:    https://magnusmetz.github.io/2013/04/mathematical-annotation-in-r/

  if ( !("ylab" %in% names(argsList)) ) {
    if ( meta$units[1] == "UG/M3") {
      # Most common case
      argsList$ylab <- expression(paste(PM[2.5] * " (", mu, "g/m"^3, ")"))
    } else if ( meta$units[1] == "" ) {
      argsList$ylab <- sprintf("%s", meta$pollutant[1])
    } else {
      argsList$ylab <- sprintf("%s (%s)", meta$pollutant[1], meta$units[1])
    }
  }

  if ( !("main" %in% names(argsList)) ) {
    if ( nrow(meta) == 1 )
      argsList$main <- sprintf("Hourly %s at %s", meta$pollutant[1], meta$locationName)
    else
      argsList$main <- paste0("Hourly ", meta$pollutant[1])
  }

  # * Plot style -----

  if ( !("pch" %in% names(argsList)) )
    argsList$pch <- 15

  # NOTE:  Save the color outside of argsList so that opacity can be applied below

  if ( "col" %in% names(argsList) ) {
    my_col <- argsList$col
  } else {
    my_col <- "black"
  }

  # * argsListBlank -----

  argsListBlank <- argsList

  argsListBlank$col <- "transparent"
  argsListBlank$axes <- FALSE

  # ----- Base plot ------------------------------------------------------------

  needToResetMargins <- FALSE

  # Base plot for background
  if ( !add ) {

    # Add space to the left if default margins are in place
    if ( all(par("mar") == c(5.1, 4.1, 4.1, 2.1)) )  {
      par("mar" = c(5.1, 5.1, 4.1, 2.1))
      needToResetMargins <- TRUE
    }

    # Create blank plot
    do.call(plot, argsListBlank)

    # Shaded Night
    if ( shadedNight ) {
      lat <- mean(meta$latitude)
      lon <- mean(meta$longitude)
      timeInfo <- MazamaTimeSeries::timeInfo(datetime, lon, lat, timezone)
      addShadedNight(timeInfo)
    }

    # Add AQI decorations underneath
    if ( addAQI ) {
      addAQIStackedBar(pollutant = meta$pollutant[1], palette = palette, NAAQS = NAAQS)
      addAQILines(pollutant = meta$pollutant[1], palette = palette, NAAQS = NAAQS)
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
    if ( validCount < 2 ) opacity <- 1.0
    else if ( validCount < 200 ) opacity <- 0.9
    else if ( validCount < 500 ) opacity <- 0.7
    else if ( validCount < 1000 ) opacity <- 0.5
    else if ( validCount < 2000 ) opacity <- 0.3
    else if ( validCount < 5000 ) opacity <- 0.2
    else opacity <- 0.15
  }

  for ( id in meta$deviceDeploymentID ) {
    argsList$y <- data[[id]] %>% round(digits = 1) # same as data[, id]
    argsList$col <- adjustcolor(my_col, alpha.f = opacity)
    # Add the points
    do.call(points, argsList)
  }

  if ( needToResetMargins )
    par("mar" = c(5.1, 4.1, 4.1, 2.1))

}

