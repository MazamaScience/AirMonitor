#' @export
#'
#' @title Generate AQI colors
#'
#' @param x Vector or matrix of PM2.5 values or an \emph{mts_monitor} object.
#' @param pollutant EPA AQS criteria pollutant.
#' @param palette Named color palette to use for AQI categories.
#' @param na.color Color assigned to missing values.
#' @param NAAQS User provided NAAQS levels to use.
#'
#' @return A vector or matrix of AQI colors to be used in maps and plots.
#'
#' @description
#' This function uses the \code{leaflet::colorBin()} function to return a
#' vector or matrix of colors derived from data values.
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
#' @seealso \code{\link{aqiCategories}}
#'
#' @examples
#' library(AirMonitor)
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


aqiColors <- function(
  x,
  pollutant = c("PM2.5", "CO", "OZONE", "PM10", "AQI"),
  palette = c("EPA", "EPA_colorVisionAssist"),
  na.color = NA,
  NAAQS = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  pollutant <- match.arg(pollutant)
  palette <- match.arg(palette)
  NAAQS = match.arg(NAAQS)

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

  # ----- Prepare data ---------------------------------------------------------

  # Pull data out of mts_monitor object if necessary
  if ( !is.numeric(x) ) {
    if ( !monitor_isValid(x) ) stop("'x' is neither numeric nor a valid mts_monitor object")
    x <- x$data[,-1]
  }

  # Convert to matrix if necessary
  ncol <- 1
  if ( !is.null(ncol(x)) ) {
    ncol <- ncol(x)
    x <- as.matrix(x)
  }

  # Force conversion to a numeric vector
  x <- as.numeric(x)

  # ----- Create colors --------------------------------------------------------

  # Generate color function
  colorFUN <- leaflet::colorBin(
    palette = colors,
    domain = c(0, 1e6),
    bins = breaks,
    na.color = na.color
  )

  # Assign colors
  cols <- colorFUN(x)

  # ----- Return ---------------------------------------------------------------

  # Restore shape
  if ( ncol > 1 ) {
    cols <- matrix(cols, ncol = ncol, byrow = FALSE)
  }

  return(cols)

}
