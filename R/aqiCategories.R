#' @export
#'
#' @title Generate AQI categories
#'
#' @param x Vector or matrix of PM2.5 values or an \emph{mts_monitor} object.
#' @param pollutant EPA AQS criteria pollutant.
#' @param NAAQS Version of NAAQS levels to use. See Note.
#' @param conversionArray Array of six text or other values to return instead of integers.
#'
#' @return A vector or matrix of AQI category indices in the range 1:6.
#'
#' @description
#' This function converts hourly PM2.5 measurements into AQI category levels.
#' These levels can then be converted to colors or names using the arrays found
#' in \code{\link{US_AQI}}.
#'
#' @details
#' By default, return values will be integers in the range 1:6 or \code{NA}. The
#' \code{conversionArray} parameter can be used to convert these integers into
#' whatever is specified in the first six elements of \code{conversionArray}. A
#' typical usage would be: \code{conversionArray = US_AQI$names_eng}.
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
#' @seealso \code{\link{aqiColors}}
#'
#' @examples
#' library(AirMonitor)
#'
#' # Lane County, Oregon AQSIDs all begin with "41039"
#' LaneCounty <-
#'   NW_Megafires %>%
#'   monitor_filter(stringr::str_detect(AQSID, '^41039')) %>%
#'   monitor_filterDate(20150822, 20150823)
#'
#' LaneCounty %>%
#'   aqiCategories()
#'
#' LaneCounty %>%
#'   aqiCategories(conversionArray = US_AQI$names_eng)



aqiCategories <- function(
  x,
  pollutant = c("PM2.5", "AQI", "CO", "NO", "OZONE", "PM10", "SO2"),
  NAAQS = c("PM2.5", "PM2.5_2024"),
  conversionArray = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  pollutant <- match.arg(pollutant)
  NAAQS = match.arg(NAAQS)

  breaks <- US_AQI[[paste0("breaks_", pollutant)]]

  # Handle the added NAAQS argument
  if ( pollutant == "PM2.5" && NAAQS == "PM2.5_2024" ) {
    breaks <- US_AQI$breaks_PM2.5_2024
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

  # ----- Create categories ----------------------------------------------------

  categories <- .bincode(x, breaks)

  if ( !is.null(conversionArray) ) {
    categories <- conversionArray[categories]
  }

  # ----- Return ---------------------------------------------------------------

  # Restore shape
  if ( ncol > 1 ) {
    categories <- matrix(categories, ncol = ncol, byrow = FALSE)
  }

  return(categories)

}
