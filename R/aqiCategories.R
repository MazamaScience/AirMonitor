#' @export
#'
#' @title Generate AQI categories
#'
#' @param x Vector or matrix of PM2.5 values or an \emph{mts_monitor} object.
#' @param pollutant EPA AQS criteria pollutant.
#' @param NAAQS User provided NAAQS levels to use.
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
  pollutant = c("PM2.5", "CO", "OZONE", "PM10", "AQI"),
  NAAQS = NULL,
  conversionArray = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  pollutant <- match.arg(pollutant)

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

  # ----- Create categories ----------------------------------------------------

  categories <- .bincode(
    round(x, digits = 1),
    breaks,
    right = TRUE,
    include.lowest = TRUE
  )

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
