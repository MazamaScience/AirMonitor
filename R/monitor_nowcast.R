#' @export
#'
#' @title Apply NowCast algorithm to \emph{mts_monitor} data
#'
#' @param monitor \emph{mts_monitor} object.
#' @param version Name of the type of nowcast algorithm to be used.
#' @param includeShortTerm Logical specifying whether to alcluate preliminary
#' NowCast values starting with the 2nd hour.
#'
#' @return A modified \code{mts_monitor} object. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @description A NowCast algorithm is applied to the data in in the
#' \code{monitor} object. The \code{version} argument specifies the minimum
#' weight factor and number of hours to be used in the calculation.
#'
#' Available versions include:
#' \enumerate{
#' \item{\code{pm}: hours = 12, weight = 0.5}
#' \item{\code{pmAsian}: hours = 3, weight = 0.1}
#' \item{\code{ozone}: hours = 8, weight = NA}
#' }
#'
#' The default, \code{version = "pm"}, is appropriate for typical usage.
#'
#' @details
#' This function calculates each hour's NowCast value based on the value
#' for the given hour and the previous N-1 hours, where N is the number
#' of hours appropriate for the \code{version} argument. For example, if
#' \code{version = "pm"}, the NowCast value for Hour 12 is based on the data
#' from hours 1-12.
#'
#' The function returns values when at least two of the previous three hours
#' have data. NA's are returned for hours where this condition is not met.
#'
#' By default, the funtion will not return a valid value until the Nth hour.
#' If \code{includeShortTerm = TRUE}, the function will return a valid value
#' after only the 2nd hour (provided, of course, that both hours are valid).
#'
#' Calculated Nowcast values are truncated to the nearest .1 ug/m3 for 'pm' and
#' nearest .001 ppm for 'ozone' regardless of the precision of the data in the
#' incoming \emph{mts_monitor} object.
#'
#' @references \url{https://en.wikipedia.org/wiki/Nowcast_(Air_Quality_Index)}
#' @references \href{https://www.airnow.gov/sites/default/files/2020-05/aqi-technical-assistance-document-sept2018.pdf}{AQI Technical Assistance Document}
#'

# NOTE:  This script is based on the javascript code at:
# NOTE:    https://github.com/chatch/nowcast-aqi/blob/master/nowcast-aqi.js
# NOTE:  To compute a valid NowCast, you must have at least two of the most recent 3 hours

# TODO:  python-aqi at: https://pypi.python.org/pypi/python-aqi

#### ----- NowCast Calculation Overview -----
#
# The process for calculating the NowCast concentration and AQI for PM2.5 or PM10 is as follows:
#
# 1. Compute the concentrations range (max-min) over the last 12 hours.
# 2. Divide the range by the maximum concentration in the 12 hour period to obtain the scaled rate of change.
# 3. Compute the weight factor by subtracting the scaled rate from 1. The weight factor must be between .5 and 1.
#    The minimum limit approximates a 3-hour average. If the weight factor is less than .5, then set it equal to .5.
# 4. Multiply each hourly concentration by the weight factor raised to the power of how many hours ago the concentration
#    was measured (for the current hour, the factor is raised to the zero power).
# 5. Compute the NowCast by summing these products and dividing by the sum of the weight factors raised to the power of
#    how many hours ago the concentration was measured.

monitor_nowcast <- function(
  monitor,
  version = c("pm", "pmAsian", "ozone"),
  includeShortTerm = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  version <- match.arg(version)
  includeShortTerm <- MazamaCoreUtils::setIfNull(includeShortTerm, FALSE)

  # A little involved to catch the case where the user forgets to pass in 'monitor'

  result <- try({
    if ( !monitor_isValid(monitor) )
      stop("First argument is not a valid 'mts_monitor' object.")
  }, silent = TRUE)

  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "object .* not found") ) {
      stop(paste0(err_msg, "\n(Did you forget to pass in the 'monitor' object?)"))
    }
  }

  if ( monitor_isEmpty(monitor) )
    stop("Parameter 'monitor' has no data.")

# ----- Choose settings --------------------------------------------------------

  # Set parameters based on version
  if ( version == "pm" ) {
    numHrs <- 12
    weightFactorMin <- 0.5
    digits <- 1
  } else if ( version == "pmAsian" ) {
    numHrs <- 3
    weightFactorMin <- 0.1
    digits <- 1
  } else if ( version == "ozone" ) {
    numHrs <- 8
    weightFactorMin <- NA  # negative values adjusted up to 0 in .weightFactor()
    digits <- 3  # NOTE: digits = 3 assumes Ozone values given in ppm; update to 0 if values given in ppb
  }

  # ----- Apply function -------------------------------------------------------

  # Apply nowcast to each data column
  newData <- apply(
    dplyr::select(monitor$data, -1),
    2,
    function(x) { .nowcast(x, numHrs, weightFactorMin, includeShortTerm) }
  )

  # NOTE:  Truncate, rather than round, per the following:
  # NOTE:    https://forum.airnowtech.org/t/the-nowcast-for-ozone-and-pm/172
  monitor$data[2:ncol(monitor$data)] <- as.data.frame(trunc(newData*10^digits)/10^digits)

  # ----- Return ---------------------------------------------------------------

  return( structure(monitor, class = c("mts_monitor", "list")) )

}

# ===== Internal Functions =====================================================

.nowcast <- function(x, numHrs, weightFactorMin, includeShortTerm) {

  if ( includeShortTerm ) {
    firstHr <- 2
  } else {
    firstHr <- numHrs
  }

  # Start at the end of the data (most recent hour) and work backwards
  # The oldest hour for which we can calculate nowcast is numHrs, unless
  # includeShortTerm = TRUE, in which case we can go back to the 2nd hour.

  for ( i in length(x):firstHr ) {

    # Apply nowcast algorithm to numHrs data points in order with more recent first
    concByHour <- x[i:max(1, i - numHrs + 1)]

    if ( sum( is.na(concByHour[1:3]) ) >= 2 ) {

      # If two or more of the most recent 3 hours are missing, no valid Nowcast will be reported

      x[i] <- NA

    } else if ( is.na(concByHour[1]) ) {

      # If the current hour is missing, no valid Nowcast will be reported

      # NOTE:  This conflicts with the algorithm as described here:
      # NOTE:    https://forum.airnowtech.org/t/daily-and-hourly-aqi-pm2-5/171
      # NOTE:
      # NOTE:  But experience shows that NowCast replacements for missing
      # NOTE:  PM2.5 values are very problematic.
      # NOTE:
      # NOTE:  The Wikipedia page: https://en.wikipedia.org/wiki/NowCast_(air_quality_index)
      # NOTE:  has the following statement without citation:
      # NOTE:    "Because the most recent hours of data are weighted so heavily in the NowCast when
      # NOTE:    PM levels are changing, EPA does not report the NowCast when data is missing for c1 or c2."
      # NOTE:
      # NOTE:  We take a compromise approach and only invalidate NowCast when data is missing for c1.

      x[i] <- NA

    } else {

      # Calculate the weight factor according to the type of air quality data
      weightFactor <- .weightFactor(concByHour, weightFactorMin)

      # NOTE:  We need to create vectors so that we can sum at the end with na.rm = TRUE

      weightedConcs <- rep(as.numeric(NA), numHrs)
      weightFactors <- rep(as.numeric(NA), numHrs)

      # Loop over hours to get individual elements
      for (j in 1:numHrs) {
        if ( !is.na( concByHour[j]) ) {
          weightedConcs[j] <- concByHour[j] * weightFactor^(j - 1)
          weightFactors[j] <- weightFactor^(j - 1)
        }
      }

      x[i] <- sum(weightedConcs, na.rm = TRUE) / sum(weightFactors, na.rm = TRUE)

    }
  }

  # Set missing values when there are not enough preceding hours
  x[1:(firstHr - 1)] <- NA

  return(x)

}

# Calculate the weight factor ('w' in the nowcast formula)
#   concByHour: vector of hourly concentration values
#   weightFactorMin (optional): wight factor minimum
#
# Assumes concByHour has at least one valid value to calculate min & max. In
# fact, .nowcast won't even call this function if more than one of the three
# most recent hours is invalid.
.weightFactor <- function(concByHour, weightFactorMin) {

  min <- min(concByHour, na.rm = TRUE)
  max <- max(concByHour, na.rm = TRUE)

  # Calculate weight factor
  # NOTE:  https://forum.airnowtech.org/t/the-nowcast-for-ozone-and-pm/172 says that there is "no minimum
  # NOTE:    weight factor" for ozone; however, we limit the value to zero since otherwise it would be possible
  # NOTE:    to get negative weights, even as large as -Inf (i.e. if min<0 & max=0).
  # NOTE:  Otherwise, we don't worry about negatives, per the following:
  # NOTE:    https://forum.airnowtech.org/t/how-does-airnow-handle-negative-hourly-concentrations/143
  weightFactor <- 1 - (max - min)/max
  weightFactor <- min(weightFactor, 1, na.rm = TRUE)
  weightFactor <- max(weightFactor, weightFactorMin, 0, na.rm = TRUE)

  return(weightFactor)

}

# ===== DEBUG =================================================================

if ( FALSE ) {

  # From: https://forum.airnowtech.org/t/the-nowcast-for-pm2-5-and-pm10/172
  x <- c(34.9, 43, 50, 64.9, 69.2, 66.2, 53.7, 48.6, 49.2, 35, NA, 21)

  .nowcast(x, 12, 0.5, FALSE)

  print(.nowcast(x, 12, 0.5, TRUE)[12], digits = 8)
  # 28.409801 matches the result in the web page.

}
