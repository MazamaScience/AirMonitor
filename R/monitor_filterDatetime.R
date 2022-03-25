#' @export
#'
#' @title Datetime filtering for \code{mts_monitor} objects
#'
#' @param monitor \emph{mts_monitor} object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret \code{startdate} and \code{enddate}.
#' @param unit Units used to determine time at end-of-day.
#' @param ceilingStart Logical specifying application of
#'   \code{\link[lubridate]{ceiling_date}} to the \code{startdate} rather than
#'   \code{\link[lubridate]{floor_date}}
#' @param ceilingEnd Logical specifying application of
#'   \code{\link[lubridate]{ceiling_date}} to the \code{enddate} rather than
#'   \code{\link[lubridate]{floor_date}}
#'
#' @description Subsets a \emph{mts_monitor} object by datetime. This function
#' allows for sub-day filtering as opposed to \code{monitor_filterDate()} which
#' always filters to day-boundaries.
#'
#' Datetimes can be anything that is understood by
#' \code{MazamaCoreUtils::parseDatetime()}. For non-\code{POSIXct} values,
#' the recommended format is \code{"YYYY-mm-dd HH:MM:SS"}.
#'
#' Timezone determination precedence assumes that if you are passing in
#' \code{POSIXct} values then you know what you are doing.
#'
#' \enumerate{
#' \item{get timezone from \code{startdate} if it is \code{POSIXct}}
#' \item{use passed in \code{timezone}}
#' \item{get timezone from \code{mts_monitor}}
#' }
#'
#' @return A subset of the given \emph{mts_monitor} object. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{monitor_filterDate}
#' @seealso \link{monitor_filterMeta}
#'
#' @examples
#' library(AirMonitor)
#'
#' Camp_Fire %>%
#'   monitor_timeRange()
#'
#' # Reduced time range returned in "UTC"
#' Camp_Fire %>%
#'   monitor_filterDatetime(
#'     "2018-11-15 02:00:00",
#'     "2018-11-22 06:00:00",
#'     timezone = "America/Los_Angeles"
#'   ) %>%
#'   monitor_timeRange()
#'
#' # Reduced time range returned in "America/Los_Angeles"
#' Camp_Fire %>%
#'   monitor_filterDatetime(
#'     "2018111502",
#'     "2018112206",
#'     timezone = "America/Los_Angeles"
#'   ) %>%
#'   monitor_timeRange(
#'     timezone = "America/Los_Angeles"
#'   )
#'

monitor_filterDatetime <- function(
  monitor = NULL,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  unit = "sec",
  ceilingStart = FALSE,
  ceilingEnd = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(startdate)
  MazamaCoreUtils::stopIfNull(enddate)

  # A little involved to catch the case where the user forgets to pass in 'monitor'
  result <- try({
    if ( !monitor_isValid(monitor) )
      stop("First argument is not a valid 'mts_monitor' object.")
  }, silent = TRUE)

  if ( class(result) %in% "try-error" ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "object .* not found") ) {
      stop(paste0(err_msg, "\n(Did you forget to pass in the 'monitor' object?)"))
    }
  }

  if ( monitor_isEmpty(monitor) )
    stop("Parameter 'monitor' has no data.")

  # Deal with missing timezones
  if ( is.null(timezone) ) {
    if ( length(unique(monitor$meta$timezone)) == 1 ) {
      timezone <- monitor$meta$timezone[1]
    } else {
      if ( lubridate::is.POSIXct(startdate) ) {
        timezone <- lubridate::tz(startdate)
      } else {
        message("Multiple timezones found and none specified. Using 'UTC'.")
        timezone <- "UTC"
      }
    }
  }

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <-
    MazamaTimeSeries::mts_filterDatetime(
      mts = monitor,
      startdate = startdate,
      enddate = enddate,
      timezone = timezone,
      unit = unit,
      ceilingStart = ceilingStart,
      ceilingEnd = ceilingEnd
    )

  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}
