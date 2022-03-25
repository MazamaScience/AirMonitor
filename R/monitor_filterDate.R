#' @export
#'
#' @title Date filtering for \emph{mts_monitor} objects
#'
#' @param monitor \emph{mts_monitor} object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates.
#' @param unit Units used to determine time at end-of-day.
#' @param ceilingStart Logical instruction to apply
#'   \code{\link[lubridate]{ceiling_date}} to the \code{startdate} rather than
#'   \code{\link[lubridate]{floor_date}}
#' @param ceilingEnd Logical instruction to apply
#'   \code{\link[lubridate]{ceiling_date}} to the \code{enddate} rather than
#'   \code{\link[lubridate]{floor_date}}
#'
#' @description Subsets a \emph{mts_monitor} object by date. This function
#' always filters to day-boundaries. For sub-day filtering, use
#' \code{monitor_filterDatetime()}.
#'
#' Dates can be anything that is understood by \code{MazamaCoreUtils::parseDatetime()}
#' including either of the following recommended formats:
#'
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' }
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
#' @note The returned data will run from the beginning of \code{startdate} until
#' the \strong{beginning} of \code{enddate} -- \emph{i.e.} no values associated
#' with \code{enddate} will be returned. The exception being when
#' \code{enddate} is less than 24 hours after \code{startdate}. In that case, a
#' single day is returned.
#'
#' @return A subset of the given \emph{mts_monitor} object. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{monitor_filterDatetime}
#' @seealso \link{monitor_filterMeta}
#'
#' @examples
#' library(AirMonitor)
#'
#' Camp_Fire %>%
#'   monitor_timeRange()
#'
#' # Day boundaries returned in "UTC"
#' Camp_Fire %>%
#'   monitor_filterDate(
#'     "2018-11-15",
#'     "2018-11-22",
#'     timezone = "America/Los_Angeles"
#'   ) %>%
#'   monitor_timeRange()
#'
#' # Day boundaries returned in "America/Los_Angeles"
#' Camp_Fire %>%
#'   monitor_filterDatetime(
#'     "20181115",
#'     "20181122",
#'     timezone = "America/Los_Angeles"
#'   ) %>%
#'   monitor_timeRange(
#'     timezone = "America/Los_Angeles"
#'   )
#'

monitor_filterDate <- function(
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
    MazamaTimeSeries::mts_filterDate(
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
