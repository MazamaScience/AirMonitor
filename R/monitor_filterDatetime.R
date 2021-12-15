#' @export
#'
#' @title Datetime filtering for \code{mts_monitor} objects
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
#' @description Subsets a \code{mts_monitor} object by datetime. This function
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
#' @return A subset of the given \emph{mts_monitor} object.
#'
#' @seealso \link{monitor_filterDate}
#' @seealso \link{monitor_filterMeta}
#'
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

  MazamaCoreUtils::stopIfNull(monitor)
  MazamaCoreUtils::stopIfNull(startdate)
  MazamaCoreUtils::stopIfNull(enddate)

  if ( !monitor_isValid(monitor) )
    stop("Parameter 'monitor' is not a valid 'mts_monitor' object.")

  if ( monitor_isEmpty(monitor) )
    stop("Parameter 'monitor' has no data.")

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
