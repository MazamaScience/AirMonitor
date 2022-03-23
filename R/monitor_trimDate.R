#' @export
#'
#' @title Trim a \emph{mts_monitor} object to full days
#'
#' @param monitor \emph{mts_monitor} object.
#' @param timezone Olson timezone used to interpret dates.
#' @param trimEmptyDays Logical specifying whether to remove days with no data
#' at the beginning and end of the time range.
#'
#' @description Trims the date range of a \emph{mts_monitor} object to local time date
#' boundaries which are \emph{within} the range of data. This has the effect
#' of removing partial-day data records at the start and end of the timeseries
#' and is useful when calculating full-day statistics.
#'
#' By default, multi-day periods of all-missing data at the beginning and end
#' of the timeseries are removed before trimming to date boundaries. If
#' \code{trimEmptyDays = FALSE} all records are retained except for partial days
#' beyond the first and after the last date boundary.
#'
#' Day boundaries are calculated using the specified \code{timezone} or, if
#' \code{NULL}, from \code{monitor$meta$timezone}.
#'
#' @return A subset of the given \emph{mts_monitor} object. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @examples
#' library(AirMonitor)
#'
#' # Non-day boundaries
#' monitor <-
#'   Camp_Fire %>%
#'   monitor_filterDatetime(
#'     "2018111502",
#'     "2018112206",
#'     timezone = "America/Los_Angeles"
#'   )
#'
#' monitor %>%
#'   monitor_timeRange(timezone = "America/Los_Angeles")
#'
#' # Trim to full days only
#' monitor %>%
#'   monitor_trimDate() %>%
#'   monitor_timeRange(timezone = "America/Los_Angeles")
#'

monitor_trimDate <- function(
  monitor = NULL,
  timezone = NULL,
  trimEmptyDays = TRUE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)

  if ( !monitor_isValid(monitor) )
    stop("Parameter 'monitor' is not a valid 'mts_monitor' object.")

  if ( monitor_isEmpty(monitor) )
    stop("Parameter 'monitor' has no data.")

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <- MazamaTimeSeries::mts_trimDate(monitor, timezone, trimEmptyDays)
  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}
