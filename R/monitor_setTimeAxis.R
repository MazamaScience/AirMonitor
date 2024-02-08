#' @export
#'
#' @title Extend/contract \emph{mts_monitor} time series to new start and end times
#'
#' @param monitor \emph{mts_monitor} object.
#' @param startdate Desired start date (ISO 8601).
#' @param enddate Desired end date (ISO 8601).
#' @param timezone Olson timezone used to interpret \code{startdate} and \code{enddate}.
#'
#' @description Extends or contracts the time range of an \emph{mts_monitor} object by
#' adding/removing time steps at the start and end and filling any new time
#' steps with missing values. The resulting time axis is guaranteed to be
#' a regular, hourly axis with no gaps using the same timezone as the incoming
#' \emph{mts_monitor} object. This is useful when you want to place separate \emph{mts_monitor}
#' objects on the same time axis for plotting.
#'
#' If either \code{startdate} or \code{enddate} is missing, the start or end of
#' the timeseries in \code{monitor} will be used.
#'
#' @note If \code{startdate} or \code{enddate} is a \code{POSIXct} value, then
#' \code{timezone} will be set to the timezone associated with \code{startdate}
#' or \code{enddate}.
#' In this common case, you don't need to specify \code{timezone} explicitly.
#'
#' If neither \code{startdate} nor \code{enddate} is a \code{POSIXct} value
#' AND no \code{timezone} is supplied, the timezone will be inferred from
#' the most common timezone found in \code{monitor}.
#'
#' @return The incoming \emph{mts_monitor} time series object defined on a new time axis.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @examples
#' library(AirMonitor)
#'
#' # Default range
#' Carmel_Valley %>%
#'   monitor_timeRange()
#'
#' # One-sided extend with user specified timezone
#' Carmel_Valley %>%
#'   monitor_setTimeAxis(enddate = 20160820, timezone = "UTC") %>%
#'   monitor_timeRange()
#'
#' # Two-sided extend with user specified timezone
#' Carmel_Valley %>%
#'   monitor_setTimeAxis(20190720, 20190820, timezone = "UTC") %>%
#'   monitor_timeRange()
#'
#' # Two-sided extend without timezone (uses monitor$meta$timezone)
#' Carmel_Valley %>%
#'   monitor_setTimeAxis(20190720, 20190820) %>%
#'   monitor_timeRange()
#'

monitor_setTimeAxis <- function(
    monitor = NULL,
    startdate = NULL,
    enddate = NULL,
    timezone = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)

  # NOTE:  Additional validation is handled by MazamaTimeSeries::mts_setTimeAxis()

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <-
    MazamaTimeSeries::mts_setTimeAxis(
      mts = monitor,
      startdate = startdate,
      enddate = enddate,
      timezone = timezone
    )

  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(invisible(monitor))

}
