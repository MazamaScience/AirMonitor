#' @export
#'
#' @title Trim a \emph{mts_monitor} object to full days
#'
#' @param monitor \emph{mts_monitor} object.
#' @param timezone Olson timezone used to interpret dates.
#'
#' @description Trims the date range of a \emph{mts_monitor} object to local time date
#' boundaries which are \emph{within} the range of data. This has the effect
#' of removing partial-day data records at the start and end of the timeseries
#' and is useful when calculating full-day statistics.
#'
#' Day boundaries are calculated using the specified \code{timezone} or, if
#' \code{NULL}, from \code{monitor$meta$timezone}.
#'
#' @return A subset of the given \emph{mts_monitor} object.
#'
#'

monitor_trimDate <- function(
  monitor = NULL,
  timezone = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)

  if ( !monitor_isValid(monitor) )
    stop("Parameter 'monitor' is not a valid 'mts_monitor' object.")

  if ( monitor_isEmpty(monitor) )
    stop("Parameter 'monitor' has no data.")

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <- MazamaTimeSeries::mts_trimDate(monitor, timezone)
  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}
