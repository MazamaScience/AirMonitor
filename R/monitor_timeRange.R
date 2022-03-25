#' @export
#' @title Get the time range for a monitor
#'
#' @description This function is a wrapper for \code{range(monitor$data$datetime)}
#' and is convenient for use in data pipelines.
#'
#' Dates will be returned in the timezone associated with
#' \code{monitor$data$datetime} which is typically "UTC" unless
#' \code{timezone} is specified.
#'
#' @param monitor \emph{mts_monitor} object.
#' @param timezone Olson timezone for the returned dates.
#'
#' @return A vector containing the minimum and maximum times of a
#' \emph{mts_monitor} object.
#'
#' @examples
#' Carmel_Valley %>%
#'   monitor_timeRange(timezone = "America/Los_Angeles")


monitor_timeRange <- function(
  monitor = NULL,
  timezone = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)

  if ( !is.null(timezone) ) {
    if ( !timezone %in% OlsonNames() )
      stop(sprintf("timezone '%s' is not a valid OlsonNames() timezone", timezone))
  }

  # ----- Return ---------------------------------------------------------------

  timeRange <- range(monitor$data$datetime)

  if ( !is.null(timezone) )
    timeRange <- lubridate::with_tz(timeRange, tzone = timezone)

  return(timeRange)

}



