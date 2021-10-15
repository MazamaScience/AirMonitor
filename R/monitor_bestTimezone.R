#' @export
#'
#' @title Return the most common timezone
#'
#' @param monitor \emph{mts_monitor} object.
#'
#' @description Evaluates all timezones in \code{monitor} and returns the
#' most common one. In the case of a tie, the alphabetically first one is
#' returned.
#'
#' @return A valid \code{OlsonNames()} timezone.
#'

monitor_bestTimezone <- function(
  monitor = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)

  # ----- Find most common timezone --------------------------------------------

  # Handle multiple timezones
  timezoneCount <- length(unique(monitor$meta$timezone))

  # Use table(timezone) to find the most common one
  if ( timezoneCount == 1 ) {
    timezone <- monitor$meta$timezone[1]
  } else {
    timezoneTable <- sort(table(monitor$meta$timezone), decreasing = TRUE)
    timezone <- names(timezoneTable)[1]
  }

  if ( !timezone %in% OlsonNames() )
    stop(sprintf("timezone '%s' is not a valid Olson timezone", timezone))

  # ----- Return ---------------------------------------------------------------

  return(timezone)

}
