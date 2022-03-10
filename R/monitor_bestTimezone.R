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
#' @return A valid \code{base::OlsonNames()} timezone.
#'

monitor_bestTimezone <- function(
  monitor = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)

  # ----- Find most common timezone --------------------------------------------

  timezoneTable <- sort(table(monitor$meta$timezone), decreasing = TRUE)
  timezone <- names(timezoneTable)[1]

  if ( !timezone %in% OlsonNames() )
    stop(sprintf("timezone '%s' is not a valid OlsonNames() timezone", timezone))

  # ----- Return ---------------------------------------------------------------

  return(timezone)

}
