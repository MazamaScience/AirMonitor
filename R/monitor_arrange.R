#' @export
#'
#' @title Order \emph{mts_monitor} time series by metadata values
#'
#' @param monitor \emph{mts_monitor} object.
#' @param ... variables in \code{mts$meta}.
#'
#' @description The variable(s) in \code{...} are used to specify columns of
#' \code{monitor$meta} to use for ordering. Under the hood, this
#' function uses \code{\link[dplyr]{arrange}} on \code{monitor$meta} and then
#' reorders \code{monitor$data} to match.
#'
#' @return A reordered version of the incoming \emph{mts} time series object.
#' (A list with \code{meta} and \code{data} dataframes.)
#'
#' @seealso \link{monitor_select}
#'
#' @examples
#' library(AirMonitor)
#'
#' Camp_Fire$meta$elevation[1:10]
#'
#' byElevation <-
#'   Camp_Fire %>%
#'   monitor_arrange(elevation)
#'
#' byElevation$meta$elevation[1:10]
#'

monitor_arrange <- function(
  monitor,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

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

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <- MazamaTimeSeries::mts_arrange(monitor, ...)
  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}
