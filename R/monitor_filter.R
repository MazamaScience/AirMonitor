#' @export
#'
#' @title General purpose data filtering for \emph{monitor} objects
#'
#' @param monitor \emph{monitor} object.
#' @param ... Logical predicates defined in terms of the variables in
#' \code{monitor$data}.
#'
#' @description A generalized data filter for \emph{monitor} objects to
#' choose rows/cases where conditions are true.  Multiple conditions should be
#' combined with \code{&} or separated by a comma. Only rows where the condition
#' evaluates to TRUE are kept. Rows where the condition evaluates to \code{NA}
#' are dropped.
#'
#' @note Filtering is done on variables in \code{monitor$data}.
#'
#' @return A subset of the incoming \code{monitor}.
#'
#' @seealso \link{monitor_filterDate}
#' @seealso \link{monitor_filterDatetime}
#' @seealso \link{monitor_filterMeta}
#'

monitor_filter <- function(
  monitor,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  # A little involved to catch the case where the user forgets to pass in 'monitor'

  result <- try({
    if ( !monitor_isValid(monitor) )
      stop("First argument is not a valid 'monitor' object.")
  }, silent = TRUE)

  if ( class(result) %in% "try-error" ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "object .* not found") ) {
      stop(paste0(err_msg, "\n(Did you forget to pass in the 'monitor' object?)"))
    }
  }

  if ( monitor_isEmpty(monitor) )
    stop("Parameter 'monitor' has no data.")

  # Remove any duplicate data records
  monitor <- monitor_distinct(monitor)

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <- MazamaTimeSeries::mts_filter(monitor, ...)
  class(monitor) <- union("monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}
