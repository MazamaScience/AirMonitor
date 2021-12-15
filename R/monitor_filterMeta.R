#' @export
#'
#' @title General purpose metadata filtering for \emph{mts_monitor} objects
#'
#' @param monitor \emph{mts_monitor} object.
#' @param ... Logical predicates defined in terms of the variables in
#' \code{monitor$meta}.
#'
#' @description A generalized metadata filter for \emph{mts_monitor} objects to
#' choose rows/cases where conditions are true.  Multiple conditions are
#' combined with \code{&} or separated by a comma. Only rows where the condition
#' evaluates to TRUE are kept. Rows where the condition evaluates to \code{NA}
#' are dropped.
#'
#' `monitor_filter()` is just a convenient alias for `monitor_filterMeta()`.
#'
#' @note Filtering is done on variables in \code{monitor$meta}.
#'
#' @return A subset of the incoming \code{mts_monitor}.
#'
#' @seealso \link{monitor_filterDate}
#' @seealso \link{monitor_filterDatetime}
#'

monitor_filterMeta <- function(
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

  monitor <- MazamaTimeSeries::mts_filterMeta(monitor, ...)
  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}

# ===== Alias ==================================================================

# TODO:  Add examples to the alias

#' @rdname monitor_filterMeta
#' @export
monitor_filter <- monitor_filterMeta
