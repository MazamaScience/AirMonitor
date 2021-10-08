#' @export
#' @importFrom rlang .data
#'
#' @title General purpose metadata filtering for \emph{monitor} objects
#'
#' @param monitor \emph{monitor} object.
#' @param ... Logical predicates defined in terms of the variables in
#' \code{monitor$meta}.
#'
#' @description A generalized metadata filter for \emph{monitor} objects to
#' choose rows/cases where conditions are true.  Multiple conditions are
#' combined with \code{&} or separated by a comma. Only rows where the condition
#' evaluates to TRUE are kept. Rows where the condition evaluates to \code{NA}
#' are dropped.
#'
#' @note Filtering is done on variables in \code{monitor$meta}.
#'
#' @return A subset of the incoming \code{monitor}.
#'
#'

monitor_filterMeta <- function(
  monitor,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  # A little involved to catch the case where the user forgets to pass in 'monitor'

  # TODO:  use monitor_isValid(monitor) when it exists

  # result <- try({
  #   if ( !monitor_isValid(monitor) )
  #     stop("First argument is not a valid 'monitor' object.")
  # }, silent = TRUE)
  #
  # if ( class(result) %in% "try-error" ) {
  #   err_msg <- geterrmessage()
  #   if ( stringr::str_detect(err_msg, "object .* not found") ) {
  #     stop(paste0(err_msg, "\n(Did you forget to pass in the 'monitor' object?)"))
  #   }
  # }

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <- MazamaTimeSeries::mts_filterMeta(monitor, ...)

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}
