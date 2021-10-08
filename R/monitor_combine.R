#' @export
#'
#' @title Combine multiple \code{monitor} objects
#'
#' @param ... Any number of valid emph{monitor} objects.
#'
#' @return A combined \code{monitor} object.
#'
#' @description Create a combined \emph{monitor} from any number of \emph{monitor}
#' objects or from a list of \emph{monitor} objects. The resulting \emph{monitor}
#' object with contain all \code{deviceDeploymentIDs} found in any incoming
#' \emph{monitor} and will have a regular time axis covering the the entire range
#' of incoming data.
#'
#' If incoming time ranges are non-contiguous, the resulting \emph{monitor} will
#' have gaps filled with \code{NA} values.
#'
#' An error is generated if the incoming \emph{monitor} objects have
#' non-identical metadata for the same \code{deviceDeploymentID}.
#'
#' @note Data are combined with a "latest is best" sensibility where any
#' data overlaps exist. Incoming \emph{monitor} objects are ordered based on the
#' time stamp of their last record. Any data records found in a "later" \emph{monitor}
#' will overwrite data associated with an "earlier" \emph{monitor}.
#'
#'

monitor_combine <- function(
  ...
) {

  # Accept any number of monitor objects
  monitorList <- list(...)

  # ----- Validate parameters --------------------------------------------------

  if ( length(monitorList) == 0 )
    stop("no 'monitor' arguments provided")

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <- MazamaTimeSeries::mts_combine(...)
  class(monitor) <- union("monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(invisible(monitor))

}
