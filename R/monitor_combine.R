#' @export
#'
#' @title Combine multiple \code{mts_monitor} objects
#'
#' @param ... Any number of valid emph{mts_monitor} objects or a list of objects.
#' @param replaceMeta Logical specifying whether to allow replacement of metadata
#' associated with \code{deviceDeploymentIDs}.
#'
#' @return A combined \code{mts_monitor} object. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @description Create a combined \emph{mts_monitor} from any number of \emph{mts_monitor}
#' objects or from a list of \emph{mts_monitor} objects. The resulting \emph{mts_monitor}
#' object with contain all \code{deviceDeploymentIDs} found in any incoming
#' \emph{mts_monitor} and will have a regular time axis covering the the entire range
#' of incoming data.
#'
#' If incoming time ranges are tempporally non-contiguous, the resulting
#' \emph{mts_monitor} will have gaps filled with \code{NA} values.
#'
#' An error is generated if the incoming \emph{mts_monitor} objects have
#' non-identical metadata for the same \code{deviceDeploymentID} unless
#' \code{replaceMeta = TRUE}.
#'
#' @note Data are combined with a "latest is best" sensibility where any
#' data overlaps exist. Incoming \emph{mts_monitor} objects are ordered based on the
#' time stamp of their last record. Any data records found in a "later" \emph{mts_monitor}
#' will overwrite data associated with an "earlier" \emph{mts_monitor}.
#'
#'
#' @examples
#' library(AirMonitor)
#'
#' # Washington State University
#' Pullman <-
#'   NW_Megafires %>%
#'   monitor_select("089a067f92712ad1_530750003") %>%
#'   monitor_filterDatetime(2015080118, 2015080203)
#'
#' # University of Idaho
#' Moscow <-
#'   NW_Megafires %>%
#'   monitor_select("d121a99bc6c2ac7f_160570005") %>%
#'   monitor_filterDatetime(2015080200, 2015080206)
#'
#' monitor_combine(Pullman, Moscow) %>%
#'   monitor_getData()
#'
#'

monitor_combine <- function(
  ...,
  replaceMeta = FALSE
) {

  # Accept any number of monitor objects
  monitorList <- list(...)

  # ----- Validate parameters --------------------------------------------------

  if ( length(monitorList) == 0 )
    stop("no 'monitor' arguments provided")

  # ----- Call MazamaTimeSeries function ---------------------------------------

  result <- try({
    monitor <- MazamaTimeSeries::mts_combine(..., replaceMeta = replaceMeta)
  }, silent = TRUE)

  # Handle errors
  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "non-identical metadata") ) {
      stop(paste(
        "device-deployments have non-identical metadata\n\n",
        "Use 'replaceMeta = TRUE' to avoid this error message."
      ))
    } else {
      stop(err_msg)
    }
  }

  # If we didn't stop, we succeeded, so continue.

  # Ensure we have the proper class name
  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(invisible(monitor))

}
