#' @export
#'
#' @title Combine multiple \code{mts_monitor} objects
#'
#' @param ... Any number of valid emph{mts_monitor} objects or a list of objects.
#' @param replaceMeta Logical specifying whether to allow replacement of metadata
#' associated when duplicate \code{deviceDeploymentIDs} are encountered.
#' @param overlapStrategy Strategy to use when data found in time series
#' overlaps.
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
#' @note Data are combined with a "later is better" sensibility where any
#' data overlaps exist. Incoming \emph{mts_monitor} objects are ordered based on the
#' time stamp of their last record. Any data records found in a "later" \emph{mts_monitor}
#' will overwrite data associated with an "earlier" \emph{mts_monitor}.
#'
#' With \code{overlapStrategy = "replace all"}, any data records found
#' in "later" \emph{mts_monitor} objects are preferentially retained before the "shared"
#' data are finally reordered by ascending \code{datetime}.
#'
#' With \code{overlapStrategy = "replace missing"}, only missing values in "earlier"
#' \emph{mts_monitor} objects are replaced with data records from "later" time series.
#'
#'
#' @examples
#' library(AirMonitor)
#'
#' # Two monitors near Pendelton, Oregon
#' #
#' # Use the interactive map to get the deviceDeploymentIDs
#' #   NW_Megafires %>% monitor_leaflet()
#'
#' Pendleton_West <-
#'   NW_Megafires %>%
#'   monitor_select("f187226671d1109a_410590121_03") %>%
#'   monitor_filterDatetime(2015082300, 2015082305)
#'
#' Pendleton_East <-
#'   NW_Megafires %>%
#'   monitor_select("6c906c6d1cf46b53_410597002_02") %>%
#'   monitor_filterDatetime(2015082300, 2015082305)
#'
#' monitor_combine(Pendleton_West, Pendleton_East) %>%
#'   monitor_getData()
#'

monitor_combine <- function(
  ...,
  replaceMeta = FALSE,
  overlapStrategy = c("replace all", "replace na")
) {

  # Accept any number of monitor objects
  monitorList <- list(...)

  # ----- Validate parameters --------------------------------------------------

  if ( length(monitorList) == 0 )
    stop("no 'monitor' arguments provided")

  overlapStrategy <- match.arg(overlapStrategy)

  # ----- Call MazamaTimeSeries function ---------------------------------------

  result <- try({
    monitor <-
      MazamaTimeSeries::mts_combine(
        ...,
        replaceMeta = replaceMeta,
        overlapStrategy = overlapStrategy
      )
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
