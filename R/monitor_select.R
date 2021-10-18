#' @export
#' @importFrom rlang .data
#'
#' @title Select monitors within a \emph{mts_monitor} object
#'
#' @param monitor \emph{mts_monitor} object.
#' @param id Vector of \code{deviceDeploymentIDs}.
#'
#' @description Selects a subset of monitors within a \emph{mts_monitor} object.
#' This function basically performs \code{dplyr::select()} on \code{monitor$data}
#' and is more convenient than using \code{monitor_filterMeta()}. Compare;
#'
#' \preformatted{
#' ids <- c(... some colleciton of deviceDeploymentIDs ...)
#'
#' monitor %>%
#'   monitor_filter(deviceDeploymentID %in% ids)
#'
#' monitor %>%
#'   monitor_select(ids)
#' }
#'
#' @note Filtering is done on variables in \code{monitor$meta}.
#'
#' @return A subset of the incoming \code{mts_monitor}.
#'
#' @seealso \link{monitor_filterMeta}
#'

monitor_select <- function(
  monitor,
  id
) {

  monitor <-
    monitor %>%
    monitor_filterMeta(.data$deviceDeploymentID %in% id)

  return(monitor)

}
