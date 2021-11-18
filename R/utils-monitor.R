#' @export
#'
#' @title Check an \emph{mts_monitor} object for validity.
#'
#' @param monitor \emph{mts_monitor} objet.
#'
#' @description Checks on the validity of an \emph{mts_monitor} object. If any test
#' fails, this function will stop with a warning message.
#'
#'
monitor_check <- function(monitor) {
  tryCatch(
    monitor_isValid(monitor, verbose = TRUE),
    warning = function(w) stop(w),
    finally = invisible(TRUE)
  )
}


#' @export
#'
#' @name monitor_isValid
#' @title Test \emph{mts_monitor} object for correct structure
#'
#' @param monitor \emph{mts_monitor} object
#' @param verbose Logical specifying whether to produce detailed warning messages.
#'
#' @description The \code{mts_monitor} is checked for the presence of core
#' \code{meta} and \code{data} columns.
#'
#' Core \code{meta} columns include: (TODO:  complete this list)
#'
#' \itemize{
#'   \item{\code{deviceDeploymentID} -- unique identifier (see \pkg{MazmaLocationUtils})}
#'   \item{\code{deviceID} -- device identifier}
#'   \item{\code{locationID} -- location identifier (see \pkg{MazmaLocationUtils})}
#'   \item{\code{locationName} -- English language name}
#'   \item{\code{longitude} -- decimal degrees E}
#'   \item{\code{latitude} -- decimal degrees N}
#'   \item{\code{elevation} -- elevation of station in m}
#'   \item{\code{countryCode} -- ISO 3166-1 alpha-2}
#'   \item{\code{stateCode} -- ISO 3166-2 alpha-2}
#'   \item{\code{timezone} -- Olson time zone}
#' }
#'
#' Core \code{data} columns include:
#'
#' \itemize{
#'   \item{\code{datetime} -- measurement time (UTC)}
#' }
#'
#' @return \code{TRUE} if \code{mts_monitor} has the correct structure,
#' \code{FALSE} otherwise.
#'
#'
monitor_isValid <- function(
  monitor = NULL,
  verbose = FALSE
) {

  MazamaCoreUtils::stopIfNull(monitor)

  msg <- ifelse(
    verbose,
    function(m) warning(m, call. = FALSE, immediate. = TRUE),
    function(m) NULL
  )

  if ( !"mts_monitor" %in% class(monitor) ) {
    msg("'monitor' is not of class 'mts_monitor'")
    return(invisible(FALSE))
  }

  # Check that it is a valid 'mts' object
  MazamaTimeSeries::mts_check(monitor)

  # Test for metadata
  missingNames <- setdiff(coreMetadataNames, names(monitor$meta))
  if ( length(missingNames) > 0 ) {
    msg(sprintf(
      "monitor$meta is missing columns: %s",
      paste0(missingNames, collapse = ", ")
    ))
    return(invisible(FALSE))
  }

  return(invisible(TRUE))

}


#' @export
#'
#' @title Test for an empty \emph{mts_monitor} object
#'
#' @param monitor \emph{mts_monitor} object
#' @return Invisibly returns \code{TRUE} if no data exist in \code{mts_monitor}, \code{FALSE} otherwise.
#' @description Convenience function for
#' \code{nrow(monitor$data) == 0 || ncol(monitor$data) == 1}.
#' This makes for more readable code in functions that need to test for this.
#'
monitor_isEmpty <- function(monitor) {

  MazamaCoreUtils::stopIfNull(monitor)
  # NOTE:  Use minimal validation for improved speed
  if ( !'data' %in% names(monitor) || !'data.frame' %in% class(monitor$data) )
    stop("monitor is not a valid 'mts_monitor' object")

  returnVal <- nrow(monitor$data) == 0 || ncol(monitor$data) == 1
  return(invisible(returnVal))

}


#' @importFrom rlang .data
#' @export
#'
#' @title Retain only distinct data records in monitor$data
#'
#' @param monitor \emph{mts_monitor} object
#'
#' @return A \emph{mts_monitor} object with no duplicated data records.
#'
#' @description Two successive steps are used to guarantee that the
#' \code{datetime} axis contains no repeated values:
#'
#' \enumerate{
#' \item{remove any duplicate records}
#' \item{guarantee that rows are in \code{datetime} order}
#' }
#'
monitor_distinct <- function(monitor) {

  # NOTE:  Use minimal validation for improved speed
  if ( !'data' %in% names(monitor) || !'data.frame' %in% class(monitor$data) )
    stop("monitor is not a valid 'mts_monitor' object")

  monitor$data <-
    monitor$data %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$datetime)

  if ( any(duplicated(monitor$data$datetime)) )
    stop("duplicate timesteps with differing values found in 'monitor' object")

  return(monitor)

}


#' @title Extract dataframes from \emph{mts_monitor} objects
#'
#' @description
#' These functions are convenient wrappers for extracting the dataframes that
#' comprise a \emph{mts_monitor} object. These functions are designed to be useful when
#' manipulating data in a pipeline chain using \code{\%>\%}.
#'
#' Below is a table showing equivalent operations for each function.
#'
#' \tabular{ll}{
#'   \strong{Function} \tab \strong{Equivalent Operation}\cr
#'   \code{monitor_extractData(monitor)} \tab \code{monitor$data}\cr
#'   \code{monitor_extractMeta(monitor)} \tab \code{monitor$meta}
#' }
#'
#' @param monitor \emph{mts_monitor} object to extract dataframe from.
#'
#' @return A dataframe from the given \emph{mts_monitor} object
#'
#' @name monitor_extractDataFrame
#' @aliases monitor_extractData monitor_extractMeta
#'
NULL


#' @export
#' @rdname monitor_extractDataFrame
#'
monitor_extractData <- function(monitor) {

  # NOTE:  Use minimal validation for improved speed
  if ( !'data' %in% names(monitor) || !'data.frame' %in% class(monitor$data) )
    stop("monitor is not a valid 'mts_monitor' object")

  return(monitor$data)

}


#' @export
#' @rdname monitor_extractDataFrame
#'
monitor_extractMeta <- function(monitor) {

  # NOTE:  Use minimal validation for improved speed
  if ( !'meta' %in% names(monitor) || !'data.frame' %in% class(monitor$meta) )
    stop("monitor is not a valid 'mts_monitor' object")

  return(monitor$meta)

}

