#' @export
#'
#' @title Collapse an \code{mts_monitor} object into a single time series
#'
#' @param monitor \emph{mts_monitor} object.
#' @param longitude Longitude of the collapsed time series.
#' @param latitude Latitude of the collapsed time series.
#' @param deviceID Device identifier for the collapsed time series.
#' @param FUN Function used to collapse multiple time series.
#' @param na.rm Logical specifying whether NA values should be ignored when FUN
#' is applied.
#' @param ... additional arguments to be passed on to the \code{apply()} function.
#'
#' @return A \emph{mts_monitor} object representingn a single time series
#'
#' @description
#' Collapses data from all time series in \code{mts_monitor} into a
#' single-time series \emph{mts_monitor} object using the function provided in the
#' \code{FUN} argument. The single-time series result will be located at the mean
#' longitude and latitude unless \code{longitude} and \code{latitude}
#' parameters are specified.
#'
#' Any columns of meta that are constant across all records will be retained in
#' the returned \emph{mts_monitor} meta.
#'
#' The core metadata associated with this location (\emph{e.g.}
#' \code{countryCode, stateCode, timezone, ...}) will be determined from
#' the most common (or average) value found in \code{mts$meta}. This will be
#' a reasonable assumption for the vast majority of intended use cases where
#' data from multiple instruments in close proximity are averaged together.
#'
#' @note
#' After \code{FUN} is applied, values of \code{+/-Inf} and \code{NaN} are
#' converted to \code{NA}. This is a convenience for the common case where
#' \code{FUN = min/max} or \code{FUN = mean} and some of the time steps have all
#' missing values. See the R documentation for \code{min} for an explanation.
#'

monitor_collapse <- function(
  monitor,
  longitude = NULL,
  latitude = NULL,
  deviceID = "generatedID",
  FUN = mean,
  na.rm = TRUE,
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  # NOTE:  Validate is handled by MazamaTimeSeries::mts_collapse()

  # ----- Call MazamaTimeSeries function ---------------------------------------

  monitor <-
    MazamaTimeSeries::mts_collapse(
      mts = monitor,
      longitude = longitude,
      latitude = latitude,
      deviceID = deviceID,
      FUN = FUN,
      na.rm = na.rm,
      ...
    )

  class(monitor) <- union("mts_monitor", class(monitor))

  # ----- Return ---------------------------------------------------------------

  return(invisible(monitor))

}