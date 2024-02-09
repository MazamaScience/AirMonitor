#' @export
#'
#' @title Create daily statistics for each monitor in an \emph{mts_monitor} object
#'
#' @param monitor \emph{mts_monitor} object.
#' @param FUN Function used to create daily statistics.
#' @param na.rm Value passed on to \code{FUN}. If \code{FUN} does not use
#' \code{na.rm}, this should be set to \code{NULL}.
#' @param minHours Minimum number of valid hourly records per day required to
#' calculate statistics. Days with fewer valid records will be assigned \code{NA}.
#' @param dayBoundary Treatment of daylight savings time:  "clock" uses daylight
#' savings time as defined in the local timezone, "LST" uses "local standard time"
#' all year round.
#' @param ... Additional arguments to be passed to \code{FUN}.
#'
#' @return A \emph{mts_monitor} object containing daily statistical summaries. (A list with
#' \code{meta} and \code{data} dataframes.)
#'
#' @description
#' Daily statstics are calculated for each time series in \code{monitor$data}
#' using \code{FUN} and any arguments passed in \code{...}.
#'
#' Because the returned \emph{mts_monitor} object is defined on a daily axis in a
#' specific time zone, it is important that the incoming \code{monitor} contain
#' timeseries associated with a single time zone.
#'
#' @note
#' When \code{dayBoundary = "clock"}, the returned \code{monitor$data$datetime}
#' time axis will be defined in the local timezone (not "UTC") with days defined
#' by midnight as it appears on a clock in that timezone. The transition from
#' DST to standard time will result in a 23 hour day and standard to DST in a
#' 25 hour day.
#'
#' When \code{dayBoundary = "LST"}, the returned \code{monitor$data$datetime}
#' time axis will be defined in "UTC" with times as they \emph{appear} in standard
#' time in the local timezone. These days will be one hour off from clock
#' time during DST but every day will consist of 24 hours.
#'
#' @examples
#' library(AirMonitor)
#'
#' Carmel_Valley %>%
#'   monitor_dailyStatistic(max) %>%
#'   monitor_getData()
#'
#' Carmel_Valley %>%
#'   monitor_dailyStatistic(min) %>%
#'   monitor_getData()

monitor_dailyStatistic <- function(
  monitor = NULL,
  FUN = mean,
  na.rm = TRUE,
  minHours = 18,
  dayBoundary = c("clock", "LST"),
  ...
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  MazamaCoreUtils::stopIfNull(FUN)
  MazamaCoreUtils::stopIfNull(minHours)
  dayBoundary <- match.arg(dayBoundary)

  if ( length(unique(monitor$meta$timezone)) > 1 )
    stop("'monitor' has muliple timezones")

  timezone <- unique(monitor$meta$timezone)

  # ----- Create LST time axis -------------------------------------------------

  if ( dayBoundary == "LST" ) {

    # NOTE:  There is no recognized timezone where LST exists so we have to be clever.

    # NOTE:  The EPA defines regulatory daily averages as midnight-to-midnight
    # NOTE:  in local-standard-time-all-year. Here we calculate LST times
    # NOTE:  but move them to the UTC timezone where no daylight savings
    # NOTE:  adjustment will be applied by the lubridate package.

    # Calculate the Local Standard Time offset
    Christmas_UTC <- lubridate::ymd_h("2019-12-25 00", tz = "UTC")
    Christmas_localTime <- lubridate::with_tz(Christmas_UTC, tzone = timezone)
    Christmas_localTime_UTC <- lubridate::force_tz(Christmas_localTime, tzone = "UTC")
    lst_offset <- as.numeric(difftime(Christmas_localTime_UTC, Christmas_UTC, units = "hours"))

    localStandardTime_UTC <-
      lubridate::with_tz(monitor$data$datetime, tzone = "UTC") +
      lst_offset * lubridate::dhours(1)

    monitor$data$datetime <- localStandardTime_UTC

  }

  # ----- Create daily statistic -----------------------------------------------

  # MazamaTimeSeries::mts_summarize() function signature:
  #
  # mts_summarize <- function(
  #   mts,
  #   timezone = NULL,
  #   unit = c("day", "week", "month", "year"),
  #   FUN = NULL,
  #   ...,
  #   minCount = NULL
  # ) {

  argsList <- list(...)

  if ( !is.null(na.rm) )
    argsList$na.rm <- na.rm

  if ( dayBoundary == "LST" )
    argsList$timezone = "UTC"

  argsList$mts <- monitor
  argsList$unit <- "day"
  argsList$FUN <- FUN
  argsList$minCount <- minHours

  daily <- do.call(MazamaTimeSeries::mts_summarize, argsList)

  # ----- Return ---------------------------------------------------------------

  return(daily)

}
