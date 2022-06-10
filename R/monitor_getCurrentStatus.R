#' @export
#' @importFrom rlang .data
#'
#' @title Get current status of monitors
#'
#' @description
#' This function augments \code{monitor$meta} with summary information derived
#' from \code{monitor$data} reflecting recent measurements.
#'
#' @param monitor \emph{mts_monitor} object.
#' @param enddate Time relative to which current status is calculated. By
#' default, it is the latest time in \code{monitor$data$datetime}. This time can
#' be given as a POSIXct time, or a string/numeric value in ymd format (\emph{e.g.}
#' 20190301). This time converted to UTC.
#' @param minHours Minimum number of valid hourly records required to calculate
#' \code{yesterday_PM2.5_avg}. Days with fewer valid records will be assigned \code{NA}.
#' @param dayBoundary Treatment of daylight savings time:  "clock" uses daylight
#' savings time as defined in the local timezone, "LST" uses "local standard time"
#' all year round. (See \code{monitor_dailyStatistic()} for more details.)
#'
#' @return The \code{monitor$meta} table augmented with current status
#' information for each time series.
#'
#' @section "Last" and "Previous":
#' The goal of this function is to provide useful information about what
#' happened recently with each time series in the provided \emph{mts_monitor} object.
#' Devices don't always consistently report data, however, and it is not alwlays
#' useful to have \code{NA}'s reported when there is recent valid data at earlier
#' times. To address this, \code{monitor_getCurrentStatus()} uses \emph{last} and
#' \emph{previous} valid times. These are the time when a monitor most recently
#' reported data, and the most recent time of valid data before that,
#' respectively. By reporting on these times, this function ensures that valid
#' data is returned and provides information on how outdated this information
#' is. This information can be used in maps to show AQI colored dots when data
#' is only a few hours old but gray dots when data is older than some threshold.
#'
#' @section Calculating latency:
#' According to https://docs.airnowapi.org/docs/HourlyDataFactSheet.pdf
#' a datum assigned to 2pm represents the average of data between 2pm and 3pm.
#' So, if we check at 3:15pm and see that we have a value for 2pm but not 3pm
#' then the data are completely up-to-date with zero latency.
#'
#' \code{monitor_getCurrentStatus()} defines latency as the difference between
#' a time index and the next most recent time index associated with a
#' valid value. If there is no more recent time index, then the difference is
#' measured to the given \code{enddate} parameter. Because \emph{mts_monitor}
#' objects are defined on an hourly axis, these differences have units of hours.
#'
#' For example, if the recorded values for a monitor are
#' \code{[16.2, 15.8, 16.4, NA, 14.0, 12.5, NA, NA, 13.3, NA]}, then the \emph{last}
#' valid value is 13.3 with an index is 9, and the \emph{previous} valid value is 12.4
#' with an index of 6. The \emph{last} latency is then 1 (hour before the end), and the
#' \emph{previous} latency is 3 (hours before the last valid value).
#'
#' @section Summary data:
#' The table created by \code{monitor_getCurrentStatus()} includes per-time series
#' summary information calculated from \code{monitor$data}.
#' The additional data fields added to \code{monitor$meta} are listed below:
#'
#' \describe{
#'   \item{currentStatus_processingTime}{Time at which this function was run}
#'   \item{currentStatus_enddate}{Time relative to which "currency" is calculated}
#'   \item{last_validIndex}{Row index of the last valid mesurement in \code{monitor$data}}
#'   \item{previous_validIndex}{Row index of the previous valid measurement in \code{monitor$data}}
#'   \item{last_validTime}{UTC time associated with \code{last_validIndex}}
#'   \item{previous_validTime}{UTC time associated with \code{previous_validIndex}}
#'   \item{last_latency}{Hours between \code{last_validTime} and \code{endtime}}
#'   \item{previous_latency}{Hours between \code{previous_validTime} and\code{last_validTime}}
#'   \item{last_validLocalTimestamp}{Local time representation of \code{last_validTime}}
#'   \item{previous_validLocalTimestamp}{Local time representation of \code{previous_validTime}}
#'   \item{last_PM2.5}{Last valid PM2.5 measurement}
#'   \item{previous_PM2.5}{Previous valid PM2.5 measurement}
#'   \item{last_nowcast}{Last valid PM2.5 NowCast value}
#'   \item{previous_nowcast}{Previous valid PM2.5 NowCast value}
#'   \item{yesterday_PM2.5_avg}{Daily average PM2.5 for the day prior to \code{enddate}}
#' }
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirMonitor)
#'
#' monitor <- airnow_loadLatest()
#' # TODO:  Needed before rebuilding of v2 database with fullAQSID
#' monitor$meta$fullAQSID <- paste0("840", monitor$meta$AQSID)
#'
#' currentStatus <-
#'   monitor %>%
#'   monitor_filter(stateCode == "WA") %>%
#'   monitor_getCurrentStatus()
#'
#' }, silent = FALSE)
#' }

monitor_getCurrentStatus <- function(
  monitor,
  enddate = NULL,
  minHours = 18,
  dayBoundary = c("clock", "LST")
) {

  # ----- Validate parameters --------------------------------------------------

  monitor_check(monitor)

  if ( monitor_isEmpty(monitor) )
    stop("monitor is empty")

  MazamaCoreUtils::stopIfNull(minHours)
  dayBoundary <- match.arg(dayBoundary)


  # ----- Subset data ----------------------------------------------------------

  startdate <- min(monitor$data$datetime)

  if ( is.null(enddate) ) {
    enddate <- max(monitor$data$datetime)
  } else {
    enddate <-
      MazamaCoreUtils::parseDatetime(enddate, timezone = "UTC") %>%
      lubridate::floor_date(unit = "hour")
    # NOTE:  Previous version subtracted another hour here. Not sure why?
  }

  monitor <-
    monitor %>%
    monitor_filterDatetime(startdate, enddate, timezone = "UTC")

  # Check again to make sure subset includes data
  if ( monitor_isEmpty(monitor) ) {
    stop(sprintf(
      "monitor contains zero valid data before %s",
      strftime(enddate, "%Y-%m-%d %H:00 UTC", tz = "UTC")
    ))
  }


  # ----- Prepare data ---------------------------------------------------------

  data <- monitor_getData(monitor)
  meta <- monitor_getMeta(monitor)

  nowcast_data <-
    monitor %>%
    monitor_nowcast(includeShortTerm = TRUE) %>%
    monitor_getData()


  # ----- Create validTimeIndices ----------------------------------------------

  # This is a tibble identifying recent valid indices for each device

  validTimeIndices <-

    # Start with data
    data %>%

    # Ensure rows are arranged by datetime and then remove 'datetime'
    dplyr::arrange(.data$datetime) %>%
    dplyr::select(-.data$datetime) %>%

    # Find last two non-NA indices
    apply(2, function(x) { rev(which(!is.na(x)))[1:2] })

  # Provide rownames that will end up as colnames
  rownames(validTimeIndices) <- c("last_validIndex", "previous_validIndex")

  # Transpose to have a row for each deviceDeploymentID
  validTimeIndices <-

    t(validTimeIndices) %>%

    # Convert matrix to tibble with sensible names
    dplyr::as_tibble(rownames = "deviceDeploymentID")

  # ----- Add latency values ---------------------------------------------------

  enhancedMeta <-

    # Start with monitor$meta
    meta %>%

    # Add times
    dplyr::mutate(
      currentStatus_processingTime = lubridate::now(tzone = "UTC"),
      currentStatus_enddate = enddate
    ) %>%

    # Add valid data indices
    dplyr::left_join(validTimeIndices, by = "deviceDeploymentID") %>%

    # Add POSIXct times
    dplyr::mutate(
      last_validTime = data$datetime[.data$last_validIndex],
      previous_validTime = data$datetime[.data$previous_validIndex]
    ) %>%

    # Add latency
    dplyr::mutate(
      last_latency = as.numeric(difftime(
        enddate,
        .data$last_validTime,
        units = "hour"
      )),
      previous_latency = as.numeric(difftime(
        .data$last_validTime,
        .data$previous_validTime,
        units = "hour"
      ))
    )

  # NOTE:  The strftime() function accepts arguments 'x, 'format' and 'tz'.
  # NOTE:  While strftime() is vectorized for 'x', this is not true for 'format' or 'tz'.
  # NOTE:  So we create local timestamps below in the timezone-dependent section.

  # ----- Add last/previous values ---------------------------------------------

  # Order data columns to match currentStatus
  dataBrick <-
    data %>%
    dplyr::select(enhancedMeta$deviceDeploymentID)

  nowcast_dataBrick <-
    nowcast_data %>%
    dplyr::select(enhancedMeta$deviceDeploymentID)

  # TODO:  We may need to separately determine last_validNowcastIndex as it will
  # TODO:  normally be one hour later than the pm25 value.

  enhancedMeta$last_PM2.5 <- mapply(
    function(x, y) { return(round(x[y], 1)) },
    dataBrick,
    enhancedMeta$last_validIndex
  )

  enhancedMeta$previous_PM2.5 <- mapply(
    function(x, y) { return(round(x[y], 1)) },
    dataBrick,
    enhancedMeta$previous_validIndex
  )

  enhancedMeta$last_nowcast <- mapply(
    function(x, y) { return(round(x[y], 1)) },
    nowcast_dataBrick,
    enhancedMeta$last_validIndex
  )

  enhancedMeta$previous_nowcast <- mapply(
    function(x, y) { return(round(x[y], 1)) },
    nowcast_dataBrick,
    enhancedMeta$previous_validIndex
  )

  # ----- Timezone dependent section -------------------------------------------

  emList <- list()

  for ( timezone in unique(enhancedMeta$timezone) ) {

    # * Add local timestamps -----

    # Subset enhancedMeta
    em <-
      enhancedMeta %>%
      dplyr::filter(.data$timezone == !!timezone)

    em$last_validLocalTimestamp <-
      strftime(
        em$last_validTime,
        format = "%Y-%m-%d %H:%M:%S",
        tz = timezone,
        usetz = TRUE
      )

    em$previous_validLocalTimestamp <-
      strftime(
        em$previous_validTime,
        format = "%Y-%m-%d %H:%M:%S",
        tz = timezone,
        usetz = TRUE
      )

    # * Add yesterday avg -----

    # Local time 24 hours representing "yesterday" relative to enddate
    dateRange <- MazamaCoreUtils::dateRange(
      enddate - lubridate::ddays(1),
      timezone = timezone,
      days = 1
    )

    # Get yesterday mean for a single timezone
    timezoneMon <-
      monitor %>%
      monitor_filter(timezone == !!timezone)

    # NOTE:  monitor_filterDate() will fail if timezoneMon is empty

    if ( monitor_isEmpty(timezoneMon) ) {

      em$yesterday_PM2.5_avg <- as.numeric(NA)

    } else {

      # Calculate yesterday average
      em$yesterday_PM2.5_avg <-

        timezoneMon %>%
        monitor_filterDate(dateRange[1], dateRange[2], timezone = timezone) %>%

        # Guarantee the same order as in cs
        monitor_reorder(em$deviceDeploymentID) %>%

        # Calculate yesterday mean
        monitor_dailyStatistic(
          FUN = mean,
          na.rm = TRUE,
          minHours = minHours,
          dayBoundary = dayBoundary
        ) %>%

        # Pull out the daily means, omitting the 'datetime' column
        monitor_getData() %>%
        dplyr::select(-c("datetime")) %>%

        # Convert single row dataframe to numeric
        dplyr::slice(1) %>%
        as.numeric() %>% round(1)

    } # END monitor_isEmpty()

    emList[[timezone]] <- em

  }

  timezoneMeta <- dplyr::bind_rows(emList)

  # ----- Retain original ordering ---------------------------------------------

  currentStatus <-
    enhancedMeta %>%
    dplyr::select(.data$deviceDeploymentID) %>%
    dplyr::left_join(timezoneMeta, by = "deviceDeploymentID")

  # ----- Return ---------------------------------------------------------------

  return(currentStatus)

}
