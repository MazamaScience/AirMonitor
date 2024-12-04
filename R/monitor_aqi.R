#' @export
#'
#' @title Calculate hourly NowCast-based AQI values
#'
#' @param monitor \emph{mts_monitor} object.
#' @param includeShortTerm Logical specifying whether to calculate preliminary
#' NowCast values starting with the 2nd hour.
#'
#' @return A modified \code{mts_monitor} object containing AQI values. (A list
#' with \code{meta} and \code{data} dataframes.)
#'
#' @description Nowcast and AQI algorithms are applied to the data in the
#' monitor object. A modified \code{mts_monitor} object is returned whre values
#' have been replaced with their Air Quality Index equivalents. See \link{monitor_nowcast}.
#'
#' By default, an appropriate set of NAAQS levels will be chosen for \code{monitor$meta$pollutant}.
#' Users \strong{cannot} currently override these values..
#'
#' @references \url{https://en.wikipedia.org/wiki/Nowcast_(Air_Quality_Index)}
#' @references \url{https://www.airnow.gov/aqi/aqi-basics/}
#' @references \href{https://document.airnow.gov/technical-assistance-document-for-the-reporting-of-daily-air-quailty.pdf}{AQI Technical Assistance Document}
#'

monitor_aqi <- function(
    monitor,
    includeShortTerm = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  includeShortTerm <- MazamaCoreUtils::setIfNull(includeShortTerm, FALSE)

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

  if ( monitor_isEmpty(monitor) )
    stop("Parameter 'monitor' has no data.")

  # ----- AQI algorithm --------------------------------------------------------

  pollutant <- toupper(unique(monitor$meta$pollutant))
  if ( length(pollutant) > 1 ) {
    pollutantString <- paste0(pollutant, collapse = ", ")
    stop(sprintf("multiple pollutants found: %s", pollutantString))
  }

  if ( pollutant == "OZONE" ) {
    version = "ozone"
  } else {
    version = "pm"
  }

  # TODO:  Should we use NowCast for non-PM, non-OZONE pollutants?

  if ( pollutant %in% c("PM2.5", "PM10", "OZONE")) {
    # Calculate NowCast
    monitor <-
      monitor %>%
      # NOTE: see https://forum.airnowtech.org/t/how-does-airnow-handle-negative-hourly-concentrations/143
      monitor_replaceValues(data < 0, 0) %>%
      monitor_nowcast(version = version, includeShortTerm = includeShortTerm)
  } else {
    monitor <-
      monitor %>%
      monitor_replaceValues(data < 0, 0)
  }

  # pull out data for AQI calculation
  data <- dplyr::select(monitor$data, -1)

  # NOTE:  See secion IV of:
  # NOTE:    https://document.airnow.gov/technical-assistance-document-for-the-reporting-of-daily-air-quailty.pdf
  if ( pollutant == "PM2.5" || pollutant == "CO") {
    digits <- 1
  } else if ( pollutant == "OZONE" ) {
    digits <- 3
  } else {
    digits <- 0
  }
  data <- trunc(data*10^digits)/10^digits

  # Assign breakpoints
  breakpointsTable <- .assignBreakpointsTable(pollutant)

  # For each datapoint find the breakpointsTable row index that corresponds to the concentration
  rowIndex <- apply(
    X = data,
    MARGIN = 2,
    FUN = findInterval,
    vec = breakpointsTable$rangeHigh,
    left.open = TRUE
  )

  rowIndex <- rowIndex + 1

  # From 40 CFR 58 Appendix G.12.ii:
  #  If the concentration is larger than the highest breakpoint in Table 2
  #  then you may use the last two breakpoints in Table 2 when you apply Equation 1.
  rowIndex[rowIndex > nrow(breakpointsTable)] <- nrow(breakpointsTable)

  # Assign breakpoints and corresponding index values
  I_Hi <- breakpointsTable$aqiHigh[rowIndex]
  I_Lo <- breakpointsTable$aqiLow[rowIndex]
  BP_Hi <- breakpointsTable$rangeHigh[rowIndex]
  BP_Lo <- breakpointsTable$rangeLow[rowIndex]

  # Apply Equation 1 from 40 CFR 58 Appendix G and round to the nearest integer
  I_p <- (I_Hi-I_Lo)/(BP_Hi-BP_Lo)*(data-BP_Lo) + I_Lo
  I_p <- round(I_p, 0)

  monitor$data[,-1] <- I_p

  # ----- Update meta ----------------------------------------------------------

  monitor$meta$pollutant <- "AQI"
  monitor$meta$units <- ""

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}

# ===== Internal Functions =====================================================

.assignBreakpointsTable <- function(pollutant = "PM2.5") {

  # TODO:  We could add older breakpoints from:
  # TODO:    Appendix G, Table 2 at https://www.ecfr.gov/current/title-40/part-58

  # NOTE:  Too hard to be clever so we just copy from here:
  # NOTE:    https://document.airnow.gov/technical-assistance-document-for-the-reporting-of-daily-air-quailty.pdf
  # NOTE:
  # NOTE:  For the last value in rangeHigh, see note 4 from Table 6, above.

  if ( pollutant == "OZONE" ) {
    breakpointsTable <- data.frame(
      rangeLow = c(.0, .055, .071, .086, .106, .201),
      rangeHigh = c(.054, .070, .085, .105, .200, .604)
    )
  } else if ( pollutant == "PM2.5") {
    breakpointsTable <- data.frame(
      rangeLow = c(0.0, 9.1, 35.5, 55.5, 125.5, 225.5),
      rangeHigh = c(9.0, 35.4, 55.4, 125.4, 225.4, 325.4)
    )
  } else if ( pollutant == "PM10") {
    breakpointsTable <- data.frame(
      rangeLow = c(0.0, 55, 155, 255, 355, 425),
      rangeHigh = c(54, 154, 254, 354, 424, 604)
    )
  } else if ( pollutant == "CO") {
    breakpointsTable <- data.frame(
      rangeLow = c(0.0, 4.5, 9.5, 12.5, 15.5, 30.5),
      rangeHigh = c(4.4, 9.4, 12.4, 15.4, 30.4, 50.4)
    )
  } else if ( pollutant == "SO2") {
    breakpointsTable <- data.frame(
      rangeLow = c(0.0, 36, 76, 186, 305, 605),
      rangeHigh = c(35, 75, 185, 304, 604, 1004)
    )
  } else if ( pollutant == "NO2") {
    breakpointsTable <- data.frame(
      rangeLow = c(0.0, 54, 101, 361, 650, 1250),
      rangeHigh = c(53, 100, 360, 649, 1249, 2049)
    )
  } else {
    stop(sprintf("pollutant '%s' is not recognized", pollutant))
  }

  breakpointsTable$aqiLow <- c(0, 51, 101, 151, 201, 301)
  breakpointsTable$aqiHigh <- c(50, 100, 150, 200, 300, 500)

  return(breakpointsTable)

}
