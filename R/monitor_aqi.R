#' @export
#'
#' @title Calculate hourly NowCast-based AQI values
#'
#' @param monitor \emph{mts_monitor} object.
#' @param version Name of the type of nowcast algorithm to be used.
#' @param includeShortTerm Logical specifying whether to alcluate preliminary
#' NowCast values starting with the 2nd hour.
#' @param NAAQS Version of NAAQS levels to use. See Note.
#'
#' @return A modified \code{mts_monitor} object containing AQI values. (A list
#' with \code{meta} and \code{data} dataframes.)
#'
#' @description Nowcast and AQI algorithms are applied to the data in the
#' monitor object. A modified \code{mts_monitor} object is returned whre values
#' have been replaced with their Air Quality Index equivalents. See \link{monitor_nowcast}.
#'
#' @note
#' On February 7, 2024, EPA strengthened the National Ambient Air Quality
#' Standards for Particulate Matter (PM NAAQS) to protect millions of Americans
#' from harmful and costly health impacts, such as heart attacks and premature
#' death. Particle or soot pollution is one of the most dangerous forms of air
#' pollution, and an extensive body of science links it to a range of serious
#' and sometimes deadly illnesses. EPA is setting the level of the primary
#' (health-based) annual PM2.5 standard at 9.0 micrograms per cubic meter to
#' provide increased public health protection, consistent with the available
#' health science.
#' See \href{https://www.epa.gov/pm-pollution/final-reconsideration-national-ambient-air-quality-standards-particulate-matter-pm}{PM NAAQS update}.
#'
#' @references \url{https://en.wikipedia.org/wiki/Nowcast_(Air_Quality_Index)}
#' @references \url{https://www.airnow.gov/aqi/aqi-basics/}
#'

monitor_aqi <- function(
  monitor,
  version = c("pm", "pmAsian", "ozone"),
  includeShortTerm = FALSE,
  NAAQS = c("PM2.5_2024", "PM2.5")
) {

  parameterName <- "PM2.5"

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(monitor)
  version <- match.arg(version)
  includeShortTerm <- MazamaCoreUtils::setIfNull(includeShortTerm, FALSE)
  NAAQS = match.arg(NAAQS)

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

  # Assign breakpoints
  breakpointsTable <- .assignBreakpointsTable(parameterName, NAAQS)

  # Calculate NowCast
  monitor <-
    monitor %>%
    # NOTE: see https://forum.airnowtech.org/t/how-does-airnow-handle-negative-hourly-concentrations/143
    monitor_replaceValues(data < 0, 0) %>%
    monitor_nowcast(version = version, includeShortTerm = includeShortTerm)

  # pull out data for AQI calculation
  data <- dplyr::select(monitor$data, -1)

  # TODO: include/expand checks to ensure values are appropriately truncated
  if ( parameterName == "PM2.5" || version == "pm" ) {
    digits <- 1
  } else {
    digits <- 0
  }
  data <- trunc(data*10^digits)/10^digits

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

.assignBreakpointsTable <- function(parameterName = "PM2.5", NAAQS = "PM2.5") {

  # TODO: Add other breakpoint table options

  if ( parameterName == "PM2.5") {
    # PM2.5 -- From Appendix G, Table 2 at https://www.ecfr.gov/current/title-40/part-58
    # PM2.5_2024 -- https://www.epa.gov/system/files/documents/2024-02/pm-naaqs-air-quality-index-fact-sheet.pdf
    if ( NAAQS == "PM2.5" ) {
      breakpointsTable <- data.frame(
        rangeLow = c(0.0, 12.001, 35.5, 55.5, 150.5, 250.5, 350.5),
        rangeHigh = c(12.0, 35.4, 55.4, 150.4, 250.4, 350.4, 500.4),
        aqiLow = c(0, 51, 101, 151, 201, 301, 401),
        aqiHigh = c(50, 100, 150, 200, 300, 400, 500)
      )
    } else {
      breakpointsTable <- data.frame(
        rangeLow = c(0.0, 9.1, 35.5, 55.5, 125.5, 225.5),
        rangeHigh = c(9.0, 35.4, 55.4, 125.4, 225.4, 500),
        aqiLow = c(0, 51, 101, 151, 201, 301, 401),
        aqiHigh = c(50, 100, 150, 200, 300, 400, 500)
      )
    }
  } else {
    stop("only PM2.5 currently supported")
  }

  return(breakpointsTable)

}
