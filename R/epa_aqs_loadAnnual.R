#' @export
#'
#' @title Load annual EPA AQS monitoring data
#'
#' @param parameterCode EPA AQS Parameter Code.
#' @param year Desired year (integer or character representing YYYY).
#' @param archiveBaseUrl Base URL for annual EPA AQS data files.
#' @param archiveBaseDir Local base directory for annual EPA AQS data files.
#' @param QC_negativeValues Type of QC to apply to negative values.
#'
#' @return A \emph{mts_monitor} object with EPA AQS data.
#'
#' @description Loads pre-generated .RData files containing annual
#' EPA data.
#'
#' EPA parameter codes include:
#' \enumerate{
#' \item{44201}{ -- Ozone}
# #' \item{42401}{ -- SO2}
#' \item{42101}{ -- CO}
# #' \item{42602}{ -- NO2}
#' \item{88101}{ -- PM2.5 FRM/FEM Mass (begins in 2008)}
#' \item{88502}{ -- PM2.5 non FRM/FEM Mass (begins in 1998)}
#' \item{81102}{ -- PM10 Mass (begins in 1988)}
# #' \item{SPEC}{ -- PM2.5 Speciation}
# #' \item{WIND}{ -- Wind}
# #' \item{TEMP}{ -- Temperature}
# #' \item{PRESS}{ -- Barometric Pressure}
# #' \item{RH_DP}{ -- RH and dewpoint}
# #' \item{HAPS}{ -- HAPs}
# #' \item{VOCS}{ -- VOCs}
# #' \item{NONOxNOy}
#' }
#'
#' @section QC:
#' EPA AQS hourly files are highly vetted but may still have some problems. One
#' particular issue is the presence of \emph{aphysical}, negative values. (There
#' is no such thing as a negative concentration.) These are most likely the
#' result of instrument drift and could potentially be carefully adjusted if
#' additional, enegineering-level raw data were available.
#'
#' But in the context of large collections of aggregated data, we must take a
#' coarser appraoch. Three types of corrections are available through the
#' \code{QC_negativeValues} parameter:
#'
#' \itemize{
#' \item{\code{"zero"} -- replace negative values with 0}
#' \item{\code{"na"} -- replace negative values with \code{NA}}
#' \item{\code{"ignore"} -- do not replace negative values}
#' }
#'

epa_aqs_loadAnnual <- function(
  parameterCode = NULL,
  year = NULL,
  archiveBaseUrl = NULL,
  archiveBaseDir = NULL,
  QC_negativeValues = c("zero", "na", "ignore")
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(parameterCode)
  MazamaCoreUtils::stopIfNull(year)

  QC_negativeValues <- match.arg(QC_negativeValues)

  if ( is.null(archiveBaseUrl) && is.null(archiveBaseDir) )
    stop("one of 'archiveBaseUrl' or 'archiveBaseDir' must be defined")

  # Parameter code
  validParameterCodes <- c(
    "44201",
    # "42401",
    "42101",
    # "42602",
    "88101",
    "88502",
    "81102"
    # "SPEC",
    # "WIND",
    # "TEMP",
    # "PRESS",
    # "RH_DP",
    # "HAPS",
    # "VOCS",
    # "NONOxNOy"
  )

  parameterCode <- as.character(parameterCode)
  if ( !parameterCode %in% validParameterCodes ) {
    stop(sprintf(
      "data for parameterCode '%s' has not been processed",
      parameterCode
    ))
  }

  year <- as.numeric(year)

  lastYear <- lubridate::now(tzone = "UTC") %>% lubridate::year() - 1

  if ( parameterCode == "42101" ) {
    parameter <- "CO"
    if ( !year %in% 1980:lastYear) {
      stop(sprintf(
        "No EPA data available for parameter code %s in year %i",
        parameterCode, year)
      )
    }
  } else if ( parameterCode == "44201" ) {
    parameter <- "OZONE"
    if ( !year %in% 1980:lastYear) {
      stop(sprintf(
        "No EPA data available for parameter code %s in year %i",
        parameterCode, year)
      )
    }
  } else if ( parameterCode == "88101" ) {
    parameter <- "PM2.5"
    if ( !year %in% 2008:lastYear) {
      stop(sprintf(
        "No EPA data available for parameter code %s in year %i",
        parameterCode, year)
      )
    }
  } else if  ( parameterCode == "88502" ) {
    parameter <- "PM2.5"
    if ( !year %in% 1998:lastYear) {
      stop(sprintf(
        "No EPA data available for parameter code %s in year %i",
        parameterCode, year)
      )
    }
  } else if  ( parameterCode == "81102" ) {
    parameter <- "PM10"
    if ( !year %in% 1988:lastYear) {
      stop(sprintf(
        "No EPA data available for parameter code %s in year %i",
        parameterCode, year)
      )
    }
  }


  # ----- Load data ------------------------------------------------------------

  # Create file name and path according to the AirMonitorIngest scheme

  if ( is.null(archiveBaseUrl) ) {
    dataUrl <- NULL
  } else {
    dataUrl <- file.path(archiveBaseUrl, "epa_aqs", parameterCode, year)
  }

  if ( is.null(archiveBaseDir) ) {
    dataDir <- NULL
  } else {
    dataDir <- file.path(archiveBaseDir, "epa_aqs", parameterCode, year)
  }

  metaFileName <- sprintf("epa_aqs_%s_%s_meta.rda", parameterCode, year)
  dataFileName <- sprintf("epa_aqs_%s_%s_data.rda", parameterCode, year)

  meta <- MazamaCoreUtils::loadDataFile(metaFileName, dataUrl, dataDir)
  data <- MazamaCoreUtils::loadDataFile(dataFileName, dataUrl, dataDir)

  monitor <- list(meta = meta, data = data)

  monitor <- structure(monitor, class = c("mts_monitor", "mts", class(monitor)))

  MazamaTimeSeries::mts_check(monitor)

  # ----- Apply QC -------------------------------------------------------------

  if ( QC_negativeValues == "zero" )
    monitor <- monitor_replaceValues(monitor, data < 0, 0)

  else if ( QC_negativeValues == "na" )
    monitor <- monitor_replaceValues(monitor, data < 0, as.numeric(NA))

  # ----- Return ---------------------------------------------------------------

  return(monitor)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  year <- 2015
  parameterCode <- 81102
  archiveBaseUrl <- NULL
  archiveBaseDir <- "~/Data/monitoring"



  monitor <- epa_loadAnnual(
    year = year,
    parameterCode = parameterCode,
    archiveBaseUrl = archiveBaseUrl,
    archiveBaseDir = archiveBaseDir
  )

  example_81102 <-
    monitor <- epa_aqs_loadAnnual(
      year = 2015,
      parameterCode = 81102,
      archiveBaseUrl = NULL,
      archiveBaseDir = "~/Data/monitoring"
    ) %>%
    monitor_filterMeta(stateCode %in% c("WA", "OR", "ID")) %>%
    monitor_filterDate(20150601, 20151101)

}
