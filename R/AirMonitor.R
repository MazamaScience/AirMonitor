#'
#' @docType package
#' @name AirMonitor
#' @title Air Quality Data Analysis
#' @description Tools for working with air quality data.

NULL

# ----- Internal Data -------------------------------------------------

#' coreMetadataNames
#'
#' @export
#' @docType data
#' @name coreMetadataNames
#' @title Names of standard metadata columns
#' @format A vector of character strings
#' @description Character string identifiers of the required \code{monitor$meta} columns.
#' These represent metadata columns that must exist in a valid \emph{mts_monitor}
#' object. Any number of additional columns may also be present.

coreMetadataNames <- c(
  "deviceDeploymentID",
  "deviceID",
  "pollutant",
  "units",
  "locationID",               # from MazamaLocationUtils
  "locationName",             # from MazamaLocationUtils
  "longitude",                # from MazamaLocationUtils
  "latitude",                 # from MazamaLocationUtils
  "elevation",                # from MazamaLocationUtils
  "countryCode",              # from MazamaLocationUtils
  "stateCode",                # from MazamaLocationUtils
  "timezone"                  # from MazamaLocationUtils
)


#' pollutantNames
#'
#' @export
#' @docType data
#' @name pollutantNames
#' @title Names of standard pollutants
#' @format A vector of character strings
#' @description Character string identifiers of recognized pollutants.

pollutantNames <- c(
  "OZONE",
  "SO2",
  "CO",
  "NO",
  "PM2.5",
  "PM10"
)

