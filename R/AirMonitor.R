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
#' @description Vector of names of the required \code{monitor$meta} columns.
#' These represent metadata columns that must exist in every valid
#' \emph{mts_monitor} object. Any number of additional columns may also be present.

coreMetadataNames <- c(
  # Specific to AirMonitor
  "deviceDeploymentID",       # -- timeseries unique identifier
  "deviceID",                 # -- device unique identifier
  "deviceType",               # -- internally-standardized identifier for the type of device (e.g. EBAM, ESAM, PA, ...)
  "deviceDescription",        # -- human readable device details
  "deviceExtra",              # -- extra device information (possibly as JSON)
  "pollutant",                # -- one of "OZONE|CO|NO2|PM2.5|PM10"
  "units",                    # -- one of "PPM|PPB|MILLIGRAMS|MICROGRAMS"
  "dataIngestSource",         # -- internally-standardized identifier for the data source (e.g. AIRNOW, WRCC, ...)
  "dataIngestURL",            # -- top level URL
  "dataIngestUnitID",         # -- unique identifier used to extract data from the URL
  "dataIngestExtra",          # -- extra data ingest information (possibly as JSON)
  "dataIngestDescription",    # -- human readable data ingest details

  # Defined in MazamaLocationUtils
  "locationID",               # -- location unique identifier
  "locationName",             # -- human readable location name
  "longitude",                # --
  "latitude",                 # --
  "elevation",                # --
  "countryCode",              # -- ISO 3166-1 alpha-2
  "stateCode",                # -- ISO 3166-2 alpha-2
  "county",                   # --
  "timezone",                 # -- Olson time zone
  "houseNumber",              # --
  "street",                   # --
  "city",                     # --
  "zip"                       # --
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

# ----- State codes -----------------------------------------------------------

#' CONUS state codes
#'
#' @export
#' @docType data
#' @name CONUS
#' @title CONUS State Codes
#' @format A vector with 49 elements
#' @description
#' State codes for the 48 contiguous states +DC that make up the CONtinental US

CONUS <- c(
       "AL","AZ","AR","CA","CO","CT","DE","FL","GA",
       "ID","IL","IN","IA","KS","KY","LA","ME","MD",
  "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
  "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
  "DC"
)

#' US state codes
#'
#' @export
#' @docType data
#' @name US_52
#' @title US State Codes
#' @format A vector with 52 elements
#' @description
#' State codes for the 50 states +DC +PR (Puerto Rico)

US_52 <- c(
  "AK","AL","AZ","AR","CA","CO","CT","DE","FL","GA",
  "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
  "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
  "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
  "DC","PR"
)

