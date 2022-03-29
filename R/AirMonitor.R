#'
#' @docType package
#' @name AirMonitor
#' @title Air Quality Data Analysis
#' @description
#' \code{
#' Utilities for working with hourly air quality monitoring data
#' with a focus on small particulates (PM2.5). A compact data model is
#' structured as a list with two dataframes. A 'meta' dataframe contains
#' spatial and measuring device metadata associated with deployments at known
#' locations. A 'data' dataframe contains a 'datetime' column followed by
#' columns of measurements associated with each "device-deployment".
#' }

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
#' @examples
#'  print(coreMetadataNames, width = 80)


# NOTE:  AirNow units include:
# NOTE:    "C", "DEGREES", "KNOTS", "M/S", "MILLIBAR", "MM",
# NOTE:    "PERCENT", "PPB", "PPM", "UG/M3", "WATTS/M2"

coreMetadataNames <- c(
  # Specific to AirMonitor
  "deviceDeploymentID",       # -- timeseries unique identifier
  "deviceID",                 # -- device unique identifier
  "deviceType",               # -- internally-standardized identifier for the type of device (e.g. EBAM, ESAM, PA, ...)
  "deviceDescription",        # -- human readable device details
  "deviceExtra",              # -- extra device information (possibly as JSON)
  "pollutant",                # -- one of "OZONE|CO|NO2|PM2.5|PM10"
  "units",                    # -- one of "PPM|PPB|UG/M3"
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
  "countyName",               # --
  "timezone",                 # -- Olson time zone
  "houseNumber",              # --
  "street",                   # --
  "city",                     # --
  "zip",                      # --

  # Extras
  "AQSID"                     # -- EPA AQS site identifier (widely used for North American air quality data)
)


#' pollutantNames
#'
#' @export
#' @docType data
#' @name pollutantNames
#' @title Names of standard pollutants
#' @format A vector of character strings
#' @description Character string identifiers of recognized pollutant names.
#' @examples
#'  print(coreMetadataNames, width = 80)

pollutantNames <- c(
  "PM2.5",
  "AQI",
  "CO",
  "NO",
  "OZONE",
  "PM10",
  "SO2"
)


#' AirFire_S3_archiveBaseUrl
#'
#' @export
#' @docType data
#' @name AirFire_S3_archiveBaseUrl
#' @title USFS maintained archive base URL
#' @format A url
#' @description The US Forest Service AirFire group maintains an archive of
#' processed monitoring data. The base URL for this archive is used as the
#' default in all \code{~_load()} functions.
#'
#' \preformatted{
#' "https://airfire-data-exports.s3.us-west-2.amazonaws.com/monitoring/v2"
#' }
AirFire_S3_archiveBaseUrl <-
  "https://airfire-data-exports.s3.us-west-2.amazonaws.com/monitoring/v2"

# ----- State codes -----------------------------------------------------------

#' CONUS state codes
#'
#' @export
#' @docType data
#' @name CONUS
#' @title CONUS state codes
#' @format A vector with 49 elements
#' @description
#' State codes for the 48 contiguous states +DC that make up the CONtinental US.
#'
#' \code{
#' CONUS <- c(
#'   "AL","AZ","AR","CA","CO","CT","DE","FL","GA",
#'   "ID","IL","IN","IA","KS","KY","LA","ME","MD",
#'   "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
#'   "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
#'   "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
#'   "DC"
#' )
#' }

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
#' @title US state codes
#' @format A vector with 52 elements
#' @description
#' State codes for the 50 states +DC +PR (Puerto Rico).
#'
#' \code{
#' US_52 <- c(
#'   "AK","AL","AZ","AR","CA","CO","CT","DE","FL","GA",
#'   "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
#'   "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
#'   "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
#'   "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
#'   "DC","PR"
#' )
#' }

US_52 <- c(
  "AK","AL","AZ","AR","CA","CO","CT","DE","FL","GA",
  "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
  "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
  "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
  "DC","PR"
)

# ----- AQI categories ---------------------------------------------------------

#' AQI breaks and associated names and colors
#'
#' @export
#' @docType data
#' @name US_AQI
#' @title US EPA AQI Index levels, names, colors and action text
#' @format A list with named elements
#' @description
#' Official, US EPA AQI levels, names, colors and action text are provided in a
#' list for easy coloring and labeling.
#'
#' @section Breaks:
#'
#' Breakpoints are given in units reported for each parameter and include:
#' \itemize{
#' \item{\code{breaks_AQI}}
#' \item{\code{breaks_CO}}
#' \item{\code{breaks_NO2}}
#' \item{\code{breaks_OZONE_1hr}}
#' \item{\code{breaks_OZONE_8hr}}
#' \item{\code{breaks_PM2.5}}
#' \item{\code{breaks_PM10}}
#' }
#'
#' @section Colors:
#'
#' Several different color palettes are provided:
#' \itemize{
#' \item{\code{colors_EPA} -- official EPA AQI colors}
#' \item{\code{colors_subdued} -- subdued colors fo use with leaflet maps}
#' \item{\code{colors_deuteranopia} -- color vision impaired colors}
#' }
#'
#' @section Names:
#'
#' Names of AQI categories are provided in several languages identified by the
#' ISO 639-2 alpha-3 code:
#' \itemize{
#' \item{\code{names_eng}}
#' \item{\code{names_spa}}
#' }
#'
#' @section Actions:
#'
#' Text for "actions to protect yourself" are provided for each
#' category in several languages identified by the
#' ISO 639-2 alpha-3 code:
#' \itemize{
#' \item{\code{actions_eng}}
#' \item{\code{actions_spa}}
#' }
#'
#' Currently supported languages include English (eng) and Spanish (spa).
#'
#' AQI breaks are defined at
#' \url{https://www.airnow.gov/sites/default/files/2020-05/aqi-technical-assistance-document-sept2018.pdf}
#' and are given in units appropriate for each pollutant.
#'
#' AQI colors are defined at \url{https://docs.airnowapi.org/aq101}
#' @note
#' The low end of each break category is used as the breakpoint.
#'
#' @examples
#' print(US_AQI$breaks_AQI)
#' print(US_AQI$colors_EPA)
#' print(US_AQI$names_eng)
#' print(US_AQI$names_spa)

US_AQI <- list(

  # NOTE:  We must have default breaks with just the parameter name
  # Breaks for all supported parameters
  breaks_AQI = c(-Inf, 50, 100, 150, 200, 300, Inf),
  breaks_CO = c(-Inf, 4.5, 9.5, 12.5, 15.5, 30.5, Inf),
  breaks_NO2 = c(-Inf, 54, 101, 361, 650, 2501, Inf),
  breaks_OZONE = c(-Inf, 0, .125, .165, .205, .405, Inf),        # Using OZONE_1hr
  breaks_PM2.5 = c(-Inf, 12, 35.5, 55.5, 150.5, 250.5, Inf),     # Using PM2.5_24hr
  breaks_PM10 = c(-Inf, 55, 155, 255, 355, 425, Inf),

  # Special breaks
  breaks_OZONE_1hr = c(-Inf, 0, .125, .165, .205, .405, Inf),    # GOOD, MOD undefined at EPA
  breaks_OZONE_8hr = c(-Inf, .055, .071, .086, .106, .405, Inf), # HAZ undefined at EPA

  # Official EPA colors
  colors_EPA = c(
    grDevices::rgb(0,228/255,0),
    grDevices::rgb(255/255,255/255,0),
    grDevices::rgb(255/255,126/255,0),
    grDevices::rgb(255/255,0,0),
    grDevices::rgb(143/255,63/255,151/255),
    grDevices::rgb(126/255,0,35/255)
  ),
  # Subdued colors used by USFS AirFire Monitoring (Mv4) site
  colors_subdued = c("#2ecc71", "#f1c40f", "#e67e22", "#e74c3c", "#9b59b6", "#8c3a3a"),
  # Color vision impaired colors recommended by Mazama Science
  colors_deuteranopia = c("#8cddf5", "#ffef00", "#f7921f", "#ed1d24", "#a3064b", "#6d0526"),

  # Names in different languages
  names_eng = c('Good', 'Moderate', 'USG', 'Unhealthy', 'Very Unhealthy', 'Hazardous'),
  names_spa = c('Buena', 'Moderada', 'IGS', 'Insalubre', 'Muy insalubre', 'Peligrosa'),

  # Action text in different languages
  # NOTE:  R packages require that unicode characters be escaped.
  actions_eng = c(
    'None.',
    'Unusually sensitive individuals should consider limiting prolonged or heavy exertion.',
    'People within Sensitive Groups should reduce prolonged or heavy outdoor exertion.',
    'People within Sensitive Groups should avoid all physical outdoor activity.',
    'Everyone should avoid prolonged or heavy exertion.',
    'Everyone should avoid any outdoor activity.'
  ),
  actions_spa = c(
    'Ninguna.',
    'Personas inusualmente sensitivas deber\\u00edan considerar limitar la labor prolongada \\u00f3 intensa.',
    'Personas dentro de los grupos sensitivos deben reducir la labor prolongada \\u00f3 intensa al aire libre.',
    'Personas dentro de los grupos sensitivos deben evitar toda actividad f\\u00edsica al aire libre.',
    'Todos deben evitar la labor prolongada \\u00f3 intensa.',
    'Todos deben evitar cualquier actividad al aire libre.'
  )

)

