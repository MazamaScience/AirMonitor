# Testing getting data from OpenAQ
#
# https://openaq.github.io/openaq-r/index.html

# ----- Setup ------------------------------------------------------------------

library(openaq)

library(dplyr)

# Load secret keys
if ( file.exists("global_vars.R") ) {
  source("global_vars.R")
} else {
  stop("file 'global_vars.R' is missing")
}

openaq::set_api_key(OPENAQ_API_KEY)

# ----- Load metadata tables ---------------------------------------------------

load(file = "countriesDF.rda")
load(file = "instrumentsDF.rda")
load(file = "licensesDF.rda")
load(file = "manufacturersDF.rda")
load(file = "parametersDF.rda")
load(file = "providersDF.rda")

# ----- Load Clarity locations -------------------------------------------------

load(file = "clarity_us_pm25_locations.rds")

locations_id <- 3016081
sensorsDF <- openaq::list_location_sensors(locations_id)

sensors_id <- 10259149

starttime <- MazamaCoreUtils::parseDatetime(2024081316, timezone = "UTC")
endtime <- starttime + lubridate::dhours(2)

measurementsDF <-
  openaq::list_sensor_measurements(
    sensors_id,
    data = "measurements",
    rollup = NULL,
    datetime_from = starttime,
    datetime_to = endtime,
    order_by = NULL,
    sort_order = NULL,
    limit = 1000,
    page = NULL,
    as_data_frame = TRUE,
    dry_run = FALSE,
    rate_limit = FALSE,
    api_key = NULL
  )

hourlyDF <-
  openaq::list_sensor_measurements(
    sensors_id,
    data = "hours",
    rollup = NULL,
    datetime_from = starttime,
    datetime_to = endtime,     # IGNORED
    order_by = NULL,
    sort_order = NULL,
    limit = 1000,
    page = NULL,
    as_data_frame = TRUE,
    dry_run = FALSE,
    rate_limit = FALSE,
    api_key = NULL
  )

myHourlyDF <-
  openaq::list_sensor_measurements(
    sensors_id,
    data = "hours",
    datetime_from = starttime,
    limit = 1000
  )




# ----- Get measurements -------------------------------------------------------

# NOTE:  On 2025-10-09, the list_location_measurements function is broken

# locations_id <- 3016081
# parameters_id <- 2
# starttime <- MazamaCoreUtils::parseDatetime(2024081400, timezone = "UTC")
# endtime <- starttime + lubridate::ddays(1)
#
# a <-
#   list_location_measurements(
#     locations_id = locations_id,
#     parameters_ids = parameters_id,
#     data = "measurements",
#     rollup = NULL,
#     datetime_from = starttime,
#     datetime_to = endtime,
#     order_by = NULL,
#     sort_order = NULL,
#     limit = 1000,
#     page = NULL,
#     as_data_frame = TRUE,
#     dry_run = FALSE,
#     rate_limit = FALSE,
#     api_key = NULL
#   )


#### DEBUG #####

if ( FALSE ) {

  starttime <- MazamaCoreUtils::parseDatetime(2024081400, timezone = "UTC")
  endtime <- starttime + lubridate::ddays(1)

  locations_id = 3016081
  parameters_ids = 2
  data = "measurements"
  rollup = NULL
  datetime_from = starttime
  datetime_to = endtime
  order_by = NULL
  sort_order = NULL
  limit = 1000
  page = NULL
  as_data_frame = TRUE
  dry_run = FALSE
  rate_limit = FALSE
  api_key = NULL

}
