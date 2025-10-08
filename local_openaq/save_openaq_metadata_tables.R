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

# ----- Save metadata tables ---------------------------------------------------

# Countires
countriesDF <- openaq::list_countries(limit = 1000)
# attributes(countriesDF)$meta     # Check that $found is < $limit
# View(countriesDF)                # Review the data
save(countriesDF, file = "countriesDF.rda")

# Instruments
instrumentsDF <- openaq::list_instruments(limit = 1000)
save(instrumentsDF, file = "instrumentsDF.rda")

# skip Latest

# Licenses
licensesDF <- openaq::list_licenses(limit = 1000)
save(licensesDF, file = "licensesDF.rda")

# skip Locations

# Manufacturers
manufacturersDF <- openaq::list_manufacturers(limit = 1000)
save(manufacturersDF, file = "manufacturersDF.rda")

# skip Measurements

# Owners ==> ERROR
###ownersDF <- openaq::list_owners(limit = 1000)
###save(ownersDF, file = "ownersDF.rda")

# Parameters
parametersDF <- openaq::list_parameters(limit = 1000)
save(parametersDF, file = "parametersDF.rda")

# Providers
providersDF <- openaq::list_providers(limit = 1000)
save(providersDF, file = "providersDF.rda")

# ----- Clarity, US, PM2.5 locations -------------------------------------------

clarity_id <-
  manufacturersDF %>%
  dplyr::filter(name == "Clarity") %>%
  dplyr::pull(id)

us_id <-
  countriesDF %>%
  dplyr::filter(code == "US") %>%
  dplyr::pull(id)

pm25_id <-
  parametersDF %>%
  dplyr::filter(display_name == "PM2.5") %>%
  dplyr::pull(id)

locations_p1 <-
  openaq::list_locations(
    parameters_id = pm25_id,
    manufacturers_id = clarity_id,
    countries_id = us_id,
    limit = 1000
  )
# limit 1000 of >1000 found

locations_p2 <-
  openaq::list_locations(
    parameters_id = pm25_id,
    manufacturers_id = clarity_id,
    countries_id = us_id,
    limit = 1000,
    page = 2
  )
# limit 1000 of 101 found

# Fix types

locations_p1 <-
  locations_p1 %>%
  dplyr::mutate(
    name = as.character(name),
    timezone = as.character(timezone),
    country_name = as.character(country_name),
    country_iso = as.character(country_iso),
    owner_name = as.character(owner_name),
    provider_name = as.character(provider_name),
    datetime_first = lubridate::as_datetime(datetime_first),
    datetime_last = lubridate::as_datetime(datetime_last)
  )

locations_p2 <-
  locations_p2 %>%
  dplyr::mutate(
    name = as.character(name),
    timezone = as.character(timezone),
    country_name = as.character(country_name),
    country_iso = as.character(country_iso),
    owner_name = as.character(owner_name),
    provider_name = as.character(provider_name),
    datetime_first = lubridate::as_datetime(datetime_first),
    datetime_last = lubridate::as_datetime(datetime_last)
  )

locationsDF <- dplyr::bind_rows(locations_p1, locations_p2)
save(locationsDF, file = "clarity_us_pm25_locations.rds")



