# From the Rx smoke-report.Rmd

library(lubridate)
library(stringr)
library(dplyr)
library(tidyr)
library(AirMonitor)

burn_info <- list(
  lon = -84,
  lat = 34
)


## GENERTE DATA FOR RECENT AIR QUALITY ----------------------------------------------

# pull hourly data within 300 km of rx burn
airnow <- airnow_loadLatest() %>%
  monitor_filterByDistance(longitude = burn_info$lon,
                           latitude = burn_info$lat,
                           radius = 300000, # 300km
                           addToMeta = TRUE)

# subset to nearest 5 monitors
near_5 <- airnow$meta %>%
  arrange(distanceFromTarget) %>%
  slice(1:5)

# filter to 3 days before burn and get mean values
near_5_df <- airnow$data %>%
  dplyr::select(datetime, any_of(near_5$deviceDeploymentID)) %>%
  mutate(datetime = ymd(as_date(datetime))) %>%
  filter(datetime >= (ymd(as_date(Sys.time())) - 3) & datetime <= (ymd(as_date(Sys.time())) - 1)) %>%
  group_by(datetime) %>%
  summarise_all(.funs = mean, na.rm = TRUE) %>%
  ungroup() %>%
  gather(deviceDeploymentID, pm_val, -datetime) %>%
  mutate(aqi_col = aqiColors(pm_val,
                             pollutant = c("PM2.5"),
                             palette = c("EPA")),
         aqi_cat = cut(pm_val,
                       breaks = US_AQI$breaks_AQI,
                       labels = US_AQI$names_eng)) %>%
  left_join(., airnow$meta) %>%
  mutate(siteName = str_to_title(locationName))

# most recent daily AQI
rec_daily_df <- near_5_df %>%
  filter(datetime == max(datetime)) %>%
  mutate(siteName = str_to_title(locationName))

# ===== Jon's version ==========================================================

# Dates and timezones can be tricky so be careful here.
tzone = "America/New_York"                       # Obtain tzone from burn metadata?
burnDate <- lubridate::now(tzone)                # Get burnDate from burn metadata?
startdate <- burnDate - lubridate::ddays(3)
enddate <- burnDate - lubridate::ddays(1)

# Recipe to get the 5 closest monitors
nearest_5 <-
  # Load latest monitoring data from AirNow
  airnow_loadLatest() %>%
  # Only retain monitors within 300km of the burn
  monitor_filterByDistance(
    longitude = burn_info$lon,
    latitude = burn_info$lat,
    radius = 300000, # 300km
    addToMeta = TRUE
  ) %>%
  # Order monitors by distance from target
  monitor_arrange(distanceFromTarget) %>%
  # Only retain the 5 closest monitors
  monitor_slice_head(5)

# Recipe to calculate local time daily averages
nearest_5_daily <-
  # Start with 5 closest monitors
  nearest_5 %>%
  # Date filtering in the local timezone
  monitor_filterDate(
    startdate = startdate,
    enddate = enddate,
    timezone = nearest_5$meta$timezone[1],      # or from burn information
    ceilingEnd = TRUE                           # include all of enddate
  ) %>%
  # Calculate daily averages
  monitor_dailyStatistic(
    FUN = mean,
    na.rm = TRUE,
    minHours = 18,
    dayBoundary = "LST"
  )

### DIDN'T FINISH:

# # Create nearest_5_df with aqi_col and aqi_cat
# nearest_5_df <-
#   nearest_5$data %>%
#   mutate(aqi_col = aqiColors(pm_val,
#                              pollutant = c("PM2.5"),
#                              palette = c("EPA")),
#          aqi_cat = cut(pm_val,
#                        breaks = US_AQI$breaks_AQI,
#                        labels = US_AQI$names_eng)) %>%
#
#
