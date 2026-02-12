# Have a Party!

library(AirMonitor)


url = "https://airfire-data-exports.s3.us-west-2.amazonaws.com/sensors/v3/PM2.5/jons_test/data/clarity_PM2.5_latest_data_mini.csv"
data <- readr::read_csv(url)

url = "https://airfire-data-exports.s3.us-west-2.amazonaws.com/sensors/v3/PM2.5/jons_test/data/clarity_PM2.5_latest_meta_mini.csv"
meta <- readr::read_csv(url)
meta$zip <- meta$postalCode

# Create monitor object
monitor <- list(meta = meta, data = data)
monitor <- structure(monitor, class = c("mts_monitor", "mts", class(monitor)))
MazamaTimeSeries::mts_check(monitor)

monitor <-
  monitor %>%
  monitor_filter(countryCode %in% c("CA","US","MX"))

# Play
monitor %>% monitor_timeseriesPlot()

monitor %>% monitor_leaflet()

