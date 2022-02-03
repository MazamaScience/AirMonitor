# Version 0.1.3

# ----- Test latest/daily ------------------------------------------------------

library(AirMonitor)

archiveBaseUrl <- "https://airfire-data-exports.s3.us-west-2.amazonaws.com/monitoring/v2"

latest <- airnow_loadLatest(archiveBaseUrl = archiveBaseUrl)
daily <- airnow_loadDaily(archiveBaseUrl = archiveBaseUrl)


# ----- Explore individual time series -----------------------------------------

monitor_leaflet(latest)

latest %>%
  monitor_select("0b97822d89739950_060631010") %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)

# Lots of smoke associated with New Year's Eve

monitor_leaflet(daily)

# Incorrect color on leaflet map! # TODO lighten up NA gray

daily %>%
  monitor_select("35986db2a890acdf_530730019") %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE, ylim=c(0,40))


daily %>%
  monitor_select("35986db2a890acdf_530730019") %>%
  monitor_dygraph()

