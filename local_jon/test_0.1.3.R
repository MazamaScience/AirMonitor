# Version 0.1.3

# ----- Test latest/daily ------------------------------------------------------

library(AirMonitor)

archiveBaseUrl <- "http://data-monitoring_v2-c1.airfire.org/monitoring-v2"

latest <- airnow_loadLatest(archiveBaseUrl = archiveBaseUrl)
daily <- airnow_loadDaily(archiveBaseUrl = archiveBaseUrl)


# ----- Explore individual time series -----------------------------------------

monitor_leaflet(latest)

latest %>%
  monitor_select("089a067f92712ad1_530750003") %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)

# Lots of smoke associated with New Year's Eve

monitor_leaflet(daily)

# Incorrect color on leaflet map! # TODO lighten up NA gray

daily %>%
  monitor_select("089a067f92712ad1_530750003") %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)

# ----- Explore metadata
