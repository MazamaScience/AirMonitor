# 2024-07-08
#
# Can you check on the monitor with site ID "840MMCA82036"? It is returning data
# from the AirNowAPI mobile/temp monitor feed but not displaying on the Fire and
# Smoke map. It is located at 34.86706,-120.29634. Thanks! -- Marcus

library(AirMonitor)

airnow <- airnow_loadLatest()

id <- "840MMCA82036"

mm <-
  airnow %>%
  monitor_filter(stringr::str_detect(AQSID, "MMCA"))

mm %>% monitor_leaflet()

# Current deployment
mm %>%
  monitor_select("9q4qqe6_840MMCA82036") %>%
  monitor_timeseriesPlot(shadedNight = TRUE)

# Earlier deployment (a single measurement)
mm %>%
  monitor_select("9q4t4wt_840MMCA82036") %>%
  monitor_timeseriesPlot(shadedNight = TRUE)
