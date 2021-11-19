# Version 0.0.3

library(dplyr)
library(AirMonitor)

a1 <-
  PWFSLSmoke::airnow_loadLatest() %>%
  monitor_fromPWFSLSmoke()

a2 <-
  airnow_loadLatest(
    "PM2.5",
    archiveBaseUrl = "https://data-monitoring1.airfire.org/monitoring-v2"
  )

a2n <-
  airnow_loadLatest(
    "PM2.5_nowcast",
    archiveBaseUrl = "https://data-monitoring1.airfire.org/monitoring-v2"
  )

ia1 <-
  a1 %>%
  monitor_filter(stateCode == "IA") %>%
  monitor_filterDate(20211116, 20211116)

ia2 <-
  a2 %>%
  monitor_filter(stateCode == "IA") %>%
  monitor_filterDate(20211116, 20211116)

ia2n <-
  a2n %>%
  monitor_filter(stateCode == "IA") %>%
  monitor_filterDate(20211116, 20211116)


# Does the AirMonitorIngest version match the PWFSLSmoke version
ia1 %>% monitor_timeseriesPlot(addAQI = FALSE, pch = 1)
ia2 %>% monitor_timeseriesPlot(addAQI = FALSE, pch = 1, col = 'blue', add = TRUE)

# ==> YES!

# Nowcast
ia2n %>% monitor_timeseriesPlot(addAQI = FALSE, pch = 1, col = 'red', add = TRUE)

# Basic plot
a2 %>%
  monitor_filter(deviceID == 191032001) %>%
  monitor_timeseriesPlot(addAQI = TRUE, palette = "deuteranopia")

