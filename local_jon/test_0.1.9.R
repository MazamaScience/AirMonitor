# Version 0.1.9

# ----- Test latest/daily ------------------------------------------------------

library(AirMonitor)
library(ggplot2)

latest <- monitor_loadLatest()

latest %>% monitor_leaflet()

# * WRCC -----

latest %>%
  monitor_filter(dataIngestSource == "WRCC") %>%
  monitor_leaflet()

latest %>%
  monitor_filter(dataIngestSource == "WRCC") %>%
  monitor_trimDate(timezone = "America/Los_Angeles") %>%
  monitor_toPWFSLSmoke() %>%
  AirMonitorPlots::monitor_ggTimeseries()

gg <-
  latest %>%
  monitor_filter(dataIngestSource == "WRCC") %>%
  monitor_trimDate(timezone = "America/Los_Angeles") %>%
  monitor_toPWFSLSmoke() %>%
  AirMonitorPlots::ggplot_pm25Timeseries()

# Use PWFSLSmoke column names
gg +
  AirMonitorPlots::geom_pm25Points(aes(color = siteName)) +
  AirMonitorPlots::stat_nowcast(aes(color = siteName)) +
  ggplot2::facet_grid(rows = vars(siteName))

# * AIRSIS -----

latest %>%
  monitor_filter(dataIngestSource == "AIRSIS") %>%
  monitor_leaflet()

# Just 1 as all the rest are available from AirNow

# * Yuba City -----

longitude <- -121.616
latitude <- 39.140
distance <- 40000 # 50 km

Yuba_City <-
  latest %>%
  monitor_filterByDistance(longitude, latitude, distance)

monitor_leaflet(Yuba_City)

Yuba_City %>%
  monitor_collapse() %>%
  monitor_dailyBarplot()

