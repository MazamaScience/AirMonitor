# Version 0.0.1

library(AirMonitor)

ws_monitor <- PWFSLSmoke::monitor_loadLatest()

monitor <- monitor_fromPWFSLSmoke(ws_monitor)


monitor %>%
  monitor_filterMeta(stateCode == "CA") %>%
  monitor_leaflet()

monitor %>%
  monitor_filterMeta(stateCode == "CA") %>%
  monitor_leaflet(slice = "2021-10-04 14:00:00")

monitor %>%
  monitor_filterMeta(deviceDeploymentID == "b26a39afa6492133_lon_.118.804_lat_36.029_arb3.2004") %>%
  monitor_extractData() %>%
  plot(type = 'l')

Carmel_Valley %>%
  monitor_filterDate(20160801, 20160810) %>%
  monitor_timeseriesPlot(
    opacity = 0.5,
    shadedNight = TRUE,
    type = 'b',
    pch = 16,
    main = "Soberanes Fire -- Carmel Valey, CA"
  )

# TODO:  Try out the CO data from the Carmel Valley event

Salinas_CO <-
  epa_aqs_loadAnnual(2016, 42101, baseDir = "~/Data/monitoring") %>%
  monitor_filterDate(20160801, 20160810, timezone = "America/Los_Angeles") %>%
  monitor_filterMeta(deviceDeploymentID == "c9fe9c3c096b858a_060531003_01")

Los_FLores_Canyon <-
  epa_aqs_loadAnnual(2016, 42101, baseDir = "~/Data/monitoring") %>%
  monitor_filterDate(20160614, 20160623, timezone = "America/Los_Angeles") %>%
  monitor_filterMeta(deviceDeploymentID == "8e3f33bcad43c1e2_060831025_01")

monitor_timeseriesPlot(Los_FLores_Canyon, shadedNight = TRUE, col = 'black', opacity = 0.5)
addAQILines(pollutant = "CO")
addAQIStackedBar(pollutant = "CO")
addAQILegend(pollutant = "CO")


