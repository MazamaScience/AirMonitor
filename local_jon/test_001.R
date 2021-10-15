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
