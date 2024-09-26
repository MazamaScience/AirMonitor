# Have a Party!

library(AirMonitor)

airnow <- airnow_loadLatest()

monitor_leaflet(airnow)

View(airnow$meta)



chiloquin <- airnow %>% monitor_select("95a7dbdccaf0270b_840410352040")

chiloquin %>% monitor_timeseriesPlot(shadedNight = TRUE)

a <-
  airnow %>%
  monitor_filterByDistance(
    chiloquin$meta$longitude,
    chiloquin$meta$latitude,
    radius = 10000
  )


