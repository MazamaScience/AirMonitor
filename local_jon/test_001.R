# Version 0.0.1

library(AirMonitor)

ws_monitor <- PWFSLSmoke::monitor_loadLatest()

monitor <- monitor_fromPWFSLSmoke(ws_monitor)


monitor %>%
  monitor_filterMeta(stateCode == "CA") %>%
  monitor_leaflet()

monitor %>%
  monitor_filterMeta(stateCode == "CA") %>%
  monitor_leaflet(slice = "2021-10-12 14:00:00")

monitor %>%
  monitor_select("e9fbd3040bbaaea8_060652002_01") %>%
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

# ----- CO ---------------------------------------------------------------------

Salinas_CO <-
  epa_aqs_loadAnnual(2016, 42101, archiveBaseDir = "~/Data/monitoring") %>%
  monitor_filterDate(20160801, 20160810, timezone = "America/Los_Angeles") %>%
  monitor_filterMeta(deviceDeploymentID == "c9fe9c3c096b858a_060531003_01")

monitor_timeseriesPlot(Salinas_CO, shadedNight = TRUE)

Los_FLores_Canyon <-
  epa_aqs_loadAnnual(2016, 42101, archiveBaseDir = "~/Data/monitoring") %>%
  monitor_filterDate(20160614, 20160623, timezone = "America/Los_Angeles") %>%
  monitor_filterMeta(deviceDeploymentID == "8e3f33bcad43c1e2_060831025_01")

monitor_timeseriesPlot(Los_FLores_Canyon, shadedNight = TRUE, col = 'black', opacity = 0.5)

# ----- OZONE ------------------------------------------------------------------

CA_Ozone <-
  epa_aqs_loadAnnual(2016, 44201, archiveBaseDir = "~/Data/monitoring") %>%
  monitor_filterDate(20160614, 20160623, timezone = "America/Los_Angeles") %>%
  monitor_filterMeta(stateCode == "CA")

monitor_leaflet(CA_Ozone)

Upland <-
  epa_aqs_loadAnnual(2016, 44201, archiveBaseDir = "~/Data/monitoring") %>%
  monitor_select("1bec9c5ac76d6d07_060711004_02")

monitor_timeseriesPlot(Upland, addAQI = FALSE)
addAQIStackedBar(pollutant = "OZONE")
addAQILines(pollutant = "OZONE")

# ----- PM10 -------------------------------------------------------------------

CA_PM10 <-
  epa_aqs_loadAnnual(2016, 81102, archiveBaseDir = "~/Data/monitoring") %>%
  monitor_filterDate(20160614, 20160623, timezone = "America/Los_Angeles") %>%
  monitor_filterMeta(stateCode == "CA")

monitor_leaflet(CA_PM10)

Los_FLores_Canyon <-
  epa_aqs_loadAnnual(2016, 81102, archiveBaseDir = "~/Data/monitoring") %>%
  monitor_filterDate(20160614, 20160623, timezone = "America/Los_Angeles") %>%
  monitor_select("8e3f33bcad43c1e2_060831025_03")

monitor_timeseriesPlot(Los_FLores_Canyon, shadedNight = TRUE)

