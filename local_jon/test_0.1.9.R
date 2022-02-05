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

# ----- WRCC Annual ------------------------------------------------------------

wrcc_2021 <- wrcc_loadAnnual(2021)
wrcc_2020 <- wrcc_loadAnnual(2020)
wrcc_2019 <- wrcc_loadAnnual(2019)
wrcc_2018 <- wrcc_loadAnnual(2019)
wrcc_2017 <- wrcc_loadAnnual(2017)
wrcc_2016 <- wrcc_loadAnnual(2016)

wrcc_2021 %>% monitor_leaflet()
wrcc_2020 %>% monitor_leaflet()
wrcc_2019 %>% monitor_leaflet()
wrcc_2018 %>% monitor_leaflet()
wrcc_2017 %>% monitor_leaflet()
wrcc_2016 %>% monitor_leaflet()

wrcc_ID <-
  monitor_combine(
    wrcc_2016,
    wrcc_2017,
    wrcc_2018,
    wrcc_2019,
    wrcc_2020,
    wrcc_2021,
    replaceMeta = TRUE
  ) %>%
  monitor_filter(stateCode == "ID")


wrcc_ID %>% monitor_leaflet(extraVars = "address")


northID <-
  wrcc_ID %>%
  monitor_filterByDistance(
    longitude = -116.3,
    latitude = 47.7,
    radius = 200000
  )


northID %>% monitor_leaflet()


northID %>%
  monitor_timeseriesPlot(ylim = c(0,500))

northID %>%
  monitor_filterDate(20160105, 20160205) %>%
  ###()
  monitor_timeseriesPlot(type = 'l', addAQI = TRUE, shadedNight = TRUE)

# * Fancy diurnal plot -----

study_monitor <-
  northID %>%
  monitor_select("87aaf7098c330877_wrcc.sm16") %>%
  monitor_filterDate(20160105, 20160205)

timeInfo <- monitor_timeInfo(study_monitor)

study_day <- study_monitor
study_day$data[timeInfo$night,2] <- NA

study_night <- study_monitor
study_night$data[timeInfo$day,2] <- NA

monitor_timeseriesPlot(study_monitor, type = "l", col = "lightgray", shadedNight = TRUE, addAQI = TRUE)
monitor_timeseriesPlot(study_day, col = "goldenrod", opacity = 1, add = TRUE, pch = 16, cex = 0.8)
monitor_timeseriesPlot(study_night, col = "midnightblue", opacity = 1, add = TRUE, pch = 16, cex = 0.8)

layout(matrix(seq(2)))
par(mar = c(5,5,4,2) + .1)
study_day %>% monitor_dailyBarplot(minHours = 3, ylim = c(0, 40), main = "Pinehurst, ID -- Daytime Average")
addAQILegend("topright", pch = 15, pt.cex = 1.5)
study_night %>% monitor_dailyBarplot(minHours = 3, ylim = c(0, 40), main = "Pinehurst, ID -- Nighttime Average")
par(mar = c(5,4,4,2) + .1)
layout(1)

