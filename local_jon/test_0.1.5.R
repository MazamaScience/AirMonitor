# Version 0.1.5

# ----- Test latest/daily ------------------------------------------------------

library(AirMonitor)

archiveBaseUrl <- "http://data-monitoring_v2-c1.airfire.org/monitoring-v2"

airsis_2020 <- airsis_loadAnnual(2020, archiveBaseUrl = archiveBaseUrl)
airsis_2019 <- airsis_loadAnnual(2019, archiveBaseUrl = archiveBaseUrl)
airsis_2018 <- airsis_loadAnnual(2018, archiveBaseUrl = archiveBaseUrl)
airsis_2017 <- airsis_loadAnnual(2017, archiveBaseUrl = archiveBaseUrl)
airsis_2016 <- airsis_loadAnnual(2016, archiveBaseUrl = archiveBaseUrl)
airsis_2015 <- airsis_loadAnnual(2015, archiveBaseUrl = archiveBaseUrl)
airsis_2014 <- airsis_loadAnnual(2014, archiveBaseUrl = archiveBaseUrl)

# ----- Explore 2020 -----------------------------------------------------------

airsis_2020 %>% monitor_timeseriesPlot(addAQI = TRUE, ylim=c(0,500), xpd = NA)

airsis_2020 %>% monitor_leaflet()

# New Mexico data is messed up
airsis_2020 %>%
  monitor_filter(stateCode == "NM") %>%
  monitor_timeseriesPlot(addAQI = TRUE)

# New Mexico data is messed up
airsis_2020 %>%
  monitor_filter(stateCode == "NM") %>%
  monitor_leaflet()

# ----- Explore 2019 -----------------------------------------------------------

airsis_2019 %>% monitor_timeseriesPlot(addAQI = TRUE, ylim=c(0,500), xpd = NA)

# Not so interesting

airsis_2019 %>% monitor_leaflet()

# Only CA


# ----- Explore 2018 -----------------------------------------------------------

airsis_2018 %>% monitor_timeseriesPlot(addAQI = TRUE, ylim=c(0,500), xpd = NA)

# Not so interesting

airsis_2018 %>% monitor_leaflet()

# CA once again is the worst

airsis_2018 %>%
  monitor_filter(stateCode == "CA") %>%
  monitor_trimDate() %>%
  monitor_timeseriesPlot(addAQI = TRUE)

# ----- Test monitor_filterByDistance() ----------------------------------------

monitor <- airsis_2014
longitude <- -120.5391
latitude <- 38.76745
radius <- 30000
count <- NULL
addToMeta <- TRUE


airsis_2014 %>%
  monitor_filterByDistance(longitude, latitude, radius = 30000) %>%
  monitor_leaflet()

airsis_2018 %>%
  monitor_filterByDistance(longitude, latitude, radius = 30000) %>%
  monitor_leaflet()

airsis_2019 %>%
  monitor_filterByDistance(longitude, latitude, radius = 30000) %>%
  monitor_leaflet()

airsis_2020 %>%
  monitor_filterByDistance(longitude, latitude, radius = 30000) %>%
  monitor_leaflet()



