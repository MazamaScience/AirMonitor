# Version 0.1.5

# ----- Test latest/daily ------------------------------------------------------

library(AirMonitor)

archiveBaseUrl <- "https://airfire-data-exports.s3.us-west-2.amazonaws.com/monitoring/v2"

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

# ----- Combine all files? -----------------------------------------------------

airsis <-
  monitor_combine(
    airsis_2017,
    airsis_2018,
    airsis_2019,
    airsis_2020
  )


ca <-
  airsis %>%
  monitor_filter(stateCode == "CA")

par(mar = c(5,5,2,2) + 0.1)
ca %>%
  monitor_dailyThreshold("unhealthy") %>%
  monitor_collapse(FUN = sum) %>%
  monitor_timeseriesPlot()
par(mar = c(5,4,2,2) + 0.1)

# ----- AirNow 2021 ------------------------------------------------------------

airnow <-
  list(
    airnow_loadMonthly(202101, archiveBaseUrl = archiveBaseUrl),
    airnow_loadMonthly(202102, archiveBaseUrl = archiveBaseUrl),
    airnow_loadMonthly(202103, archiveBaseUrl = archiveBaseUrl),
    airnow_loadMonthly(202104, archiveBaseUrl = archiveBaseUrl),
    airnow_loadMonthly(202105, archiveBaseUrl = archiveBaseUrl),
    airnow_loadMonthly(202106, archiveBaseUrl = archiveBaseUrl),
    airnow_loadMonthly(202107, archiveBaseUrl = archiveBaseUrl),
    airnow_loadMonthly(202108, archiveBaseUrl = archiveBaseUrl),
    airnow_loadMonthly(202109, archiveBaseUrl = archiveBaseUrl),
    airnow_loadMonthly(202110, archiveBaseUrl = archiveBaseUrl),
    airnow_loadMonthly(202111, archiveBaseUrl = archiveBaseUrl),
    airnow_loadMonthly(202112, archiveBaseUrl = archiveBaseUrl)
  ) %>%
  monitor_combine()

# TODO:  Cretae 'data' and 'meta' and save them separately.


monitor_leaflet(airnow)

airnow %>% monitor_select("e05aac8a6c6486ca_460710001") %>% monitor_timeseriesPlot()

airnow %>% monitor_select("b6ef8130a407261b_410290019") %>% monitor_timeseriesPlot()

airnow %>%
  monitor_select("b6ef8130a407261b_410290019") %>%
  monitor_filterDate(20210705, 20210719) %>%
  monitor_timeseriesPlot(addAQI = TRUE, shadedNight = TRUE, type = 'l')





