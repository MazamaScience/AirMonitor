# ASIC 2022 Tutorial

# Load the package
library(AirMonitor)

# Load the latest 10-days of data
latest <- monitor_loadLatest()

# What is this 'latest' object?
class(latest)
names(latest)
dim(latest$meta)
dim(latest$data)

# Look at metadata for 10 monitors (10 rows)
View(latest$meta[1:10,])

# Look at data for 10 monitors ('datetime' + 10 columns)
View(latest$data[,1:11])

# Look at the map
latest %>% monitor_leaflet()

# Use the deviceDeploymentID to select a single monitor
monitor <-
  latest %>%
  monitor_select("4acfb3843fa942f0_040213015") # Hidden Valley, Arizona

dim(monitor$meta)
dim(monitor$data)

# Interactive time series
monitor %>%
  monitor_dygraph()

# Use the AirMonitorPlots package for publication ready plots

# Timeseries + NowCast
monitor %>%
  AirMonitorPlots::monitor_ggTimeseries_archival(
    title = "Foundry Park"
  )

# Time-of-day
monitor %>%
  AirMonitorPlots::monitor_ggDailyByHour_archival(
    title = "Foundary Park Hourly Average"
  )

# Pull out all monitors within 100 km
area_monitors <-
  latest %>%
  monitor_filterByDistance(
    monitor$meta$longitude,
    monitor$meta$latitude,
    radius = 100000           # meters
  )

# Have a look
area_monitors %>%
  monitor_leaflet()


# Plot many at once
area_monitors %>%
  monitor_timeseriesPlot(
    shadedNight = TRUE,
    addAQI = TRUE
  )

# Look at the locations for last fall
ids <- area_monitors$meta$deviceDeploymentID

# NOTE:  This download can take 5-20 sec
monitor_2021 <- monitor_loadAnnual(2021)

# Get phoenix area monitors during last fall
Phoenix_2021 <-
  monitor_2021 %>%
  monitor_select(ids) %>%
  monitor_filterDate(20210901, 20211231) %>%
  monitor_dropEmpty()

Phoenix_2021 %>%
  monitor_timeseriesPlot(
    ylim = c(0, 100),
    xpd = NA,
    addAQI = TRUE
  )

addAQILines()


