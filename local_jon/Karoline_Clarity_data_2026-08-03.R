# Here is a script that will load an AirFire copy of Clarity data for 2026.
#
# It demonstrates working with a single sensor in Chicago and all sensors in St. Paul.

library(AirMonitor)

# Custom loading script that is not part of the package
source("https://raw.githubusercontent.com/MazamaScience/AirMonitor/refs/heads/main/local_jon/clarity_loadAnnual.R")

clarity_2026 <- clarity_loadAnnual(2026)

# Create a 'monitor' object for July
clarity_July <-
  clarity_2026 %>%
  monitor_filterDate(
    startdate = "2026-07-01",
    enddate = "2026-08-01",
    timezone = "America/New_York"
  )

# Timeseries plot
clarity_July %>%
  monitor_timeseriesPlot(
    shadedNight = TRUE,
    addAQI = TRUE
  )

# Create a 'monitor' object for the smoke event
clarity_Canada_smoke <-
  clarity_2026 %>%
  monitor_filterDate(
    startdate = "2026-07-15",
    enddate = "2026-07-22",
    timezone = "America/New_York"
  )

# Interactive map
clarity_Canada_smoke %>%
  monitor_leaflet()

# Zoom in to Chicago and pick the sensor on the waterfront in The Loop
# Copy the deviceDeploymentID in bold

# Single monitor
DPCUT7707 <-
  clarity_Canada_smoke %>%
  monitor_select("dp3wq0nnd_clarity.DPCUT7707")

# Timeseries plot
DPCUT7707 %>% monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)

# Air quality categories
DPCUT7707 %>% monitor_toAQCTable()

# CSV file
DPCUT7707 %>%
  monitor_toCSV() %>%
  cat(file = "DPCUT7707.csv")

# You can also work with multi-sensor 'monitor' objects
# Use the interactive map to get the location of a sensor in St. Paul
#
#.   longitude = -93.10267, latitude = 44.94405

St_Paul <-
  clarity_Canada_smoke %>%
  monitor_filterByDistance(
    longitude = -93.10267,
    latitude = 44.94405,
    radius = 50000 # meters
  )

# Did we get what we wanted? ==> Yes.
St_Paul %>% monitor_leaflet()

# Timeseries Plot
St_Paul %>% monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)

# Interactive viewer
St_Paul %>% monitor_getData() %>% View()

# Air quality categories
St_Paul %>% monitor_toAQCTable()


