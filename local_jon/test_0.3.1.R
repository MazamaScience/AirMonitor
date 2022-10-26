# Version 0.3.1

# ----- Test airnow_latest -----------------------------------------------------

library(AirMonitor)

airnow_latest <- airnow_loadLatest()

# Is the airnow data using fullAQSID as the deviceID? If so, all should be 12 chars long.
table(stringr::str_length(airnow_latest$meta$deviceID))

# Deployment type should be known for all monitors in sites_meta
any(is.na(airnow_latest$meta$deploymentType))

# Are we lifting up negative values?
any(airnow_latest$data[,-1] < 0, na.rm = TRUE) # Should be FALSE

# Do we have any duplicated locations?
any(duplicated(airnow_latest$meta$locationID)) # May be TRUE

# Do we have any duplicated deviceDeploymentIDs?
any(duplicated(airnow_latest$meta$deviceDeploymentID)) # Must be FALSE

# Check New Mexico
airnow_latest %>%
  monitor_filter(stateCode == "NM") %>%
  monitor_leaflet()

# Anthony, NM
airnow_latest %>%
  monitor_select("c733850d59531f1e_840350130016") %>%
  monitor_timeseriesPlot(shadedNight = TRUE, addAQI = TRUE)

# Check temporary monitors
airnow_latest %>%
  monitor_filter(deploymentType == "Temporary") %>%
  monitor_leaflet()


# ----- Test airnow_daily ------------------------------------------------------

airnow_daily <- airnow_loadDaily()

# Do we have any duplicated locations?
any(duplicated(airnow_daily$meta$deviceDeploymentID)) # Must be FALSE

# Check size
dim(airnow_daily$data) # Should be ~ 1080 hrs X 1300 monitors

# Check time axis
range(airnow_daily$data$datetime)


# ----- Test airnow_loadMonthly ------------------------------------------------

airnow_monthly <- airnow_loadMonthly(202205, archiveBaseUrl = "http://data-monitoring_v2-c1.airfire.org/monitoring-v2/")

# Do we have any duplicated locations?
any(duplicated(airnow_monthly$meta$deviceDeploymentID)) # Must be FALSE

# Check size
dim(airnow_monthly$data) # Should be ~ 720 hrs X 1300 monitors

# Check time axis
range(airnow_monthly$data$datetime) # from day 1 00:00 UTC to day 1 00:00 UTC of next month

monitor_leaflet(airnow_monthly)

# South Valley, NM
airnow_monthly %>%
  monitor_select("fdd18ef585fcbb6b_001350010029") %>%
  monitor_timeseriesPlot(addAQI = TRUE)

# ----- Test epa_aqs_loadAnnual ------------------------------------------------

aqs_88101 <-
  epa_aqs_loadAnnual(
    2015,
    parameterCode = "88101",
    archiveDataDir = "~/Projects/AirFire/monitoring-data-ingest-v2/test/data"
  )



