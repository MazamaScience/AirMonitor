# Version 0.0.8

library(AirMonitor)

archiveBaseUrl <- "https://airfire-data-exports.s3.us-west-2.amazonaws.com/monitoring/v2"

l1 <- airnow_loadLatest(archiveBaseUrl = archiveBaseUrl)
l2 <- airsis_loadLatest(archiveBaseUrl = archiveBaseUrl)
l3 <- wrcc_loadLatest(archiveBaseUrl = archiveBaseUrl)

###d1 <- airnow_loadDaily(archiveBaseUrl = archiveBaseUrl)
d2 <- airsis_loadDaily(archiveBaseUrl = archiveBaseUrl)
d3 <- wrcc_loadDaily(archiveBaseUrl = archiveBaseUrl)

ca <-
  d2 %>% monitor_filter(stateCode == "CA")

ca %>% monitor_leaflet()

# ----- Daily averages -----

ids <- ca$meta$deviceDeploymentID

layout(matrix(1:4, ncol = 2, byrow = TRUE))

for ( id in ids[1:4] ) {

  try({
    monitor_dailyBarplot(ca, id = id, xlab = NULL, sub = "")
  }, silent = FALSE)

}

layout(1)

