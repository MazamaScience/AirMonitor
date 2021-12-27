# Version 0.0.8

library(AirMonitor)

archiveBaseUrl <- "http://data-monitoring_v2-c1.airfire.org/monitoring-v2"

latest <- airnow_loadLatest(archiveBaseUrl = archiveBaseUrl)
latest_nowcast <- latest %>% monitor_nowcast()

nowcast <- airnow_loadLatest("PM2.5_nowcast", archiveBaseUrl = archiveBaseUrl)

j = 655
plot(nowcast$data[,c(1,j)], pch = 15)
points(latest_nowcast$data[,c(1,j)], cex = 1.2, col = "salmon")
title(nowcast$meta$locationName[j-1])

# NOTE:  Some differences for j = 52,  on 2021-12-23 immediately after missing data
# NOTE:  Some differences for j = 134 on 2021-12-27 immediately after missing data
# NOTE:  Some differences for j = 135 on 2021-12-26 immediately after zero values (lifted by load function?)
# NOTE:  Some differences for j = 138 on 2021-12-22 immediately after missing data
# NOTE:  Some differences for j = 618 on 2021-12-23 immediately after ???
