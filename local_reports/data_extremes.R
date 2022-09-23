# 2022-09-23
#
# Explore data extrems with newly rebuilt AIRSIS data archives
#

library(AirMonitor)

airsis_2022 <- airsis_loadAnnual(2022)
airsis_2021 <- airsis_loadAnnual(2021)
airsis_2020 <- airsis_loadAnnual(2020)
airsis_2019 <- airsis_loadAnnual(2019)
airsis_2018 <- airsis_loadAnnual(2018)
airsis_2017 <- airsis_loadAnnual(2017)
airsis_2016 <- airsis_loadAnnual(2016)
airsis_2015 <- airsis_loadAnnual(2015)


# ----- Extend airsis_2022 -----------------------------------------------------

# Create a tibble with a regular time axis
hourlyTbl <- dplyr::tibble(
  datetime = seq(
    MazamaCoreUtils::parseDatetime(20220101, timezone = "UTC"),
    MazamaCoreUtils::parseDatetime(20230101, timezone = "UTC"),,
    by = "hours")
)

airsis_2022$data <- dplyr::left_join(hourlyTbl, airsis_2022$data, by = "datetime")

# ----- Plot -------------------------------------------------------------------

layout(matrix(seq(8), ncol = 2, byrow = TRUE))
par(mar = c(3,4,4,2)+.1)
monitor_timeseriesPlot(airsis_2022, xlab = "", ylim=c(0, 1500), main = "AIRSIS 2022 data")
monitor_timeseriesPlot(airsis_2021, xlab = "", ylim=c(0, 1500), main = "AIRSIS 2021 data")
monitor_timeseriesPlot(airsis_2020, xlab = "", ylim=c(0, 1500), main = "AIRSIS 2020 data")
monitor_timeseriesPlot(airsis_2019, xlab = "", ylim=c(0, 1500), main = "AIRSIS 2019 data")
monitor_timeseriesPlot(airsis_2018, xlab = "", ylim=c(0, 1500), main = "AIRSIS 2018 data")
monitor_timeseriesPlot(airsis_2017, xlab = "", ylim=c(0, 1500), main = "AIRSIS 2017 data")
monitor_timeseriesPlot(airsis_2016, xlab = "", ylim=c(0, 1500), main = "AIRSIS 2016 data")
monitor_timeseriesPlot(airsis_2015, xlab = "", ylim=c(0, 1500), main = "AIRSIS 2015 data")
par(mar = c(5,4,4,2) + .1)
layout(1)

