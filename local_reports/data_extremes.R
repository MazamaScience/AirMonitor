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
    MazamaCoreUtils::parseDatetime(20230101, timezone = "UTC"),
    by = "hours")
)

airsis_2022$data <- dplyr::left_join(hourlyTbl, airsis_2022$data, by = "datetime")

# ----- Plot -------------------------------------------------------------------

layout(matrix(seq(8), ncol = 2, byrow = TRUE))
par(mar = c(3,4,4,2)+.1)
monitor_timeseriesPlot(airsis_2022, xlab = "", ylim=c(0, 1500), xpd = NA, main = "AIRSIS 2022 data")
monitor_timeseriesPlot(airsis_2021, xlab = "", ylim=c(0, 1500), xpd = NA, main = "AIRSIS 2021 data")
monitor_timeseriesPlot(airsis_2020, xlab = "", ylim=c(0, 1500), xpd = NA, main = "AIRSIS 2020 data")
monitor_timeseriesPlot(airsis_2019, xlab = "", ylim=c(0, 1500), xpd = NA, main = "AIRSIS 2019 data")
monitor_timeseriesPlot(airsis_2018, xlab = "", ylim=c(0, 1500), xpd = NA, main = "AIRSIS 2018 data")
monitor_timeseriesPlot(airsis_2017, xlab = "", ylim=c(0, 1500), xpd = NA, main = "AIRSIS 2017 data")
monitor_timeseriesPlot(airsis_2016, xlab = "", ylim=c(0, 1500), xpd = NA, main = "AIRSIS 2016 data")
monitor_timeseriesPlot(airsis_2015, xlab = "", ylim=c(0, 1500), xpd = NA, main = "AIRSIS 2015 data")
par(mar = c(5,4,4,2) + .1)
layout(1)

# ===== EXPLORE AIRSIS 2022 ====================================================

threshold <- 500

worst_sites <-
  airsis_2022 %>%
  monitor_selectWhere(
    function(x) { any(x >= threshold, na.rm = TRUE) }
  )

worst_sites %>% monitor_leaflet()

worst_sites %>%
  monitor_filterDate(20220726, 20220810) %>%
  monitor_dropEmpty() %>%
  monitor_select("7189571beee8f765_arb3.2017") %>%
  #AirMonitorPlots::monitor_ggTimeseries()
  #monitor_timeseriesPlot()
  monitor_leaflet()

# ===== WRCC ===================================================================

wrcc_2022 <- wrcc_loadAnnual(2022)
wrcc_2021 <- wrcc_loadAnnual(2021)
wrcc_2020 <- wrcc_loadAnnual(2020)
wrcc_2019 <- wrcc_loadAnnual(2019)
wrcc_2018 <- wrcc_loadAnnual(2018)
wrcc_2017 <- wrcc_loadAnnual(2017)
wrcc_2016 <- wrcc_loadAnnual(2016)
wrcc_2015 <- wrcc_loadAnnual(2015)

# ----- Extend wrcc_2022 -----------------------------------------------------

# Create a tibble with a regular time axis
hourlyTbl <- dplyr::tibble(
  datetime = seq(
    MazamaCoreUtils::parseDatetime(20220101, timezone = "UTC"),
    MazamaCoreUtils::parseDatetime(20230101, timezone = "UTC"),
    by = "hours")
)

wrcc_2022$data <- dplyr::left_join(hourlyTbl, wrcc_2022$data, by = "datetime")

# ----- Plot -------------------------------------------------------------------

layout(matrix(seq(8), ncol = 2, byrow = TRUE))
par(mar = c(3,4,4,2)+.1)
monitor_timeseriesPlot(wrcc_2022, xlab = "", ylim=c(0, 1500), xpd = NA, main = "WRCC 2022 data")
monitor_timeseriesPlot(wrcc_2021, xlab = "", ylim=c(0, 1500), xpd = NA, main = "WRCC 2021 data")
monitor_timeseriesPlot(wrcc_2020, xlab = "", ylim=c(0, 1500), xpd = NA, main = "WRCC 2020 data")
monitor_timeseriesPlot(wrcc_2019, xlab = "", ylim=c(0, 1500), xpd = NA, main = "WRCC 2019 data")
monitor_timeseriesPlot(wrcc_2018, xlab = "", ylim=c(0, 1500), xpd = NA, main = "WRCC 2018 data")
monitor_timeseriesPlot(wrcc_2017, xlab = "", ylim=c(0, 1500), xpd = NA, main = "WRCC 2017 data")
monitor_timeseriesPlot(wrcc_2016, xlab = "", ylim=c(0, 1500), xpd = NA, main = "WRCC 2016 data")
monitor_timeseriesPlot(wrcc_2015, xlab = "", ylim=c(0, 1500), xpd = NA, main = "WRCC 2015 data")
par(mar = c(5,4,4,2) + .1)
layout(1)



