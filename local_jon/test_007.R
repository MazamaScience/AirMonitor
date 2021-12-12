# Version 0.0.7

library(AirMonitor)

archiveBaseUrl <- "http://data-monitoring_v2-c1.airfire.org/monitoring-v2"

l1 <- airnow_loadLatest(archiveBaseUrl = archiveBaseUrl)
l2 <- airsis_loadLatest(archiveBaseUrl = archiveBaseUrl)
l3 <- wrcc_loadLatest(archiveBaseUrl = archiveBaseUrl)

###d1 <- airnow_loadDaily(archiveBaseUrl = archiveBaseUrl)
d2 <- airsis_loadDaily(archiveBaseUrl = archiveBaseUrl)
d3 <- wrcc_loadDaily(archiveBaseUrl = archiveBaseUrl)

# ----- Check on QC of negative values -----

layout(matrix(seq(1:3), nrow = 1))

airsis_loadDaily(archiveBaseUrl = archiveBaseUrl, QC_negativeValues = "ignore") %>%
  monitor_timeseriesPlot(ylim=c(-20,50), xpd = NA, main = "QC 'ignore'")

airsis_loadDaily(archiveBaseUrl = archiveBaseUrl, QC_negativeValues = "na") %>%
  monitor_timeseriesPlot(ylim=c(-20,50), xpd = NA, main = "QC 'na'")

airsis_loadDaily(archiveBaseUrl = archiveBaseUrl, QC_negativeValues = "zero") %>%
  monitor_timeseriesPlot(ylim=c(-20,50), xpd = NA, main = "QC 'zero'")

layout(1)

# ----- Daily averages -----

# #round dates down to week
# df$week <- floor_date(df$date, "week")
#
# #find mean sales by week
# df %>%
#   group_by(week) %>%
#   summarize(mean = mean(sales))

# See:  https://dplyr.tidyverse.org/articles/colwise.html

d3_daily_data <-
  d3$data %>%
  dplyr::mutate(
    day = lubridate::floor_date(.data$datetime, "day")
  ) %>%
  dplyr::select(-.data$datetime) %>%
  dplyr::group_by(.data$day) %>%
  dplyr::summarize(across(everything(), mean, na.rm = TRUE)) %>%
  dplyr::mutate(across(everything(), function(x) { x[!is.finite(x)] <- NA; return(x) })) %>%
  dplyr::rename(datetime = .data$day)


