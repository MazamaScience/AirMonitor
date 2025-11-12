library(AirMonitor)

url <- "https://airfire-data-exports.s3.us-west-2.amazonaws.com/sensors/v3/PM2.5"

annual_orig <- clarity_loadAnnual(2025, url)

annual_orig %>% monitor_filter(stateCode == "WA") %>% monitor_leaflet(jitter = 0)

id1 <- "c0rvvr0hh_clarity.DMDXS3594"
mon1 <- annual_orig %>% monitor_select(id1)

id2 <- "c0rvvr07f_clarity.DMDXS3594"
mon2 <- annual_orig %>% monitor_select(id2)

mon2 %>% monitor_timeseriesPlot()

mon1 %>% monitor_timeseriesPlot(add = TRUE, col = "dodgerblue")

annual <-
  annual_orig %>%
  monitor_move(
    id1,
    mon2$meta$longitude,
    mon2$meta$latitude
  )

annual %>% monitor_filter(stateCode == "WA") %>% monitor_leaflet(jitter = 0)

annual %>% monitor_select(id2) %>% monitor_timeseriesPlot()
