library(AirMonitor)

latest_orig <- clarity_loadLatest()

###latest_orig %>% monitor_filter(stateCode == "WA") %>% monitor_leaflet()

id1 <- "c0rvvr0hh_clarity.DMDXS3594"
mon1 <- latest_orig %>% monitor_select(id1)

id2 <- "c0rvvr07f_clarity.DMDXS3594"
mon2 <- latest_orig %>% monitor_select(id2)

mon2 %>% monitor_timeseriesPlot()

mon1 %>% monitor_timeseriesPlot(add = TRUE, col = "dodgerblue")

latest <-
  latest_orig %>%
  monitor_move(
    id1,
    mon2$meta$longitude,
    mon2$meta$latitude
  )
