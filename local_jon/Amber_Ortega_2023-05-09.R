# Amber Ortega 2023-05-09
#
# We have 3 monitors out on a prescribed fire actively igniting in Durango,
# Smoke 21, 215, 216.  Two of the three monitors are showing on F&S Map, but all
# 3 are in V4 Monitoring and WRCC.  Can we get Smoke 215 on F&S Map? Is there a
# reason it’s not?  Is there anything I can do on my end to avoid this in the
# future?

library(AirMonitor)

wrcc <- wrcc_loadLatest()
airnow <- airnow_loadLatest()

wrcc %>% monitor_filter(stateCode == "CO") %>% monitor_leaflet()
wrcc %>% monitor_select("2a4531588cfb8a59_wrcc.s215") %>% monitor_timeseriesPlot()
airnow %>% monitor_filter(stateCode == "CO") %>% monitor_leaflet()
airnow %>% monitor_select("2a4531588cfb8a59_840MMFS10215") %>% monitor_timeseriesPlot()

airnow %>% monitor_select("2a4531588cfb8a59_840MMFS10215") %>% monitor_getData() %>% View()
airnow %>% monitor_select("2a4531588cfb8a59_840MMFS10215") %>% monitor_getMeta() %>% dplyr::glimpse()
wrcc %>% monitor_select("2a4531588cfb8a59_wrcc.s215") %>% monitor_getMeta() %>% dplyr::glimpse()
wrcc %>% monitor_select("2a4531588cfb8a59_wrcc.s215") %>% monitor_getData() %>% View()

