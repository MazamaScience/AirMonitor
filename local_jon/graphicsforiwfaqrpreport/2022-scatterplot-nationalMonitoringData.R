#############################################################################################################
## Script to extract monitoring data from airnow, airsis, wrcc, epa frm, and epa non-frm monitors based on a
## stateCode, year, and months within the year. Do daily averages. Plot all monitors together. 
## Plot all monitors in leaflet.
##
## Values to set:
##    year (yyyy)
##    mmdd_beg - beggining month (mmdd)
##    mmdd_end - ending month (mmdd)
##    mystate - NULL for entire US
##    max_ylim - y-axis limit for the AQI timeseries graph
##    title_txt - title text for the AQI timeseries graph
##

#source ("/Users/Susan/Projects/Rscripts/monitoringDataUtilityFunctions.R")
setwd("C:/Users/pcorrigan/Documents/R/IWFAQRP/Output")

library(PWFSLSmoke)
library(leaflet)

# ---------------------------------------------------------------------
# Variables to set
# ---------------------------------------------------------------------
year <- 2022
mmdd_beg <- "0415" # mmdd
mmdd_end <- "1101" # mmdd 
#pick one...
#mystate <- "MT"
#WEST--DEPLOYED STATES
#mystate <- c('OR','WA','CA','ID','NV','MT','CO','UT','AZ','NM','WY') # add "AK" for Alaska, but without Michigan Wisconsin and Minnesota ("MN","MI","WI")

mystate <- NULL # entire US
max_ylim <- 1200
title_txt <- "Timing of Smoke Impacts over entire US (all AQI levels), Apr 15-Nov 1" # (24 hr midnight to midnight average of PM2.5)
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# Begin script
# ---------------------------------------------------------------------
tbeg <- paste (year, mmdd_beg, sep = "") # yyyymmdd
tend <- paste (year, mmdd_end, sep = "") # yyyymmdd

# Load temporary PM2.5 monitoring data from the WRCC (Western Regional Climate Center) system.
wrcc <- NULL
wrcc <- wrcc_loadAnnual (year) %>%
  monitor_subset(stateCodes = mystate, tlim = c(tbeg,tend)) %>%
  monitor_dailyStatistic() %>%
  monitor_subset(vlim=(c(0,1e6))) # remove monitors with zero data

# Load temporary PM2.5 monitoring data from the airsis system.
airsis <- NULL
airsis <- airsis_loadAnnual (year) %>%
  monitor_subset(stateCodes = mystate, tlim = c(tbeg,tend)) %>%
  monitor_dailyStatistic() %>%
  monitor_subset(vlim=(c(0,1e6))) # remove monitors with zero data

# Load PM2.5 monitoring data from the EPA AirNow system.
airnow <- NULL
airnow <- airnow_loadAnnual (year) %>%
  monitor_subset(stateCodes = mystate, tlim = c(tbeg,tend)) %>%
  monitor_dailyStatistic() %>%
  monitor_subset(vlim=(c(0,1e6))) # remove monitors with zero data

# Load PM2.5 monitoring data from the EPA AQS system
epa_frm <- NULL
epa_frm <- epa_loadAnnual (year, parameterCode = "88101") %>%
  monitor_subset(stateCodes = mystate, tlim = c(tbeg,tend)) %>%
  monitor_dailyStatistic() %>%
  monitor_subset(vlim=(c(0,1e6))) # remove monitors with zero data

# Load temporary PM2.5 monitoring data from the EPA AQS Temporary monitors
epa_nonfrm <- NULL
epa_nonfrm <- epa_loadAnnual (year, parameterCode = "88502") %>%
  monitor_subset(stateCodes = mystate, tlim = c(tbeg,tend)) %>%
  monitor_dailyStatistic() %>%
  monitor_subset(vlim=(c(0,1e6))) # remove monitors with zero data

# ---------------------------------------------------------------------
# Combine all the monitor datasets into a single monitor object
# ---------------------------------------------------------------------
mlist<-list()
listval <- 0
# due to dupliate records can't use all these together. Implemented a heirarchy of preference.
# Use airnow (first choice), if that doesn't exist, then use the EPA AQS data.
if (length(airnow) > 0){
  listval <- listval + 1
  mlist[[listval]]<-airnow
} else { 
  if (length(epa_frm) > 0){
    listval <- listval + 1
    mlist[[listval]]<-epa_frm
  } 
  if (length(epa_nonfrm) > 0){
    listval <- listval + 1
    mlist[[listval]]<-epa_nonfrm
  }
}
# always use airsis if available
if (length(airsis) > 0){
  listval <- listval + 1
  mlist[[listval]]<-airsis
}
# always use wrcc if available
if (length(wrcc) > 0){
  listval <- listval + 1
  mlist[[listval]]<-wrcc
}
all_monitors<-monitor_combine(mlist)
#max_all <- max(all_monitors$data[,2:length(all_monitors$meta$monitorID)+1], na.rm = TRUE)



# ---------------------------------------------------------------------
# Plot the time series of all the monitors (AQI colors)
# ---------------------------------------------------------------------
monitorPlot_timeseries(all_monitors, 
                       style = 'aqidots', 
                       pch=13, 
                       ylab = "24-hr Average PM2.5 (micrograms/m^3)", 
                       xlab = year, 
                       ylim=c(0,800))
addAQILegend(x="topleft")
mytitle <- paste (year, title_txt, sep = " ")
title(mytitle)



# ---------------------------------------------------------------------
# plot the monitor locations in leaflet using different colors for the
# monitor source
# ---------------------------------------------------------------------
airnow_meta <- monitor_extractMeta(airnow)
airsis_meta <- monitor_extractMeta(airsis)
wrcc_meta <- monitor_extractMeta(wrcc)
myleaf <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
  addCircles(data=airnow_meta, color = "red")%>%
  addCircles(data=airsis_meta, color = "blue")%>%
  addCircles(data=wrcc_meta, color = "green")
myleaf

