# Create maps showing number of days at USG, U, VU or HAZ

# ----- Load 2022 monitor data -------------------------------------------------

library(ggplot2)
library(ggmap)
library(AirMonitor)

all_monitors <-

  # 1) load all monitors
  #    - negative values are lifted up to zero
  #    - use AirNow data rather than EPA AQS
  monitor_loadAnnual(
    year = 2022,
    QC_negativeValues = "zero",
    epaPreference = "airnow"
  ) %>%

  # 2) US only
  monitor_filter(countryCode == "US") %>%

  # 3) drop any monitors with no data
  monitor_dropEmpty()

# ----- Calculate daily means --------------------------------------------------

# NOTE:  Calculating daily means will depend upon the timezone so, from here on,
# NOTE:  we do things on a per-timezone basis.

dailyMeanList <- list()

for ( tz in unique(all_monitors$meta$timezone) ) {

  dailyMeanList[[tz]] <-
    a <-
    all_monitors %>%
    monitor_filter(timezone == tz) %>%
    monitor_filterDate(20220415, 20221101) %>%
    monitor_dailyStatistic(
      FUN = mean,
      na.rm = TRUE,
      minHours = 18,
      dayBoundary = "LST"
    )

}

# NOTE: The documentation for ?monitor_dailyStatistic has:
#
#   When dayBoundary = "LST", the returned monitor$data$datetime time axis will
#   be defined in "UTC" with times as they appear in standard time in the local
#   timezone. These days will be one hour off from clock time during DST but
#   every day will consist of 24 hours.
#
# NOTE:  Because everything is in UTC, we can combine them together.

all_monitors_daily <- monitor_combine(dailyMeanList)



################################################################################
################################################################################
################################################################################



# only doing USG and above, but can by altered

df <- data.frame(
  deviceDeploymentID = all_monitors_daily$meta$deviceDeploymentID,
  latitude = all_monitors_daily$meta$latitude,
  longitude = all_monitors_daily$meta$longitude,
  usg = NA,
  unhealthy = NA,
  very_unhealthy = NA,
  hazardous = NA,
  stringsAsFactors = FALSE
)

for( i in 1:nrow(df) ){
  id <- df$deviceDeploymentID[i]
  pm_data <- all_monitors_daily$data[,id]
  pm_aqi <- .bincode(pm_data, breaks = US_AQI$breaks_PM2.5)
  pm_aqi <- pm_aqi[!is.na(pm_aqi)]

  df$usg[i] <- sum(pm_aqi >= 3)
  df$unhealthy[i] <- sum(pm_aqi >= 4)
  df$very_unhealthy[i] <- sum(pm_aqi >= 5)
  df$hazardous[i] <- sum(pm_aqi >= 6)
}


### get plotting elements/data for plotting ###

states_outline <- ggplot2::map_data('state')
# unfortunately had to specify the states again here because I need full name to filter- be sure to make the lists match
###states_outline <- dplyr::filter(states_outline, region %in% c('Alabama', 'Alaska', 'American Samoa', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Guam', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Minor Outlying Islands', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Northern Mariana Islands', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Puerto Rico', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'U.S. Virgin Islands', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming'))


# bin number of days in df to set bins:
# note: since 0 is not included in bins_threhsolds, monitors with zero days in an AQI will get filtered out automatically yay!
bin_thresholds <- c(1, 7, 14, 21, 28, Inf) # in days
bin_labels <- paste(bin_thresholds, bin_thresholds[-1]-1, sep = '-')
bin_labels <- bin_labels[-which(grepl('Inf-', bin_labels))] # remove last one
bin_labels <- sub('-Inf','+', bin_labels) # change last label to be X+ instead of X-Inf

df$usg_bin <- .bincode(df$usg, breaks = bin_thresholds, right = F)
df$unhealthy_bin <- .bincode(df$unhealthy, breaks = bin_thresholds, right = F)
df$very_unhealthy_bin <- .bincode(df$very_unhealthy, breaks = bin_thresholds, right = F)
df$hazardous_bin <- .bincode(df$hazardous, breaks = bin_thresholds, right = F)


### create plots ###

# panel version: one png, all AQI category maps arranged
# usg
p1 <- ggplot() +
  xlim(min(states_outline$long),max(states_outline$long)) +
  ylim(min(states_outline$lat),max(states_outline$lat)) +
  geom_polygon(data = states_outline, mapping = aes(long, lat, group = group), fill = NA, colour = "grey50") +
  geom_point(data = df, mapping = aes(longitude, latitude, size = usg_bin), color = 'black') + # this is here to make the legend have black circles and not colored
  geom_point(data = df, mapping = aes(longitude, latitude, size = usg_bin), color = US_AQI$colors_EPA[3], show.legend = FALSE) +
  scale_size(breaks = 1:(length(bin_thresholds)-1), labels = bin_labels, name = 'Number of days') +
  #theme_nothing(legend = TRUE) +
  coord_map()

# unhealthy
p2 <- ggplot() +
  xlim(min(states_outline$long),max(states_outline$long)) +
  ylim(min(states_outline$lat),max(states_outline$lat)) +
  geom_polygon(data = states_outline, mapping = aes(long, lat, group = group), fill = NA, colour = "grey50") +
  geom_point(data = df, mapping = aes(longitude, latitude, size = unhealthy_bin), color = 'black') + # this is here to make the legend have black circles and not colored
  geom_point(data = df, mapping = aes(longitude, latitude, size = unhealthy_bin), color = US_AQI$colors_EPA[4], show.legend = FALSE) +
  scale_size(breaks = 1:(length(bin_thresholds)-1), labels = bin_labels, name = 'Number of days') +
 # theme_nothing(legend = TRUE) +
  coord_map()

# very unhealthy
p3 <- ggplot() +
  xlim(min(states_outline$long),max(states_outline$long)) +
  ylim(min(states_outline$lat),max(states_outline$lat)) +
  geom_polygon(data = states_outline, mapping = aes(long, lat, group = group), fill = NA, colour = "grey50") +
  geom_point(data = df, mapping = aes(longitude, latitude, size = very_unhealthy_bin), color = US_AQI$colors_EPA[5]) +
  scale_size(breaks = 1:(length(bin_thresholds)-1), labels = bin_labels) +
 # theme_nothing() +
  coord_map()

# hazardous
p4 <- ggplot() +
  xlim(min(states_outline$long),max(states_outline$long)) +
  ylim(min(states_outline$lat),max(states_outline$lat)) +
  geom_polygon(data = states_outline, mapping = aes(long, lat, group = group), fill = NA, colour = "grey50") +
  geom_point(data = df, mapping = aes(longitude, latitude, size = hazardous_bin), color = US_AQI$colors_EPA[6], show.legend = FALSE) +
  scale_size(breaks = 1:(length(bin_thresholds)-1), labels = bin_labels) +
 # theme_nothing() +
  coord_map()

# note legend is in p1 (just has to be in one of them for the common.legend=TRUE)
# give filename
ggsave(ggpubr::ggarrange(p1, p2, p3, p4, ncol=4, nrow=1, common.legend = TRUE, legend="bottom"),
       filename = '2022_whole_us_monitor_days.png', bg="white")

### looks like scale_size() has an argument "range" where you can set the min and max size of the dots
### this might be useful as the large circle is a little dominating. default is c(1,6)


# can also save one of those pX maps individually via something like...
ggsave(p1, filename = 'C:/Users/pcorrigan/Documents/R/IWFAQRP/Output/2022_USGmap.png')
ggsave(p2, filename = 'C:/Users/pcorrigan/Documents/R/IWFAQRP/Output/2022_Unhealthymap.png')
ggsave(p3, filename = 'C:/Users/pcorrigan/Documents/R/IWFAQRP/Output/2022_VeryUnhealthymap.png')
ggsave(p4, filename = 'C:/Users/pcorrigan/Documents/R/IWFAQRP/Output/2022_Hazmap.png')

