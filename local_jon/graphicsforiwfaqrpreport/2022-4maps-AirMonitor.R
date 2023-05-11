## generate maps of number of days in AQI thresholds
# amy marsha, with mods by Lee Tarnay
setwd("C:/Users/pcorrigan/Documents/R/IWFAQRP/Output")


### packages ###
library(ggplot2)
library(ggmap)
library(AirMonitor)

### args ###

# here you could specify different args, like the year or states included...
states <- c('AK', 'AL', 'AR', 'AS', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL', 'GA', 'GU', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 'MD', 'ME', 'MI', 'MN', 'MO', 'MP', 'MS', 'MT', 'NC', 'ND', 'NE', 'NH', 'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'PR', 'RI', 'SC', 'SD', 'TN', 'TX', 'UM', 'UT', 'VA', 'VI', 'VT', 'WA', 'WI', 'WV', 'WY')

year <- 2022


### calculate 24 hr averages from pm2.5 monitors ###

# monitors including:
# 1. permanent
# 2. temporary

# 1. permanent- airnow
# minimum number of hours needed set to be 20, can be adjusted
airnow_monitors <- AirMonitor::airnow_loadAnnual(year = year)
airnow_monitors <- PWFSLSmoke::monitor_subset(airnow_monitors, stateCodes = states)
tzs <- unique(airnow_monitors$meta$timezone)
airnow_monitors_avg <- NULL
for(tz in tzs){
  monitor_list <- airnow_monitors$meta$monitorID[airnow_monitors$meta$timezone == tz]
  sub <- PWFSLSmoke::monitor_subset(airnow_monitors, monitorIDs = monitor_list)
  
  pm_data_avg_tz <- AirMonitor::monitor_dailyStatistic(sub, FUN=get('mean'), minHours = 20)
  pm_data_avg_tz$data$datetime <- as.Date(pm_data_avg_tz$data$datetime)
  
  if(is.null(airnow_monitors_avg)){
    airnow_monitors_avg <- pm_data_avg_tz
  } else{
    airnow_monitors_avg <- AirMonitor::monitor_combine(list(airnow_monitors_avg, pm_data_avg_tz))
  }
}

# 2. temporary- airsis
# minimum number of hours needed set to be 20, can be adjusted
airsis_monitors <- AirMonitor::airsis_loadAnnual(year = year)
airsis_monitors <- PWFSLSmoke::monitor_subset(airsis_monitors, stateCodes = states)
tzs <- unique(airsis_monitors$meta$timezone)
airsis_monitors_avg <- NULL
for(tz in tzs){
  monitor_list <- airsis_monitors$meta$monitorID[airsis_monitors$meta$timezone == tz]
  sub <- PWFSLSmoke::monitor_subset(airsis_monitors, monitorIDs = monitor_list)
  
  pm_data_avg_tz <- AirMonitor::monitor_dailyStatistic(sub, minHours = 20)
  pm_data_avg_tz$data$datetime <- as.Date(pm_data_avg_tz$data$datetime)
  
  if(is.null(airsis_monitors_avg)){
    airsis_monitors_avg <- pm_data_avg_tz
  } else{
    airsis_monitors_avg <- AirMonitor::monitor_combine(list(airsis_monitors_avg, pm_data_avg_tz))
  }
}

# 3. temporary- wrcc
# minimum number of hours needed set to be 20, can be adjusted'
wrcc_monitors <- AirMonitor::wrcc_loadAnnual(year = year)
wrcc_monitors <- PWFSLSmoke::monitor_subset(wrcc_monitors, stateCodes = states)
tzs <- unique(wrcc_monitors$meta$timezone)
wrcc_monitors_avg <- NULL
for(tz in tzs){
  monitor_list <- wrcc_monitors$meta$monitorID[wrcc_monitors$meta$timezone == tz]
  sub <- PWFSLSmoke::monitor_subset(wrcc_monitors, monitorIDs = monitor_list)
  
  pm_data_avg_tz <- AirMonitor::monitor_dailyStatistic(sub, minHours = 20)
  pm_data_avg_tz$data$datetime <- as.Date(pm_data_avg_tz$data$datetime)
  
  if(is.null(wrcc_monitors_avg)){
    wrcc_monitors_avg <- pm_data_avg_tz
  } else{
    wrcc_monitors_avg <- AirMonitor::monitor_combine(list(wrcc_monitors_avg, pm_data_avg_tz))
  }
}

# combine
all_monitors_daily <- AirMonitor::monitor_combine(list(airnow_monitors_avg, airsis_monitors_avg, wrcc_monitors_avg))


### calculate number of days by AQI category ###

# only doing USG and above, but can by altered

df <- data.frame(monitorID = all_monitors_daily$meta$monitorID,
                 latitude = all_monitors_daily$meta$latitude,
                 longitude = all_monitors_daily$meta$longitude,
                 usg = NA,
                 unhealthy = NA,
                 very_unhealthy = NA,
                 hazardous = NA,
                 stringsAsFactors = F)

for(i in 1:nrow(df)){
  id <- df$monitorID[i]
  pm_data <- all_monitors_daily$data[,id]
  pm_aqi <- .bincode(pm_data, breaks = AQI$breaks_24)
  pm_aqi <- pm_aqi[!is.na(pm_aqi)]
  
  df$usg[i] <- sum(pm_aqi >= 3)
  df$unhealthy[i] <- sum(pm_aqi >= 4)
  df$very_unhealthy[i] <- sum(pm_aqi >= 5)
  df$hazardous[i] <- sum(pm_aqi >= 6)
}


### get plotting elements/data for plotting ###

states_outline <- ggplot2::map_data('state')
# unfortunately had to specify the states again here because I need full name to filter- be sure to make the lists match
states_outline <- dplyr::filter(states_outline, region %in% c('Alabama', 'Alaska', 'American Samoa', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Guam', 'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Minor Outlying Islands', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Northern Mariana Islands', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Puerto Rico', 'Rhode Island', 'South Carolina', 'South Dakota', 'Tennessee', 'Texas', 'U.S. Virgin Islands', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming'))


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
  geom_point(data = df, mapping = aes(longitude, latitude, size = usg_bin), color = AQI$colors[3], show.legend = FALSE) +
  scale_size(breaks = 1:(length(bin_thresholds)-1), labels = bin_labels, name = 'Number of days') +
  theme_nothing(legend = TRUE) +
  coord_map() 

# unhealthy
p2 <- ggplot() +
  xlim(min(states_outline$long),max(states_outline$long)) +
  ylim(min(states_outline$lat),max(states_outline$lat)) +
  geom_polygon(data = states_outline, mapping = aes(long, lat, group = group), fill = NA, colour = "grey50") +
  geom_point(data = df, mapping = aes(longitude, latitude, size = unhealthy_bin), color = 'black') + # this is here to make the legend have black circles and not colored
  geom_point(data = df, mapping = aes(longitude, latitude, size = unhealthy_bin), color = AQI$colors[4], show.legend = FALSE) +
  scale_size(breaks = 1:(length(bin_thresholds)-1), labels = bin_labels, name = 'Number of days') +
  theme_nothing(legend = TRUE) +
  coord_map() 

# very unhealthy
p3 <- ggplot() +
  xlim(min(states_outline$long),max(states_outline$long)) +
  ylim(min(states_outline$lat),max(states_outline$lat)) +
  geom_polygon(data = states_outline, mapping = aes(long, lat, group = group), fill = NA, colour = "grey50") +
  geom_point(data = df, mapping = aes(longitude, latitude, size = very_unhealthy_bin), color = AQI$colors[5]) +
  scale_size(breaks = 1:(length(bin_thresholds)-1), labels = bin_labels) +
  theme_nothing() +
  coord_map() 

# hazardous
p4 <- ggplot() +
  xlim(min(states_outline$long),max(states_outline$long)) +
  ylim(min(states_outline$lat),max(states_outline$lat)) +
  geom_polygon(data = states_outline, mapping = aes(long, lat, group = group), fill = NA, colour = "grey50") +
  geom_point(data = df, mapping = aes(longitude, latitude, size = hazardous_bin), color = AQI$colors[6], show.legend = FALSE) +
  scale_size(breaks = 1:(length(bin_thresholds)-1), labels = bin_labels) +
  theme_nothing() +
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

