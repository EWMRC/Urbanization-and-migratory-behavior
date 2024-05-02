# Data Prep for Zoe's Analysis
# October 2023
# Last revision: SJC December 2023

library(tidyverse)
library(lubridate)

# Input is classified AMWO data from this paper for spring and fall
# remove pre-and post migration areas? doing that for now, can always re-do later

# Data -----
fbirds <- read.csv('G:\\My Drive\\Woodcock\\migration_strategies\\fall_amwo_class_011722.csv')
sbirds <- read.csv('G:\\My Drive\\Woodcock\\migration_strategies\\SPR_amwo_class_012523.csv')

# Stopover Data Calculations -----
# *****************************************************************************

# Fall
# Keep only migration points, get rid of correlated random walk points ("fake data")
# get rid of a few individuals that broke the code for ACF hunting season analysis

fbirds <- fbirds %>%
  filter(class=='stop'|class=='stopover') %>% 
  filter(locType=='o') %>% 
  mutate(time=as_datetime(time))

brd <- list()
fid <- unique(fbirds$ID)
for (i in 1:length(unique(fbirds$ID))){
  brd[[i]] <- fbirds[which(fbirds$ID==fid[i]),]
}

# Calculate stopover metrics
# Create Metric table to fill in 
# Loop to fill in first set of metrics 
all_stopovers <- list()
for (i in 1:length(brd)){ 
  if (length(which(brd[[i]]$class=='stop'))>0){
  # Data frame for stops first (one point per stop)
  tmp_stop <- brd[[i]][which(brd[[i]]$class=='stop'),]
  tbl_stop <- data.frame(
    id=tmp_stop$ID,
    label= 66:(66+(nrow(tmp_stop)-1)),
    start_time = as_datetime(tmp_stop$time),
    end_time = tmp_stop$time + 43200, # 12 hours later in seconds
    admin_unit = NA,
    start_lat = tmp_stop$lat,
    start_lon = tmp_stop$lon,
    end_lat = tmp_stop$lat, # start/end the same for stop
    end_lon = tmp_stop$lon,
    year=year(tmp_stop$time),
    st.pr=NA,
    duration=rep(12, nrow(tmp_stop)),
    centroid_lat =tmp_stop$lat,
    centroid_lon=tmp_stop$lon,
    mean_step_length=0,
    stop_type='stop'
  )}
  # Data frame for stopovers (more than 1 point per stop)
  if (length(which(brd[[i]]$class=='stopover'))>0){
    tmp <- brd[[i]][which(brd[[i]]$class=='stopover'),]
    tbl_stopover <- matrix(NA, nrow=length(unique(tmp$label[which(tmp$class=='stopover')])), ncol=11) #add columns here
    tbl_stopover <- data.frame(tbl_stopover)
    names(tbl_stopover) <- c('id', 'label', 'start_time', 'end_time', 'admin_unit', 'start_lat', 'start_lon',
                             'end_lat', 'end_lon', 'year', 'duration') #add column labels here
    tbl_stopover$id <- rep(tmp$ID[1], times=nrow(tbl_stopover))
    tbl_stopover$label <- unique(tmp$label)
    for (p in 1:nrow(tbl_stopover)){ # anything else you want to calculate by individual stopover you put in here
      yam <- tmp[which(tmp$label==tbl_stopover$label[p]),]
      tbl_stopover$start_time[p] <- yam$time[1]
      tbl_stopover$end_time[p] <- yam$time[nrow(yam)]
      tbl_stopover$admin_unit[p] <- NA # ignore this until later
      tbl_stopover$start_lat[p] <- yam$lat[1]
      tbl_stopover$end_lat[p] <- yam$lat[nrow(yam)]
      tbl_stopover$start_lon[p] <- yam$lon[1]
      tbl_stopover$end_lon[p] <- yam$lon[nrow(yam)]
      tbl_stopover$year[p] <- year(yam$time[1]) # this was causing the 1970 thing I think
      tbl_stopover$st.pr[p] <- NA
      if (nrow(yam)>1){
        tbl_stopover$duration[p] <- abs(difftime(as_datetime(yam$time[1]), as_datetime(yam$time[nrow(yam)]), tz="UTC", units='hours'))}
      else if (nrow(yam)==1){ 
        tbl_stopover$duration[p] <- 12}
      tbl_stopover$centroid_lat[p] <- mean(yam$lat)
      tbl_stopover$centroid_lon[p] <- mean(yam$lon)
      tbl_stopover$mean_step_length[p] <- mean(yam$dist[which(yam$stop==1)])
      tbl_stopover$stop_type[p] <- 'stopover'
    }
  tbl_stopover$start_time <- as_datetime(tbl_stopover$start_time) # for some reason this converts here but not above
  tbl_stopover$end_time <- as_datetime(tbl_stopover$end_time) 
  all_stopovers[[i]] <- bind_rows(tbl_stop, tbl_stopover)
  }# else if (length(which(brd[[i]]$class=='stopover'))==0){
  #all_stopovers[[i]] <- tbl_stop
#}
  if (length(which(brd[[i]]$class=='stopover'))>0){
    all_stopovers[[i]] <- rbind(tbl_stop, tbl_stopover)} else {
      all_stopovers[[i]] <- tbl_stop}
  all_stopovers[[i]] <- all_stopovers[[i]] %>% arrange(end_time)
  print(i) # completes 174 total birds, timezone warning seems fine
}


# Combine all into a data frame
# This is 1 row for each stopover (id is bird id, label is stopover label)
# duration is in hours
fall_stopover_data <- bind_rows(all_stopovers)
fall_stopover_data <- fall_stopover_data %>% rename(duration_hours = duration, mean_step_length_km=mean_step_length) %>%
  select(-admin_unit, -st.pr) %>%
  mutate(duration_days = duration_hours/24, season='fall')

head(fall_stopover_data)

# Repeat for spring
# Keep only migration points, get rid of correlated random walk points ("fake data")
# get rid of a few individuals that broke the code for ACF hunting season analysis

sbirds <- sbirds %>%
  filter(class=='stop'|class=='stopover') %>% 
  filter(locType=='o') %>% 
  mutate(time=as_datetime(time))

brd <- list()
sid <- unique(sbirds$ID)
for (i in 1:length(unique(sbirds$ID))){
  brd[[i]] <- sbirds[which(sbirds$ID==sid[i]),]
}

# Calculate stopover metrics
# Create Metric table to fill in 
# Loop to fill in first set of metrics 
all_stopovers <- list()
for (i in 1:length(brd)){ 
  if (length(which(brd[[i]]$class=='stop'))>0){
    # Data frame for stops first (one point per stop)
    tmp_stop <- brd[[i]][which(brd[[i]]$class=='stop'),]
    tbl_stop <- data.frame(
      id=tmp_stop$ID,
      label= 66:(66+(nrow(tmp_stop)-1)),
      start_time = as_datetime(tmp_stop$time),
      end_time = tmp_stop$time + 43200, # 12 hours later in seconds
      admin_unit = NA,
      start_lat = tmp_stop$lat,
      start_lon = tmp_stop$lon,
      end_lat = tmp_stop$lat, # start/end the same for stop
      end_lon = tmp_stop$lon,
      year=year(tmp_stop$time),
      st.pr=NA,
      duration=rep(12, nrow(tmp_stop)),
      centroid_lat =tmp_stop$lat,
      centroid_lon=tmp_stop$lon,
      mean_step_length=0,
      stop_type='stop'
    )}
  # Data frame for stopovers (more than 1 point per stop)
  if (length(which(brd[[i]]$class=='stopover'))>0){
    tmp <- brd[[i]][which(brd[[i]]$class=='stopover'),]
    tbl_stopover <- matrix(NA, nrow=length(unique(tmp$label[which(tmp$class=='stopover')])), ncol=11) #add columns here
    tbl_stopover <- data.frame(tbl_stopover)
    names(tbl_stopover) <- c('id', 'label', 'start_time', 'end_time', 'admin_unit', 'start_lat', 'start_lon',
                             'end_lat', 'end_lon', 'year', 'duration') #add column labels here
    tbl_stopover$id <- rep(tmp$ID[1], times=nrow(tbl_stopover))
    tbl_stopover$label <- unique(tmp$label)
    for (p in 1:nrow(tbl_stopover)){ # anything else you want to calculate by individual stopover you put in here
      yam <- tmp[which(tmp$label==tbl_stopover$label[p]),]
      tbl_stopover$start_time[p] <- yam$time[1]
      tbl_stopover$end_time[p] <- yam$time[nrow(yam)]
      tbl_stopover$admin_unit[p] <- NA # ignore this until later
      tbl_stopover$start_lat[p] <- yam$lat[1]
      tbl_stopover$end_lat[p] <- yam$lat[nrow(yam)]
      tbl_stopover$start_lon[p] <- yam$lon[1]
      tbl_stopover$end_lon[p] <- yam$lon[nrow(yam)]
      tbl_stopover$year[p] <- year(yam$time[1])
      tbl_stopover$st.pr[p] <- NA
      if (nrow(yam)>1){
        tbl_stopover$duration[p] <- abs(difftime(as_datetime(yam$time[1]), as_datetime(yam$time[nrow(yam)]), tz="UTC", units='hours'))}
      else if (nrow(yam)==1){ 
        tbl_stopover$duration[p] <- 12}
      tbl_stopover$centroid_lat[p] <- mean(yam$lat)
      tbl_stopover$centroid_lon[p] <- mean(yam$lon)
      tbl_stopover$mean_step_length[p] <- mean(yam$dist[which(yam$stop==1)])
      tbl_stopover$stop_type[p] <- 'stopover'
    }
    tbl_stopover$start_time <- as_datetime(tbl_stopover$start_time) # for some reason this converts here but not above
    tbl_stopover$end_time <- as_datetime(tbl_stopover$end_time) 
    all_stopovers[[i]] <- bind_rows(tbl_stop, tbl_stopover)
  }# else if (length(which(brd[[i]]$class=='stopover'))==0){
  #  all_stopovers[[i]] <- tbl_stop
  #}
  if (length(which(brd[[i]]$class=='stopover'))>0){
    all_stopovers[[i]] <- rbind(tbl_stop, tbl_stopover)} else {
      all_stopovers[[i]] <- tbl_stop}
  all_stopovers[[i]] <- all_stopovers[[i]] %>% arrange(end_time)
  #print(i) # completes 174 total birds, timezone warning seems fine
}

# Combine all into a data frame
# This is 1 row for each stopover (id is bird id, label is stopover label)
# duration is in hours
spring_stopover_data <- bind_rows(all_stopovers)
spring_stopover_data <- spring_stopover_data %>% rename(duration_hours = duration, mean_step_length_km=mean_step_length) %>%
  select(-admin_unit, -st.pr) %>%
  mutate(duration_days = duration_hours/24, season='spring')

head(spring_stopover_data)

# combine spring and fall stopover data
stopover_data_all_seasons <- rbind(fall_stopover_data, spring_stopover_data)

stopover_data_all_seasons$uniqueID <- paste0(stopover_data_all_seasons$id, stopover_data_all_seasons$label, stopover_data_all_seasons$season)
length(stopover_data_all_seasons$uniqueID[which(duplicated(stopover_data_all_seasons)==T)])
stopover_data_all_seasons <- stopover_data_all_seasons[!duplicated(stopover_data_all_seasons),]

write.csv(stopover_data_all_seasons, 'stopover_data_all_seasons_for_ZP_UPDATED_120723.csv')

# 02 clean up full migration metric data -----
# ******************************************************************************

met_fall <- read.csv('FALL_amwo_metric_table_FOR_PCA_011723.csv')
met_spring <- read.csv('spring_amwo_metric_table_FOR_PCA_012523.csv')

head(met_fall)

met_fall <- met_fall %>%  mutate(season='fall') %>%
  rename(mean_stop_duration_hours = mean_stoptotal_duration_hours,
         distance_difference=straight_mig_dist_diff) %>%
  select(-X.1, -X)

met_spring <- met_spring %>% mutate(season='spring')%>%
  rename(mean_stop_duration_hours = mean_stoptotal_duration_hours,
         distance_difference=straight_mig_dist_diff) %>%
  select(-X.1, -X, -resid,-nstopover)

head(met_fall)
head(met_spring)

migration_data_all_seasons <- rbind(met_fall, met_spring)

write.csv(migration_data_all_seasons, 'migration_data_all_seasons_for_ZP_101323.csv')
