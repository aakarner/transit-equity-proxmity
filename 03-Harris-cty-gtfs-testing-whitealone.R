library(tidytransit)
library(mapview)
library(tidycensus)
library(sf)
library(ggplot2)
library(ggthemes)
census_api_key("3a277651e6ec078927e10356269269b3698a0cfa")
# GTFS feeds obtained from transitfeeds.com
# The “before” dataset represents service from May 23, 2015 to August 15, 2015 
# and the “after” dataset represents service from August 16, 2015 to 
# January 23, 2016. 
# More information about System Reimagining is available here: 
# https://www.ridemetro.org/Pages/Reimagining.aspx

pre_sr_gtfs <- read_gtfs("data/20150517_htx.zip", 
                         local = TRUE,
                         geometry = TRUE,
                         frequency = TRUE)

post_sr_gtfs <- read_gtfs("data/20150818_htx.zip", 
                          local = TRUE,
                          geometry = TRUE,
                          frequency = TRUE)

#standardize frequency for weekdays b/w 6am and 10pm before/after change
pre_stop_freq <- get_stop_frequency(pre_sr_gtfs, start_hour = 6, end_hour = 22,
                                    dow = c(1,1,1,1,1,0,0), by_route=FALSE)
post_stop_freq <- get_stop_frequency(post_sr_gtfs, start_hour = 6, end_hour = 22,
                                    dow = c(1,1,1,1,1,0,0), by_route=FALSE)

#test#pre_stopfreq_sf <- stops_df_as_sf(pre_sr_gtfs$stops_frequency_df)

#merge stop frequency df with stop spatial locations ($stops_sf) & draw 400 meter buffer
pre_merged <- merge(pre_stop_freq$stops_sf, pre_stop_freq$stops_frequency_df, by = "stop_id")
pre_merged_t <- st_transform(pre_merged, 32139)
pre_stop_buffer <- st_buffer(pre_merged_t, dist = 400)
#mapview(pre_stop_buffer, add = "TRUE") #display pre stop buffers on leaflet
post_merged <- merge(post_stop_freq$stops_sf, post_stop_freq$stops_frequency_df, by = "stop_id")
post_merged_t <- st_transform(post_merged, 32139)
post_stop_buffer <- st_buffer(post_merged_t, dist = 400)
#mapview(post_stop_buffer, add = "TRUE")

# Get demographic data from the ACS
hlstatusvars <- c("B03002_003", # white alone
                  "B03002_004", # black alone
                  "B03002_006", # Asian
                  "B03002_012") # Hispanic or Latino

harris_trct_race <- get_acs(geography = "tract", variables = hlstatusvars, 
                            state = "TX", year = 2015,
                            county = "Harris County",
                            geometry = TRUE,
                            summary_var = "B03002_003") 
#add population density column
harris_trct_race$pop_dens <- calc_densities(harris_trct_race, var = "summary_est")

#transform ACS data for proper geospatial references
harris_trct_race_t <- st_transform(harris_trct_race, 32139)
#mapview(harris_trct_race_t, add = "FALSE")
#harris_trct_race_t
#intersect stop buffer geometries with ACS demographic data
pre_stop_acs_int <- st_intersection(pre_stop_buffer, harris_trct_race_t)
#mapview(pre_stop_acs_int) #doesn't load :(
post_stop_acs_int <- st_intersection(post_stop_buffer, harris_trct_race_t)
#add area attributes
pre_stop_acs_int$area <- st_area(pre_stop_acs_int)
post_stop_acs_int$area <- st_area(post_stop_acs_int)
#get new total population estimate value for new area by adding column
pre_stop_acs_int$pop_est_new <- pre_stop_acs_int$area*pre_stop_acs_int$pop_dens
post_stop_acs_int$pop_est_new <- post_stop_acs_int$area*post_stop_acs_int$pop_dens

#Population weighted mean metrics for entire system/population
pre_wmd <- weighted.mean(pre_stop_acs_int$departures, pre_stop_acs_int$pop_est_new, na.rm = FALSE)
pre_wmd #population weighted mean before = 45.50 departures/day
pre_wmh <- weighted.mean(pre_stop_acs_int$headway, pre_stop_acs_int$pop_est_new, na.rm = FALSE)
pre_wmh #population weighted mean headway before = 31.35 minutes of those within 400 m

post_wmd <- weighted.mean(post_stop_acs_int$departures, post_stop_acs_int$pop_est_new, na.rm = FALSE)
post_wmd #population weighted mean after = 49.13 departures/day
post_wmh <- weighted.mean(post_stop_acs_int$headway, post_stop_acs_int$pop_est_new, na.rm = FALSE)
post_wmh #population weighted mean headway before = 28.98 minutes of those within 400 m

