# This script downloads GTFS files representing public transit service provided 
# by Houston METRO both before and after their major bus system redesign 
# (often referred to as the "System Reimagining").

library(tidytransit)
library(dplyr)
library(sf)

# GTFS feeds obtained from transitfeeds.com
# The before dataset represents service from the Spring 2015 service period 
# (weekday) - 1/25/15 through 6/6/15 - and the after dataset represents 
# service from the Spring 2016 service period (weekday) - 1/24/16 thru 5/28/16.
# The actual system service change took place on August 16, 2015.
# More information about System Reimagining is available here: 
# https://www.ridemetro.org/Pages/Reimagining.aspx

# Houston Metro's feed is well behaved. One service ID covers regular
# weekday service, so there's no need to go through the detailed procedures
# outlined here: http://tidytransit.r-transit.org/articles/servicepatterns.html

processGTFS <- function(feed, serviceIDs) {
# Read in GTFS feed, add geometry and route frequency information
# Calculate 400m buffer around bus stops (route_type = 3) and 
# 800m buffer around rail stops/stations (route_type = 0) using a given
# service_id
  
  this_feed <- 
    read_gtfs(feed) %>%
    gtfs_as_sf(.)

  freq <- get_route_frequency(this_feed, 
                              service_ids = serviceIDs, 
                              start_hour = 6, 
                              end_hour = 10) 

  # Join geometry, frequency, and other GTFS information into a single
  # sf dataframe that also contains route-level buffers
  routes <- 
    get_route_geometry(this_feed, service_ids = serviceIDs) %>%
    inner_join(this_feed$routes) %>%
    inner_join(freq) %>%
    # filter(route_type == 3) %>%
    st_transform(32139) %>%
    st_simplify(.) %>%
    # st_polygonize(.) %>%
    st_buffer(., ifelse(.$route_type == 3, 400, 800))
}

gtfs <- rbind(
  mutate(processGTFS("data/20150508_htx.zip", 6), when = "before"),
  mutate(processGTFS("data/20150818_htx.zip", 6), when = "after"))
# post_gtfs <- processGTFS("data/20160321_htx.zip") # Alternate post date