#[Walking Distnace to Nearest Bus Stop Calculations - ADVANCED METHODS]
#This code calculates the average walking distance to the nearest bus stop from the center of each 
#census block group before and after the SR and compares these metrics across demographic groups.

library(dodgr)
library(sf)
library(tidycensus)
library(viridis)
library(tidyverse)
library(maptools)
census_api_key("3a277651e6ec078927e10356269269b3698a0cfa")

#pull down entire street network from open street map using dodgr package (WARNING: Takes a few minutes)
harriscty <- dodgr_streetnet("harris county texas")
#weight the street network for walking to prevent routes from allowing walk paths along limited-access freeways
graph <- weight_streetnet(harriscty, wt_profile = "foot", type_col = "highway",
                          id_col = "osm_id", keep_cols = NULL)

#convert dodgr_streetnet file to sf.tibble (WARNING: Takes a long time)
graph_sf <- dodgr_to_sf(graph) #linestring xy WGS84-4326 w/ 1159768 features and 13 fields
#convert to crs (NAD83 / Texas Central with UOM = meters for later processing steps)
graph_sf_t <- st_transform(graph_sf, 32139) #linestring xy 32139 w/ 1159769 features and 13 fields

#Get Block Group data from ACS for folllwing demographic groups (summary variable is total pop):
hlstatusvars <- c("B03002_003", # white alone
                  "B03002_004", # black alone
                  "B03002_006", # Asian
                  "B03002_012") # Hispanic or Latino
harris_bg <- get_acs(geography = "block group", variables = hlstatusvars, 
                        state = "TX", year = 2015,
                        county = "Harris County",
                        geometry = TRUE,
                        summary_var = "B03002_001") 

#harris_tract <- get_acs(geography = "tract", variables = hlstatusvars, 
#                     state = "TX", year = 2015,
#                     county = "Harris County",
#                     geometry = TRUE,
#                     summary_var = "B03002_001") 
#harris_tract

#transform ACS data to centroid of block groups to create 'origins' for analysis
harris_bg_cent <- st_centroid(harris_bg)
#transform bg centroids to wgs84 to be consistent crs with graph streetnet for dodgr calcs.
harris_bg_cent_wgs <- st_transform(harris_bg_cent, 4326)
harris_bg_cent_nad83 <- st_transform(harris_bg_cent, 32139)

#snap bg centroids to closest link on street network
harris_bg_cent_wgs_snap <- snapPointsToLines(harris_bg_cent_wgs, graph_sf, maxDist = 400, withAttrs = TRUE)
###returns the following:[ Error in (function (classes, fdef, mtable)  : 
###unable to find an inherited method for function ‘is.projected’ for signature ‘"sf"’]

#snap bg centroids to closest node on street network (backup since above didn't work)
#(WARNING... the following command takes more than 2 DAYS!...)
harris_bg_cent_wgs_snap <- st_snap(harris_bg_cent_nad83, graph_sf_t, tolerance = 400)
#previous is actually in nad83 (meters). Following command converts back to wgs84 lat long
harris_bg_cent_wgs_snap_t <- st_transform(harris_bg_cent_wgs_snap, 4326)
#fromsnap <- st_snap(harris_tract_c_t, graph_sf_t, tolerance = 99999999)
#fromsnap_t <- st_transform(fromsnap, 4269)
#fromsnap_t
#nrow(graph)


#harris_tract_t <- st_transform(harris_tract, 4269)
#pre_stops_t <- st_transform(pre_merged , 4269)
#post_stops_t <- st_transform(post_merged , 4269)
#graph
#harris_tract_c
#harris_tract_c <- st_centroid(harris_tract_t)

#setting to (bus stops) and from (bg centroids) snapped to street network (graph) for the dodgr path matrix
#testing from and to as same set of points first
from <- harris_bg_cent_wgs_snap_t
to_pre <- harris_bg_cent_wgs_snap_t
to_post <- harris_bg_cent_wgs_snap_t

#from$latlon <- from$geometry
#to_pre$latlon <- to_pre$geometry
#to_post$latlon <- to_post$geometry
#from_sc <- fromsnap_t %>%
#  mutate(fromx = unlist(map(fromsnap_t$geometry, 1)),
#         fromy = unlist(map(fromsnap_t$geometry, 2)))
#to_pre_sc <- to_pre %>%
#  mutate(fromx = unlist(map(to_pre$geometry, 1)),
#         fromy = unlist(map(to_pre$geometry, 2)))
#from <- from_sc
#to <- to_pre_sc
#from_sampletest <- sample(graph$from_id, size = 100)
#to_sampletest <- sample(graph$to_id, size = 50)

walkdist_preSR <- dodgr_dists(graph, from, to_pre, wt_profile = "foot", expand = 0, 
                              heap = "BHeap", parallel = TRUE, quiet = TRUE)

walkdist_postSR <- dodgr_dists(graph, from, to_post, wt_profile = "foot", expand = 0, 
                              heap = "BHeap", parallel = TRUE, quiet = TRUE)
walkdist_preSR


