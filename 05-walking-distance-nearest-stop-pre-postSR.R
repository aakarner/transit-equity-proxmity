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

##pull down entire street network from open street map using dodgr package (WARNING: Takes a few minutes)
#harriscty <- dodgr_streetnet("harris county texas")
pasadenatest <-dodgr_streetnet("pasadena texas")
##weight the street network for walking to prevent routes from allowing walk paths along limited-access freeways
#graph <- weight_streetnet(harriscty, wt_profile = "foot", type_col = "highway",
#                          id_col = "osm_id", keep_cols = NULL)
graphtest <- weight_streetnet(pasadenatest, wt_profile = "foot", type_col = "highway",
                          id_col = "osm_id", keep_cols = NULL)

##convert dodgr_streetnet file to sf.tibble (WARNING: Takes a long time)
#graph_sf <- dodgr_to_sf(graph) #linestring xy WGS84-4326 w/ 1159768 features and 13 fields
graphtest_sf <- dodgr_to_sf(graphtest) #linestring xy WGS84-4326 w/ 1159768 features and 13 fields

##convert to crs (NAD83 / Texas Central with UOM = meters for later processing steps)
#graph_sf_t <- st_transform(graph_sf, 32139) #linestring xy 32139 w/ 1159769 features and 13 fields
graphtest_sf_t <- st_transform(graphtest_sf, 32139) #linestring xy 32139 w/ 1159769 features and 13 fields


##Get Block Group data from ACS for folllwing demographic groups (summary variable is total pop):
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

##transform ACS data to centroid of block groups to create 'origins' for analysis
harris_bg_cent <- st_centroid(harris_bg)
##transform bg centroids to wgs84 to be consistent crs with graph streetnet for dodgr calcs.
harris_bg_cent_wgs <- st_transform(harris_bg_cent, 4326)
harris_bg_cent_nad83 <- st_transform(harris_bg_cent, 32139)
##snap bg centroids to closest link on street network
harris_bg_cent_wgs_snap <- snapPointsToLines(harris_bg_cent_wgs, graph_sf, maxDist = 400, withAttrs = TRUE)
###returns the following:[ Error in (function (classes, fdef, mtable)  : 
###unable to find an inherited method for function ‘is.projected’ for signature ‘"sf"’]

##snap bg centroids to closest node on street network (backup since above didn't work)
##(WARNING... the following command takes more than 2 DAYS!...)
##(WARNING...test function takes approximately 45 mins)
harris_bg_cent_wgs_snap <- st_snap(harris_bg_cent_nad83, graphtest_sf_t, tolerance = 400)
pre_merged_wgs_snap <- st_snap(pre_merged_t, graphtest_sf_t, tolerance = 400)
post_merged_wgs_snap <- st_snap(post_merged_t, graphtest_sf_t, tolerance = 400)
##previous is actually in nad83 (meters). Following command converts back to wgs84 lat long
harris_bg_cent_wgs_snap_t <- st_transform(harris_bg_cent_wgs_snap, 4326)
pre_merged_wgs_snap_t <- st_transform(pre_merged_wgs_snap, 4326)
post_merged_wgs_snap_t <- st_transform(post_merged_wgs_snap, 4326)

##setting to (bus stops) and from (bg centroids) snapped to street network (graph) for the dodgr path matrix
from <- harris_bg_cent_wgs_snap_t
to_pre <- pre_merged_wgs_snap_t
to_post <- post_merged_wgs_snap_t
##converting to xy vector to pass through dodgr_dists function
from_crs <- st_coordinates(from)
to_pre_crs <- st_coordinates(to_pre)
to_post_crs <- st_coordinates(to_post)

##generating walking distnace matrix between from/to (note this is a test because from and to are same)
##WARNING...the following command takes XXX minutes)
walkdist_preSR <- dodgr_dists(graphtest, from_crs, to_pre_crs, wt_profile = "foot", expand = 0, 
                              heap = "BHeap", parallel = TRUE, quiet = TRUE)
write.csv(walkdist_preSR, file = "walkdist_preSR_distmatrix.csv")
walkdist_postSR <- dodgr_dists(graph, from_crs, to_post_crs, wt_profile = "foot", expand = 0, 
                              heap = "BHeap", parallel = TRUE, quiet = TRUE)
write.csv(walkdist_postSR, file = "walkdist_postSR_distmatrix.csv")

write.csv(harris_bg_cent, file = "harris_bg_cent_ref.csv")
write.csv(pre_merged, file "preSR_stops.csv")
write.csv(post_merged, file "postSR_stops.csv")
write.csv(harris_bg_cent_wgs_snap_t, file = "harris_bg_snapref.csv")
write.csv(pre_merged_wgs_snap_t, file = "pre_SR_stops_snapref.csv")
write.csv(post_merged_wgs_snap_t, file = "post_SR_stops_snapref.csv")
