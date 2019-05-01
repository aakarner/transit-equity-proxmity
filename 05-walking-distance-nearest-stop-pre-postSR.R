#[Walking Distnace to Nearest Bus Stop Calculations - ADVANCED METHODS]
#This code calculates the average walking distance to the nearest bus stop from the center of each 
#census block group before and after the SR and compares these metrics across demographic groups.
library(dodgr)
library(sf)
library(tidycensus)
library(tidyr)
library(viridis)
library(tidyverse)
library(maptools)
library(matrixStats)
census_api_key("3a277651e6ec078927e10356269269b3698a0cfa")

###_______________________________________________________________only need to use if don't do 01 & 03
library(tidytransit)

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

#merge stop frequency df with stop spatial locations ($stops_sf) & draw 400 meter buffer
pre_merged <- merge(pre_stop_freq$stops_sf, pre_stop_freq$stops_frequency_df, by = "stop_id")
pre_merged_t <- st_transform(pre_merged, 32139)
pre_stop_buffer <- st_buffer(pre_merged_t, dist = 400)
#mapview(pre_stop_buffer, add = "TRUE") #display pre stop buffers on leaflet
post_merged <- merge(post_stop_freq$stops_sf, post_stop_freq$stops_frequency_df, by = "stop_id")
post_merged_t <- st_transform(post_merged, 32139)
post_stop_buffer <- st_buffer(post_merged_t, dist = 400)

###______________________________________________________________________________________________________

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
graphtest_sf_nad83 <- st_transform(graphtest_sf, 32139) #linestring xy 32139 w/ 1159769 features and 13 fields


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
##reshape long dataset to wide dataset for future calcluations
harris_bg_wide <- spread(harris_bg, variable, estimate, fill = NA, convert = FALSE)
harris_bg_drop <- subset(harris_bg, select = c(GEOID, variable, estimate, summary_est))
harris_bg_wide <- spread(harris_bg_drop, variable, estimate, convert = FALSE)


#harris_tract <- get_acs(geography = "tract", variables = hlstatusvars, 
#                     state = "TX", year = 2015,
#                     county = "Harris County",
#                     geometry = TRUE,
#                     summary_var = "B03002_001") 
#harris_tract_drop <- subset(harris_tract, select = c(GEOID, variable, estimate, summary_est))
#harris_tract_wide <- spread(harris_tract_drop, variable, estimate, convert = FALSE)
#harris_tract_wide <- (SEE ABOVE)


##transform ACS data to centroid of block groups to create 'origins' for analysis
harris_bg_cent <- st_centroid(harris_bg_wide)
##transform bg centroids to wgs84 to be consistent crs with graph streetnet for dodgr calcs.
harris_bg_cent_wgs <- st_transform(harris_bg_cent, 4326)
harris_bg_cent_nad83 <- st_transform(harris_bg_cent, 32139)

##snap bg centroids to closest link on street network (Attempt #1)
#harris_bg_cent_wgs_snap <- snapPointsToLines(harris_bg_cent_wgs, graph_sf, maxDist = 400, withAttrs = TRUE)
###returns the following:[ Error in (function (classes, fdef, mtable)  : 
###unable to find an inherited method for function ‘is.projected’ for signature ‘"sf"’]

##convert street network to spatial for snapPointsToLines functions 
graph_sp <- as_Spatial(graphtest_sf)
graph_nad83_sp <- as_Spatial(graphtest_sf_nad83)
##converting centroids to spatial for snapPointsToLines function
harris_bg_cent_nad83_sp <- as_Spatial(harris_bg_cent_nad83)

#graph_sub_to <- subset(graphtest, select = c(to_lon, to_lat))
#graph_sub_from <- subset(graphtest, select = c(from_lon, from_lat))
#graph_fromto <- cbind(graph_sub_from, graph_sub_to)
#graph_matrix <- coordinates(graph_fromto)
#graph_sp <- sp::SpatialPoints(graph_sub_from)
#graph_sl <- SpatialLines(list(Lines(Line(graph_sp), ID = "a")))

##snap bg centroids to closest link on street network (Attempt #2)
harris_bg_cent_sp <- sp::SpatialPoints(st_coordinates(harris_bg_cent))
from_snap_sp <- maptools:::snapPointsToLines(harris_bg_cent_sp, graph_sl, maxDist = 0.1, 
                                             withAttrs = FALSE, idField = NA)
###something went wrong...

##snap bg centroids to closest link on street network (Attempt #3)
from_snap_sp <- maptools:::snapPointsToLines(harris_bg_cent_nad83_sp, graph_nad83_sp, maxDist = 200, 
                                                withAttrs = FALSE, idField = NA)
##snap pre-SR stops to closest link on street network
to_pre_sp <- as_Spatial(pre_merged_t)
to_pre_snap_sp <- maptools:::snapPointsToLines(to_pre_sp, graph_nad83_sp, maxDist = 200, 
                                               withAttrs = FALSE, idField = NA)
##snap post-SR stops to closest link on street network
to_post_sp <- as_Spatial(post_merged_t)
to_post_snap_sp <- maptools:::snapPointsToLines(to_post_sp, graph_nad83_sp, maxDist = 200, 
                                                withAttrs = FALSE, idField = NA)
from_snap_sp_sf <- st_as_sf(from_snap_sp)
from_snap_sp_crs <- st_coordinates(from_snap_sp_sf)
to_pre_snap_sp_sf <- st_as_sf(to_pre_snap_sp)
to_pre_snap_sp_crs <- st_coordinates(to_pre_snap_sp_sf)
to_post_snap_sp_sf <- st_as_sf(to_post_snap_sp)
to_post_snap_sp_crs <- st_coordinates(to_post_snap_sp_sf)

walkdist_preSR1 <- dodgr_dists(graphtest, from_snap_sp_crs, to_pre_snap_sp_crs, wt_profile = "foot", expand = 0, 
                               heap = "BHeap", parallel = TRUE, quiet = TRUE)
walkdist_postSR1 <- dodgr_dists(graphtest, from_snap_sp_crs, to_post_snap_sp_crs, wt_profile = "foot", expand = 0, 
                               heap = "BHeap", parallel = TRUE, quiet = TRUE)


plot(from_snap_sp)


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

##generating walking distnace matrix between from/to 
###WARNING...the following command takes XXX minutes)
walkdist_preSR <- dodgr_dists(graphtest, from_crs, to_pre_crs, wt_profile = "foot", expand = 0, 
                              heap = "BHeap", parallel = TRUE, quiet = TRUE)
#write.csv(walkdist_preSR, file = "walkdist_preSR_distmatrix.csv")
walkdist_postSR <- dodgr_dists(graphtest, from_crs, to_post_crs, wt_profile = "foot", expand = 0, 
                              heap = "BHeap", parallel = TRUE, quiet = TRUE)
#write.csv(walkdist_postSR, file = "walkdist_postSR_distmatrix.csv")
#write.csv(harris_bg_cent, file = "harris_bg_cent_ref.csv")
#write.csv(pre_merged, file = "preSR_stops.csv")
#write.csv(post_merged, file = "postSR_stops.csv")
#write.csv(harris_bg_cent_wgs_snap_t, file = "harris_bg_snapref.csv")
#write.csv(pre_merged_wgs_snap_t, file = "pre_SR_stops_snapref.csv")
#write.csv(post_merged_wgs_snap_t, file = "post_SR_stops_snapref.csv")

##now want to find minimum path on matrix between each origin to nearest destination by id number and 
##then re-join these metrics with the original demographic information associated with each block group

pre_df <-as.data.frame(walkdist_preSR, 1)
pre_df$min <- rowMins(walkdist_preSR, na.rm = TRUE)
##drop all rows except for the minimum value column to make the file smaller and more manageable
pre_df <- pre_df[ -c(1:11202)]

post_df <-as.data.frame(walkdist_postSR, 1)
post_df$min <- rowMins(walkdist_postSR, na.rm = TRUE)
##drop all rows except for the minimum value column to make the file smaller and more manageable
post_df <- post_df[ -c(1:9883)]
#write.csv(post_df, file="coltest.csv")

##join minimum walk distance values back to census block group ACS data
pre_df_join_acs <-inner_join(pre_df, harris_bg_wide, rownum)
pre_df_join_acs



##transform to wide dataset for demographic pwm calcs. (see https://uc-r.github.io/tidyr)
pre_df_join_acs_wide <- spread(pre_df_join_acs, variable, estimate, fill = NA, convert = FALSE)
wide <- reshape(pre_df_join_acs, v.names = "estimate", timevar = "variable", idvar = "estimate", direction = "wide")#select rows where pre_df_join_acs$variable = B03002_003
preSR_pwm_white <- weighted.mean(pre_df_join_acs_wide$min, pre_df_join_acs_wide$B03002_002, na.rm=TRUE)
write.csv(pre_df_join_acs_wide, file = "coltest.csv")
