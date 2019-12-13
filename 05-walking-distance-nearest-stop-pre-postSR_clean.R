library(dodgr)
library(sf)
library(tidycensus)
library(tidytransit)
library(tidyr)
library(viridis)
library(maptools)
library(matrixStats)
library(dplyr)
library(tictoc)
library(osmdata)
library(rgeos)

##start time counter
tic("1")

###Do not need to run the following code if you've already run 01-download-GTFS.R____###
## GTFS feeds obtained from transitfeeds.com
## The “before” dataset represents service from May 23, 2015 to August 15, 2015 
## and the “after” dataset represents service from August 16, 2015 to 
## January 23, 2016. 
## More information about System Reimagining is available here: 
## https://www.ridemetro.org/Pages/Reimagining.aspx

##sets destination points pre SR as the stops in service on 5/17/2015
#pre_sr_gtfs <- read_gtfs("data/20150517_htx.zip", 
#                         local = TRUE,
#                         geometry = TRUE,
#                         frequency = TRUE)

##sets destination points post SR as the stops in service on 8/18/2015
#post_sr_gtfs <- read_gtfs("data/20150818_htx.zip", 
#                          local = TRUE,
#                          geometry = TRUE,
#                          frequency = TRUE)

###__________________________________________________________________________________###

##pull lat/long coordinates from all transit stops in pre & post SR GTFS feeds
pts_destination_preSR <- st_coordinates (get_stop_geometry (pre_sr_gtfs$stops))
pts_destination_postSR <- st_coordinates (get_stop_geometry (post_sr_gtfs$stops))

##You must first run 01-CensusKey.R replacing ADD_YOUR_CENSUS_KEY_HERE with a census
##API key that can be obtained for free at http://api.census.gov/data/key_signup.html
census_api_key(Sys.getenv("CENSUS_KEY"))

##specificies tables to pull down from census API (population totals by demographic group)
hlstatusvars <- c("B03002_003", # white alone
                  "B03002_004", # black alone
                  "B03002_006", # Asian
                  "B03002_012") # Hispanic or Latino
##specifices geographies that cover the transit system service area
metro_svc_area <- c("Harris County",
                    "Fort Bend County")
metro_bg <- get_acs(geography = "block group", variables = hlstatusvars, 
                    state = "TX", year = 2015,
                    county = metro_svc_area,
                    geometry = TRUE,
                    summary_var = "B03002_001")
##defines bounding box for future generation of street network
metro_bg_bbox <- bbox(st_coordinates(metro_bg))
metro_bg_bbox

###WORK IN PROGRESS________________###
##limit origin bg ton only be those within 1/2 mile of a transit stop (see 03-proximity-equity
##analysis-system.R)

#pre_stop_buffer_t <- st_transform(get_stop_geometry(pre_sr_gtfs$stops), 32139)
#pre_stop_buffer_800m <- st_buffer(pre_stop_buffer_t, dist = 800)
#post_stop_buffer_t <- st_transform(get_stop_geometry(post_sr_gtfs$stops), 32139)
#post_stop_buffer_800m <- st_buffer(post_stop_buffer_t, dist = 800)
#metro_bg_32139 <-st_transform(metro_bg, 32139)
#metro_buf_int_pre <- st_intersects(st_union(pre_stop_buffer_800m), metro_bg_32139)
#metro_buf_int_post <- st_intersects(st_union(post_stop_buffer_800m), metro_bg_32139)
#metro_bg_preSR <- metro_buf_int_pre[pre_stop_buffer_800m[[1]],]
#metro_bg_postSR <- metro_buf_int_post[post_stop_buffer_800m[[1]],]
#plot(st_geometry(metro_bg_32139), border="#aaaaaa", 
#     main="Census tracts that fall within 800m of transit stop before SR")
#plot(st_geometry(metro_bg_preSR), add=T, col="red")
#plot(st_geometry(post_stop_buffer_800m), add=T, lwd = 2)
###WORK IN PROGRESS________________###


##reshape long dataset to wide dataset for future calcluations
metro_bg_wide <- spread(metro_bg, variable, estimate, fill = NA, convert = FALSE)
metro_bg_drop <- subset(metro_bg, select = c(GEOID, variable, estimate, summary_est))
metro_bg_wide <- spread(metro_bg_drop, variable, estimate, convert = FALSE)

pts_origin_preSR <- st_coordinates (st_centroid (metro_bg_wide))
pts_origin_postSR <- st_coordinates (st_centroid (metro_bg_wide))
##can use the following to speed up processing perhaps?
#pts_origin <- dodgr::match_points_to_graph(dodgr_verticies(graph), pts_origin, connected = FALSE)


##test coordinates of origins and destinations
head (pts_origin_preSR); head (pts_origin_postSR);
head (pts_destination_preSR); head (pts_destination_postSR)

##set to and from for input into street network walking distance program
from <- pts_origin
to_preSR <- pts_destination_preSR
to_postSR <- pts_destination_postSR

tic("2")

##generates street network for chosen geography and weights it to return roadways that can be used
##on foot, which basically includes all roadways aside from limited-access freeways/highways.
metro_streetnet <- dodgr_streetnet(metro_bg_bbox, quiet = FALSE)
graph <- weight_streetnet (metro_streetnet, wt_profile = "foot")
nrow(graph)

###following line returns fatal error due to bug, so have commented out...
##simplifies street network to eliminiate dupliciative line segments to improve processing speed
#graph <- dodgr_contract_graph(graph)
#nrow(graph$graph)
##save street network to disk to speed up future processing speeds
saveRDS(graph, file = "metro_streetnettx.rds")

toc("2") #529.12 seconds from tic("2")

tic("3") #runtime counter for dodgr_dists() functions

##generate matrix of walk distnaces (in meters) from all origins to all destinations before/after SR
d_preSR <- dodgr_dists (graph, from = from, to = to_preSR)
d_postSR <- dodgr_dists (graph, from = from, to = to_postSR)
toc("3") #approx. 6 minutes from tic("3")

##convert walk distance matricies into a dataframe to find the shortest distance from each census
##bg to the nearest transit stop before SR
pre_df <-as.data.frame(d_preSR, 1)
pre_df$min <- rowMins(d_preSR, na.rm = TRUE)
##drop all rows except for the minimum value column to make the file smaller and more manageable
pre_df <- pre_df[ -c(1:ncol(pre_df)-1)]

##convert walk distance matricies into a dataframe to find the shortest distance from each census
##bg to the nearest transit stop after SR
post_df <-as.data.frame(d_postSR, 1)
post_df$min <- rowMins(d_postSR, na.rm = TRUE)
##drop all rows except for the minimum value column to make the file smaller and more manageable
post_df <- post_df[ -c(1:ncol(post_df)-1)]

##join minimum walk distance values back to census block group ACS data
pre_df$rownum <- 1:nrow(pre_df)
metro_bg_wide$rownum <- 1:nrow(metro_bg_wide)
pre_df_join_acs <- inner_join(pre_df, metro_bg_wide, by = "rownum")
is.na(pre_df_join_acs$min) <- sapply(pre_df_join_acs$min, is.infinite)
#pre_df_join_acs <- inner_join(pre_df, metro_bg_wide, rownum)
post_df$rownum <- 1:nrow(post_df)
post_df_join_acs <- dplyr::inner_join(post_df, metro_bg_wide, by = "rownum")
is.na(post_df_join_acs$min) <- sapply(post_df_join_acs$min, is.infinite)
#post_df_join_acs <- inner_join(post_df, metro_bg_wide, rownum)

##calculate population weighted mean walk distance to nearest 
##transit stop by demographic group (before SR)
pre_wm_walkdist_white <- weighted.mean(pre_df_join_acs$min, 
                                       pre_df_join_acs$B03002_003, na.rm = TRUE)
pre_wm_walkdist_black <- weighted.mean(pre_df_join_acs$min, 
                                       pre_df_join_acs$B03002_004, na.rm = TRUE)
pre_wm_walkdist_asian <- weighted.mean(pre_df_join_acs$min, 
                                       pre_df_join_acs$B03002_006, na.rm = TRUE)
pre_wm_walkdist_latin <- weighted.mean(pre_df_join_acs$min, pre_df_join_acs$B03002_012, na.rm = TRUE)
##calculate population weighted mean walk distance to nearest 
##transit stop by demographic group (after SR)
post_wm_walkdist_white <- weighted.mean(post_df_join_acs$min, 
                                        post_df_join_acs$B03002_003, na.rm = TRUE)
post_wm_walkdist_black <- weighted.mean(post_df_join_acs$min, 
                                        post_df_join_acs$B03002_004, na.rm = TRUE)
post_wm_walkdist_asian <- weighted.mean(post_df_join_acs$min, 
                                        post_df_join_acs$B03002_006, na.rm = TRUE)
post_wm_walkdist_latin <- weighted.mean(post_df_join_acs$min, 
                                        post_df_join_acs$B03002_012, na.rm = TRUE)

##put into df to produce barplot graph comparing the min. walkdist to bus stops by 
##race before/after SR
data_wm_walkdist <-c(pre_wm_walkdist_white, post_wm_walkdist_white, 
                     pre_wm_walkdist_black, post_wm_walkdist_black, 
                     pre_wm_walkdist_asian, post_wm_walkdist_asian, 
                     pre_wm_walkdist_latin, post_wm_walkdist_latin)
before_after_walkdist <-c('Before', 'After', 'Before', 'After', 
                          'Before', 'After', 'Before', 'After')
race_walkdist <-c('White', 'White', 'Black', 'Black', 
                  'Asian', 'Asian', 'LatinX', 'LatinX')
df_wm_walkdist <-data.frame(race_walkdist, before_after_walkdist, data_wm_walkdist)
plot_wm_walkdist <- barplot(df_wm_walkdist$data_wm_walkdist, 
                            names.arg = df_wm_walkdist$race_walkdist, 
                            xlab = "Race", 
                            ylab = "Weighted Mean Walk Distance to Nearest Bus Stop (Meters)", 
                            col = c("red","green"), 
                            main = "Weighted Avg. Walk Dist. to Transit By Race Before/After SR (m)", 
                            border = "black",
                            ylim = c(0, 5400))
legend("topright", c("Before", "After"), fill = c("red", "green"))
text(plot_wm_walkdist, df_wm_walkdist$data_wm_walkdist + 2*sign(df_wm_walkdist$data_wm_walkdist), 
     labels=round(df_wm_walkdist$data_wm_walkdist, 2), xpd=TRUE)
plot_wm_walkdist
df_wm_walkdist

##end time counter
toc("1")
