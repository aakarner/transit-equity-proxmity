library(dodgr)
library(sf)
library(tidycensus)
library(tidytransit)
library(tidyr)
library(viridis)
library(maptools)
library(matrixStats)
library(dplyr)

harriscty <- dodgr_streetnet("houston texas", quiet = FALSE)
graph <- weight_streetnet (harriscty, wt_profile = "foot")
nrow(graph)
###running the following line causes a fatal error that shuts the system down
#graph <- dodgr_contract_graph(graph)
#nrow(graph$graph)
saveRDS(graph, file = "harrisctytx.rds")

census_api_key("3a277651e6ec078927e10356269269b3698a0cfa")
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
pts_origin <- st_coordinates (st_centroid (harris_bg_wide))
#pts_origin <- dodgr::match_points_to_graph(dodgr_verticies(graph), pts_origin, connected = FALSE)

###______ONLY NEED TO DO THIS IF DID NOT DO BEFORE________________________________________________________________________###
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
pts_destination_preSR <- st_coordinates (get_stop_geometry (pre_sr_gtfs$stops))
post_sr_gtfs <- read_gtfs("data/20150818_htx.zip", 
                          local = TRUE,
                          geometry = TRUE,
                          frequency = TRUE)
pts_destination_postSR <- st_coordinates (get_stop_geometry (post_sr_gtfs$stops))

head (pts_origin); head (pts_destination_preSR); head (pts_destination_postSR)

from <- pts_origin
to_preSR <- pts_destination_preSR
to_postSR <- pts_destination_postSR
d_preSR <- dodgr_dists (graph, from = from, to = to_preSR)
#knitr::kable (d_preSR)
d_postSR <- dodgr_dists (graph, from = from, to = to_postSR)
#knitr::kable (d_postSR)
#write.csv(d_preSR, file = "coltest.csv")

pre_df <-as.data.frame(d_preSR, 1)
pre_df$min <- rowMins(d_preSR, na.rm = TRUE)
##drop all rows except for the minimum value column to make the file smaller and more manageable
pre_df <- pre_df[ -c(1:ncol(pre_df)-1)]

post_df <-as.data.frame(d_postSR, 1)
post_df$min <- rowMins(d_postSR, na.rm = TRUE)
##drop all rows except for the minimum value column to make the file smaller and more manageable
post_df <- post_df[ -c(1:ncol(post_df)-1)]

##join minimum walk distance values back to census block group ACS data
pre_df$rownum <- 1:nrow(pre_df)
harris_bg_wide$rownum <- 1:nrow(harris_bg_wide)
pre_df_join_acs <- inner_join(pre_df, harris_bg_wide, by = "rownum")
is.na(pre_df_join_acs$min) <- sapply(pre_df_join_acs$min, is.infinite)
#pre_df_join_acs <- inner_join(pre_df, harris_bg_wide, rownum)
write.csv(pre_df_join_acs, file = "coltest2.csv")
post_df$rownum <- 1:nrow(post_df)
post_df_join_acs <- dplyr::inner_join(post_df, harris_bg_wide, by = "rownum")
is.na(post_df_join_acs$min) <- sapply(post_df_join_acs$min, is.infinite)
#post_df_join_acs <- inner_join(post_df, harris_bg_wide, rownum)

#pre_sf_join_acs <- st_as_sf(pre_df_join_acs)
#post_sf_join_acs <- st_as_sf(post_df_join_acs)

##calculate population weighted mean walk distance to nearest transit stop by demographic group (before SR)
pre_wm_walkdist_white <- weighted.mean(pre_df_join_acs$min, pre_df_join_acs$B03002_003, na.rm = TRUE)
pre_wm_walkdist_black <- weighted.mean(pre_df_join_acs$min, pre_df_join_acs$B03002_004, na.rm = TRUE)
pre_wm_walkdist_asian <- weighted.mean(pre_df_join_acs$min, pre_df_join_acs$B03002_006, na.rm = TRUE)
pre_wm_walkdist_latin <- weighted.mean(pre_df_join_acs$min, pre_df_join_acs$B03002_012, na.rm = TRUE)
##calculate population weighted mean walk distance to nearest transit stop by demographic group (after SR)
post_wm_walkdist_white <- weighted.mean(post_df_join_acs$min, post_df_join_acs$B03002_003, na.rm = TRUE)
post_wm_walkdist_black <- weighted.mean(post_df_join_acs$min, post_df_join_acs$B03002_004, na.rm = TRUE)
post_wm_walkdist_asian <- weighted.mean(post_df_join_acs$min, post_df_join_acs$B03002_006, na.rm = TRUE)
post_wm_walkdist_latin <- weighted.mean(post_df_join_acs$min, post_df_join_acs$B03002_012, na.rm = TRUE)

##put into df to produce ggplot graph comparing the min. walkdist to bus stops by race before/after SR
data_wm_walkdist <-c(pre_wm_walkdist_white, post_wm_walkdist_white, pre_wm_walkdist_black, post_wm_walkdist_black, 
                     pre_wm_walkdist_asian, post_wm_walkdist_asian, pre_wm_walkdist_latin, post_wm_walkdist_latin)
before_after_walkdist <-c('Before', 'After', 'Before', 'After', 'Before', 'After', 'Before', 'After')
race_walkdist <-c('White', 'White', 'Black', 'Black', 'Asian', 'Asian', 'LatinX', 'LatinX')
df_wm_walkdist <-data.frame(race_walkdist, before_after_walkdist, data_wm_walkdist)
plot_wm_walkdist <- barplot(df_wm_walkdist$data_wm_walkdist, names.arg = df_wm_walkdist$race_walkdist, 
                                 xlab = "Race", 
                                 ylab = "Weighted Mean Walk Distance to Nearest Bus Stop (Meters?)", 
                                 col = c("red","green"), 
                                 main = "Weighted Average Walk Distance to Transit Race Before/After System Reimagining", 
                                 border = "black",
                                 ylim = c(0, 6))
legend("topright", c("Before", "After"), fill = c("red", "green"))
text(plot_wm_walkdist, df_wm_walkdist$data_wm_walkdist + 2*sign(df_wm_walkdist$data_wm_walkdist), 
     labels=round(df_wm_walkdist$data_wm_walkdist, 2), xpd=TRUE)
plot_wm_walkdist
df_wm_walkdist