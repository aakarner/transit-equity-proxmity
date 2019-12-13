library(tidytransit)
library(mapview)
library(tidycensus)
library(sf)
library(ggplot2)
library(ggthemes)
library(ggmap)
library(maps)
library(mapdata)
library(tmaptools)

###Before running, you must to run the 01-CensusKey.R replacing ADD_YOUR_CENSUS_KEY_HERE 
###with a census API key that can be obtained for free at http://api.census.gov/data/key_signup.html
census_api_key(Sys.getenv("CENSUS_KEY"))

###Do not need to run the following code if you've already run 01-download-GTFS.R____###
#pre_sr_gtfs <- read_gtfs("data/20150517_htx.zip", 
#                         local = TRUE,
#                         geometry = TRUE,
#                         frequency = TRUE)
#post_sr_gtfs <- read_gtfs("data/20150818_htx.zip", 
#                          local = TRUE,
#                          geometry = TRUE,
#                          frequency = TRUE)

###__________________________________________________________________________________###

#standardize frequency for weekdays b/w 6am and 10pm before/after change
pre_stop_freq <- get_stop_frequency(pre_sr_gtfs, start_hour = 6, end_hour = 22,
                                    dow = c(1,1,1,1,1,0,0), by_route=FALSE)
post_stop_freq <- get_stop_frequency(post_sr_gtfs, start_hour = 6, end_hour = 22,
                                    dow = c(1,1,1,1,1,0,0), by_route=FALSE)

#merge stop frequency df with stop spatial locations ($stops_sf) & draw 400 meter buffer
pre_merged <- merge(pre_stop_freq$.$stops_sf, pre_stop_freq$.$stops_frequency, by = "stop_id")
pre_merged_t <- st_transform(pre_merged, 32139)
pre_stop_buffer <- st_buffer(pre_merged_t, dist = 400)
#mapview(pre_stop_buffer, add = "TRUE") #display pre stop buffers on leaflet
post_merged <- merge(post_stop_freq$.$stops_sf, post_stop_freq$.$stops_frequency, by = "stop_id")
post_merged_t <- st_transform(post_merged, 32139)
post_stop_buffer <- st_buffer(post_merged_t, dist = 400)

#mapview(post_stop_buffer, add = "TRUE")

# Get demographic data from the ACS
hlstatusvars <- c("B03002_003", # white alone
                  "B03002_004", # black alone
                  "B03002_006", # Asian
                  "B03002_012") # Hispanic or Latino
##specifices geographies that cover the transit system service area
metro_svc_area <- c("Harris County",
                    "Fort Bend County")

#white ONLY CALCULATIONS
harris_trct_white <- get_acs(geography = "tract", variables = "B03002_003", 
                            state = "TX", year = 2015,
                            county = metro_svc_area,
                            geometry = TRUE,
                            summary_var = "B03002_003") 

#add population density column
harris_trct_white$pop_dens <- calc_densities(harris_trct_white, var = "summary_est")
harris_trct_white
#transform ACS data for proper geospatial references
harris_trct_white_t <- st_transform(harris_trct_white, 32139)
#intersect stop buffer geometries with ACS demographic data
pre_stop_acs_int_white <- st_intersection(pre_stop_buffer, harris_trct_white_t)
post_stop_acs_int_white <- st_intersection(post_stop_buffer, harris_trct_white_t)

#add area attributes
pre_stop_acs_int_white$area <- st_area(pre_stop_acs_int_white)
pre_stop_acs_int_white$areakm2 <- pre_stop_acs_int_white$area/1000000
post_stop_acs_int_white$area <- st_area(post_stop_acs_int_white)
post_stop_acs_int_white$areakm2 <- post_stop_acs_int_white$area/1000000
#get new total population estimate value for new area by adding column
pre_stop_acs_int_white$pop_est_new <- pre_stop_acs_int_white$area*pre_stop_acs_int_white$pop_dens
post_stop_acs_int_white$pop_est_new <- post_stop_acs_int_white$area*post_stop_acs_int_white$pop_dens

#BLACK ONLY CALCULATIONS
harris_trct_black <- get_acs(geography = "tract", variables = "B03002_004", 
                             state = "TX", year = 2015,
                             county = "Harris County",
                             geometry = TRUE,
                             summary_var = "B03002_004") 
#add population density column
harris_trct_black$pop_dens <- calc_densities(harris_trct_black, var = "summary_est")

#transform ACS data for proper geospatial references
harris_trct_black_t <- st_transform(harris_trct_black, 32139)
#intersect stop buffer geometries with ACS demographic data
pre_stop_acs_int_black <- st_intersection(pre_stop_buffer, harris_trct_black_t)
post_stop_acs_int_black <- st_intersection(post_stop_buffer, harris_trct_black_t)
#add area attributes
pre_stop_acs_int_black$area <- st_area(pre_stop_acs_int_black)
pre_stop_acs_int_black$areakm2 <- pre_stop_acs_int_black$area/1000000
post_stop_acs_int_black$area <- st_area(post_stop_acs_int_black)
post_stop_acs_int_black$areakm2 <- post_stop_acs_int_black$area/1000000
#get new total population estimate value for new area by adding column
pre_stop_acs_int_black$pop_est_new <- pre_stop_acs_int_black$area*pre_stop_acs_int_black$pop_dens
post_stop_acs_int_black$pop_est_new <- post_stop_acs_int_black$area*post_stop_acs_int_black$pop_dens

#ASIAN ONLY CALCULATIONS
harris_trct_asian <- get_acs(geography = "tract", variables = "B03002_006", 
                             state = "TX", year = 2015,
                             county = "Harris County",
                             geometry = TRUE,
                             summary_var = "B03002_006") 
#add population density column
harris_trct_asian$pop_dens <- calc_densities(harris_trct_asian, var = "summary_est")

#transform ACS data for proper geospatial references
harris_trct_asian_t <- st_transform(harris_trct_asian, 32139)
#intersect stop buffer geometries with ACS demographic data
pre_stop_acs_int_asian <- st_intersection(pre_stop_buffer, harris_trct_asian_t)
post_stop_acs_int_asian <- st_intersection(post_stop_buffer, harris_trct_asian_t)
#add area attributes
pre_stop_acs_int_asian$area <- st_area(pre_stop_acs_int_asian)
pre_stop_acs_int_asian$areakm2 <- pre_stop_acs_int_asian$area/1000000
post_stop_acs_int_asian$area <- st_area(post_stop_acs_int_asian)
post_stop_acs_int_asian$areakm2 <- post_stop_acs_int_asian$area/1000000
#get new total population estimate value for new area by adding column
pre_stop_acs_int_asian$pop_est_new <- pre_stop_acs_int_asian$area*pre_stop_acs_int_asian$pop_dens
post_stop_acs_int_asian$pop_est_new <- post_stop_acs_int_asian$area*post_stop_acs_int_asian$pop_dens

#HISPANIC/LATINX ONLY CALCULATIONS
harris_trct_latin <- get_acs(geography = "tract", variables = "B03002_012", 
                             state = "TX", year = 2015,
                             county = "Harris County",
                             geometry = TRUE,
                             summary_var = "B03002_012") 
#add population density column
harris_trct_latin$pop_dens <- calc_densities(harris_trct_latin, var = "summary_est")

#transform ACS data for proper geospatial references
harris_trct_latin_t <- st_transform(harris_trct_latin, 32139)
#intersect stop buffer geometries with ACS demographic data
pre_stop_acs_int_latin <- st_intersection(pre_stop_buffer, harris_trct_latin_t)
post_stop_acs_int_latin <- st_intersection(post_stop_buffer, harris_trct_latin_t)
#add area attributes
pre_stop_acs_int_latin$area <- st_area(pre_stop_acs_int_latin)
pre_stop_acs_int_latin$areakm2 <- pre_stop_acs_int_latin$area/1000000
post_stop_acs_int_latin$area <- st_area(post_stop_acs_int_latin)
post_stop_acs_int_latin$areakm2 <- post_stop_acs_int_latin$area/1000000
#get new total population estimate value for new area by adding column
pre_stop_acs_int_latin$pop_est_new <- pre_stop_acs_int_latin$area*pre_stop_acs_int_latin$pop_dens
post_stop_acs_int_latin$pop_est_new <- post_stop_acs_int_latin$area*post_stop_acs_int_latin$pop_dens

#SUM POPULATIONS WITH ANY TRANSIT ACCESS BY RACE BEFORE/AFTER

stop_buff_all_pre <- st_union(pre_stop_buffer)
stop_buff_all_post <- st_union(post_stop_buffer)

#white ONLY CALCULATIONS
#intersect stop buffer geometries with ACS demographic data
harris_trct_white_t$pop_dens <- calc_densities(harris_trct_white_t, var = "summary_est")
pre_allstop_acs_int_white <- st_intersection(harris_trct_white_t, stop_buff_all_pre)
post_allstop_acs_int_white <- st_intersection(harris_trct_white_t, stop_buff_all_post)
#add area attributes
pre_allstop_acs_int_white$area <- st_area(pre_allstop_acs_int_white)
post_allstop_acs_int_white$area <- st_area(post_allstop_acs_int_white)
#get new total population estimate value for new area by adding column
pre_allstop_acs_int_white$pop_est_new <- pre_allstop_acs_int_white$area*pre_allstop_acs_int_white$pop_dens
post_allstop_acs_int_white$pop_est_new <- post_allstop_acs_int_white$area*post_allstop_acs_int_white$pop_dens

#black ONLY CALCULATIONS
#intersect stop buffer geometries with ACS demographic data
harris_trct_black_t$pop_dens <- calc_densities(harris_trct_black_t, var = "summary_est")
pre_allstop_acs_int_black <- st_intersection(harris_trct_black_t, stop_buff_all_pre)
post_allstop_acs_int_black <- st_intersection(harris_trct_black_t, stop_buff_all_post)
#add area attributes
pre_allstop_acs_int_black$area <- st_area(pre_allstop_acs_int_black)
post_allstop_acs_int_black$area <- st_area(post_allstop_acs_int_black)
#get new total population estimate value for new area by adding column
pre_allstop_acs_int_black$pop_est_new <- pre_allstop_acs_int_black$area*pre_allstop_acs_int_black$pop_dens
post_allstop_acs_int_black$pop_est_new <- post_allstop_acs_int_black$area*post_allstop_acs_int_black$pop_dens

#asian ONLY CALCULATIONS
#intersect stop buffer geometries with ACS demographic data
harris_trct_asian_t$pop_dens <- calc_densities(harris_trct_asian_t, var = "summary_est")
pre_allstop_acs_int_asian <- st_intersection(harris_trct_asian_t, stop_buff_all_pre)
post_allstop_acs_int_asian <- st_intersection(harris_trct_asian_t, stop_buff_all_post)
#add area attributes
pre_allstop_acs_int_asian$area <- st_area(pre_allstop_acs_int_asian)
post_allstop_acs_int_asian$area <- st_area(post_allstop_acs_int_asian)
#get new total population estimate value for new area by adding column
pre_allstop_acs_int_asian$pop_est_new <- pre_allstop_acs_int_asian$area*pre_allstop_acs_int_asian$pop_dens
post_allstop_acs_int_asian$pop_est_new <- post_allstop_acs_int_asian$area*post_allstop_acs_int_asian$pop_dens

#latin ONLY CALCULATIONS
#intersect stop buffer geometries with ACS demographic data
harris_trct_latin_t$pop_dens <- calc_densities(harris_trct_latin_t, var = "summary_est")
pre_allstop_acs_int_latin <- st_intersection(harris_trct_latin_t, stop_buff_all_pre)
post_allstop_acs_int_latin <- st_intersection(harris_trct_latin_t, stop_buff_all_post)
#add area attributes
pre_allstop_acs_int_latin$area <- st_area(pre_allstop_acs_int_latin)
post_allstop_acs_int_latin$area <- st_area(post_allstop_acs_int_latin)
#get new total population estimate value for new area by adding column
pre_allstop_acs_int_latin$pop_est_new <- pre_allstop_acs_int_latin$area*pre_allstop_acs_int_latin$pop_dens
post_allstop_acs_int_latin$pop_est_new <- post_allstop_acs_int_latin$area*post_allstop_acs_int_latin$pop_dens

#Put population sum data in dataframe
pop_est_by_race <-c(sum(pre_allstop_acs_int_white$pop_est_new), sum(post_allstop_acs_int_white$pop_est_new), 
                    sum(pre_allstop_acs_int_black$pop_est_new), sum(post_allstop_acs_int_black$pop_est_new), 
                    sum(pre_allstop_acs_int_asian$pop_est_new), sum(post_allstop_acs_int_asian$pop_est_new), 
                    sum(pre_allstop_acs_int_latin$pop_est_new), sum(post_allstop_acs_int_latin$pop_est_new))
pop_numeric <- as.numeric(pop_est_by_race)
before_after_1 <-c('Before', 'After', 'Before', 'After', 'Before', 'After', 'Before', 'After')
race_1 <-c('White', 'White', 'Black', 'Black', 'Asian', 'Asian', 'LatinX', 'LatinX')

final.data_popsums <-data.frame(race_1, before_after_1, pop_est_by_race, pop_numeric)

final.data_popsums

#plot population sum data

barplot(final.data_popsums$pop_numeric, 
        names.arg = final.data_popsums$race_1, 
        xlab = "Race", 
        ylab = "Total Population within 400m of Bus Stop", 
        col = c("yellow","pink"), 
        main = "Population By Race w/in 400m of Bus Stop Before/After System Reimagining", 
        border = "black",
        ylim = c(0,750000))
legend("topleft", c("Before", "After"), fill = c("yellow", "pink"))

#END OF SIMPLE SYSTEM-WIDE ACCESSIBILITY PROXIMITY ANALYSIS

