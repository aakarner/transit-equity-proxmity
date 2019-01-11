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
pre_route_freq <- get_route_frequency(pre_sr_gtfs, start_hour = 6, end_hour = 22,
                                    dow = c(1,1,1,1,1,0,0))
post_route_freq <- get_route_frequency(post_sr_gtfs, start_hour = 6, end_hour = 22,
                                    dow = c(1,1,1,1,1,0,0))

#merge route frequency df with route spatial locations ($routes_sf) & draw 400 meter buffer
pre_merged_route1 <- merge(pre_route_freq$routes_sf, pre_route_freq$routes_df, by = intersect("route_id","route_id"))
pre_merged_route2 <- merge(pre_merged_route1, pre_route_freq$routes_frequency_df, by = "route_id")
pre_merged_route_t <- st_transform(pre_merged_route2, 32139)
pre_route_buffer <- st_buffer(pre_merged_route_t, dist = 400)
#mapview(pre_stop_buffer, add = "TRUE") #display pre stop buffers on leaflet
post_merged_route1 <- merge(post_route_freq$routes_sf, post_route_freq$routes_df, by = intersect("route_id","route_id"))
post_merged_route2 <- merge(post_merged_route1, post_route_freq$routes_frequency_df, by = "route_id")
post_merged_route_t <- st_transform(post_merged_route2, 32139)
post_route_buffer <- st_buffer(post_merged_route_t, dist = 400)
#mapview(post_route_buffer, add = "TRUE")

# Get demographic data from the ACS
hlstatusvars <- c("B03002_003", # white alone
                  "B03002_004", # black alone
                  "B03002_006", # Asian
                  "B03002_012") # Hispanic or Latino


#WHITE ONLY CALCULATIONS
harris_trct_white <- get_acs(geography = "tract", variables = hlstatusvars, 
                            state = "TX", year = 2015,
                            county = "Harris County",
                            geometry = TRUE,
                            summary_var = "B03002_003") 
#add population density column
harris_trct_white$pop_dens <- calc_densities(harris_trct_white, var = "summary_est")

#transform ACS data for proper geospatial references
harris_trct_white_t <- st_transform(harris_trct_white, 32139)
#intersect route buffer geometries with ACS demographic data
pre_route_acs_int_white <- st_intersection(pre_route_buffer, harris_trct_white_t)
post_route_acs_int_white <- st_intersection(post_route_buffer, harris_trct_white_t)
#add area attributes
pre_route_acs_int_white$area <- st_area(pre_route_acs_int_white)
post_route_acs_int_white$area <- st_area(post_route_acs_int_white)
#get new total population estimate value for new area by adding column
pre_route_acs_int_white$pop_est_new <- pre_route_acs_int_white$area*pre_route_acs_int_white$pop_dens
post_route_acs_int_white$pop_est_new <- post_route_acs_int_white$area*post_route_acs_int_white$pop_dens
pre_route_acs_int_white$
#Population weighted mean metrics for entire system/population
pre_wmh_white <- weighted.mean(pre_route_acs_int_white$mean_headways, pre_route_acs_int_white$pop_est_new, na.rm = FALSE)
post_wmh_white <- weighted.mean(post_route_acs_int_white$mean_headways, post_route_acs_int_white$pop_est_new, na.rm = FALSE)

#BLACK ONLY CALCULATIONS
harris_trct_black <- get_acs(geography = "tract", variables = hlstatusvars, 
                             state = "TX", year = 2015,
                             county = "Harris County",
                             geometry = TRUE,
                             summary_var = "B03002_004") 
#add population density column
harris_trct_black$pop_dens <- calc_densities(harris_trct_black, var = "summary_est")

#transform ACS data for proper geospatial references
harris_trct_black_t <- st_transform(harris_trct_black, 32139)
#intersect route buffer geometries with ACS demographic data
pre_route_acs_int_black <- st_intersection(pre_route_buffer, harris_trct_black_t)
post_route_acs_int_black <- st_intersection(post_route_buffer, harris_trct_black_t)
#add area attributes
pre_route_acs_int_black$area <- st_area(pre_route_acs_int_black)
post_route_acs_int_black$area <- st_area(post_route_acs_int_black)
#get new total population estimate value for new area by adding column
pre_route_acs_int_black$pop_est_new <- pre_route_acs_int_black$area*pre_route_acs_int_black$pop_dens
post_route_acs_int_black$pop_est_new <- post_route_acs_int_black$area*post_route_acs_int_black$pop_dens

#Population weighted mean metrics for entire system/population
pre_wmh_black <- weighted.mean(pre_route_acs_int_black$mean_headways, pre_route_acs_int_black$pop_est_new, na.rm = FALSE)
post_wmh_black <- weighted.mean(post_route_acs_int_black$mean_headways, post_route_acs_int_black$pop_est_new, na.rm = FALSE)


#ASIAN ONLY CALCULATIONS
harris_trct_asian <- get_acs(geography = "tract", variables = hlstatusvars, 
                             state = "TX", year = 2015,
                             county = "Harris County",
                             geometry = TRUE,
                             summary_var = "B03002_006") 
#add population density column
harris_trct_asian$pop_dens <- calc_densities(harris_trct_asian, var = "summary_est")

#transform ACS data for proper geospatial references
harris_trct_asian_t <- st_transform(harris_trct_asian, 32139)
#intersect route buffer geometries with ACS demographic data
pre_route_acs_int_asian <- st_intersection(pre_route_buffer, harris_trct_asian_t)
post_route_acs_int_asian <- st_intersection(post_route_buffer, harris_trct_asian_t)
#add area attributes
pre_route_acs_int_asian$area <- st_area(pre_route_acs_int_asian)
post_route_acs_int_asian$area <- st_area(post_route_acs_int_asian)
#get new total population estimate value for new area by adding column
pre_route_acs_int_asian$pop_est_new <- pre_route_acs_int_asian$area*pre_route_acs_int_asian$pop_dens
post_route_acs_int_asian$pop_est_new <- post_route_acs_int_asian$area*post_route_acs_int_asian$pop_dens

#Population weighted mean metrics for entire system/population
pre_wmh_asian <- weighted.mean(pre_route_acs_int_asian$mean_headways, pre_route_acs_int_asian$pop_est_new, na.rm = FALSE)
post_wmh_asian <- weighted.mean(post_route_acs_int_asian$mean_headways, post_route_acs_int_asian$pop_est_new, na.rm = FALSE)


#HISPANIC/LATINO ONLY CALCULATIONS
harris_trct_latin <- get_acs(geography = "tract", variables = hlstatusvars, 
                             state = "TX", year = 2015,
                             county = "Harris County",
                             geometry = TRUE,
                             summary_var = "B03002_012") 
#add population density column
harris_trct_latin$pop_dens <- calc_densities(harris_trct_latin, var = "summary_est")

#transform ACS data for proper geospatial references
harris_trct_latin_t <- st_transform(harris_trct_latin, 32139)
#intersect route buffer geometries with ACS demographic data
pre_route_acs_int_latin <- st_intersection(pre_route_buffer, harris_trct_latin_t)
post_route_acs_int_latin <- st_intersection(post_route_buffer, harris_trct_latin_t)
#add area attributes
pre_route_acs_int_latin$area <- st_area(pre_route_acs_int_latin)
post_route_acs_int_latin$area <- st_area(post_route_acs_int_latin)
#get new total population estimate value for new area by adding column
pre_route_acs_int_latin$pop_est_new <- pre_route_acs_int_latin$area*pre_route_acs_int_latin$pop_dens
post_route_acs_int_latin$pop_est_new <- post_route_acs_int_latin$area*post_route_acs_int_latin$pop_dens

#Population weighted mean metrics for entire system/population
pre_wmh_latin <- weighted.mean(pre_route_acs_int_latin$mean_headways, pre_route_acs_int_latin$pop_est_new, na.rm = FALSE)
post_wmh_latin <- weighted.mean(post_route_acs_int_latin$mean_headways, post_route_acs_int_latin$pop_est_new, na.rm = FALSE)

#plot results in bar chart
data_wmh <-c(pre_wmh_white, post_wmh_white,pre_wmh_black, post_wmh_black, 
              pre_wmh_asian, post_wmh_asian, pre_wmh_latin, post_wmh_latin)
data_wmd <-c(pre_wmd_white, post_wmd_white,pre_wmd_black, post_wmd_black, 
             pre_wmd_asian, post_wmd_asian, pre_wmd_latin, post_wmd_latin)
before_after <-c('Before', 'After', 'Before', 'After', 'Before', 'After', 'Before', 'After')
race <-c('White', 'White', 'Black', 'Black', 'Asian', 'Asian', 'LatinX', 'LatinX')

final.data_byroute <-data.frame(race, before_after, data_wmh, data_wmd)

plot_wmh <- barplot(final.data_byroute$data_wmh, names.arg = final.data_byroute$race, xlab = "Race", 
        ylab = "Weighted Mean Headway (Minutes)", 
        col = c("red","green"), 
        main = "Weighted Average Transit Headway By Race Before/After System Reimagining (By Stop)", 
        border = "black",
        ylim = c(0, 90))
legend("topright", c("Before", "After"), fill = c("red", "green"))
text(plot_wmh, final.data_byroute$data_wmh + 2*sign(final.data_byroute$data_wmh), labels=round(final.data_byroute$data_wmh, 2), xpd=TRUE)

#plot_wmd <- barplot(final.data_byroute$data_wmd, names.arg = final.data_byroute$race, xlab = "Race", 
                    #ylab = "Weighted Mean Daily Departures", 
                    #col = c("orange","blue"), 
                    #main = "Weighted Average Transit Departures By Race Before/After System Reimagining (By Route)", 
                    #border = "black",
                    #ylim = c(0, 60))
#legend("topright", c("Before", "After"), fill = c("orange", "blue"))
#text(plot_wmd, final.data_byroute$data_wmd + 2*sign(final.data_byroute$data_wmd), labels=round(final.data_byroute$data_wmd, 2), xpd=TRUE)

