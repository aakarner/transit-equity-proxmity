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
pre_stop_freq <- get_stop_frequency(pre_sr_gtfs, start_hour = 6, end_hour = 22,
                                    dow = c(1,1,1,1,1,0,0), by_route=FALSE)
post_stop_freq <- get_stop_frequency(post_sr_gtfs, start_hour = 6, end_hour = 22,
                                    dow = c(1,1,1,1,1,0,0), by_route=FALSE)

#test#pre_stopfreq_sf <- stops_df_as_sf(pre_sr_gtfs$stops_frequency_df)
pre_stop_freq$routes_frequency_df
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
#intersect stop buffer geometries with ACS demographic data
pre_stop_acs_int_white <- st_intersection(pre_stop_buffer, harris_trct_white_t)
post_stop_acs_int_white <- st_intersection(post_stop_buffer, harris_trct_white_t)
#add area attributes
pre_stop_acs_int_white$area <- st_area(pre_stop_acs_int_white)
post_stop_acs_int_white$area <- st_area(post_stop_acs_int_white)
#get new total population estimate value for new area by adding column
pre_stop_acs_int_white$pop_est_new <- pre_stop_acs_int_white$area*pre_stop_acs_int_white$pop_dens
post_stop_acs_int_white$pop_est_new <- post_stop_acs_int_white$area*post_stop_acs_int_white$pop_dens

#Population weighted mean metrics for entire system/population
pre_wmd_white <- weighted.mean(pre_stop_acs_int_white$departures, pre_stop_acs_int_white$pop_est_new, na.rm = FALSE)
#pre_wmd_white #population weighted mean before = 45.50 departures/day
pre_wmh_white <- weighted.mean(pre_stop_acs_int_white$headway, pre_stop_acs_int_white$pop_est_new, na.rm = FALSE)
#pre_wmh_white #population weighted mean headway before = 31.35 minutes of those within 400 m

post_wmd_white <- weighted.mean(post_stop_acs_int_white$departures, post_stop_acs_int_white$pop_est_new, na.rm = FALSE)
#post_wmd_white #population weighted mean after = 49.13 departures/day
post_wmh_white <- weighted.mean(post_stop_acs_int_white$headway, post_stop_acs_int_white$pop_est_new, na.rm = FALSE)
#post_wmh_white #population weighted mean headway before = 28.98 minutes of those within 400 m


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
#intersect stop buffer geometries with ACS demographic data
pre_stop_acs_int_black <- st_intersection(pre_stop_buffer, harris_trct_black_t)
post_stop_acs_int_black <- st_intersection(post_stop_buffer, harris_trct_black_t)
#add area attributes
pre_stop_acs_int_black$area <- st_area(pre_stop_acs_int_black)
post_stop_acs_int_black$area <- st_area(post_stop_acs_int_black)
#get new total population estimate value for new area by adding column
pre_stop_acs_int_black$pop_est_new <- pre_stop_acs_int_black$area*pre_stop_acs_int_black$pop_dens
post_stop_acs_int_black$pop_est_new <- post_stop_acs_int_black$area*post_stop_acs_int_black$pop_dens

#Population weighted mean metrics for entire system/population
pre_wmd_black <- weighted.mean(pre_stop_acs_int_black$departures, pre_stop_acs_int_black$pop_est_new, na.rm = FALSE)
#pre_wmd_black #population weighted mean before = 42.08 departures/day
pre_wmh_black <- weighted.mean(pre_stop_acs_int_black$headway, pre_stop_acs_int_black$pop_est_new, na.rm = FALSE)
#pre_wmh_black #population weighted mean headway before = 35.68 minutes of those within 400 m

post_wmd_black <- weighted.mean(post_stop_acs_int_black$departures, post_stop_acs_int_black$pop_est_new, na.rm = FALSE)
#post_wmd_black #population weighted mean after = 42.14 departures/day
post_wmh_black <- weighted.mean(post_stop_acs_int_black$headway, post_stop_acs_int_black$pop_est_new, na.rm = FALSE)
#post_wmh_black #population weighted mean headway before = 33.06 minutes of those within 400 m


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
#intersect stop buffer geometries with ACS demographic data
pre_stop_acs_int_asian <- st_intersection(pre_stop_buffer, harris_trct_asian_t)
post_stop_acs_int_asian <- st_intersection(post_stop_buffer, harris_trct_asian_t)
#add area attributes
pre_stop_acs_int_asian$area <- st_area(pre_stop_acs_int_asian)
post_stop_acs_int_asian$area <- st_area(post_stop_acs_int_asian)
#get new total population estimate value for new area by adding column
pre_stop_acs_int_asian$pop_est_new <- pre_stop_acs_int_asian$area*pre_stop_acs_int_asian$pop_dens
post_stop_acs_int_asian$pop_est_new <- post_stop_acs_int_asian$area*post_stop_acs_int_asian$pop_dens

#Population weighted mean metrics for entire system/population
pre_wmd_asian <- weighted.mean(pre_stop_acs_int_asian$departures, pre_stop_acs_int_asian$pop_est_new, na.rm = FALSE)
#pre_wmd_asian #population weighted mean before = 45.07 departures/day
pre_wmh_asian <- weighted.mean(pre_stop_acs_int_asian$headway, pre_stop_acs_int_asian$pop_est_new, na.rm = FALSE)
#pre_wmh_asian #population weighted mean headway before = 31.00 minutes of those within 400 m

post_wmd_asian <- weighted.mean(post_stop_acs_int_asian$departures, post_stop_acs_int_asian$pop_est_new, na.rm = FALSE)
#post_wmd_asian #population weighted mean after = 49.48 departures/day
post_wmh_asian <- weighted.mean(post_stop_acs_int_asian$headway, post_stop_acs_int_asian$pop_est_new, na.rm = FALSE)
#post_wmh_asian #population weighted mean headway before = 27.93 minutes of those within 400 m


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
#intersect stop buffer geometries with ACS demographic data
pre_stop_acs_int_latin <- st_intersection(pre_stop_buffer, harris_trct_latin_t)
post_stop_acs_int_latin <- st_intersection(post_stop_buffer, harris_trct_latin_t)
#add area attributes
pre_stop_acs_int_latin$area <- st_area(pre_stop_acs_int_latin)
post_stop_acs_int_latin$area <- st_area(post_stop_acs_int_latin)
#get new total population estimate value for new area by adding column
pre_stop_acs_int_latin$pop_est_new <- pre_stop_acs_int_latin$area*pre_stop_acs_int_latin$pop_dens
post_stop_acs_int_latin$pop_est_new <- post_stop_acs_int_latin$area*post_stop_acs_int_latin$pop_dens

#Population weighted mean metrics for entire system/population
pre_wmd_latin <- weighted.mean(pre_stop_acs_int_latin$departures, pre_stop_acs_int_latin$pop_est_new, na.rm = FALSE)
#pre_wmd_latin #population weighted mean before = 40.76 departures/day
pre_wmh_latin <- weighted.mean(pre_stop_acs_int_latin$headway, pre_stop_acs_int_latin$pop_est_new, na.rm = FALSE)
#pre_wmh_latin #population weighted mean headway before = 33.89 minutes of those within 400 m
post_wmd_latin <- weighted.mean(post_stop_acs_int_latin$departures, post_stop_acs_int_latin$pop_est_new, na.rm = FALSE)
#post_wmd_latin #population weighted mean after = 42.42 departures/day
post_wmh_latin <- weighted.mean(post_stop_acs_int_latin$headway, post_stop_acs_int_latin$pop_est_new, na.rm = FALSE)
#post_wmh_latin #population weighted mean headway before = 32.53 minutes of those within 400 m

#plot results in bar chart
data_wmh <-c(pre_wmh_white, post_wmh_white,pre_wmh_black, post_wmh_black, 
              pre_wmh_asian, post_wmh_asian, pre_wmh_latin, post_wmh_latin)
data_wmd <-c(pre_wmd_white, post_wmd_white,pre_wmd_black, post_wmd_black, 
             pre_wmd_asian, post_wmd_asian, pre_wmd_latin, post_wmd_latin)
before_after <-c('Before', 'After', 'Before', 'After', 'Before', 'After', 'Before', 'After')
race <-c('White', 'White', 'Black', 'Black', 'Asian', 'Asian', 'LatinX', 'LatinX')

final.data <-data.frame(race, before_after, data_wmh, data_wmd)

plot_wmh <- barplot(final.data$data_wmh, names.arg = final.data$race, xlab = "Race", 
        ylab = "Weighted Mean Headway (Minutes)", 
        col = c("red","green"), 
        main = "Weighted Average Transit Headway By Race Before/After System Reimagining", 
        border = "black",
        ylim = c(0, 46))
legend("topright", c("Before", "After"), fill = c("red", "green"))
text(plot_wmh, final.data$data_wmh + 2*sign(final.data$data_wmh), labels=round(final.data$data_wmh, 2), xpd=TRUE)

plot_wmd <- barplot(final.data$data_wmd, names.arg = final.data$race, xlab = "Race", 
                    ylab = "Weighted Mean Daily Departures", 
                    col = c("orange","blue"), 
                    main = "Weighted Average Transit Departures By Race Before/After System Reimagining", 
                    border = "black",
                    ylim = c(0, 60))
legend("topright", c("Before", "After"), fill = c("orange", "blue"))
text(plot_wmd, final.data$data_wmd + 2*sign(final.data$data_wmd), labels=round(final.data$data_wmd, 2), xpd=TRUE)

