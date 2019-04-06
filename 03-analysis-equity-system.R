# This script assesses the equity impacts of SR using simple measures based
# on proximity to transit service. For more information about these methods and
# their limitations, see Karner, A. and A. Golub (2015). "Comparison of two 
# common approaches to public transit service equity evaluation." Transportation 
# Research Record: Journal of the Transportation Research Board 2531: 170-179.
# https://trrjournalonline.trb.org/doi/abs/10.3141/2531-20

library(tidycensus)
library(sf)
library(ggplot2)
library(ggthemes)
census_api_key("3a277651e6ec078927e10356269269b3698a0cfa")

# Define a map-appropriate ggplot theme
# See: http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
no_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
  )

# Since the System Reimagining affected most routes in the METRO network, 
# simply identifying routes that were affected positively or negatively is 
# difficult. 
# Here, we employ changes in average headways at the stop level as a 
# basis for comparison.

stops_pre <- inner_join(pre_sr_gtfs$stops_frequency_df, 
                        pre_sr_gtfs$stops_df)

stops_post <- post_sr_gtfs$stops_frequency_df

pre_coords <- as.data.frame(st_coordinates(pre_sr_gtfs$stops_sf))
post_coords <- as.data.frame(st_coordinates(post_sr_gtfs$stops_sf))




# Get demographic data from the ACS
hlstatusvars <- c("B03002_003", # white alone
                  "B03002_004", # black alone
                   "B03002_006", # Asian
                   "B03002_012") # Hispanic or Latino
          
harris_trct_race <- get_acs(geography = "tract", variables = hlstatusvars, 
                        state = "TX", year = 2015,
                        county = "Harris County",
                        geometry = TRUE,
                        summary_var = "B03002_001") 

harris_trct_race <- st_transform(harris_trct_race, "+init=epsg:32614")


ggplot() + 
  geom_sf(data = harris_trct_race) + 
  geom_hex(data = pre_coords, aes(x = X, y = Y)) +
  xlab(NULL) + ylab(NULL) + 
  no_axes
  


  #geom_sf(data = , color = "blue") + 
  #geom_sf(data = post_sr_gtfs$stops_sf, color = "green")
