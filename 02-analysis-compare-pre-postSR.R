# This script compares pre- and post-System Reimagining routes to assess
# changes in the number and location of high-frequency routes.
# You must run 01-download-GTFS-R before running this script. 

library(dplyr)
library(ggplot2)
library(viridis)

routes_pre <- inner_join(pre_sr_gtfs$routes_frequency_df, pre_sr_gtfs$routes_df)
routes_pre$time <- "before"

routes_post <- inner_join(post_sr_gtfs$routes_frequency_df, post_sr_gtfs$routes_df)
routes_post$time <- "after"

routes <- rbind(routes_pre, routes_post) 

# Density plot of frequencies under both service configurations
ggplot(routes) + 
  geom_density(aes(mean_headways, fill = time), alpha = 0.5) + 
  xlab("mean headways (min)") + 
  scale_fill_viridis(discrete = TRUE, end = 0.85) + 
  coord_cartesian(xlim = c(15, 100)) + 
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank())


